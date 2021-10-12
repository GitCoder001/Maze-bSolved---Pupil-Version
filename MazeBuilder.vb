Imports System.Runtime.InteropServices ' used for console maximisation
Module MazeBuilder
    ' this is a skeleton program for pupils to built onto with a maze solving algorithm
    ' it includes a maze building tool (separate module)
    ' maze sizes can be set in the maze files (and maker)
    ' additional challenge is to build a maze generator
    ' could support mazes with different shapes, but to keep simple, expects a rectangular maze

    ' (Note:  Maze structure changed from Boolean to Char to extend functionality and allow future maze size changes)
    ' The constants below are used for different maze elements.  Could change to ENUM to allow conciseness.  
    ' Changing the top values will affect how the file is read in. Change the bottom values to affect the output only

    ' You need to add your A* (or alternative algorithm) to the MazeSolve module

    Public Const MazeBorder As Char = "B"
    Public Const MazeWall As Char = "W"
    Public Const MazePath As Char = " "
    Public Const MazeTravelledPath = "."
    ' These can be changed to affect how they are shown on the screen (edit mode)
    Const BorderPiece As Short = &H2588 ' the ASCII piece to use for border wall (in HEX) for edit mode (maze solver module will use correct wall pieces at runtime)
    Const WallPiece As Short = &H2593 ' ASCII code for wall piece
    Const PathPiece As Short = &H20 ' ASCII code for area where player can travel
    ' PathTravelPiece is not shown in edit mode, so no need for inclusion here

#Region "General Globals"
    ' note: all values assume 0 as starting point for maze
    Public Class Position
        Public x As Short
        Public y As Short
        Public Sub New(XVal As Short, YVal As Short)
            x = XVal
            y = YVal
        End Sub
        Public Sub New(pos As Position)
            ' this is needed as overriding the = means object assignment is persisting a pointer rather than static values
            x = pos.x
            y = pos.y
        End Sub
        ' need to override = and <> so we can compare two positions
        Public Shared Operator =(c1 As Position, c2 As Position)
            Return c1.x = c2.x And c1.y = c2.y
        End Operator
        Public Shared Operator <>(c1 As Position, c2 As Position)
            Return Not (c1.x = c2.x And c1.y = c2.y)
        End Operator
        Public Overrides Function ToString() As String
            Return x & "," & y
        End Function
    End Class
    Public Class UndoItem
        ' allows stack to be populated for the undo list
        Public GridRef As Position
        Public GridItem As Char
        Sub New(GridLocation As Position, Item As Char)
            GridRef = GridLocation
            GridItem = Item
        End Sub
    End Class

    Dim Maze(,) As Char ' each block in the maze - size initialised later when created, loaded, etc
    Const NotSetValue As Short = -1 ' ToDo: is this still needed? Haven't needed it yet
    Const FileExtension As String = ".maze"
    Public MazeFilePath As String = IO.Directory.GetCurrentDirectory & "\"

    Dim DrawOffset As (Short, Short) = (0, 2) ' create a Tuple to set where the maze is drawn.  Screen coordinates are translated to the map array
    Dim MazeLoaded As Boolean = False ' set to true if editing a maze
    Dim MazeEditsSaved As Boolean = True ' set to false when any edit has been made until saved

    Dim MaxMaze As New Tuple(Of Short, Short)(120, 70) ' tuple representing maximum maze size (constants do not run well with tuples)
    Dim MazeWidth As Short = 80 ' width of maze with default values
    Dim MazeHeight As Short = 50 ' height of maze
    Dim MinimumMazeSize As (Short, Short) = (3, 3)
    Dim MazeStart As New Position(NotSetValue, NotSetValue)   'position of maze start
    Dim MazeEnd As New Position(NotSetValue, NotSetValue) ' position (x,y) of finish point

#End Region

    Sub Main()
        ' Set up console environment
        Win32Native.SetConsoleWindowPosition(0, 0) ' move console to top left position
        Win32Native.MaximizeConsoleWindow() ' maximise console to full screen
        Console.OutputEncoding = Text.Encoding.UTF8 ' ensure that compatible unicode characters can be displayed

        Console.Clear()
        Menu()
    End Sub
    Sub Menu()
        Do ' stays here until control returned back to calling module
            Console.Clear()
            ClearKB()
            Console.WriteLine(If(MazeLoaded, "Maze loaded", "No maze loaded") & vbCrLf)
            Console.WriteLine("Menu" & vbCrLf)
            ' grey out options that are not valid without loaded maze
            Console.ForegroundColor = If(MazeLoaded, ConsoleColor.White, ConsoleColor.DarkGray)
            Console.WriteLine("0 - Solve" & vbCrLf)

            Console.ForegroundColor = ConsoleColor.White
            Console.WriteLine("1 - Create maze (overwrites current maze)")

            Console.ForegroundColor = If(MazeLoaded, ConsoleColor.White, ConsoleColor.DarkGray)
            Console.WriteLine("2 - Save maze")
            Console.ForegroundColor = ConsoleColor.White

            Console.WriteLine("3 - Load maze")
            Console.ForegroundColor = If(MazeLoaded, ConsoleColor.White, ConsoleColor.DarkGray)
            Console.WriteLine("4 - Edit maze" & vbCrLf)
            Console.ForegroundColor = ConsoleColor.White

            Console.WriteLine("5 - Exit program" & vbCrLf)
            Dim opt As Short = GetValue(0, 5)
            Select Case opt
                Case 0
                    If MazeLoaded Then MazeSolve.Main(Maze, MazeStart, MazeEnd) ' call the MazeSolve module
                Case 1
                    CreateMaze()
                Case 2
                    If MazeLoaded Then Save()
                Case 3
                    Load()
                Case 4
                    If MazeLoaded Then EditMaze()
                Case 5
                    Quit()
            End Select
        Loop
    End Sub
    Sub CreateMaze()

        ' if there's maze data loaded, warn
        If MazeLoaded Or MazeEditsSaved = False Then
            Console.WriteLine("This action will wipe current data and create a new, empty maze. 1 to cancel, 2 to proceed")
            If GetValue(1, 2) = 1 Then Return
        End If

        ' get maze dimensions
        Console.Clear()
        Console.WriteLine($"Please set X value (currently set to {MazeWidth})")
        MazeWidth = GetValue(MinimumMazeSize.Item1, MaxMaze.Item1)

        Console.Clear()
        Console.WriteLine($"Please set Y value (currently set to {MazeHeight})")
        MazeWidth = GetValue(MinimumMazeSize.Item2, MaxMaze.Item2)

        ' Initialises maze array (using dimensions)
        ReDim Maze(MazeWidth - 1, MazeHeight - 1)
        ' initialise all elements to false except for outline
        For Y As Short = 0 To MazeHeight - 1
            For X As Short = 0 To MazeWidth - 1
                If (X = 0 Or X = MazeWidth - 1) Or (Y = 0 Or Y = MazeHeight - 1) Then
                    Maze(X, Y) = MazeBorder
                Else
                    Maze(X, Y) = MazePath
                End If
            Next
        Next
        MazeStart.x = 2
        MazeStart.y = 0
        MazeEnd.x = MazeWidth - 3 '(-3 offsets 1 block to left of border)
        MazeEnd.y = MazeHeight - 1
        MazeLoaded = True
    End Sub

    Sub EditMaze()
        ' main sub that allows maze editing
        ' maze must either be loaded or initialized
        Dim Done As Boolean = False
        Dim UnDoStack As New Stack(Of UndoItem) ' used to enable undo mode - when implemented
        Dim EditPosition As New Position(1, 1) ' tracks where edit cursor currently is in relation to ARRAY NOT SCREEN
        Dim PenDown As Boolean = False ' if down, moving cursor will paint current tile
        Dim PaintWall As Boolean = True ' toggles between wall and path (if adding other objects, this logic needs removal)
        Dim KeyPress As ConsoleKeyInfo ' stores key pressed
        Dim NeedPaint As Boolean = False ' if true, will paint current position

        Console.Clear()
        Console.CursorVisible = True ' used for highlighting position
        Console.CursorSize = 100 ' maximise cursor size to show position

        ShowEditBar(PenDown, PaintWall)
        ShowMaze()
        Console.SetCursorPosition(EditPosition.x + DrawOffset.Item1, EditPosition.y + DrawOffset.Item2) ' make cursor relate to screen
        Do
            If Console.KeyAvailable Then ' prevents sticking in loop (only needed if needing to multitask)
                NeedPaint = False ' will get set to true if needed
                KeyPress = Console.ReadKey(True)

                Select Case KeyPress.Key
                    ' need to allow cursor to travel into border for setting start and end position (if required there)
                    Case ConsoleKey.UpArrow ' move
                        If EditPosition.y >= 1 Then ' do not allow cursor out of known border
                            EditPosition.y -= 1
                        End If
                        ' paint item onto maze and screen (will ignore if within outside border)
                        NeedPaint = True

                    Case ConsoleKey.DownArrow ' move
                        If EditPosition.y < MazeHeight - 1 Then ' do not allow cursor out of known border
                            EditPosition.y += 1
                        End If
                        ' paint item onto maze and screen (will ignore if within outside border)
                        NeedPaint = True

                    Case ConsoleKey.LeftArrow ' move
                        If EditPosition.x >= 1 Then ' do not allow cursor out of known border
                            EditPosition.x -= 1
                        End If
                        ' paint item onto maze and screen (will ignore if within outside border)
                        NeedPaint = True

                    Case ConsoleKey.RightArrow ' move
                        If EditPosition.x < MazeWidth - 1 Then ' do not allow cursor out of known border
                            EditPosition.x += 1
                        End If
                        ' paint item onto maze and screen (will ignore if within outside border)
                        NeedPaint = True

                    Case ConsoleKey.Escape ' exit editor
                        If UnDoStack.Count > 0 Then MazeEditsSaved = False ' flag that there are unsaved edits in maze
                        Exit Sub

                    Case ConsoleKey.Spacebar ' toggle pen mode
                        PenDown = Not PenDown ' toggle status
                        NeedPaint = True

                    Case ConsoleKey.S ' set start point
                        ' check not in same position as finish point
                        ' currently cannot be undone
                        If Not (EditPosition = MazeStart Or EditPosition = MazeEnd) Then
                            MazeStart = New Position(EditPosition)
                            ShowMaze() ' easier to refresh maze than repaint
                        End If

                    Case ConsoleKey.E ' set end point
                        ' check not in same position as start point
                        ' currently cannot be undone
                        If Not (EditPosition = MazeStart Or EditPosition = MazeEnd) Then
                            MazeEnd = New Position(EditPosition)
                            ShowMaze() ' easier to refresh maze than repaint
                        End If

                    Case ConsoleKey.F ' fill maze with wall object
                        ' check if changes need saving
                        Console.SetCursorPosition(0, 0)
                        Console.WriteLine("This will overwrite all cells in the maze with wall objects and cannot be undone.  1 to cancel, 2 to proceed")
                        If GetValue(1, 2) = 2 Then
                            Fill()
                            UnDoStack.Clear() ' clear stack of undo items
                        End If
                        Console.Clear() ' wipe screen clear
                        ShowMaze()

                    Case ConsoleKey.P ' paint path
                        If PaintWall Then
                            PaintWall = False
                            NeedPaint = True
                        End If

                    Case ConsoleKey.W ' paint wall
                        If Not PaintWall Then
                            PaintWall = True
                            NeedPaint = True
                        End If

                    Case ConsoleKey.Z ' check undo
                        If KeyPress.Modifiers And ConsoleModifiers.Control Then ' control also pressed (needs this method when using select case)
                            ' undo
                            If UnDoStack.Count > 0 Then ' there are items in the undo list
                                Dim temp As UndoItem = UnDoStack.Pop
                                Maze(temp.GridRef.x, temp.GridRef.y) = temp.GridItem ' restore maze data
                                ' now re-paint maze to old value
                                PaintItem(New Position(temp.GridRef.x + DrawOffset.Item1, temp.GridRef.y + DrawOffset.Item2), temp.GridItem)

                                ' ToDo: extend this so that it can check for start and end point - not difficult but equally not a simple fix
                            End If
                        End If

                End Select

                If NeedPaint Then ' called if an update is needed to maze (only if pen is down)
                    If PenDown Then Paint(UnDoStack, EditPosition, New Position(EditPosition.x + DrawOffset.Item1, EditPosition.y + DrawOffset.Item2), PaintWall)
                End If
                ' show edit bar (in case anything changed) and then re-position cursor
                ShowEditBar(PenDown, PaintWall) ' show edit bar
                Console.SetCursorPosition(EditPosition.x + DrawOffset.Item1, EditPosition.y + DrawOffset.Item2) ' make cursor relate to screen
            End If
        Loop Until Done
    End Sub

    Sub Save()
        Dim File As String = "" ' the chosen file name
        Dim line As New Text.StringBuilder ' the maze line of chars for file writing
        Dim Writer As IO.StreamWriter = Nothing ' defined outside of try catch to allow the file to be closed in finally (or scope will not be visible)

        Console.Clear()
        ShowFiles() ' don't case about return value - treat like subroutine
        Console.WriteLine("Please enter a name for the maze. You can omit the " & FileExtension)
        Console.WriteLine("NOTE: Duplicate names will overwrite previous file automatically")

        File = Console.ReadLine ' get filename

        If File = "" Then Return ' exit if nothing entered

        ' check if user entered file extention, if not add it
        If Not (File.Length > FileExtension.Length AndAlso LCase(Right(File, FileExtension.Length)) = LCase(FileExtension)) Then ' uses short-circuiting logical conjunction to avoid crash if len<file ext
            File &= FileExtension
        End If

        Try
            ' concatenates each row into a string and writes that line
            ' there are other ways of achieving this, such as using New String(char array) but it's messy with 2-d arrays
            Writer = New IO.StreamWriter(MazeFilePath & File)
            Writer.WriteLine(MazeStart.ToString)
            Writer.WriteLine(MazeEnd.ToString)
            For Y As Short = 0 To MazeHeight - 1
                line.Clear()
                For X As Short = 0 To MazeWidth - 1
                    line.Append(Maze(X, Y)) ' stringbuilder is far more efficient when concatenating
                Next
                Writer.WriteLine(line.ToString)
            Next
            MazeEditsSaved = True ' note that file has saved
            Console.WriteLine("Save successful. Press any key to continue")
            Console.ReadKey()
        Catch ex As Exception
            Console.WriteLine("Error writing file. Please check file path.  Press any key to continue.")
            Console.WriteLine("Exact error: " & ex.ToString)
            Console.ReadKey()
            Return
        Finally
            If Writer.BaseStream IsNot Nothing Then Writer.Close() 'close file if stream object successfully created
        End Try

    End Sub
    Sub Load()
        ' loads a maze into the array
        ' load procedure will determine the maze size
        ' file structure consists of start position (x,y), end position (x,y) and successive rows of binary data for the maze
        Dim file As String ' the filename that is required to be loaded
        Dim line As String ' used to temporarily store some data from file read
        Dim reader As IO.StreamReader = Nothing ' used to read the text file
        Dim MazeLines As New List(Of String) ' stores maze data as being read. Enables calculation on how to re-dimension the maze array
        Dim ValidMazeFile As Boolean = False ' true once valid file given

        ' keep looping until valid file given or user wants to abandon
        Do
            ' call ShowFiles to show user what maze files currently exist
            Console.Clear()
            If ShowFiles() = False Then ' there were no files found
                Console.WriteLine("Nothing to load.  Press any key to continue")
                Console.ReadKey()
                Return
            End If

            Console.WriteLine($"{vbCrLf}Please enter file name to load (you can omit the {FileExtension}). Do not enter anything to abort.")
            file = Console.ReadLine

            If file = "" Or file = FileExtension Then Return ' abandon load

            ' check if user entered extension
            If Not (file.Length > FileExtension.Length AndAlso LCase(Right(file, FileExtension.Length)) = LCase(FileExtension)) Then ' uses short-circuiting logical conjunction to avoid crash if len<file ext
                ' extension not added, so add it before loading
                file &= FileExtension
            End If

            ' does file exist?
            If IO.File.Exists(MazeFilePath & file) = False Then
                Console.WriteLine("Maze file does not exist, please try again.  Press key to continue.")
                Console.ReadKey()
            Else
                ValidMazeFile = True
            End If
        Loop Until ValidMazeFile ' loop until select file that exists

        ' attempt to load the file
        Try
            reader = New IO.StreamReader(MazeFilePath & file) ' initialise the stream
            line = reader.ReadLine
            MazeStart.x = CShort(line.Split(",")(0)) ' split returns an array, so get first item (left most string) from this array.  This is not very efficient
            MazeStart.y = CShort(line.Split(",")(1)) ' get the right string

            line = reader.ReadLine
            MazeEnd.x = CShort(line.Split(",")(0)) ' split returns an array, so get first item (left most string) from this array.  This is not very efficient
            MazeEnd.y = CShort(line.Split(",")(1)) ' get the right string

            ' now read array and populate temporary list
            Do While Not reader.EndOfStream
                MazeLines.Add(reader.ReadLine)
            Loop
        Catch ex As Exception
            Console.WriteLine("Error reading file. Please check file path or possible corruption. Press any key to continue")
            Console.WriteLine("Exact error: " & ex.ToString)
            Console.ReadKey()
            Return
        Finally
            If reader.BaseStream IsNot Nothing Then reader.Close() 'close file if stream object still open
        End Try

        ' now split data and populate maze array.  First determine maze size

        ' set the known width and height of this maze
        MazeWidth = MazeLines(0).Length ' set the known width and height of this maze
        MazeHeight = MazeLines.Count
        ReDim Maze(MazeWidth - 1, MazeHeight - 1) ' account for lbound 0

        ' load the data into the array
        For y = 0 To MazeLines.Count - 1
            For x = 0 To MazeWidth - 1 ' dies not support jagged array so each line should NOT be different length
                Maze(x, y) = MazeLines(y)(x) ' strip each character out into maze array
            Next
        Next
        MazeLoaded = True
    End Sub
    Sub ClearKB()
        ' sub to clear keyboard buffer
        While Console.KeyAvailable
            Console.ReadKey()
        End While
    End Sub
    ' helper subs/functions
    ' these are not part of the menu, but assist in the program's functionality
    Sub ShowEditBar(PenDown As Boolean, WallMode As Boolean)
        ' will show the editing toolbar
        Console.ForegroundColor = ConsoleColor.Yellow
        Console.BackgroundColor = If(WallMode, ConsoleColor.DarkYellow, ConsoleColor.Black) ' highlight either wall or path mode
        Console.SetCursorPosition(0, 0)
        Console.Write("W: Paint Wall;")

        Console.SetCursorPosition(15, 0)
        Console.BackgroundColor = If(Not WallMode, ConsoleColor.DarkYellow, ConsoleColor.Black)
        Console.Write("P: Paint Path;")

        Console.BackgroundColor = ConsoleColor.Black
        Console.SetCursorPosition(30, 0)
        Console.Write("Esc]: Exit; S: Set Start; E: Set End")

        Console.SetCursorPosition(0, 1)
        Console.ForegroundColor = If(PenDown, ConsoleColor.Green, ConsoleColor.Red)
        Console.Write($"Pen {If(PenDown, "down", "up")}")
        Console.ForegroundColor = ConsoleColor.Yellow
        Console.Write(" ([space] to toggle)  ")

        Console.ForegroundColor = ConsoleColor.White
    End Sub
    Sub Fill()
        ' Fills a maze with wall pieces avoiding outer border
        For Y As Short = 1 To MazeHeight - 2
            For X As Short = 1 To MazeWidth - 2
                Maze(X, Y) = MazeWall
            Next
        Next

    End Sub
    Sub Paint(ByRef UndoBuffer As Stack(Of UndoItem), MazePosition As Position, ScreenPosition As Position, WallMode As Boolean)
        ' responsible for drawing on the maze - used for edit mode
        ' params needed as makes no assumption about where to edit maze array or paint on screen coordinates (maximum flexibility)
        ' separate sub as logic is too complex to repeat 4 times for each arrow move

        ' create item piece which is what we need to paint.  Separated in logic in case extend items that can be painted in future
        Dim item As Char = If(WallMode, MazeWall, MazePath)

        ' first check that paint request is NOT within outside border (if it is, ignore) OR what is there already
        If (MazePosition.x = 0 Or MazePosition.x = MazeWidth - 1) Or
            (MazePosition.y = 0 Or MazePosition.y = MazeHeight - 1) OrElse ' short citcuit to save time
            (Maze(MazePosition.x, MazePosition.y) = item) Then ' lookup current mode's paint with maze grid contents
            Return ' ignore paint request, within outside border or not different piece to existing
        End If

        ' at this point, we are not in the border area and the painted piece is different to existing piece
        UndoBuffer.Push(New UndoItem(New Position(MazePosition), Maze(MazePosition.x, MazePosition.y))) ' add existing maze grid item to undo stack
        Maze(MazePosition.x, MazePosition.y) = item ' change item in maze
        PaintItem(ScreenPosition, item) ' update screen imagry
    End Sub

    Sub PaintItem(ScreenPosition As Position, item As Char)
        ' takes a screen position and the maze item and detremines how to paint this onto the screen
        Console.SetCursorPosition(ScreenPosition.x, ScreenPosition.y)
        If item = MazeBorder Then
            Console.Write(ChrW(BorderPiece))
        ElseIf item = MazeWall Then
            Console.Write(ChrW(WallPiece))
        ElseIf item = MazePath Then
            Console.Write(ChrW(PathPiece))
        End If
    End Sub
    Sub Quit()
        ' check if edits or end
        If MazeEditsSaved = False Then
            Console.WriteLine("You have edits that are unsaved. Press 1 to return, 2 to quit")
            If GetValue(1, 2) = 1 Then
                Return
            End If
        End If
        End
    End Sub
    Function ShowFiles()
        ' will show all maze files (from current cursor position)
        ' could be re-written easily to map horizontally rather than just vertically using string padding and file name length control
        ' returns FALSE if there are no files found
        ' ToDo: put this into a TRY CATCH to eliminate possible errors
        Dim files As String()
        Console.WriteLine("Path: " & MazeFilePath)
        Console.WriteLine("Existing file(s):")
        files = IO.Directory.GetFiles(MazeFilePath, "*" & FileExtension)

        If files.Length = 0 Then
            Console.WriteLine("No .maze files found")
            Return False
        End If
        Console.ForegroundColor = ConsoleColor.Yellow
        For Each file As String In files
            Console.WriteLine(IO.Path.GetFileName(file))
        Next

        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine($"{vbCrLf}{files.Length} file{If(files.Length > 1, "s", "")} found" & vbCrLf)

        Return True
    End Function
    Sub ShowMaze()
        ' displays the maze (in edit mode), using screen ofsets
        For Y As Short = 0 To MazeHeight - 1
            For X As Short = 0 To MazeWidth - 1
                PaintItem(New Position(X + DrawOffset.Item1, Y + DrawOffset.Item2), Maze(X, Y))
            Next
        Next
        Console.SetCursorPosition(MazeStart.x + DrawOffset.Item1, MazeStart.y + DrawOffset.Item2)
        Console.Write("S")

        Console.SetCursorPosition(MazeEnd.x + DrawOffset.Item1, MazeEnd.y + DrawOffset.Item2)
        Console.Write("E")

    End Sub
    Function GetValue(min As Short, max As Short) As Short
        ' validates user input to get a value
        Dim Value As String
        Dim valid As Boolean = False
        Console.WriteLine($"Please enter a value between {min} and {max}")
        Do

            Value = Console.ReadLine
            If IsNumeric(Value) Then
                If CInt(Value) >= min And CInt(Value) <= max Then
                    valid = True
                Else
                    Console.WriteLine($"Please ensure value is between {min} and {max}. Please try again.")
                End If
            Else
                Console.WriteLine("Input must be a number.  Please try again.")
            End If
        Loop Until valid
        Return CShort(Value)
    End Function
    Public Class Win32Native
        ' class handles the Win32 pointers to control the console window as no native support in .NET for this
        Private Const SWP_NOZORDER As Integer = &H4
        Private Const SWP_NOACTIVATE As Integer = &H10
        Private Const SW_MAXIMIZE As Integer = &H3

        <StructLayout(LayoutKind.Sequential)>
        Private Structure RECT
            Public Left As Integer
            Public Top As Integer
            Public Right As Integer
            Public Bottom As Integer
        End Structure

        <DllImport("kernel32")>
        Private Shared Function GetConsoleWindow() As IntPtr
        End Function

        <DllImport("user32")>
        Private Shared Function SetWindowPos(hWnd As IntPtr, hWndInsertAfter As IntPtr,
        x As Integer, y As Integer, cx As Integer, cy As Integer, flags As Integer) As Boolean
        End Function

        <DllImport("user32.dll")>
        Private Shared Function GetWindowRect(ByVal hWnd As IntPtr, ByRef lpRect As RECT) As Boolean
        End Function

        <DllImport("user32.dll")>
        Public Shared Function ShowWindow(hWnd As IntPtr, cmdShow As Integer) As Boolean
        End Function

        Public Shared Sub SetConsoleWindowPosition(x As Integer, y As Integer)
            Dim r As RECT
            GetWindowRect(GetConsoleWindow(), r)

            SetWindowPos(GetConsoleWindow(), IntPtr.Zero,
                     x, y,
                     r.Left + r.Right, r.Top + r.Bottom,
                     SWP_NOZORDER Or SWP_NOACTIVATE)
        End Sub

        Public Shared Sub MaximizeConsoleWindow()
            Dim p As Process = Process.GetCurrentProcess()
            ShowWindow(p.MainWindowHandle, SW_MAXIMIZE)
        End Sub
    End Class
End Module
