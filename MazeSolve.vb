Module MazeSolve
#Region "Globals"
    ' values are set from the Solve method when called
    Dim Maze(,) As Char
    Dim MazeWidth As Short ' width of maze with d
    Dim MazeHeight As Short  ' height of maze
    Dim MazeStart As Position  'position of maze start
    Dim MazeEnd As Position 'position (x,y) of finish point
    Public Enum CellType ' ASCII values for wall pieces (uses extended ASCII)
        Outer = &H2588
        Path = &H20
        Travelled = &H25CF ' could also use 2736 (star) or 25CA (diamond)
        InnerBlock = &H2593 ' used for double wall sections (when implemented)
        Unknown = &H20 ' used when maze item is unknown
        HorizontalOnly = &H2501
        VerticalOnly = &H2503
        Xroads = &H2548
        TUp = &H253B
        TDown = &H2533
        TLeft = &H252B
        TRight = &H2523
        CornerTL = &H251B
        CornerTR = &H2517
        CornerBL = &H2513
        CornerBR = &H250F
    End Enum
    Sub InitialiseWallDict()
        WallLookup.Clear()
        WallLookup.Add("01110", CellType.HorizontalOnly)
        WallLookup.Add("10101", CellType.VerticalOnly)
        WallLookup.Add("11111", CellType.Xroads)
        WallLookup.Add("11110", CellType.TUp)
        WallLookup.Add("01111", CellType.TDown)
        WallLookup.Add("11101", CellType.TLeft)
        WallLookup.Add("10111", CellType.TRight)
        WallLookup.Add("11100", CellType.CornerTL)
        WallLookup.Add("10110", CellType.CornerTR)
        WallLookup.Add("01101", CellType.CornerBL)
        WallLookup.Add("00111", CellType.CornerBR)
        WallLookup.Add("01100", CellType.HorizontalOnly)
        WallLookup.Add("00110", CellType.HorizontalOnly)
        WallLookup.Add("10100", CellType.VerticalOnly)
        WallLookup.Add("00101", CellType.VerticalOnly)
        WallLookup.Add("XXXXX", CellType.InnerBlock)

    End Sub

    '  1   - wall lookup is calculated taking the 5 points in a given grid to determine the shape
    ' 234    e.g. 01110 is a solid horizontal line
    '  5
    ' this will then point to the appropriate enum.  Could have set dictionary to point to ASCII calue, but
    ' less intuitive to draw manually, or to change ASCII symbols
    Dim WallLookup As New Dictionary(Of String, CellType)
#End Region

    Sub Main(MazeArray(,) As Char, StartPoint As MazeBuilder.Position, EndPoint As MazeBuilder.Position)
        ' this is the main calling procedure
        ' takes in a 2-d char array 
        Maze = MazeArray
        MazeWidth = Maze.GetLength(0) - 1 ' get length needs dimension value passing in
        MazeHeight = Maze.GetLength(1) - 1 ' the height is stored in the second dimension
        MazeStart = StartPoint
        MazeEnd = EndPoint

        InitialiseWallDict() ' set dictionary with wall types (needed when drawing maze)
        ShowMaze() ' display the maze
        Solve() ' solve the maze
        Console.ReadKey() ' pause before exiting module
    End Sub
    Public Sub ShowMaze()
        ' display the maze (doesn't use offsets)
        Console.Clear()

        For Y As Short = 0 To MazeHeight
            For X As Short = 0 To MazeWidth
                Console.SetCursorPosition(X, Y)
                ' check which type of grid square this is
                Select Case Maze(X, Y)
                    Case MazeBuilder.MazePath
                        Console.WriteLine(ChrW(CellType.Path))
                    Case MazeBuilder.MazeBorder
                        Console.WriteLine(ChrW(CellType.Outer))
                    Case MazeBuilder.MazeWall
                        Console.WriteLine(ChrW(ReturnWallType(X, Y)))
                    Case MazeBuilder.MazeTravelledPath
                        Console.ForegroundColor = ConsoleColor.Green
                        Console.WriteLine(ChrW(CellType.Travelled))
                        Console.ForegroundColor = ConsoleColor.White
                End Select
            Next
        Next
        Console.SetCursorPosition(MazeStart.x, MazeStart.y)
        Console.Write("S")

        Console.SetCursorPosition(MazeEnd.x, MazeEnd.y)
        Console.Write("E")
    End Sub
    Sub Solve()
        ' #####################################
        ' # WRITE YOUR SOLVING ALGORITHM HERE #
        ' #####################################

        ' instructions:
        ' Maze is an array of chars(x,y) with a given size MazeWidth and MazeHeight
        ' mazes are loaded in from calling module, from a file or edited from scratch

        ' Each maze grid reference will be one of several chars (see MazeBuilder initialisation for definitive list)
        ' e.g. 'B' indicated maze border, while 'W' shows a wall. The default char for the maze path is a space
        ' IMPORTANT: The defining chars (above) should be checked against the constant variable, rather than literal char
        ' e.g. if maze(x,y) = MazeBorder, or if maze(x,y) = MazePath NOT if maze(x,y)="W"c

        ' A maze can be expected to have narrow walls (i.e. All paths run in a single row or column flanked by wall/border pieces unless corner)

        ' You can determine path by updating the maze grid cell with a MazeTravelledPath char.  
        ' Should you use the latter, the maze show will highlight this automatically in green

        ' Start and End positions are always assumed to be down (e.g. direction of travel) unless on side wall.  However, as long as there is a clear path away from these
        ' then your solving algorithm only need to search for this point in the maze as it isn't a physical grid item but start/destination point

        ' Finally, the start and end points are given as separate variables (as they could be anywhwre on the maze - not just the border)
        ' These coordinates are found in MazeStart and MazeEnd each having an x and y property


    End Sub
    Function ReturnWallType(x As Short, y As Short) As Short
        ' takes a cell position and determines what type of wall piece should be drawn
        ' it will compare surrounding cells and select the appropriate ASCII graphic

        ' checks each part of grid individually to form a 5 char pattern.  The pattern then gets looked up in dictionary to select appropriate wall piece

        Dim LookupString(4) As Char
        'Dim ReturnValue As Short ' this is the value

        ' top
        If y > 0 AndAlso (Maze(x, y - 1) = MazeBuilder.MazeWall Or Maze(x, y - 1) = MazeBuilder.MazeBorder) Then
            LookupString(0) = "1"c
        Else
            LookupString(0) = "0"c
        End If

        ' centre left
        If x > 0 AndAlso (Maze(x - 1, y) = MazeBuilder.MazeWall Or Maze(x - 1, y) = MazeBuilder.MazeBorder) Then
            LookupString(1) = "1"c
        Else
            LookupString(1) = "0"c
        End If

        ' centre
        LookupString(2) = "1"c ' this wouldn't be called if the grid tile wasn't a wall piece

        ' centre right
        If x < MazeWidth - 1 AndAlso (Maze(x + 1, y) = MazeBuilder.MazeWall Or Maze(x + 1, y) = MazeBuilder.MazeBorder) Then
            LookupString(3) = "1"c
        Else
            LookupString(3) = "0"c
        End If

        ' bottom
        If y < MazeHeight - 1 AndAlso (Maze(x, y + 1) = MazeBuilder.MazeWall Or Maze(x, y + 1) = MazeBuilder.MazeBorder) Then
            LookupString(4) = "1"c
        Else
            LookupString(4) = "0"c
        End If

        ' check if 4 cells filled (doesn't matter where) and override item type with "xxxxx"
        ' to save lots of statements, will look for 3 1s in lookupstring first then specific maze section
        ' upper-left block check
        If (LookupString(0) + LookupString(1) + LookupString(2) = "111") AndAlso Maze(x - 1, y - 1) = MazeBuilder.MazeWall Then
            LookupString = "XXXXX".ToCharArray 'overwrite lookup string to force 
        End If

        ' lower-left block check
        If (LookupString(1) + LookupString(2) + LookupString(4) = "111") AndAlso Maze(x - 1, y + 1) = MazeBuilder.MazeWall Then
            LookupString = "XXXXX".ToCharArray 'overwrite lookup string to force 
        End If

        ' Upper-right block check
        If (LookupString(0) + LookupString(2) + LookupString(3) = "111") AndAlso Maze(x + 1, y - 1) = MazeBuilder.MazeWall Then
            LookupString = "XXXXX".ToCharArray 'overwrite lookup string to force 
        End If

        ' lower-right block check
        If (LookupString(2) + LookupString(3) + LookupString(4) = "111") AndAlso Maze(x + 1, y + 1) = MazeBuilder.MazeWall Then
            LookupString = "XXXXX".ToCharArray 'overwrite lookup string to force 
        End If

        If WallLookup.ContainsKey(New String(LookupString)) Then
            Return WallLookup(New String(LookupString)) ' convert char array to string, feed into dictionary to return corresponding enum Value
        Else
            Return CellType.Unknown ' a default piece if type not found (used to force full block rather than re-configure the block counting system)
        End If

    End Function

End Module
