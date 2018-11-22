-module(maze).

-export([
    generate_maze/0,
    is_empty/3,
    is_wall/3
]).

-include("maze.hrl").

generate_maze() ->
    FirstRoom = [generate_room()],
    %% Make sure rooms on the list are before doors:
    lists:reverse(generate_maze(FirstRoom)).

generate_maze(Maze) ->
    generate_door(Maze) ++ Maze.

generate_room() ->
    Width = ?MinWidth + rand:uniform(?MaxWidth - ?MinWidth),
    Height = ?MinHeight + rand:uniform(?MaxHeight - ?MinHeight),
    
    X = rand:uniform(?ScreenWidth - Width),
    Y = rand:uniform(?ScreenHeight - Height),

    {room, {X, Y, Width, Height}}.

generate_door(Maze) ->
    X = rand:uniform(?ScreenWidth),
    Y = rand:uniform(?ScreenHeight),
    
    case is_wall(Maze, X, Y) andalso
        not is_empty(Maze, X, Y) andalso
        not is_corner(Maze, X, Y) andalso
        not is_edge(X, Y) of
        true ->
            generate_corridor(Maze, X, Y) ++ [{door, {X, Y}}];
        false ->
            generate_door(Maze)
    end.
    
generate_corridor(Maze, X, Y) ->
    SegmentCount = rand:uniform(?MaxCorridorSegmentCount),
    generate_corridor(Maze, X, Y, SegmentCount).

generate_corridor(_Maze, _X, _Y, 0) ->
    [];
generate_corridor(Maze, X, Y, SegmentCount) ->
    {DeltaX, DeltaY} = case rand:uniform(4) of
        1 ->
            {-1, 0};
        2 ->
            {1, 0};
        3 ->
            {0, -1};
        4 ->
            {0, 1}
    end,
    SegmentLength = rand:uniform(?MaxCorridorSegmentLength),
    EndX = X + SegmentLength * DeltaX,
    EndY = Y + SegmentLength * DeltaY,
    %% Make sure the order of coordinates is from lower to higher
    %% so that comparisons for testing inclusion are easier:
    Segment = case DeltaX + DeltaY of
        1 ->
            [{corridor, {{X, Y}, {EndX, EndY}}}];
        -1 ->
            [{corridor, {{EndX, EndY}, {X, Y}}}]
    end,
    Segment ++ generate_corridor(Maze, EndX, EndY, SegmentCount - 1).

is_empty([{room, {X, Y, Width, Height}} | _T], PosX, PosY) when
    X < PosX, Y < PosY, X + Width > PosX + 1, Y + Height > PosY + 1 ->
    true;
is_empty([{door, {X, Y}} | _T], PosX, PosY) when
    X == PosX, Y == PosY ->
    true;
is_empty([{corridor, {{X1, Y1}, {X2, Y2}}} | _T], PosX, PosY) when
    X1 =< PosX, Y1 =< PosY, X2 >= PosX, Y2 >= PosY ->
    true;
is_empty([H | T], PosX, PosY) ->
    is_empty(T, PosX, PosY);
is_empty([], _PosX, _PosY) ->
    false.

is_wall([{room, {X, Y, Width, Height}} | _T] = Maze, PosX, PosY) when
    X == PosX orelse X + Width == PosX + 1, Y =< PosY, Y + Height >= PosY + 1;
    Y == PosY orelse Y + Height == PosY + 1, X =< PosX, X + Width >= PosX + 1 ->
    not is_empty(Maze, PosX, PosY);
is_wall([_H | T], PosX, PosY) ->
    is_wall(T, PosX, PosY);
is_wall([], _PosX, _PosY) ->
    false.

is_corner([{room, {X, Y, Width, Height}} | _T], PosX, PosY) when
    X == PosX, Y == PosY;
    X + Width == PosX + 1, Y == PosY;
    X == PosX, Y + Height == PosY + 1;
    X + Width == PosX + 1, Y + Height == PosY + 1 ->
    true;
is_corner([_H | T], PosX, PosY) ->
    is_corner(T, PosX, PosY);
is_corner([], _PosX, _PosY) ->
    false.

is_edge(X, Y) when
    X == 1; Y == 1; X == ?ScreenWidth; Y == ?ScreenHeight ->
    true;
is_edge(_X, _Y) ->
    false.