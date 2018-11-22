-module(maze).

-export([
    generate_maze/0,
    is_empty/3,
    is_wall/3,
    is_door/3
]).

-include("maze.hrl").

generate_maze() ->
    FirstRoom = [generate_room()],
    %% Make sure rooms on the list are before doors:
    lists:reverse(generate_maze(FirstRoom)).

generate_maze(Maze) ->
    generate_door(Maze) ++ Maze.

generate_room() ->
    Width = ?MinRoomWidth + rand:uniform(?MaxRoomWidth - ?MinRoomWidth),
    Height = ?MinRoomHeight + rand:uniform(?MaxRoomHeight - ?MinRoomHeight),
    
    X = rand:uniform(?ScreenWidth - Width),
    Y = rand:uniform(?ScreenHeight - Height),

    {room, {{X, Y}, {X + Width - 1, Y + Height - 1}}}.

generate_door(Maze) ->
    X = rand:uniform(?ScreenWidth),
    Y = rand:uniform(?ScreenHeight),
    
    case is_wall(Maze, X, Y) andalso
        not is_empty(Maze, X, Y) andalso
        not is_corner(Maze, X, Y) andalso
        not is_edge(X, Y) of
        true ->
            IsVert = rand:uniform(2) == 1,
            {DeltaX, DeltaY} = util:generate_delta(IsVert),
            {NewX, NewY} = {X + DeltaX, Y + DeltaY},
            
            case is_wall(Maze, NewX, NewY) orelse
                is_door(Maze, NewX, NewY) orelse
                is_corner(Maze, NewX, NewY) orelse
                is_empty(Maze, NewX, NewY) of
                true ->
                    generate_door(Maze);
                false ->
                    generate_corridor(Maze, NewX, NewY, DeltaX, DeltaY) ++ [{door, {X, Y}}]
            end;
        false ->
            generate_door(Maze)
    end.
    
generate_corridor(Maze, X, Y, DeltaX, DeltaY) ->
    SegmentCount = rand:uniform(?MaxCorridorSegmentCount),
    generate_corridor(Maze, X, Y, DeltaX, DeltaY, SegmentCount).

generate_corridor(_Maze, _X, _Y, _DeltaX, _DeltaY, 0) ->
    [];
generate_corridor(Maze, X, Y, DeltaX, DeltaY, SegmentCount) ->
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
    DeltaChange = case rand:uniform(2) of
        1 ->
            1;
        2 ->
            -1
    end,
    Segment ++ generate_corridor(Maze, EndX, EndY, DeltaY * DeltaChange, DeltaX * DeltaChange, SegmentCount - 1).

is_empty([{room, {{X1, Y1}, {X2, Y2}}} | _T], PosX, PosY) when
    X1 < PosX, Y1 < PosY, X2 > PosX, Y2 > PosY ->
    true;
is_empty([{door, {X, Y}} | _T], PosX, PosY) when
    X == PosX, Y == PosY ->
    true;
is_empty([{corridor, {{X1, Y1}, {X2, Y2}}} | _T], PosX, PosY) when
    X1 =< PosX, Y1 =< PosY, X2 >= PosX, Y2 >= PosY ->
    true;
is_empty([_H | T], PosX, PosY) ->
    is_empty(T, PosX, PosY);
is_empty([], _PosX, _PosY) ->
    false.

is_wall([{room, {{X1, Y1}, {X2, Y2}}} | _T] = Maze, PosX, PosY) when
    X1 == PosX orelse X2 == PosX, Y1 =< PosY, Y2 >= PosY;
    Y1 == PosY orelse Y2 == PosY, X1 =< PosX, X2 >= PosX ->
    not is_empty(Maze, PosX, PosY);
is_wall([_H | T], PosX, PosY) ->
    is_wall(T, PosX, PosY);
is_wall([], _PosX, _PosY) ->
    false.

is_corner([{room, {{X1, Y1}, {X2, Y2}}} | _T], PosX, PosY) when
    X1 == PosX, Y1 == PosY;
    X2 == PosX, Y1 == PosY;
    X1 == PosX, Y2 == PosY;
    X2 == PosX, Y2 == PosY ->
    true;
is_corner([_H | T], PosX, PosY) ->
    is_corner(T, PosX, PosY);
is_corner([], _PosX, _PosY) ->
    false.

is_door([{door, {X, Y}} | _T], PosX, PosY) when
    X == PosX, Y == PosY ->
    true;
is_door([_H | T], PosX, PosY) ->
    is_door(T, PosX, PosY);
is_door([], _PosX, _PosY) ->
    false.

is_edge(X, Y) when
    X =< 1; Y =< 1; X >= ?ScreenWidth; Y >= ?ScreenHeight ->
    true;
is_edge(_X, _Y) ->
    false.