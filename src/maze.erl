-module(maze).

-export([
    generate_unoccupied_point/1,
    generate_maze/1,
    remove_item/3,
    remove_monster/3,
    is_empty/3,
    is_wall/3,
    is_door/3,
    is_stairs/3,
    is_item/3,
    is_monster/3,
    get_monster/3
]).

-include("board.hrl").

-define(MazeComplexity, 10).
-define(MaxRoomWidth, 30).
-define(MinRoomWidth, 3).
-define(MaxRoomHeight, 10).
-define(MinRoomHeight, 3).
-define(MaxCorridorSegmentCount, 10).
-define(MaxCorridorSegmentLength, 10).
-define(ReciprocalDeadEnd, 5).
-define(MonsterStrength, 10).

generate_empty_point(Maze) ->
    {X, Y} = board:generate_point(),
    
    case is_empty(Maze, X, Y) of
        true ->
            {X, Y};
        false ->
            generate_empty_point(Maze)
    end.

generate_unoccupied_point(Maze) ->
    {X, Y} = generate_empty_point(Maze),
    
    case is_monster(Maze, X, Y) of
        false ->
            {X, Y};
        true ->
            generate_unoccupied_point(Maze)
    end.
    
generate_maze(IsLastLevel) ->
    FirstRoom = [generate_room()],
    generate_maze(IsLastLevel, FirstRoom, ?MazeComplexity).

generate_maze(false, Maze, 0) ->
    NewMaze = [generate_stairs(Maze)] ++ Maze,
    generate_monsters(NewMaze);
generate_maze(true, Maze, 0) ->
    NewMaze = [generate_treasure(Maze)] ++ Maze,
    generate_monsters(NewMaze);
generate_maze(IsLastLevel, Maze, Trials) ->
    generate_maze(IsLastLevel, generate_door(Maze) ++ Maze, Trials - 1).

generate_stairs(Maze) ->
    {X, Y} = generate_empty_point(Maze),
    
    case not maze:is_door(Maze, X, Y) of
        true ->
            {stairs, {X, Y}};
        false ->
            generate_stairs(Maze)
    end.

generate_treasure(Maze) ->
    {X, Y} = generate_empty_point(Maze),
    
    case not maze:is_door(Maze, X, Y) of
        true ->
            {item, {X, Y}, treasure};
        false ->
            generate_treasure(Maze)
    end.

generate_room_dimensions() -> {
    ?MinRoomWidth + rand:uniform(?MaxRoomWidth - ?MinRoomWidth),
    ?MinRoomHeight + rand:uniform(?MaxRoomHeight - ?MinRoomHeight)
}.

generate_room() ->
    {Width, Height} = generate_room_dimensions(),

    X = rand:uniform(?BoardWidth - Width),
    Y = rand:uniform(?BoardHeight - Height),

    {room, {{X, Y}, {X + Width - 1, Y + Height - 1}}}.

generate_door(Maze) ->
    {X, Y} = board:generate_point(),
    
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
                    Corridor = generate_corridor(Maze, NewX, NewY, DeltaX, DeltaY),
                    
                    case length(Corridor) of
                        0 ->
                            generate_door(Maze);
                        _ ->
                            Corridor ++ [{door, {X, Y}}]
                    end
            end;
        false ->
            generate_door(Maze)
    end.
    
generate_corridor(Maze, X, Y, DeltaX, DeltaY) ->
    SegmentCount = rand:uniform(?MaxCorridorSegmentCount),
    generate_corridor(Maze, X, Y, DeltaX, DeltaY, SegmentCount).

generate_corridor(Maze, X, Y, DeltaX, DeltaY, 0) ->
    case rand:uniform(?ReciprocalDeadEnd) of
        1 ->
            [];
        _ ->
            DoorX = X + DeltaX,
            DoorY = Y + DeltaY,
    
            {RoomWidth, RoomHeight} = generate_room_dimensions(),
            RoomCoords = case DeltaX of
                0 ->
                    %% Careful not to put the door in the room's corner:
                    RoomX1 = X - RoomWidth + 1 + rand:uniform(RoomWidth - 2),
                    RoomX2 = RoomX1 + RoomWidth,
                    case DeltaY of
                        1 ->
                            {{RoomX1, DoorY - RoomHeight}, {RoomX2, DoorY}};
                        -1 ->
                            {{RoomX1, DoorY}, {RoomX2, DoorY + RoomHeight}}
                    end;
                _ ->
                    %% Careful not to put the door in the room's corner:
                    RoomY1 = Y - RoomHeight + 1 + rand:uniform(RoomHeight - 2),
                    RoomY2 = RoomY1 + RoomHeight,
                    case DeltaX of
                        1 ->
                            {{DoorX, RoomY1}, {DoorX + RoomWidth, RoomY2}};
                        -1 ->
                            {{DoorX - RoomWidth, RoomY1}, {DoorX, RoomY2}}
                    end
            end,
            
            Room = {room, RoomCoords},
            
            case is_outside(Room) orelse room_overlaps(Maze, Room) of
                true ->
                    generate_corridor(Maze, X, Y, DeltaX, DeltaY, 0);
                false ->
                    [{door, {DoorX, DoorY}}, {room, RoomCoords}]
            end
    end;
generate_corridor(Maze, X, Y, DeltaX, DeltaY, SegmentCount) ->
    SegmentLength = rand:uniform(?MaxCorridorSegmentLength),
    
    EndX = min(max(X + SegmentLength * DeltaX, 1), ?BoardWidth),
    EndY = min(max(Y + SegmentLength * DeltaY, 1), ?BoardHeight),
    
    Segment = case X == EndX andalso Y == EndY of
        true ->
            %% Can't go in that direction;
            %% try changing direction to a perpendicular one.
            [];
        false ->
            %% Make sure the order of coordinates is from lower to higher
            %% so that comparisons for inclusion testing are easier:
            case DeltaX + DeltaY of
                1 ->
                    {corridor, {{X, Y}, {EndX, EndY}}};
                -1 ->
                    {corridor, {{EndX, EndY}, {X, Y}}}
            end
    end,
    
    DeltaChange = case rand:uniform(2) of
        1 ->
            1;
        2 ->
            -1
    end,

    case (Segment == []) orelse overlaps_rooms(Maze, Segment) of
        true ->
            generate_corridor(Maze, X, Y, DeltaX * DeltaChange, DeltaY * DeltaChange, SegmentCount - 1);
        false ->
            %% DeltaX/Y are substituted intentionally to switch direction to a perpendicular one:
            [Segment] ++ generate_corridor([Segment] ++ Maze, EndX, EndY, DeltaY * DeltaChange, DeltaX * DeltaChange, SegmentCount - 1)
    end.

generate_monsters(Maze) ->
    {X, Y} = maze:generate_unoccupied_point(Maze),
    [{monster, {X, Y}, {orc, rand:uniform(?MonsterStrength)}}] ++ Maze.

remove_item(Maze, X, Y) ->
    remove_item(Maze, X, Y, {undefined, []}).

remove_item([{item, {X, Y}, Item} | T], X, Y, {undefined, NewMaze}) ->
    {Item, T ++ NewMaze};
remove_item([H | T], X, Y, {undefined, NewMaze}) ->
    remove_item(T, X, Y, {undefined, [H] ++ NewMaze});
remove_item([], _X, _Y, Result) ->
    Result.

remove_monster(Maze, X, Y) ->
    remove_monster(Maze, X, Y, {undefined, []}).

remove_monster([{monster, {X, Y}, Monster} | T], X, Y, {undefined, NewMaze}) ->
    {Monster, T ++ NewMaze};
remove_monster([H | T], X, Y, {undefined, NewMaze}) ->
    remove_monster(T, X, Y, {undefined, [H] ++ NewMaze});
remove_monster([], _X, _Y, Result) ->
    Result.

is_empty([{room, {{X1, Y1}, {X2, Y2}}} | _T], X, Y) when
    X1 < X, Y1 < Y, X2 > X, Y2 > Y ->
    true;
is_empty([{door, {X, Y}} | _T], X, Y) ->
    true;
is_empty([{stairs, {X, Y}} | _T], X, Y) ->
    true;
is_empty([{item, {X, Y}, _} | _T], X, Y) ->
    true;
is_empty([{corridor, {{X1, Y1}, {X2, Y2}}} | _T], X, Y) when
    X1 =< X, Y1 =< Y, X2 >= X, Y2 >= Y ->
    true;
is_empty([_H | T], X, Y) ->
    is_empty(T, X, Y);
is_empty([], _X, _Y) ->
    false.

is_wall([{room, {{X1, Y1}, {X2, Y2}}} | _T] = Maze, X, Y) when
    X1 == X orelse X2 == X, Y1 =< Y, Y2 >= Y;
    Y1 == Y orelse Y2 == Y, X1 =< X, X2 >= X ->
    not is_door(Maze, X, Y);
is_wall([_H | T], X, Y) ->
    is_wall(T, X, Y);
is_wall([], _X, _Y) ->
    false.

is_corner([{room, {{X1, Y1}, {X2, Y2}}} | _T], X, Y) when
    X1 == X, Y1 == Y;
    X2 == X, Y1 == Y;
    X1 == X, Y2 == Y;
    X2 == X, Y2 == Y ->
    true;
is_corner([_H | T], X, Y) ->
    is_corner(T, X, Y);
is_corner([], _X, _Y) ->
    false.

is_object(Object, [{Object, {X, Y}} | _T], X, Y) ->
    true;
is_object(Object, [_H | T], X, Y) ->
    is_object(Object, T, X, Y);
is_object(_Object, [], _X, _Y) ->
    false.

is_door(Maze, X, Y) ->
    is_object(door, Maze, X, Y).

is_stairs(Maze, X, Y) ->
    is_object(stairs, Maze, X, Y).

is_item([{item, {X, Y}, _} | _T], X, Y) ->
    true;
is_item([_H | T], X, Y) ->
    is_item(T, X, Y);
is_item([], _X, _Y) ->
    false.

is_monster([{monster, {X, Y}, _} | _T], X, Y) ->
    true;
is_monster([_H | T], X, Y) ->
    is_monster(T, X, Y);
is_monster([], _X, _Y) ->
    false.

get_monster([{monster, {X, Y}, _} = Monster | _T], X, Y) ->
    Monster;
get_monster([_H | T], X, Y) ->
    get_monster(T, X, Y);
get_monster([], _X, _Y) ->
    undefined.

is_edge(X, Y) when
    X == 1; Y == 1; X == ?BoardWidth; Y == ?BoardHeight ->
    true;
is_edge(_X, _Y) ->
    false.

is_outside(X, Y) when
    X < 1; Y < 1; X > ?BoardWidth; Y > ?BoardHeight ->
    true;
is_outside(_X, _Y) ->
    false.

is_outside({room, {{X1, Y1}, {X2, Y2}}}) ->
    is_outside(X1, Y1) orelse is_outside(X2, Y2).

overlaps({{X1, Y1}, {X2, Y2}}, {{X3, Y3}, {X4, Y4}}) ->
    not (max(X1, X2) < min(X3, X4) orelse
        min(X1, X2) > max(X3, X4) orelse
        max(Y1, Y2) < min(Y3, Y4) orelse
        min(Y1, Y2) > max(Y3, Y4)).

room_overlaps([{_, {{_X1, _Y1}, {_X2, _Y2}} = Coords1} | T], {room, Coords2} = Room) ->
    overlaps(Coords1, Coords2) orelse room_overlaps(T, Room);
room_overlaps([_H | T], Room) ->
    room_overlaps(T, Room);
room_overlaps([], _Room) ->
    false.

overlaps_rooms([{room, Coords1} | T], {corridor, Coords2} = Corridor) ->
    overlaps(Coords1, Coords2) orelse overlaps_rooms(T, Corridor);
overlaps_rooms([_H | T], Corridor) ->
    overlaps_rooms(T, Corridor);
overlaps_rooms([], _Corridor) ->
    false.
