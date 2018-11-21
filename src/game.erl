-module(game).

-export([
    play/0
]).

play() ->
    Maze = maze:generate_maze(),
    Hero = hero:initialize_hero(Maze),
    
    play(Maze, Hero).

play(Maze, Hero) ->
    console:draw_screen(Maze, Hero),
    Command = console:get_command(),

    case Command of
        "q" ->
            console:quit();
        Go ->
            case lists:member(Go, ["i", "k", "j", "l"]) of
                true ->
                    {hero, [X, Y]} = Hero,
                    
                    [NewX, NewY] = case Go of
                        "i" ->
                            [X, Y - 1];
                        "k" ->
                            [X, Y + 1];
                        "j" ->
                            [X - 1, Y];
                        "l" ->
                            [X + 1, Y]
                    end,
                    
                    case maze:is_empty(Maze, NewX, NewY) of
                        true ->
                            play(Maze, {hero, [NewX, NewY]});
                        _ ->
                            play(Maze, Hero)
                    end;
                false ->
                    play(Maze, Hero)
            end
    end.