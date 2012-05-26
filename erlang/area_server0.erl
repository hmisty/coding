-module(area_server0).
-export([start/0]).

start() ->
    receive
        {rectangle, Width, Ht} ->
            io:format("rectangle:~p~n", [Width * Ht]);
        {circle, R} ->
            io:format("circle:~p~n", [3.14159 * R * R]);
        Other ->
            io:format("unknown:~p~n", [Other])
    end,
    start().
