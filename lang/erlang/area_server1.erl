-module(area_server1).
-export([start/0]).

start() ->
    receive
        {From, {rectangle, Width, Ht}} ->
            Result = Width * Ht,
            io:format("rectangle:~p~n", [Result]),
            From ! Result;
        {From, {circle, R}} ->
            Result = 3.14159 * R * R,
            io:format("circle:~p~n", [Result]),
            From ! Result;
        {From, Other} ->
            io:format("unknown:~p~n", [Other]),
            From ! unknown;
        Other ->
            io:format("unknown:~p~n", [Other])
    end,
    start().
