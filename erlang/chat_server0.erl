-module(chat_server0).
-export([start/1]).

start(Port) ->
    spawn(fun() -> listen(Port) end).

listen(Port) ->
    {ok, Listen} = gen_tcp:listen(Port,
        [binary, {packet, 0}, {reuseaddr, true},
            {nodelay, true}, {active, false}]),
    accept(Listen).

accept(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("server opened socket:~p~n", [Socket]),
    spawn(fun() -> loop(Socket) end),
    accept(Listen).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bytes} ->
            Reply = handle(Bytes),
            Str = case Reply of
                {ok, Msg} ->
                    "ok " ++ Msg;
                {error, Msg} ->
                    "error " ++ Msg;
                _ ->
                    "error unknown"
            end,
            StrBytes = list_to_binary(Str),
            gen_tcp:send(Socket, StrBytes),
            loop(Socket);
        {error, closed} ->
            io:format("socket closed. exit.~n")
    end.

handle(Bytes) ->
    %% Bytes is not marshalled term
    %% string is just a list of int()
    Str = binary_to_list(Bytes), 
    StrChomped = string:strip(string:strip(Str, both, $\n)),
    Parts = string:tokens(StrChomped, " "),
    Reply = case Parts of 
        ["id"|Id] ->
            io:format("got id:~p~n", [Id]),
            {ok, "welcome, " ++ Id ++ ".\n"};
        ["m",From,To|Msgs] ->
            Msg = string:join(Msgs, " "),
            io:format("got msg:~p from:~p to:~p~n", [Msg, From, To]),
            {ok, "got your msg: " ++ Msg ++ ".\n"};
        _ ->
            io:format("got unknown:~p~n", [Parts]),
            {error, "unknown command\n"}
    end,
    Reply.

