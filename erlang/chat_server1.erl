%% hmisty 20120520
%% usage:
%% term 1:
%%      $ erl
%%      > c(chat_server1).
%%      > chat_server1:start(4000).
%% term 2:
%%      $ nc localhost 4000
%%      id a
%%      m a b hi i am alice
%% term 3:
%%      $ nc localhost 4000
%%      id b
%%      m b a hi i am bob
-module(chat_server1).
-export([start/1]).

start(Port) ->
    spawn(fun() -> listen(Port) end).

listen(Port) ->
    {ok, Listen} = gen_tcp:listen(Port,
        [binary, {packet, 0}, {reuseaddr, true},
            {nodelay, true}, {active, false}]),
    TabIds = ets:new(ids, [public, set]),
    accept(Listen, TabIds).

accept(Listen, TabIds) ->
    ets:insert(TabIds, {a,1}),
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("server opened socket:~p~n", [Socket]),
    spawn(fun() -> loop(Socket, TabIds) end),
    accept(Listen, TabIds).

loop(Socket, TabIds) ->
    ets:insert(TabIds, {b,1}),
    PidWriter = spawn(fun() -> loop_writer(Socket) end),
    case gen_tcp:recv(Socket, 0) of
        {ok, Bytes} ->
            Reply = handle(Bytes, TabIds, PidWriter),
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
            loop(Socket, TabIds);
        {error, closed} ->
            io:format("socket closed. exit.~n")
    end.

loop_writer(Socket) ->
    receive
        Bytes ->
            gen_tcp:send(Socket, Bytes),
            loop_writer(Socket)
    end.

handle(Bytes, TabIds, PidWriter) ->
    ets:insert(TabIds, {c,1}),
    %% Bytes is not marshalled term
    %% string is just a list of int()
    Str = binary_to_list(Bytes), 
    StrChomped = string:strip(string:strip(Str, both, $\n)),
    Parts = string:tokens(StrChomped, " "),
    Reply = case Parts of 
        ["id",Id|_] ->
            io:format("got id:~p~n", [Id]),
            io:format("insert {~p,~p} to ets table ~p~n", 
                [Id, PidWriter, TabIds]),
            ets:insert(TabIds, {Id, PidWriter}),
            {ok, "welcome, " ++ Id ++ ".\n"};
        ["m",From,To|Msgs] ->
            Msg = string:join(Msgs, " "),
            io:format("got msg:~p from:~p to:~p~n", [Msg, From, To]),
            case ets:lookup(TabIds, To) of
                [{To,PidTo}|_] ->
                    io:format("ets lookup ~p successfully~n", [To]),
                    PidTo ! Bytes,
                    {ok, "successfully sent your msg: " ++ Msg ++ ".\n"};
                _ ->
                    io:format("ets lookup ~p failed~n", [To]),
                    {error, "sent failed, " ++ To ++ " is not online.\n"}
            end;
        _ ->
            io:format("got unknown:~p~n", [Parts]),
            {error, "unknown command\n"}
    end,
    Reply.

