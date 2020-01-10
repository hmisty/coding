%% hmisty
%%
%% a distributed chat server
%% just a simple change of chat_server1 using global:
%%
%% usage:
%% host 1:
%%      $ erl -name a@hostip1 -setcookie chats1234
%%      > chat_server3:start(4000).
%%
%% host 2:
%%      $ erl -name b@hostip2 -setcookie chats1234
%%      > net_adm:ping('a@hostip1').
%%      > chat_server3:start(4000).
%% 
%% client 1:
%%      $ nc hostip1 4000
%%      id a
%%      m a b hi i am alice
%%
%% client 2:
%%      $ nc hostip2 4000
%%      id b
%%      m b a hi i am bob
-module(chat_server3).
-export([start/1, stop/0]).

start(Port) ->
    register(chat_server, spawn(fun() -> listen(Port) end)).

stop() ->
    chat_server ! stop.

listen(Port) ->
    {ok, Listen} = gen_tcp:listen(Port,
        [binary, {packet, 0}, {reuseaddr, true},
            {nodelay, true}, {active, false}]),
    accept(Listen).

accept(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} -> 
            %io:format("server opened socket:~p~n", [Socket]),
            spawn(fun() -> loop(Socket) end);
        {error, Reason} ->
            io:format("gen_tcp:accept error ~p~n", [Reason])
    end,
    receive
        stop ->
            gen_tcp:close(Listen)
    after 0 ->
            accept(Listen)
    end.

loop(Socket) ->
    PidWriter = spawn(fun() -> loop_writer(Socket) end),
    case gen_tcp:recv(Socket, 0) of
        {ok, Bytes} ->
            Reply = handle(Bytes, PidWriter),
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
            %io:format("socket closed. exit.~n"),
            PidWriter ! exit
    end.

loop_writer(Socket) ->
    receive
        exit ->
            %io:format("pid writer exits.~n"),
            exit;
        {send, Bytes} ->
            gen_tcp:send(Socket, Bytes),
            loop_writer(Socket)
    end.

handle(Bytes, PidWriter) ->
    %% Bytes is not marshalled term
    %% string is just a list of int()
    Str = binary_to_list(Bytes), 
    StrChomped = string:strip(string:strip(Str, both, $\n)),
    Parts = string:tokens(StrChomped, " "),
    Reply = case Parts of 
        ["id",Id|_] ->
            %io:format("got id:~p~n", [Id]),
            PidName = list_to_atom(Id),
            case global:whereis_name(PidName) of
                undefined ->
                    global:register_name(PidName, PidWriter);
                _ ->
                    global:re_register_name(PidName, PidWriter)
            end,
            {ok, "welcome, " ++ Id ++ ".\n"};
        ["m",_From,To|Msgs] ->
            Msg = string:join(Msgs, " "),
            %io:format("got msg:~p from:~p to:~p~n", [Msg, From, To]),
            PidName = list_to_atom(To),
            case global:whereis_name(PidName) of
                undefined ->
                    %io:format("~p not found~n", [PidName]),
                    {error, "sent failed, " ++ To ++ " is not online.\n"};
                PidTo ->
                    %io:format("found ~p, send out ~p~n", [PidName, Bytes]),
                    PidTo ! {send, Bytes},
                    {ok, "successfully sent your msg: " ++ Msg ++ ".\n"}
            end;
        _ ->
            %io:format("got unknown:~p~n", [Parts]),
            {error, "unknown command\n"}
    end,
    Reply.

