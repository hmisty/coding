%% hmisty
%%
%% a distributed chat server
%%
%% usage:
%% host 1:
%%      $ erl -name a@hostip1 -setcookie chats1234
%%      > chat_server2:start(4000).
%%
%% host 2:
%%      $ erl -name b@hostip2 -setcookie chats1234
%%      > cluster2:join('a@hostip1').
%%      > chat_server2:start(4000).
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
-module(chat_server2).
-export([start/1]).

start(Port) ->
    spawn(fun() -> listen(Port) end).

%% the chat server
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
    accept(Listen).

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
            exit
    end.

loop_writer(Socket) ->
    receive
        {From, Bytes} ->
            From ! gen_tcp:send(Socket, Bytes),
            loop_writer(Socket)
    end.

handle(Bytes, PidWriter) ->
    % Bytes is not marshalled term
    % string is just a list of int()
    Str = binary_to_list(Bytes), 
    StrChomped = string:strip(string:strip(Str, both, $\n)),
    Parts = string:tokens(StrChomped, " "),
    Reply = case Parts of 
        ["id",Id|_] ->
            %io:format("got id:~p~n", [Id]),
            PidName = list_to_atom(Id),
            case whereis(PidName) of
                undefined ->
                    register(PidName, PidWriter);
                _ ->
                    unregister(PidName),
                    register(PidName, PidWriter)
            end,
            {ok, "welcome, " ++ Id ++ ".\n"};
        ["m",_From,To|Msgs] ->
            Msg = string:join(Msgs, " "),
            %io:format("got msg:~p from:~p to:~p~n", [Msg, From, To]),
            PidName = list_to_atom(To),
            case cluster2:send(PidName, Bytes) of
                undefined ->
                    %io:format("~p not found in the cluster~n", [PidName]),
                    {error, "sent failed, " ++ To ++ " is not online.\n"};
                _ ->
                    %io:format("found ~p, send out ~p~n", [PidName, Bytes]),
                    {ok, "successfully sent your msg: " ++ Msg ++ ".\n"}
            end;
        _ ->
            %io:format("got unknown:~p~n", [Parts]),
            {error, "unknown command\n"}
    end,
    Reply.

