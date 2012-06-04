%% hmisty
%%
%% a distributed chatroom server
%%
%% usage:
%% host 1:
%%      $ erl -name a@hostip1 -setcookie chats1234
%%      > chatroom_server:start(4000).
%%
%% host 2:
%%      $ erl -name b@hostip2 -setcookie chats1234
%%      > net_adm:ping('a@hostip1').
%%      > chatroom_server:start(4000).
%% 
%% client 1:
%%      $ nc hostip1 4000
%%      /nick alice
%%      /join room1
%%      hi i am alice
%%      /whisper bob hi bob, how are you?
%%      /leave room1
%%
%% client 2:
%%      $ nc hostip1 4000
%%      /nick alice
%%      /join room2
%%      hi i am alice too
%%
%% client 3:
%%      $ nc hostip2 4000
%%      /nick bob
%%      /join room1
%%      /who
%%      hi i am bob
-module(chatroom_server).
-export([start/1, stop/0]).

stop() ->
    chatroom_server ! stop.

start(Port) ->
    {ok, Listen} = gen_tcp:listen(Port,
        [binary, {packet, 0}, {reuseaddr, true},
            {nodelay, true}, {active, true}]),
    register(chatroom_server, spawn(fun() -> accept(Listen) end)).

accept(Listen) ->
    receive
        stop -> exit(stop)
    after 0 ->
            ok
    end,
    {ok, Socket} = gen_tcp:accept(Listen),
    unregister(chatroom_server),
    register(chatroom_server, spawn(fun() -> accept(Listen) end)),
    loop(Socket). % in controlling process

loop(Socket) ->
    Reply = receive
        {tcp, Socket, Bin} ->
            Result = handle(Bin),
            io_lib:format("-> ~p\n", [Result]);
        {tcp_closed, Socket} ->
            exit(tcp_closed);

        {message, Nick, Message} ->
            io_lib:format("=> ~p: ~p\n", [Nick, Message]);
        {whisper, Nick, Message} ->
            io_lib:format("=> *~p*: ~p\n", [Nick, Message]);
        {join, Nick} ->
            io_lib:format("=> *~p joined the room*\n", [Nick]);
        {leave, Nick} ->
            io_lib:format("=> *~p left the room*\n", [Nick]);

        Any ->
            io_lib:format("unknown: ~p\n", [Any])
    end,
    gen_tcp:send(Socket, list_to_binary(Reply)),
    loop(Socket).

handle(Bin) ->
    Str = binary_to_list(Bin), 
    StrChomped = string:strip(string:strip(Str, both, $\n)),
    Parts = string:tokens(StrChomped, " "),
    case Parts of 
        ["/nick",Nick|_] ->
            Nickname = list_to_atom(Nick),
            put(nick, Nickname),
            chatroom:login(Nickname, self());
        ["/join",Room|_] ->
            RoomName = list_to_atom(Room),
            put(room, RoomName),
            chatroom:join(get(nick), RoomName);
        ["/who"|_] ->
            chatroom:get_roommates(get(nick), get(room));
        ["/leave"|_] ->
            RoomName = get(room),
            erase(room),
            chatroom:leave(get(nick), RoomName);
        ["/quit"|_] ->
            chatroom:logout(get(nick));
        ["/whisper",To|Msgs] ->
            Msg = string:join(Msgs, " "),
            chatroom:whisper(get(nick), list_to_atom(To), Msg);
        Msgs ->
            Msg = string:join(Msgs, " "),
            chatroom:shout(get(nick), get(room), Msg)
    end.
