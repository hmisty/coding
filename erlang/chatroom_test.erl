-module(chatroom_test).
-compile(export_all).

-define(ROOMNAME, room1).

init() ->
    spawn(fun() -> chat_client(alice) end),
    spawn(fun() -> chat_client(bob) end),
    spawn(fun() -> chat_client(tom) end).

stop() ->
    chatroom:logout(bob),
    chatroom:logout(alice),
    chatroom:logout(tom).

post() ->
    chatroom:create(alice, ?ROOMNAME),
    chatroom:join(bob, ?ROOMNAME),
    chatroom:shout(alice, ?ROOMNAME, hi),
    chatroom:whisper(alice, bob, hi),
    chatroom:whisper(alice, tom, hi),
    chatroom:shout(bob, ?ROOMNAME, hi),
    chatroom:join(tom, ?ROOMNAME),
    chatroom:leave(alice, ?ROOMNAME),
    chatroom:shout(tom, ?ROOMNAME, hi).

chat_client(Nickname) ->
    chatroom:login(Nickname, self()),
    handle_msg(Nickname).

handle_msg(Nickname) ->
    receive
        {message, Message} ->
            io:format("~p received message: ~p~n", [Nickname, Message]);
        {whisper, Nick, Message} ->
            io:format("~p received whisper from ~p : ~p~n", [Nickname, Nick, Message]);
        stop ->
            io:format("~p received stop~n", [Nickname]),
            exit(stop);
        Any ->
            io:format("~p received unknown msg: ~p~n", [Nickname, Any])
    after 10000 ->
            io:format("~p is alive...~n", [Nickname])
    end,
    handle_msg(Nickname).

