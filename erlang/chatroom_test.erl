-module(chatroom_test).
-compile(export_all).

-define(ROOMNAME, room1).

start(NickName) ->
    spawn(fun() -> init(NickName) end).

init(NickName) ->
    chatroom:login(self(), NickName),
    chatroom:create(NickName, ?ROOMNAME),
    loop(NickName).

loop(NickName) ->
    receive
        exit ->
            chatroom:logout(NickName);
        {join, F} ->
            global:send(F, {info, self(), process_info(self(), [registered_name])}),
            loop(NickName);
        {leave, _F} ->
            loop(NickName);
        {shout, _F, M} ->
            io:format("shout received: ~p~n", [M]),
            loop(NickName);
        {whisper, _F, M} ->
            io:format("whisper received: ~p~n", [M]),
            loop(NickName);
        {info, F, M} ->
            io:format("info of ~p received: ~p~n", [F, M]),
            loop(NickName);
        M ->
            io:format("msg received: ~p~n", [M]),
            loop(NickName)
    end.

