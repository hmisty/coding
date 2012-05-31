-module(chatroom).
-compile(export_all).

-type nickname() :: atom().

-spec login(pid(), nickname()) -> any().
login(Pid, NickName) ->
    register(NickName, Pid),
    global:register_name(NickName, Pid).

-spec logout(nickname()) -> any().
logout(NickName) ->
    unregister(NickName),
    global:unregister_name(NickName).

-spec create(nickname(), any()) -> any().
create(Creator, RoomName) ->
    pg2:create(RoomName),
    join(Creator, RoomName).

-spec join(nickname(), any()) -> any().
join(Who, RoomName) ->
    pg2:join(RoomName, global:whereis_name(Who)),
    [ Pid ! {join, Who} || Pid <- pg2:get_members(RoomName) ].

-spec leave(nickname(), any()) -> any().
leave(Who, RoomName) ->
    pg2:leave(RoomName, global:whereis_name(Who)),
    [ Pid ! {leave, Who} || Pid <- pg2:get_members(RoomName) ].
    %FIXME unexpected leaving?

-spec shout(nickname(), any(), any()) -> [any()].
shout(From, RoomName, Message) ->
    [ Pid ! {shout, From, Message} || Pid <- pg2:get_members(RoomName) ].

-spec whisper(nickname(), nickname(), any()) -> any().
whisper(From, To, Message) ->
    global:whereis_name(To) ! {whisper, From, Message}.

