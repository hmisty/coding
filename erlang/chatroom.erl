-module(chatroom).
-compile(export_all).

setnick(NickName) ->
    global:register_name(NickName, self()).

create(RoomName) ->
    pg2:create(RoomName),
    pg2:join(RoomName, self()).

join(RoomName) ->
    pg2:join(RoomName, self()).

shout(RoomName, Message) ->
    [ Pid ! {shout, self(), Message} || Pid <- pg2:get_members(RoomName) ].

whisper(NickName, Message) ->
    global:whereis_name(NickName) ! {whisper, self(), Message}.

