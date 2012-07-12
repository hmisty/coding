-module(chatroom).
-compile(export_all).

-define(TIMEOUT, 1000).
-type nickname() :: atom().

-spec default_handler() -> pid().
default_handler() ->
    spawn(fun() -> default_handler_loop() end).

default_handler_loop() ->
    receive
        X -> io:format("default_handler received ~p~n", [X])
    end,
    default_handler_loop().

-spec receive_any() -> any() | {error, timeout}.
receive_any() ->
    receive
        {_, Any} -> Any
    after ?TIMEOUT ->
            {error, timeout}
    end.

%% login to the chat system
-spec login(nickname(), pid()) -> ok.
login(Nickname, Handler) ->
    case global:whereis_name(Nickname) of
        undefined ->
            pg2:create(Nickname),
            pg2:join(Nickname, Handler),
            Pid = spawn_link(fun() -> chat_agent(Nickname) end),
            global:register_name(Nickname, Pid);
        _ ->
            pg2:join(Nickname, Handler),
            ok
    end.

%% logout from the chat system
-spec logout(nickname()) -> ok | {error, timeout}.
logout(Nickname) ->
    case global:whereis_name(Nickname) of
        undefined ->
            ok;
        Pid ->
            Pid ! {self(), logout},
            receive_any()
    end.

-spec join(nickname(), any()) ->  ok | {error, not_login} | {error, timeout}.
join(Who, RoomName) ->
    case global:whereis_name(Who) of
        undefined ->
            {error, not_login};
        Pid ->
            Pid ! {self(), join, RoomName},
            receive_any()
    end.

-spec leave(nickname(), any()) -> ok | {error, not_login}.
leave(Who, RoomName) ->
    case global:whereis_name(Who) of
        undefined ->
            {error, not_login};
        Pid ->
            Pid ! {self(), leave, RoomName},
            receive_any()
    end.
    
-spec shout(nickname(), any(), any()) -> ok | {error, not_login} | {error, timeout}.
shout(From, RoomName, Message) ->
    case global:whereis_name(From) of
        undefined ->
            {error, not_login};
        Pid ->
            Pid ! {self(), shout, RoomName, Message},
            receive_any()
    end.

-spec get_roommates(nickname(), any()) -> [nickname()] | {error, room_not_found} | {error, timeout}.
get_roommates(Nickname, RoomName) ->
    case global:whereis_name(Nickname) of
        undefined ->
            {error, not_login};
        Pid ->
            Pid ! {self(), get_roommates, RoomName},
            receive_any()
    end.

-spec whisper(nickname(), nickname(), any()) -> ok | {error, not_login} | {error, receiver_not_login} | {error, receiver_cannot_handle} | {error, timeout}.
whisper(From, To, Message) ->
    case global:whereis_name(From) of
        undefined ->
            {error, not_login};
        _Pid ->
            case global:whereis_name(To) of
                undefined ->
                    {error, receiver_not_login};
                Pid2 ->
                    Pid2 ! {self(), whisper, From, Message},
                    ok
            end
    end.

-spec get_rooms() -> [any()].
get_rooms() ->
    pg2:which_groups().

%% the agent
-spec chat_agent(nickname()) -> stop.
chat_agent(Nickname) ->
    receive
        {From, logout} ->
            From ! {self(), ok},
            exit(logout);
        {From, join, RoomName} ->
            Members = pg2:get_members(RoomName),
            case Members of
                {error, _} ->
                    pg2:create(RoomName),
                    pg2:join(RoomName, self()),
                    self() ! {self(), ping, Nickname, RoomName},
                    From ! {self(), ok};
                _ ->
                    case lists:member(self(), Members) of
                        true ->
                            ok;
                        false ->
                            pg2:join(RoomName, self()),
                            [ Pid ! {self(), ping, Nickname, RoomName} || Pid <- pg2:get_members(RoomName) ]
                    end,
                    From ! {self(), ok}
            end;
        {From, leave, RoomName} ->
            Members = pg2:get_members(RoomName),
            case Members of
                {error, _} ->
                    From ! {self(), {error, room_not_found}};
                _ ->
                    [ Pid ! {self(), bye, Nickname, RoomName} || Pid <- Members ],
                    pg2:leave(RoomName, self()),
                    From ! {self(), ok}
            end;
        {From, shout, RoomName, Message} ->
            Members = pg2:get_members(RoomName),
            case Members of
                {error, _} ->
                    From ! {self(), {error, room_not_found}};
                _ ->
                    [ Pid ! {self(), message, Nickname, Message} || Pid <- Members ],
                    From ! {self(), ok}
            end;
        {From, get_roommates, RoomName} ->
            Members = pg2:get_members(RoomName),
            case Members of
                {error, _} ->
                    From ! {self(), {error, room_not_found}};
                _ ->
                    From ! {self(), [ get(Pid) || Pid <- Members ]}
            end;
        %% ask the handler to handle the messages
        {_From, whisper, Nick, Message} ->
            case pg2:get_members(Nickname) of
                {error, _} ->
                    no_receiver;
                Handlers ->
                    [ H ! {whisper, Nick, Message} || H <- Handlers ]
            end;
        {_From, message, Nick, Message} ->
            case pg2:get_members(Nickname) of
                {error, _} ->
                    no_receiver;
                Handlers ->
                    [ H ! {message, Nick, Message} || H <- Handlers ]
            end;
        %% for internal only
        {From, ping, Nick, Room} ->
            case pg2:get_members(Nickname) of
                {error, _} ->
                    no_receiver;
                Handlers ->
                    [ H ! {join, Nick, Room} || H <- Handlers ]
            end,
            put(From, Nick),
            From ! {self(), pong, Nickname};
        {From, pong, Nick} ->
            put(From, Nick);
        {From, bye, Nick, Room} ->
            case pg2:get_members(Nickname) of
                {error, _} ->
                    no_receiver;
                Handlers ->
                    [ H ! {leave, Nick, Room} || H <- Handlers ]
            end,
            erase(From);
        %% for trapping exit
        {'EXIT', _Pid, _Why} ->
            exit('EXIT')
    end,
    chat_agent(Nickname).

