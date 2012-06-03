-module(chatroom).
-compile(export_all).

-define(TIMEOUT, 1000).
-type nickname() :: atom().

-spec default_handler() -> any().
default_handler() ->
    receive
        X -> io:format("default_handler received ~p~n", [X])
    end,
    default_handler().

-spec receive_any() -> any() | {error, timeout}.
receive_any() ->
    receive
        {_, Any} -> Any
    after ?TIMEOUT ->
            {error, timeout}
    end.

%% login to the chat system
-spec login(nickname(), fun()) -> ok.
login(Nickname, HandlerFunc) ->
    case global:whereis_name(Nickname) of
        undefined ->
            Handler = spawn(HandlerFunc),
            Pid = spawn(fun() -> 
                        process_flag(trap_exit, true),
                        link(Handler),
                        chat_agent(Nickname, Handler) end),
            global:register_name(Nickname, Pid);
        _ ->
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

%% create a chatroom
-spec create(nickname(), any()) -> ok | {error, not_login} | {error, timeout}.
create(Creator, RoomName) ->
    case global:whereis_name(Creator) of
        undefined ->
            {error, not_login};
        Pid ->
            Pid ! {self(), create, RoomName},
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
-spec chat_agent(nickname(), pid()) -> stop.
chat_agent(Nickname, Handler) ->
    receive
        {From, logout} ->
            From ! {self(), ok},
            stop;
        {From, create, RoomName} ->
            pg2:create(RoomName),
            self() ! {From, join, RoomName},
            chat_agent(Nickname, Handler);
        {From, join, RoomName} ->
            case pg2:join(RoomName, self()) of
                ok ->
                    [ Pid ! {self(), ping, Nickname} || Pid <- pg2:get_members(RoomName) ],
                    From ! {self(), ok};
                {error, _} ->
                    From ! {self(), {error, room_not_found}}
            end,
            chat_agent(Nickname, Handler);
        {From, leave, RoomName} ->
            Members = pg2:get_members(RoomName),
            case Members of
                {error, _} ->
                    From ! {self(), {error, room_not_found}};
                _ ->
                    [ Pid ! {self(), bye} || Pid <- Members ],
                    pg2:leave(RoomName, self()),
                    From ! {self(), ok}
            end,
            chat_agent(Nickname, Handler);
        {From, shout, RoomName, Message} ->
            Members = pg2:get_members(RoomName),
            case Members of
                {error, _} ->
                    From ! {self(), {error, room_not_found}};
                _ ->
                    [ Pid ! {self(), message, Message} || Pid <- Members ],
                    From ! {self(), ok}
            end,
            chat_agent(Nickname, Handler);
        {From, get_roommates, RoomName} ->
            Members = pg2:get_members(RoomName),
            case Members of
                {error, _} ->
                    From ! {self(), {error, room_not_found}};
                _ ->
                    From ! {self(), [ get(Pid) || Pid <- Members ]}
            end,
            chat_agent(Nickname, Handler);
        %% ask the handler to handle the messages
        {_From, whisper, Nick, Message} ->
            Handler ! {whisper, Nick, Message},
            chat_agent(Nickname, Handler);
        {_From, message, Message} ->
            Handler ! {message, Message},
            chat_agent(Nickname, Handler);
        %% for internal only
        {From, ping, Nick} ->
            put(From, Nick),
            From ! {self(), pong, Nickname},
            chat_agent(Nickname, Handler);
        {From, pong, Nick} ->
            put(From, Nick),
            chat_agent(Nickname, Handler);
        {From, bye} ->
            erase(From),
            chat_agent(Nickname, Handler);
        %% for trapping exit
        {'EXIT', Pid, Why} ->
            io:format("handler ~p died for: ~p. stop.", [Pid, Why]),
            stop
    end.

