%% hmisty
%%
%% the distributed chat server
%%
%% usage:
%% host 1:
%%      $ erl -name s1@hostip1 -setcookie chats1234
%%      > c(?MODULE).
%%      > ?MODULE:start(4000).
%%
%% host 2:
%%      $ erl -name s1@hostip2 -setcookie chats1234
%%      > c(?MODULE).
%%      > ?MODULE:start(4000).
%%      > ?MODULE:join_cluster(s1@hostip1).
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
-export([start/1, start_cmgr/0, join_cluster/1, exit_cluster/0, cluster_whereis/1, cluster_send/3, join/1, exit/1, lookup/1, cluster/0, cluster/1, send/2]).

start(Port) ->
    spawn(fun() -> listen(Port) end),
    start_cmgr().

start_cmgr() ->
    register(cmgr, spawn(fun() -> init_cluster_mgr() end)).

%% the cluster client
join_cluster(PeerNode) ->
    {ok, Nodes} = rpc:call(PeerNode, ?MODULE, join, [node()]),
    cluster(Nodes),
    OtherNodes = lists:filter(fun(N)->(N/=PeerNode) and (N/=node()) end, Nodes),
    lists:foreach(fun(N)->rpc:call(N, ?MODULE, join, [node()]) end, OtherNodes).

exit_cluster() ->
    Nodes = cluster(),
    OtherNodes = lists:filter(fun(N)->N/=node() end, Nodes),
    lists:foreach(fun(N)->rpc:call(N, ?MODULE, exit, [node()]) end, OtherNodes),
    cluster([node()]).

cluster_whereis(PidName) ->
    Nodes = cluster(),
    lists:filter(fun(N)-> found == rpc:call(N, ?MODULE, lookup, [PidName]) end, Nodes).

cluster_send(Node, PidName, Message) ->
    rpc:call(Node, ?MODULE, send, [PidName, Message]).

%% the cluster mgr
join(Node) ->
    rpc(cmgr, {join, Node}).

exit(Node) ->
    rpc(cmgr, {exit, Node}).

lookup(PidName) ->
    rpc(cmgr, {lookup, PidName}).

cluster(Nodes) ->
    rpc(cmgr, {cluster, Nodes}).

cluster() ->
    rpc(cmgr, {cluster}).

send(PidName, Message) ->
    rpc(PidName, Message).

rpc(PidName, Message) ->
    PidName ! {self() , Message},
    receive
        Reply ->
            Reply
    end.

init_cluster_mgr() ->
    put(nodes, [node()]),
    cluster_mgr().

cluster_mgr() ->
    receive
        {From, {join, Node}} ->
            io:format("cluster_mgr received join from ~p~n", [Node]),
            Nodes = get(nodes),
            NodesNew = lists:filter(fun(X)->X/=Node end, Nodes) ++ [Node],
            put(nodes, NodesNew),
            From ! {ok, NodesNew};
        {From, {exit, Node}} ->
            io:format("cluster_mgr received exit from ~p~n", [Node]),
            Nodes = get(nodes),
            NodesNew = lists:filter(fun(X)->X/=Node end, Nodes),
            put(nodes, NodesNew),
            From ! {ok, NodesNew};
        {From, {lookup, PidName}} ->
            io:format("cluster_mgr received lookup for ~p~n", [PidName]),
            From ! case whereis(PidName) of
                undefined ->
                    notfound;
                _ ->
                    found
            end;
        {From, {cluster, Nodes}} ->
            io:format("cluster_mgr received set cluster to ~p~n", [Nodes]),
            put(nodes, Nodes),
            From ! ok;
        {From, {cluster}} ->
            io:format("cluster_mgr received get cluster~n"),
            From ! get(nodes)
    end,
    cluster_mgr().

%% the chat server
listen(Port) ->
    {ok, Listen} = gen_tcp:listen(Port,
        [binary, {packet, 0}, {reuseaddr, true},
            {nodelay, true}, {active, false}]),
    accept(Listen).

accept(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("server opened socket:~p~n", [Socket]),
    spawn(fun() -> loop(Socket) end),
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
            io:format("socket closed. exit.~n")
    end.

loop_writer(Socket) ->
    receive
        Bytes ->
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
            io:format("got id:~p~n", [Id]),
            PidName = list_to_atom(Id),
            case whereis(PidName) of
                undefined ->
                    register(PidName, PidWriter);
                _ ->
                    unregister(PidName),
                    register(PidName, PidWriter)
            end,
            {ok, "welcome, " ++ Id ++ ".\n"};
        ["m",From,To|Msgs] ->
            Msg = string:join(Msgs, " "),
            io:format("got msg:~p from:~p to:~p~n", [Msg, From, To]),
            PidName = list_to_atom(To),
            case whereis(PidName) of
                undefined ->
                    io:format("~p not found~n", [PidName]),
                    {error, "sent failed, " ++ To ++ " is not online.\n"};
                PidTo ->
                    io:format("found ~p, send out ~p~n", [PidName, Bytes]),
                    PidTo ! Bytes,
                    {ok, "successfully sent your msg: " ++ Msg ++ ".\n"}
            end;
        _ ->
            io:format("got unknown:~p~n", [Parts]),
            {error, "unknown command\n"}
    end,
    Reply.

