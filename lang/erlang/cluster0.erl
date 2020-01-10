%% hmisty
%%      a simple p2p cluster manger
%% usage:
%% host 1:
%%      $ erl -name a@hostip1 -setcookie chats1234
%%      > c(cluster0).
%%      > cluster0:start().
%%
%% host 2:
%%      $ erl -name b@hostip2 -setcookie chats1234
%%      > c(cluster0).
%%      > cluster0:start().
%%      > cluster0:join('a@hostip1').
%%      > cluster0:list().
%%      
%%      > register(area, spawn(fun area_server1:start/0)).
%%
%% host 1:
%%      > cluster0:find(area).
%%      > cluster0:send('b@hostip2', area, {circle, 3}).
%%
%%      > cluster0:quit().
%%
%%      > cluster0:stop().
%%
-module(cluster0).
-export([start/0, 
        stop/0,
        join/1, 
        quit/0, 
        list/0,
        find/1, 
        send/3,
        rpc_join/1, 
        rpc_quit/1, 
        rpc_lookup/1, 
        rpc_cluster/0,
        rpc_cluster/1,
        rpc_send/2]).

start() ->
    register(cmgr, spawn(fun() -> init() end)).

stop() ->
    cmgr ! stop.

%% the cluster client
join(PeerNode) ->
    {ok, Nodes} = rpc:call(PeerNode, ?MODULE, rpc_join, [node()]),
    rpc_cluster(Nodes),
    OtherNodes = lists:filter(fun(N)->(N/=PeerNode) and (N/=node()) end, Nodes),
    lists:foreach(fun(N)->rpc:call(N, ?MODULE, rpc_join, [node()]) end, OtherNodes).

quit() ->
    Nodes = rpc_cluster(),
    OtherNodes = lists:filter(fun(N)->N/=node() end, Nodes),
    lists:foreach(fun(N)->rpc:call(N, ?MODULE, rpc_quit, [node()]) end, OtherNodes),
    rpc_cluster([node()]).

list() ->
    rpc_cluster().

find(PidName) ->
    Nodes = rpc_cluster(),
    lists:filter(fun(N)-> found == rpc:call(N, ?MODULE, rpc_lookup, [PidName]) end, Nodes).

send(Node, PidName, Message) ->
    rpc:call(Node, ?MODULE, rpc_send, [PidName, Message]).

%% the cluster mgr
rpc_join(Node) ->
    rpc(cmgr, {join, Node}).

rpc_quit(Node) ->
    rpc(cmgr, {quit, Node}).

rpc_lookup(PidName) ->
    rpc(cmgr, {lookup, PidName}).

rpc_cluster(Nodes) ->
    rpc(cmgr, {cluster, Nodes}).

rpc_cluster() ->
    rpc(cmgr, {cluster}).

rpc_send(PidName, Message) ->
    rpc(PidName, Message).

rpc(PidName, Message) ->
    PidName ! {self() , Message},
    receive
        Reply ->
            Reply
    end.

init() ->
    put(nodes, [node()]),
    loop().

loop() ->
    receive
        {From, {join, Node}} ->
            io:format("cluster_mgr received join from ~p~n", [Node]),
            Nodes = get(nodes),
            NodesNew = lists:filter(fun(X)->X/=Node end, Nodes) ++ [Node],
            put(nodes, NodesNew),
            From ! {ok, NodesNew},
            loop();
        {From, {quit, Node}} ->
            io:format("cluster_mgr received quit from ~p~n", [Node]),
            Nodes = get(nodes),
            NodesNew = lists:filter(fun(X)->X/=Node end, Nodes),
            put(nodes, NodesNew),
            From ! {ok, NodesNew},
            loop();
        {From, {lookup, PidName}} ->
            io:format("cluster_mgr received lookup for ~p~n", [PidName]),
            From ! case whereis(PidName) of
                undefined ->
                    notfound;
                _ ->
                    found
            end,
            loop();
        {From, {cluster, Nodes}} ->
            io:format("cluster_mgr received set cluster to ~p~n", [Nodes]),
            put(nodes, Nodes),
            From ! ok,
            loop();
        {From, {cluster}} ->
            io:format("cluster_mgr received get cluster~n"),
            From ! get(nodes),
            loop();
        stop ->
            io:format("cluster_mgr received stop. now stop~n"),
            stop
    end.

