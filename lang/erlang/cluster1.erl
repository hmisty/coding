%% hmisty
%%      a simple p2p cluster manger
%% usage:
%% host 1:
%%      $ erl -name a@hostip1 -setcookie chats1234
%%      > c(cluster1).
%%      > cluster1:start().
%%
%% host 2:
%%      $ erl -name b@hostip2 -setcookie chats1234
%%      > c(cluster1).
%%      > cluster1:start().
%%      > cluster1:join('a@hostip1').
%%      > cluster1:list().
%%      
%%      > register(area, spawn(fun area_server1:start/0)).
%%
%% host 1:
%%      > cluster1:find(area).
%%      > cluster1:send('b@hostip2', area, {circle, 3}).
%%
%%      > cluster1:quit().
%%
%%      > cluster1:stop().
%%
-module(cluster1).
-export([start/0, 
        stop/0,
        join/1, 
        list/0,
        find/1, 
        send/3,
        rpc_join/1, 
        rpc_lookup/1, 
        rpc_send/2]).

start() ->
    register(cmgr, spawn(fun() -> loop() end)).

stop() ->
    cmgr ! stop.

%% the cluster client
join(PeerNode) ->
    Nodes = rpc:call(PeerNode, ?MODULE, rpc_join, [node()]),
    lists:foreach(fun(N)->net_adm:ping(N) end, Nodes).

list() ->
    nodes().

find(PidName) ->
    lists:filter(fun(N)-> found == rpc:call(N, ?MODULE, rpc_lookup, [PidName]) end, nodes()).

send(Node, PidName, Message) ->
    rpc:call(Node, ?MODULE, rpc_send, [PidName, Message]).

%% the cluster mgr
rpc_join(Node) ->
    rpc(cmgr, {join, Node}).

rpc_lookup(PidName) ->
    rpc(cmgr, {lookup, PidName}).

rpc_send(PidName, Message) ->
    rpc(PidName, Message).

rpc(PidName, Message) ->
    PidName ! {self() , Message},
    receive
        Reply ->
            Reply
    end.

loop() ->
    receive
        {From, {join, Node}} ->
            io:format("cluster_mgr received join from ~p~n", [Node]),
            From ! nodes(),
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
        stop ->
            io:format("cluster_mgr received stop. now stop~n"),
            stop
    end.

