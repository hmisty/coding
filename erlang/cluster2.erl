%% hmisty
%%      a simple p2p cluster manger
%% usage:
%% host 1:
%%      $ erl -name a@hostip1 -setcookie chats1234
%%      > c(cluster2).
%%
%% host 2:
%%      $ erl -name b@hostip2 -setcookie chats1234
%%      > c(cluster2).
%%      > cluster2:join('a@hostip1').
%%      > cluster2:list().
%%      
%%      > register(area, spawn(fun area_server1:start/0)).
%%
%% host 1:
%%      > cluster2:find(area).
%%      
%%      > cluster2:send(area, {circle, 3}).
%%
-module(cluster2).
-export([join/1, list/0, find/1, send/2,
        rpc_nodes/0, rpc_lookup/1, rpc_send/2]).

%% on the local node
join(PeerNode) ->
    Nodes = rpc:call(PeerNode, ?MODULE, rpc_nodes, []),
    lists:foreach(fun(N)->net_adm:ping(N) end, Nodes).

list() ->
    nodes().

find(PidName) ->
    case get(PidName) of
        undefined ->
            Nodes = lists:filter(fun(N)-> found == rpc:call(N, ?MODULE, rpc_lookup, [PidName]) end, [node()] ++ nodes()),
            case Nodes of
                [Node|_] ->
                    put(PidName, Node),
                    Node;
                _ ->
                    undefined
            end;
        Node ->
            Node
    end.

send(PidName, Message) ->
    Node = find(PidName),
    rpc:call(Node, ?MODULE, rpc_send, [PidName, Message]).

%% on the remote node
rpc_nodes() ->
    nodes().

rpc_lookup(PidName) ->
    case whereis(PidName) of
        undefined ->
            notfound;
        _ ->
            found
    end.

rpc_send(PidName, Message) ->
    PidName ! {self() , Message},
    receive
        Reply ->
            Reply
    end.
