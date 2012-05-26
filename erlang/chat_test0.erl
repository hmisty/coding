-module(chat_test0).
-export([run/1, start/3, stop/1, counter/0]).

run(N) ->
    start('192.168.0.1', 4000, N),
    wait(10, N),
    stop(N).

wait(Max, N) ->
    wait1(Max, N, {0, 0}).

wait1(Max, N, {Nok, Nerror}) when (Max /= 0) and (Nok+Nerror < N) ->
    receive
    after 1000 ->
            C = counter(),
            io:format("counter=~p~n", [C]),
            wait1(Max-1, N, C)
    end;
wait1(Max, _, {Nok, Nerror}) when Max == 0 ->
    {Nok, Nerror};
wait1(_, _, {Nok, Nerror}) ->
    {Nok, Nerror}.

start(Host, Port, N) ->
    register(counter, spawn(fun() -> count({0,0}) end)),
    loop(Host, Port, N).

stop(0) ->
    quit(counter);
stop(N) ->
    Name = list_to_atom(integer_to_list(N)),
    quit(Name),
    stop(N-1).

quit(Name) ->
    case whereis(Name) of
        undefined ->
            undefined;
        Pid ->
            Pid ! exit
    end.

counter() ->
    counter ! {self(), counter},
    receive
        {Nok, Nerror} -> {Nok, Nerror}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(_Host, _Port, 0) -> 0;
loop(Host, Port, N) ->
    Name = list_to_atom(integer_to_list(N)),
    register(Name, spawn(fun() -> connect(Host, Port, N) end)),
    loop(Host, Port, N-1).
    
count({Nok, Nerror}) ->
    receive
        {text, Text} ->
            Parts = string:tokens(Text, " "),
            case Parts of
                ["ok"|_] ->
                    count({Nok+1, Nerror});
                ["error"|_] ->
                    count({Nok, Nerror+1})
            end;
        {From, counter} ->
            From ! {Nok, Nerror},
            count({Nok, Nerror});
        exit ->
            %io:format("count got exit!~n"),
            exit
    end.

connect(Host, Port, N) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
        {ok,Socket} ->
            ok = gen_tcp:send(Socket, "id " ++ integer_to_list(N) ++ "\n"),
            receive_data(Socket);
        {error,Reason} ->
            counter ! {text, "error cannot connect"}
    end.

receive_data(Socket) ->
    receive
        {tcp,Socket,Bin} ->
            Bytes = binary_to_list(Bin),
            counter ! {text, Bytes},
            receive_data(Socket);
        {tcp_closed,Socket} ->
            closed;
        exit ->
            %io:format("receiver got exit!~n"),
            exit
    end.
