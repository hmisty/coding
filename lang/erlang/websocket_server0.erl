-module(websocket_server0).
-compile(export_all).
 
%% web server
start_web() ->
   {ok, Listen} = gen_tcp:listen(80, [{packet,0},
                                        {reuseaddr,true},
                                        {active, true}]),
   spawn(fun() -> web_accept(Listen) end).

web_accept(Listen) ->
   {ok, Bin} = file:read_file("websocket_client0.html"),
   Html = binary_to_list(Bin),
   {ok, Socket} = gen_tcp:accept(Listen),
   spawn(fun() -> web_accept(Listen) end),
   web_loop(Socket, Html).

web_loop(Socket, Html) ->
   receive
       {tcp, Socket, Data} ->
           io:format("received:~p~n",[Data]),
		   Len = string:len(Html),
           Msg = "HTTP/1.1 200 OK\r\n" ++
				 "Content-Length: " ++ integer_to_list(Len) ++
				 "\r\nContent-Type: text/html\r\n\r\n" ++
				 Html,
           gen_tcp:send(Socket, Msg),
		   web_loop(Socket, Html);
       {tcp_closed, Socket} ->
		   io:format("Server socket closed~n")
   end.

%% websocket server
start() ->
   {ok, Listen} = gen_tcp:listen(8080, [{packet,0},
                                        {reuseaddr,true},
                                        {active, true}]),
   spawn(fun() -> par_connect(Listen) end).
 
par_connect(Listen) ->
   {ok, Socket} = gen_tcp:accept(Listen),
   spawn(fun() -> par_connect(Listen) end),
   wait(Socket).
 
wait(Socket) ->
   receive
       {tcp, Socket, Data} ->
           io:format("received:~p~n",[Data]),
			Seckey = get_seckey(list_to_binary(Data)),
		   Secacc = get_secacc(Seckey),
           Msg = prefix() ++
               "WebSocket-Origin: http://localhost\r\n" ++
               "WebSocket-Location: ws://localhost:8080/\r\n" ++
				"Sec-Websocket-Accept: " ++ Secacc ++
				"\r\n\r\n",
		   io:format("Send handshake: ~p~n", [Msg]),
           gen_tcp:send(Socket, Msg),
           loop(Socket);
       Any ->
           io:format("Received:~p~n",[Any]),
           wait(Socket)
   end.

get_secacc(Seckey) ->
	Concat = Seckey ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
	SHA1 = sha1:binstring(Concat),
	Secacc = base64:encode_to_string(SHA1),
	Secacc.

get_seckey(HttpData) ->
	{ok, _, Header} = erlang:decode_packet(http, HttpData, []),
	get_seckey_from_header(Header).

get_seckey_from_header(Header) ->
	Result = erlang:decode_packet(httph, Header, []),
	case Result of
		{ok, {http_header, _, "Sec-Websocket-Key", _, Value}, _} -> 
			Value;
		{ok, _, Rest} ->
			get_seckey_from_header(Rest);
		_ ->
			notfound
	end.
 
prefix() ->
   "HTTP/1.1 101 Web Socket Protocol Handshake\r\n" ++ 
   "Upgrade: WebSocket\r\nConnection: Upgrade\r\n".
 
loop(Socket) ->
   receive
       {tcp, Socket, Data} ->
           io:format("received: ~p~n", [Data]),
			Str = "hello from erlang",
           io:format("send: ~p~n", [Str]),
           gen_tcp:send(Socket, Str),
           loop(Socket);
       Any ->
           io:format("Received:~p~n",[Any]),
           loop(Socket)
   end.
 
frame(Str) ->
	Len = string:len(Str),
	Data = list_to_binary(Str),
	<<1, 0:3, 1:4, 0, Len:7/integer, Data/binary>>.
	%% http://tools.ietf.org/html/rfc6455

