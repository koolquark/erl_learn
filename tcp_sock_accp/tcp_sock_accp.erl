-module(tcp_sock_accp).
-export([listen/1,
         start_acceptors/2,
	 run_example/0
       ]).

%% This module demonstrates a simple acceptor pool on a tcp listen socket
%% N processes are started to listen on an acceptor port.

run_example() ->
  Port =  5000,
  NumAcceptors = 5,
  ListenSock = listen(Port),
  start_acceptors(ListenSock,NumAcceptors),
  io:format("~n~b Acceptors started on 127.0.0.1:~b",[NumAcceptors,Port]),
  io:format("~n Open Telenet from other shell as in following command ~n"),
  io:format("for i in {1..5} ; do xterm -e telnet 127.0.0.1 5000 ; done ~n"),
  ok.
  
listen(Port) ->
  {ok,ListenSocket} = gen_tcp:listen(Port, [binary, {packet,0}, {active,false}]),
  ListenSocket.


start_acceptors(ListenSock,N) ->
   Accept = fun() ->
     {ok,ConnectionSock} = gen_tcp:accept(ListenSock),
     Msg = io_lib:format("~nAccepted by PID = ~p",[self()]),
     io:format([Msg]),
     ok = gen_tcp:send(ConnectionSock,list_to_binary(Msg)),
     ok = gen_tcp:close(ConnectionSock)     
   end,
   [ spawn( fun() -> Accept() end ) || _K <- lists:seq(1,N) ].
   
    
 
   
  
  
