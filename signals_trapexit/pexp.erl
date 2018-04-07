-module(pexp). 

%% Example module to demonstrate Erlang Signals and TrapExit

-export([
  te_spawn/2,
  s_spawn/2,
  run/1,
  run/2,
  run/3,
  ex_exit_reason/0,
  ex_exit_badarith/0,
  ex_normal/0,
  ex_exit_normal/0
]).


%% Example  
%% pexp:run(te,ex_exit_badarith).
%% pexp:run(te,ex_normal,fun(Pid) -> exit(Pid,kill) end).

%% Run an example case with trapexit enabled spawner 
run(te,Ex) -> 
   spawn(fun() -> pexp:te_spawn(Ex, fun(_SpawnedPid) -> ok end ) end ).

%% Run an example case without trapexit enabled spawner 
run(Ex) -> 
   spawn(fun() -> pexp:s_spawn(Ex, fun(_SpawnedPid) -> ok end ) end ).

%% Run an example case with trapexit enabled spawner and fire a fun after spawn
run(te,Ex,Fun) -> 
   spawn(fun() -> pexp:te_spawn(Ex, Fun ) end ).


%% Spawner process w/o trapexit 
s_spawn(FunctionName,PostFun) ->
   io:format("[ Spawner ] : Pid = ~p ~n",[self()]),
   process_flag(trap_exit,false),
   Pid = spawn_link(fun() -> pexp:FunctionName() end),
   PostFun(Pid),
   receive 
     Any -> io:format("[ Spawner ] : Pid = ~p Received : ~p~n",[self(),Any])
   end. 
  
%% Spawner process with trapexit 
te_spawn(FunctionName,PostFun) ->
   io:format("[ TE Spawner ] : Pid = ~p ~n",[self()]),
   process_flag(trap_exit,true),
   Pid = spawn_link(fun() -> pexp:FunctionName() end),
   PostFun(Pid),
   receive 
     Any -> io:format("[ TE Spawner ] : Pid = ~p Received : ~p~n",[self(),Any])
   end. 

%% Example case : process completes normal
ex_normal() ->
   io:format("[ Spawned Process ]: Pid is ~p~n",[self()]),
   io:format("[ Spawned Process ]: Will call end w/o any error~n"),
   timer:sleep(5000).
   
%% Example case : process exits with exit(normal)
ex_exit_normal() ->
   io:format("[ Spawned Process ]: Pid is ~p~n",[self()]),
   io:format("[ Spawned Process ]: Will call end with exit(normal)~n"),
   timer:sleep(5000),
   exit(normal).



%% Example case : process exits with exit(reason)
ex_exit_reason() ->
   io:format("[ Spawned Process ]: Pid is ~p~n",[self()]),
   io:format("[ Spawned Process ]: Will call exit(reason) after 5000~n"),
   timer:sleep(5000),
   exit(reason). 

%% Example case : process exits with badarith
ex_exit_badarith() ->
   io:format("[ Spawned Process ]: Pid is ~p~n",[self()]),
   io:format("[ Spawned Process ]: Will call 1/0 after 5000~n"),
   timer:sleep(5000),
   1/0.

