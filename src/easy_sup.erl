
-module(easy_sup).

-export([start_link/1, init/1]).

-export([one_for_one/2,  one_for_one/3]).
-export([one_for_all/2,  one_for_all/3]).
-export([rest_for_one/2, rest_for_one/3]).
-export([simple_one_for_one/2,  simple_one_for_one/3]).
-export([worker/1, worker/2]).

-export([test/0]).

-export([testWorker/0]).

strip_supervisor_childspec({_Name, {easy_sup, start_link,
				    [{Register, Args = {_Type, _MaxRestarts, _MaxTime, _ChildSpecs}}]},
			    transient, 1000, supervisor, []}) ->
    {Register, Args}.

start_link({Register, Args}) ->
    supervisor:start_link(Register, easy_sup, Args);
start_link(SupervisorChildspec) ->
    start_link(strip_supervisor_childspec(SupervisorChildspec)).

init({Type, MaxRestarts, MaxTime, ChildSpecs}) ->
    {ok, {{Type, MaxRestarts, MaxTime}, ChildSpecs}}.

one_for_all(Name, ChildSpecs)          -> one_for_all(Name, [], ChildSpecs).
one_for_all(Name, Options, ChildSpecs) -> supervisor_childspec(Name, one_for_all, Options, ChildSpecs).

rest_for_one(Name, ChildSpecs)          -> rest_for_one(Name, [], ChildSpecs).
rest_for_one(Name, Options, ChildSpecs) -> supervisor_childspec(Name, rest_for_one, Options, ChildSpecs).

one_for_one(Name, ChildSpecs)          -> one_for_one(Name, [], ChildSpecs).
one_for_one(Name, Options, ChildSpecs) -> supervisor_childspec(Name, one_for_one, Options, ChildSpecs).

simple_one_for_one(Name, ChildSpec)          -> simple_one_for_one(Name, [], ChildSpec).
simple_one_for_one(Name, Options, ChildSpec) -> supervisor_childspec(Name, simple_one_for_one, Options, [ChildSpec]).

worker(Name)          -> worker(Name, []).
worker(Name, Options) -> worker_childspec(Name, Options).

supervisor_childspec(Name, Type, Options, ChildSpecs) when is_list(ChildSpecs) ->
    Register    = {proplists:get_value(register,     Options, local), Name},
    MaxRestarts =  proplists:get_value(max_restarts, Options, 10),
    MaxTime     =  proplists:get_value(max_time,     Options, 10),
    SupervisorChildspec = {Name, {easy_sup, start_link, [{Register, {Type, MaxRestarts, MaxTime, ChildSpecs}}]},
			   transient, 1000, supervisor, []},
    supervisor:check_childspecs([SupervisorChildspec]),
    SupervisorChildspec.

worker_childspec(Name0, Options) ->
    Name               = spec_name(Name0),
    Mfa={Module, _, _} = spec_mfa(Name0),
    Restart            = proplists:get_value(restart,  Options, transient),
    Shutdown           = proplists:get_value(shutdown, Options, 1000),
    Modules            = proplists:get_value(modules,  Options, [Module]),
    {Name, Mfa, Restart, Shutdown, worker, Modules}.

spec_name(Mfa) when is_atom(Mfa)  ->
    Mfa;
spec_name({M,_F,_A}) ->
    M.

spec_mfa(Mfa) when is_atom(Mfa)  ->
    {Mfa, start_link, []};
spec_mfa(Mfa={_M,_F,_A}) ->
    Mfa.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests (Homegrown testing HACK!) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    Parent = self(),
    spawn_link(fun() ->
		       process_flag(trap_exit, true),
		       dot(),test_supervisor_start(),
		       dot(),test_worker(),
		       dot(),test_supervisor(),
		       flush(),
		       io:format("~n"),
		       Parent ! done
	       end),
    receive
	done -> receive after 100 -> 'Test Success' end; M -> {'FAILURE', M} after 5000 -> 'FAILURE - TIMEOUT' end.

flush() ->
    receive M -> io:format("Flush: ~p~n", [M]), flush() after 0 -> ok end.

dot() ->
    io:format(".").

test_supervisor() ->
    {top,{easy_sup,start_link,[{{local,top},{one_for_all,10,10,[_]}}]},transient,1000,supervisor,[]}
	= one_for_all(top, [worker(test)]),
    {top,{_,_,[{_,{rest_for_one,10,10,[_]}}]},transient,1000,supervisor,[]} = rest_for_one(top, [worker(test)]),
    {top,{_,_,[{_,{one_for_one,10,10,[_]}}]},transient,1000,supervisor,[]} = one_for_one(top, [worker(test)]),
    {top,{_,_,[{_,{simple_one_for_one,10,10,[_]}}]},transient,1000,supervisor,[]} = simple_one_for_one(top, worker(test)).

test_worker() ->
    {test, {test, start_link, []}, transient, 1000, worker, [test]} = worker(test),
    {test, {test, start, [a,b,c]}, transient, 1000, worker, [test]} = worker({test, start, [a,b,c]}),
    {test, {test, start_link, []}, transient, 1000, worker, [test]} = worker(test, [{modules, [test]}]).

test_supervisor_start() ->
    {ok, Pid} = easy_sup:start_link(
		  easy_sup:one_for_one( root,
					[easy_sup:one_for_all( middle , [{max_restarts, 5}, {max_time, 20}],
							       [easy_sup:worker({easy_sup, testWorker, []}),
								easy_sup:simple_one_for_one( dynamic1,
											     easy_sup:worker(dyn_worker1))]),
					 easy_sup:rest_for_one( middle2,
								[easy_sup:simple_one_for_one( dynamic2 ,
											      [{max_restarts, 3}, {max_time, 33}],
											      easy_sup:worker(dyn_worker1)),
								 easy_sup:simple_one_for_one( dynamic3 , easy_sup:worker(dyn_worker1))]),
					 easy_sup:worker( {easy_sup, testWorker, []}),
					 easy_sup:simple_one_for_one( dynamic4 , easy_sup:worker(dyn_worker2, [{restart, temporary}]))])),

    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
	{'DOWN', MRef, process, Pid, shutdown} ->
	    ok
    after
	5000 ->
	    exit(testfailure)
    end,
    receive
	{'EXIT', Pid, shutdown} ->
	    ok
    after
	5000 ->
	    exit(testfailure)
    end.

testWorker() ->
    Parent = self(),
    spawn_link(fun() ->
		       process_flag(trap_exit, true),
		       Parent ! {ok, self()},
		       receive M -> M
		       after 5000 -> timeout
		       end
	       end),
    receive M -> M after 5000 -> timeout2 end.

