
-module(easy_sup).

-export([start_link/1]).

-export([one_for_one/2,  one_for_one/3]).
-export([one_for_all/2,  one_for_all/3]).
-export([one_for_rest/2, one_for_rest/3]).
-export([simple_one_for_one/2,  simple_one_for_one/3]).
-export([worker/1, worker/2]).

-export([test/0]).

start_link({Register, Mfa, MaxRestarts, MaxTime, ChildSpecs}) ->
    supervisor:start_link(Register, easy_sup, [{Mfa, MaxRestarts, MaxTime, ChildSpecs}]).

one_for_all(Name, ChildSpecs) -> one_for_all(Name, [], ChildSpecs).
one_for_all(Name, Options, ChildSpecs) -> supervisor_spec(Name, one_for_all, Options, ChildSpecs).

one_for_rest(Name, ChildSpecs) -> one_for_rest(Name, [], ChildSpecs).
one_for_rest(Name, Options, ChildSpecs) -> supervisor_spec(Name, one_for_rest, Options, ChildSpecs).

one_for_one(Name, ChildSpecs) -> one_for_one(Name, [], ChildSpecs).
one_for_one(Name, Options, ChildSpecs) -> supervisor_spec(Name, one_for_one, Options, ChildSpecs).

simple_one_for_one(Name, ChildSpec) -> simple_one_for_one(Name, [], ChildSpec).
simple_one_for_one(Name, Options, ChildSpec) -> supervisor_spec(Name, simple_one_for_one, Options, [ChildSpec]).

supervisor_spec(Name, Type, Options, ChildSpecs) ->
    Register    = {proplists:get_value(register,     Options, local), Name},
    MaxRestarts =  proplists:get_value(max_restarts, Options, 10),
    MaxTime     =  proplists:get_value(max_time,     Options, 10),
    {Register, {easy_sup, start_link, [{Type, MaxRestarts, MaxTime, ChildSpecs}]}}.

worker(Name) ->
    worker(Name, []).
worker(Name0, Options) ->
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
%%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    dot(),test_worker(),
    dot(),test_supervisor(),
    dot(),test_big(),
    io:format("~n"),
    'Test success!'.

test_supervisor() ->
    {{local, top}, {easy_sup, start_link, [{one_for_all,        10, 10, [_]}]}} = one_for_all       (top, [worker(test)]),
    {{local, top}, {easy_sup, start_link, [{one_for_rest,       10, 10, [_]}]}} = one_for_rest      (top, [worker(test)]),
    {{local, top}, {easy_sup, start_link, [{simple_one_for_one, 10, 10, [_]}]}} = simple_one_for_one(top, worker(test)),
    {{local, top}, {easy_sup, start_link, [{one_for_one,        10, 10, [_]}]}} = one_for_one       (top, [worker(test)]).

test_worker() ->
    {test, {test, start_link, []}, transient, 1000, worker, [test]} = worker(test),
    {test, {test, start, [a,b,c]}, transient, 1000, worker, [test]} = worker({test, start, [a,b,c]}),
    {test, {test, start_link, []}, transient, 1000, worker, [test]} = worker(test, [{modules, [test]}]).

test_big() ->
    io:format("~n~p~n", [one_for_all(se_sup, [worker(se_mapper),
					      simple_one_for_one(se_endpoints_sup, worker({se_endpoint, start, [ahe]}))
		    ])]).

dot() ->
    io:format(".").
