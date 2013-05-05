-module(easy_sup).

-compile([export_all]).

worker(Name) ->
    worker(Name, []).
worker(Name0, Options) ->
    Name               = spec_name(Name0),
    Mfa={Module, _, _} = spec_mfa(Name0),
    Restart            = proplists:get_value(restart,  Options, transient),
    Shutdown           = proplists:get_value(shutdown, Options, 1000),
    Modules            = proplists:get_value(modules,  Options, [Module]),
    Spec = {Name, Mfa, Restart, Shutdown, worker, Modules},
    ok = supervisor:check_childspecs([Spec]),
    Spec.

spec_name(Mfa) when is_atom(Mfa)  ->
    Mfa;
spec_name({M,_F,_A}) ->
    M.

spec_mfa(Mfa) when is_atom(Mfa)  ->
    {Mfa, start_link, []};
spec_mfa(Mfa={_M,_F,_A}) ->
    Mfa.

test() ->
    test_worker().

test_worker() ->
    {test, {test, start_link, []}, transient, 1000, worker, [test]} = worker(test),
    {test, {test, start, [a,b,c]}, transient, 1000, worker, [test]} = worker({test, start, [a,b,c]}),
    {test, {test, start_link, []}, transient, 1000, worker, [test]} = worker(test, [{modules, [test]}]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Brainstorm %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

experiment() ->

%    easy_sup(one_for_all(se, [ worker( se_mapper),
%		      dynamic( se_endpoints )
%		    ])),

    easy_sup({se_sup, {one_for_all,
		       [{se_mapper,    worker},
			{se_endpoints, simple_one_for_one}]}}).





%			{se_foo_sup,       {delayed_restart, [{delay, [0,10,100,1000,10000,30000]}],
%					    [{se_foo, {worker,
%						       {se_foo, start_link, []}}}]}}]}}).


easy_sup(_) ->
    toto.
