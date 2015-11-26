-module(mocba_ep_sup_tests).
-include_lib("eunit/include/eunit.hrl").

mocba_ep_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun should_not_find_child_spec/1,
      fun should_not_start_two/1
     ]}.

setup() ->
    %?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = mocba_ep_sup:start_link(),
    Pid.

teardown(Pid) ->
    Ref = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, _, _, _} ->
            ?debugMsg("Pid down")
    after
        100 ->
            ?debugMsg("timeout")
    end.

should_not_find_child_spec(Pid) ->
    fun() ->
        {ok, _} = mocba_ep_sup:start_ep(test_ep, #{}),
        ok = mocba_ep_sup:stop_ep(test_ep),
        {error, _} = supervisor:get_childspec(Pid, test_ep)
    end.

should_not_start_two(_Pid) ->
    fun() ->
        {ok, _} = mocba_ep_sup:start_ep(test_ep, #{}),
        {ok, _} = mocba_ep_sup:start_ep(test_ep, #{})
    end.
