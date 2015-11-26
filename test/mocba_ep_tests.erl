-module(mocba_ep_tests).
-include_lib("eunit/include/eunit.hrl").

mocba_ep_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun it_is_alive/1,
      fun should_find/1,
      fun should_cycle/1,
      fun should_not_find/1
     ]}.

setup() ->
    %?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = mocba_ep:start_link(#{<<"GET">> => 
                                      [
                                       {200, #{}, <<"OK">>},
                                       {200, #{}, <<"OK2">>}
                                      ]}),
    Pid.

teardown(Pid) ->
    exit(Pid, kill).

it_is_alive(Pid) -> 
    ?_assertEqual(true, is_process_alive(Pid)).

should_find(Pid) ->
    %?debugMsg("omething"),
    fun() -> {ok, {200, #{}, <<"OK">>}} = mocba_ep:handle_request(Pid, <<"GET">>) end.

should_cycle(Pid) ->
    %?debugMsg("omething"),
    fun() -> 
            {ok, {200, #{}, <<"OK">>}} = mocba_ep:handle_request(Pid, <<"GET">>),
            {ok, {200, #{}, <<"OK2">>}} = mocba_ep:handle_request(Pid, <<"GET">>),
            {ok, {200, #{}, <<"OK">>}} = mocba_ep:handle_request(Pid, <<"GET">>)
    end.

should_not_find(Pid) ->
    %?debugMsg("omething"),
    fun() -> {error, _} = mocba_ep:handle_request(Pid, <<"POST">>) end.



