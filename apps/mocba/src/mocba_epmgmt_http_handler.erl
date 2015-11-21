-module(mocba_epmgmt_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3,handle/2,terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, []}.

handle(Req, State) ->
  {Cat, _} = cowboy_req:binding(epname, Req),
  io:format("epname: ~s~n", [Cat]),
  case cowboy_req:method(Req) of
      {_M, Req2} when 
            _M =:= <<"POST">>; 
            _M =:= <<"PUT">>;
            _M =:= <<"GET">> 
            ->
          {ok, Req3} = cowboy_req:reply(200, [], io_lib:format("~s~n", [Cat]), Req2),
          {ok, Req3, State};
      {<<"GET">>, Req2} ->
          {ok, Req3} = cowboy_req:reply(200, [], Cat, Req2),
          {ok, Req3, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
