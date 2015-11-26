-module(mocba_epmgmt_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3,handle/2,terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, []}.

handle(Req, State) ->
  {Cat, _} = cowboy_req:binding(epname, Req),
  {ok, Data, Req2} = cowboy_req:body(Req),
  {Headers, _} = cowboy_req:headers(Req),
  Json = jsone:decode(Data),
  io:format("json: ~p~n", [Json]),
  io:format("headers: ~p~n", [Headers]),
  case cowboy_req:method(Req2) of
      {M, Req3} when 
            M =:= <<"POST">>; 
            M =:= <<"PUT">> ->
          {ok, Req4} = cowboy_req:reply(200, [], io_lib:format("~s~n", [Cat]), Req3),
          {ok, Req4, State};
      {<<"GET">>, Req3} ->
          {ok, Req4} = cowboy_req:reply(200, [], Cat, Req3),
          {ok, Req4, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
