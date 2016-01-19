-module(mocba_epmgmt_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3,handle/2,terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, []}.

to_config(#{<<"mappings">> := Mappings}) ->
  %io:format("json: ~p~n", [Stuff]),
  lists:foldl(fun(#{<<"method">> := Method, <<"replies">> := Replies}, Acc) ->
                          C2 = [{Code, [], base64:decode(Data)} || 
                                #{<<"code">> := Code, <<"data">> := Data} <- Replies],
                          Acc#{Method => C2} end,
                   #{}, Mappings).

handle(Req, State) ->
  {Cat, _} = cowboy_req:binding(epname, Req),
  %{Headers, _} = cowboy_req:headers(Req),
  EpName = list_to_atom(bitstring_to_list(Cat)),
  %io:format("headers: ~p~n", [Headers]),
  case cowboy_req:method(Req) of
      {<<"PUT">>, Req2} ->
          {ok, Data, Req3} = cowboy_req:body(Req2),
          C = to_config(jsone:decode(Data)),
          {ok, _} = mocba_ep_sup:start_ep(EpName, C),
          {ok, Req4} = cowboy_req:reply(200, [], io_lib:format("~s~n", [Cat]), Req3),
          {ok, Req4, State};
      {<<"GET">>, Req3} ->
          {ok, Req4} = cowboy_req:reply(200, [], Cat, Req3),
          {ok, Req4, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
