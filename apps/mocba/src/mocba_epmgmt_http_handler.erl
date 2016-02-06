-module(mocba_epmgmt_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3,handle/2,terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, []}.

to_config(#{<<"mappings">> := Mappings}) ->
    lists:foldl(fun(#{<<"method">> := Method, <<"replies">> := Replies}, Acc) ->
                          C2 = [{Code, [], base64:decode(Data)} || 
                                #{<<"code">> := Code, <<"data">> := Data} <- Replies],
                          Acc#{Method => C2} end,
                   #{}, Mappings).

ep_uri(Req, EpName) ->
    {H, Req2} = cowboy_req:host(Req),
    {P, Req3} = cowboy_req:port(Req2),
    {lists:concat(["http://", binary_to_list(H), ":", P, "/ep/", EpName]), Req3}.

handle(Req, State) ->
  {Cat, _} = cowboy_req:binding(epname, Req),
  EpName = list_to_atom(bitstring_to_list(Cat)),
  case cowboy_req:method(Req) of
      {<<"PUT">>, Req2} ->
          {ok, Data, Req3} = cowboy_req:body(Req2),
          {EpUri, Req4} = ep_uri(Req3, EpName),
          C = to_config(jsone:decode(Data)),
          {ok, _} = mocba_ep_sup:start_ep(EpName, C),
          {ok, Req5} = cowboy_req:reply(200, [], EpUri, Req4),
          {ok, Req5, State};
      {<<"GET">>, Req3} ->
          {ok, Req4} = cowboy_req:reply(200, [], Cat, Req3),
          {ok, Req4, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
