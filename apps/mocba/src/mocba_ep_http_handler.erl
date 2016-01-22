-module(mocba_ep_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3,handle/2,terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, []}.

handle_request(Req, EpName, MHB) ->
    case mocba_ep:handle_request(EpName, MHB) of
        {ok, {Code, OutHeaders, OutBody}} ->
          cowboy_req:reply(Code, OutHeaders, OutBody, Req);
        {error, Msg} ->
          cowboy_req:reply(404, [], Msg, Req)
    end.

extract_mhb(Req) ->
    {Method, Req0} = cowboy_req:method(Req),
    {ok, Body, Req1} = cowboy_req:body(Req0),
    {Headers, Req2} = cowboy_req:headers(Req1),
    {Req2, {Method, Headers, Body}}.

handle(Req, State) ->
  {Cat, _} = cowboy_req:binding(epname, Req),
  EpName = list_to_atom(bitstring_to_list(Cat)),
  EpExists = whereis(EpName) /= undefined,
  case EpExists of 
      true ->
          {Req1, MHB} = extract_mhb(Req),
          {ok, Req2} = handle_request(Req1, EpName, MHB),
          {ok, Req2, State};
      _ ->
          error_logger:warning_msg("endpoint ~p not registered", [EpName]),
          {ok, Req1} = cowboy_req:reply(404, [], <<"endpoint not registered\n">>, Req),
          {ok, Req1, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
