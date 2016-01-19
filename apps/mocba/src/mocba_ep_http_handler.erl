-module(mocba_ep_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3,handle/2,terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, []}.

handle(Req, State) ->
  {Cat, _} = cowboy_req:binding(epname, Req),
  EpName = list_to_atom(bitstring_to_list(Cat)),
  EpExists = whereis(EpName) /= undefined,

  case EpExists of 
      true ->
          {Method, Req0} = cowboy_req:method(Req),
          {ok, Body, Req1} = cowboy_req:body(Req0),
          {Headers, Req2} = cowboy_req:headers(Req1),
          case mocba_ep:handle_request(EpName, {Method, Headers, Body} ) of
              {ok, {Code, OutHeaders, OutBody}} ->
                  {ok, Req3} = cowboy_req:reply(Code, OutHeaders, OutBody, Req2),
                  {ok, Req3, State};
              {error, Msg} ->
                  {ok, Req3} = cowboy_req:reply(404, [], Msg, Req2),
                  {ok, Req3, State}
          end;
      _ ->
          error_logger:warning_msg("endpoint ~p not registered", [EpName]),
          {ok, Req1} = cowboy_req:reply(404, [], <<"endpoint not registered\n">>, Req),
          {ok, Req1, State}
  end.


terminate(_Reason, _Req, _State) ->
  ok.
