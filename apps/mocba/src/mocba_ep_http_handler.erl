-module(mocba_ep_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3,handle/2,terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, []}.

handle(Req, State) ->
  {Cat, _} = cowboy_req:binding(epname, Req),
  EpName = list_to_atom(bitstring_to_list(Cat)),
  EpExists = whereis(EpName) /= undefined,
  {Method, Req0} = cowboy_req:method(Req),

  case EpExists of 
      true ->
          case mocba_ep:handle_request(EpName, Method) of
              {ok, {Code, _Headers, Body}} ->
                  {ok, Req1} = cowboy_req:reply(Code, [], Body, Req0),
                  {ok, Req1, State};
              {error, Msg} ->
                  {ok, Req1} = cowboy_req:reply(404, [], Msg, Req0),
                  {ok, Req1, State}
          end;
      _ ->
          error_logger:warning_msg("endpoint ~p not registered", [EpName]),
          {ok, Req1} = cowboy_req:reply(404, [], <<"endpoint not registered">>, Req0),
          {ok, Req1, State}
  end.


terminate(_Reason, _Req, _State) ->
  ok.
