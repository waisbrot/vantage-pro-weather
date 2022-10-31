-module(vp_prometheus_handler).
-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, prometheus_text_format:format(), Req0),
    {ok, Req, State}.
