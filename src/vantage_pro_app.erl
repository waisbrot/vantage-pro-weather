%%%-------------------------------------------------------------------
%% @doc vantage-pro-weather public API
%% @end
%%%-------------------------------------------------------------------

-module(vantage_pro_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(APP, vantage_pro).

start(_StartType, _StartArgs) ->
    configure(),
    ok = logger:update_primary_config(#{level => stillir:get_config(vantage_pro, log_level)}),
    ok = setup_cowboy(),
    vantage_pro_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
-spec configure() -> ok.
configure() ->
    Config = [
        {device, "DEVICE", [{default, "/dev/ttyUSB0"}]},
        {log_level, "LOG_LEVEL", [{default, debug}, {transform, atom}]},
        {http_port, "HTTP_PORT", [{default, 8082}, {transform, integer}]}
    ],
    Config1 = read_app_config(Config),
    ok = stillir:set_config(?APP, Config1).

-spec read_app_config(stillir:config_specs()) -> stillir:config_specs().
read_app_config(Config) ->
    lists:map(fun ({AppVar, EnvVar, Opts0}) ->
                      Opts1 = case application:get_env(?APP, AppVar) of
                                  {ok, Value} ->
                                      lists:keystore(default, 1, Opts0, {default, Value});
                                  undefined ->
                                      Opts0
                              end,
                      {AppVar, EnvVar, Opts1}
              end,
              Config).

setup_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/metrics", vp_prometheus_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, stillir:get_config(vantage_pro, http_port)}],
        #{env => #{dispatch => Dispatch}, request_timeout => 60000}
    ),
    ok.