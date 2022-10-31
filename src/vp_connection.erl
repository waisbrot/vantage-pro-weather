-module(vp_connection).
-behaviour(gen_statem).


-export([stop/0, start_link/0]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([initialize/3, idle/3]).
-export([
     firmware_version/0
    ,get_time/0
    ,get_data/0
    ,get_rx_stats/0
]).

-include_lib("srly/include/serctl.hrl").
-include_lib("kernel/include/logger.hrl").

-record(data, {
    device_path
    ,device_fd
}).


%%% API
stop() ->
    ?LOG_DEBUG("Stop"),
    gen_statem:stop(?MODULE).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

firmware_version() ->
    gen_statem:call(?MODULE, firmware_version).

get_time() ->
    gen_statem:call(?MODULE, get_time).

get_data() ->
    ?LOG_DEBUG(#{msg=>"get_data/0"}),
    gen_statem:call(?MODULE, loop_data).

get_rx_stats() ->
    gen_statem:call(?MODULE, rx_stats).

%%% gen_statem
init(_Args) ->
    Data = #data{
        device_path = stillir:get_config(vantage_pro, device)
    },
    ?LOG_DEBUG("Init", #{data => Data}),
    {ok, initialize, Data, {next_event, internal, initialize}}.

callback_mode() ->
    state_functions.

% handle_event(enter, _OldState, _State, _Data) ->
%     keep_state_and_data;

% handle_event(_EventType, _EventContent, _State, _Data) ->
%     keep_state_and_data.

terminate(_Reason, _State, #data{device_fd = undefined}) ->
    ok;
terminate(_Reason, _State, #data{device_fd = FD}) ->
    serctl:close(FD).

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% State functions

initialize(internal, initialize, Data) ->
    ?LOG_DEBUG("Opening serial connection"),
    {ok, FD} = serctl:open(Data#data.device_path),
    Data1 = Data#data{device_fd = FD},
    vp_serial:configure(FD),
    {next_state, idle, Data1}.

idle(internal, initialize, _Data) ->
    keep_state_and_data;
idle({call, From}, loop_data, #data{device_fd = FD}) ->
    ?LOG_DEBUG(#{msg=>"loop data req"}),
    ok = vp_serial:request(FD, <<"LOOP 1">>),
    ok = vp_serial:read_ack(FD),
    {ok, Reply} = vp_serial:read(FD, 99),
    CallReply = {data, vp_message:decode_loop(Reply)},
    {keep_state_and_data, {reply, From, CallReply}};
idle({call, From}, rx_stats, #data{device_fd = FD}) ->
    ?LOG_DEBUG(#{msg=>"RX check"}),
    ok = vp_serial:request(FD, <<"RXCHECK">>),
    ok = vp_serial:read_ok(FD),
    {ok, Reply} = vp_serial:read(),
    CallReply = {rx_stats, Reply},
    {keep_state_and_data, {reply, From, CallReply}};
idle({call, From}, firmware_version, #data{device_fd = FD}) ->
    ?LOG_DEBUG(#{msg=>"firmware version req"}),
    ok = vp_serial:request(FD, <<"VER">>),
    {ok, Reply} = vp_serial:read(FD),
    CallReply = {version, Reply},
    {keep_state_and_data, {reply, From, CallReply}};
idle({call, From}, get_time, #data{device_fd = FD}) ->
    ?LOG_DEBUG(#{msg=>"get time request"}),
    ok = vp_serial:request(FD, <<"GETTIME">>),
    {ok, Reply} = vp_serial:read(FD, 9),
    CallReply = {time, vp_message:decode_time(Reply)},
    {keep_state_and_data, {reply, From, CallReply}}.

%%% Internal functions

