% Utility functions for reading and writing data to Vantage Pro
-module(vp_serial).

-export([
     configure/1
    ,read/1, read/2
    ,read_ack/1
    ,read_ok/1
    ,request/2
    ]).

-define(SLEEP, 1000).
-define(MAX_READ_TRIES, 3).
-define(WAKE_TRIES, 3).
-include_lib("kernel/include/logger.hrl").
-include_lib("srly/include/serctl.hrl").

-spec configure(serctl:fd()) -> ok.
configure(FD) ->
        % Set the terminal attributes to:
    %   raw, no hardware flow control, 9600
    ?LOG_DEBUG(#{msg => "Set raw mode"}),
    Termios = serctl:mode(raw),
    ?LOG_DEBUG(#{msg => "Set cflags"}),
    Termios1 = lists:foldl(
        fun(Flag, Acc) -> serctl:setflag(Acc, [{cflag, [{Flag, true}]}]) end,
        Termios,
        [
            crtscts
            ,cs8
            ,clocal
            ,cread
        ]
    ),
    ?LOG_DEBUG(#{msg => "Set iflags"}),
    Termios2 = lists:foldl(
        fun(Flag, Acc) -> serctl:setflag(Acc, [{iflag, [{Flag, true}]}]) end,
        Termios1,
        [
            ignbrk
            ,ignpar
        ]
    ),
    CC = lists:foldl(
        fun({Offset, Val}, Bin) ->
            serctl:offset(Bin, {Offset, Val})
        end,
        % zero'ed bytes
        <<0:(serctl:constant(nccs) * 8)>>,
        [
            % Minimum number of characters
            {serctl:constant(vmin), 0},
            % Timeout in deciseconds
            {serctl:constant(vtime), 10}
        ]
    ),
    ?LOG_DEBUG(#{msg=>"Set cc", cc => CC}),
    Termios3 = Termios2#termios{cc = CC},
    ?LOG_DEBUG(#{msg=>"Set ispeed"}),
    Termios4 = serctl:ispeed(Termios3, b19200),
    ?LOG_DEBUG(#{msg=>"Set ospeed"}),
    Termios5 = serctl:ospeed(Termios4, b19200),
    ?LOG_DEBUG(#{msg=>"Set attributes", attr => Termios5}),
    ok = serctl:tcsetattr(FD, tcsaflush, Termios5),
    ?LOG_DEBUG(#{msg=>"Flush"}),
    ok = serctl:tcflush(FD, tcioflush),
    ok = wake_station(FD).

-spec wake_station(serctl:fd()) -> ok | {error, atom()}.
wake_station(FD) ->
    wake_station(FD, 0).
-spec wake_station(serctl:fd(), non_neg_integer()) -> ok | {error, atom()}.
wake_station(_FD, ?WAKE_TRIES) ->
    ?LOG_ERROR(#{msg => "max tries to wake station"}),
    {error, station_not_awake};
wake_station(FD, Tries) ->
    ?LOG_DEBUG(#{msg=>"writing wake message to station", tries => Tries}),
    ok = serctl:write(FD, <<13:8>>),
    case read(FD, 1, noretry) of
        {ok, Response} ->
            ?LOG_DEBUG(#{msg=>"read OK (ignored response)", response=> Response}),
            ok;
        {error, max_retries} ->
            ?LOG_DEBUG(#{msg=>"try waking again..."}),
            wake_station(FD, Tries+1)
    end.

-spec request(serctl:fd(), binary()) -> ok.
request(FD, Request) ->
    wake_station(FD),
    ?LOG_DEBUG(#{msg=>"writing to serial", request => Request}),
    ok = serctl:write(FD, <<Request/binary, "\n">>).

read(FD) ->
    read(FD, 128).

read(FD, ReadSize) ->
    read(FD, ReadSize, ReadSize, ?MAX_READ_TRIES).

read(FD, ReadSize, noretry) ->
    read(FD, ReadSize, ReadSize, 1).

-spec read_ack(serctl:fd()) -> ok | {error, atom()}.
read_ack(FD) ->
    Ack = read(FD, 1),
    ?LOG_DEBUG(#{msg=> "read ack", reply=>Ack}),
    case Ack of
        {ok, <<16#6>>} -> 
            ok;
        {ok, <<"\r">>} -> 
            read_ack(FD);  % Try again when we get a \r
        {ok, <<"\n">>} ->
            read_ack(FD);  % Try again when we get a \n
        {ok, <<16#21>>} -> 
            {error, nak};
        {ok, <<16#18>>} -> 
            {error, crc_failed};
        {error, _} = E -> E
    end.

-spec read_ok(serctl:fd()) -> ok | {error, atom()}.
read_ok(FD) ->
    OK = read(FD, 6),
    case OK of
        {ok, <<"\n\rOK\n\r">>} -> 
            ok;
        {ok, Other} -> 
            ?LOG_ERROR(#{msg=>"response other than OK", response=>Other}), 
            {error, not_ok};
        {error, _} = E ->
            E
    end.

-spec read(serctl:fd(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> {ok, binary()} | {error, atom()}.
read(_FD, _ReadSize, _ExpectSize, 0) ->
    {error, max_retries};
read(FD, ReadSize, ExpectSize, Retries) ->
    ?LOG_DEBUG(#{msg=>"sleep", sleep=> ?SLEEP}),
    timer:sleep(?SLEEP),
    ?LOG_DEBUG(#{msg=>"reading from serial", size => ReadSize, tries_left => Retries}),
    case serctl:read(FD, ReadSize) of
        {ok, <<Response:ExpectSize/binary>>} ->
            ?LOG_DEBUG(#{msg => "finished read", response => Response}),
            {ok, Response};
        {ok, <<>>} ->
            ?LOG_DEBUG(#{msg => "empty reply; retry"}),
            read(FD, ReadSize, ExpectSize, Retries-1);
        {error, eagain} ->
            ?LOG_DEBUG(#{msg=>"got EAGAIN; retry"}),
            read(FD, ReadSize, ExpectSize, Retries-1)
    end.
