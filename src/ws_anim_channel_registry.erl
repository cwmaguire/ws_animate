-module(ws_anim_channel_registry).

-include("ws_anim.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([list/0]).
-export([start/0]).
-export([lookup/1]).
-export([sub/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {next_id = 1,
                channels = [],
                subs = []}).

-define(REF, registry).

start_link() ->
    gen_server:start({local, ?REF}, ?MODULE, _Args = undefined, _Opts = []).

list() ->
    gen_server:call(?REF, list).

start() ->
    gen_server:call(?REF, start).

lookup(Name) ->
    gen_server:call(?REF, {lookup, Name}).

sub(<<"channels">>) ->
    gen_server:call(?REF, {sub, <<"channels">>}).

init(_Args) ->
    io:format("registry started~n", []),
    {ok, #state{}}.

handle_call(list, _From, State) ->
    NamesList = [Name || {Name, _} <- State#state.channels],
    Names = iolist_to_binary(lists:join(<<", ">>, NamesList)),

    Log = ws_anim_utils:log(<<"{\"Channels\": [", Names/binary, "]}">>),
    {reply, Log, State};
handle_call(start,
            {From, _},
            State = #state{next_id = NextId,
                           channels = Channels,
                           subs = Subs}) ->
    Id = integer_to_binary(NextId),
    {ok, Pid} = ws_anim_channel:start(Id, From),
    _Ref = monitor(process, Pid),
    NewChannels = [{Id, Pid} | Channels],
    %io:format(user, "New channel (~p) started by Socket (~p), sending NewChannels (~p) to Subs (~p)~n", [Pid, From, NewChannels, Subs]),
    send_channels(Subs, NewChannels),
    {reply,
     {Pid, Id},
     State#state{next_id = NextId + 1,
                 channels = NewChannels}};
handle_call({lookup, Name}, _From, State = #state{channels = Channels}) ->
    MaybePid = proplists:get_value(Name, Channels),
    {reply, MaybePid, State};
handle_call({sub, Type = <<"channels">>}, {From, _}, State = #state{subs = Subs0, channels = Channels}) ->
    case lists:member({Type, From}, Subs0) of
        false ->
            Msgs = [?utils:info(#{channel => Id}) || {Id, _} <- Channels],
            Subs = [{From, Type} | Subs0],
            {reply, Msgs, State#state{subs = Subs}};
        _ ->
            {reply, [], State}
    end;
handle_call(Other, _From, State) ->
    io:format("Received unexpected channel registry message: ~p~n", [Other]),
    {reply, [], State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Info}, State = #state{channels = Channels0, subs = Subs}) ->
    io:format(user, "Channel ~p down: info = ~p~n", [Pid, Info]),
    Channels =
        case lists:member(Pid, Channels0) of
            true ->
                io:format("Channel Registry: removing channel ~p; received monitor message 'DOWN'~n", [Pid]),
                lists:delete(Pid, Channels0);
            _ ->
                Channels0
        end,
    send_channels(Subs, Channels),
    {noreply, State#state{channels = Channels}}.

send_channels(Subs, Channels) ->
    ClearMsg = ?utils:info(#{info => <<"clear_channels">>}),
    AddMsgs = [?utils:info(#{channel => Id}) || {Id, _} <- Channels],
    Msgs = [ClearMsg | AddMsgs],
    [Socket ! {send, Msg} || {Socket, <<"channels">>} <- Subs, Msg <- Msgs].

