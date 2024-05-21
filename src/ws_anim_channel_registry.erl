-module(ws_anim_channel_registry).

-behaviour(gen_server).

-export([start_link/0]).
-export([list/0]).
-export([start/0]).
-export([lookup/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-record(state, {next_id = 1,
                channels = []}).

-define(REF, registry).

start_link() ->
    gen_server:start({local, ?REF}, ?MODULE, _Args = undefined, _Opts = []).

list() ->
    gen_server:call(?REF, list).

start() ->
    gen_server:call(?REF, start).

lookup(Name) ->
    gen_server:call(?REF, {lookup, Name}).

init(_Args) ->
    io:format("registry started~n", []),
    {ok, #state{}}.

handle_call(list, _From, State) ->
    NamesList = [Name || {Name, _} <- State#state.channels],
    Names = iolist_to_binary(lists:join(<<", ">>, NamesList)),
    Log = <<"{\"Channels\": [", Names/binary, "]}">>,
    {reply, Log, State};
handle_call(start, _From, State = #state{next_id = NextId, channels = Channels}) ->
    Id = integer_to_binary(NextId),
    {ok, Pid} = ws_anim_channel:start(Id),
    {reply,
     {Pid, Id},
     State#state{next_id = NextId + 1,
                 channels = [{Id, Pid} | Channels]}};
handle_call({lookup, Name}, _From, State) ->
    MaybePid = proplists:get_value(Name, State),
    {reply, MaybePid, State}.

handle_cast(_, State) ->
    {noreply, State}.

