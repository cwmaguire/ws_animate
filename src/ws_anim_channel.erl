-module(ws_anim_channel).

-behaviour(gen_server).

-export([start/1]).
-export([join/1]).
-export([leave/1]).
-export([add_animator/2]).
-export([sub/2]).
-export([subs/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {id = "no ID set",
                sockets = [],
                animators = [],
                subs = []}).

start(Id) ->
    gen_server:start(?MODULE, _Args = Id, _Opts = []).

join(ChannelPid) ->
    gen_server:call(ChannelPid, join).

leave(ChannelPid) ->
    gen_server:call(ChannelPid, leave).

add_animator(ChannelPid, Name) ->
    gen_server:call(ChannelPid, {add_animator, Name}).

sub(ChannelPid, Type) ->
    gen_server:call(ChannelPid, {sub, Type}).

subs(ChannelPid) ->
    gen_server:call(ChannelPid, subs).

init(Id) ->
    {ok, #state{id = Id}}.

handle_call(join, {From, _}, State = #state{id = Id, sockets = Sockets, subs = Subs}) ->
    Log = "{\"log\": \"Joined " ++ Id ++ "\"}",
    {reply, Log, State#state{sockets = [From | Sockets], subs = [{From, log} | Subs]}};
handle_call(leave, {From, _}, State = #state{id = Id, sockets = Sockets, subs = Subs}) ->
    Log = "{\"log\": \"Left " ++ Id ++ "\"}",
    Filter = fun({From_, _}) when From_ == From -> false; (_) -> true end,
    NewSubs = lists:filter(Filter, Subs),
    NewSockets = lists:delete(From, Sockets),
    {reply, Log, State#state{sockets = NewSockets, subs = NewSubs}};
handle_call({add_animator, Name}, _From, State = #state{animators = Animators}) ->
    {Log, Animator} = add_animator_(Name, State),
    {reply, Log, State#state{animators = [Animator | Animators]}};
handle_call({sub, TypeBin}, {From, _}, State = #state{subs = Subs}) ->
    Type = type(TypeBin),
    case {Type, lists:member({From, Type}, Subs)} of
        {undefined, _} ->
            Log = log(<<"Invalid type: ", TypeBin/binary>>),
            {reply, Log, State};
        {_, true} ->
            Log = log(<<"Already subbed to ", TypeBin/binary>>),
            {reply, Log, State};
        {_, false} ->
            Log = log(<<"Subbed to ", TypeBin/binary>>),
            {reply, Log, State#state{subs = [{From, Type} | Subs]}}
    end;
handle_call(subs, {From, _}, State = #state{subs = Subs}) ->
    io:format(user, "From = ~p~n", [From]),
    io:format(user, "Subs = ~p~n", [Subs]),
    Types = [atom_to_binary(Type) || {Socket, Type} <- Subs, Socket == From],
    io:format(user, "Types = ~p~n", [Types]),
    IoList = [<<"Subbed to [">>, lists:join(<<", ">>, Types), <<"]">>],
    Log = log(iolist_to_binary(IoList)),
    {reply, Log, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({send, Type, Text}, State = #state{subs = Subs}) ->
    [Socket ! {send, Text} || {Socket, Type_} <- Subs, Type_ == Type],
    {noreply, State};
handle_info(Info, State) ->
    io:format("Received erlang message: ~n~p~n", [Info]),
    {ok, State}.

add_animator_(Spec, State) ->
    case get_animator(Spec) of
        {error, Bin} ->
            Error = <<"Invalid animator add command \"", Bin/binary, "\"">>,
            Log = <<"{\"Log\": \"", Error/binary, "\"}">>,
            {Log, State};
        {error, Bin1, _Bin2} ->
            Error = <<"Could not find animator \"", Bin1/binary, "\"">>,
            Log = <<"{\"Log\": \"", Error/binary, "\"}">>,
            {Log, State};
        {ok, AnimatorModule, Name} ->
            {ok, Pid} = AnimatorModule:start(Name),
            Log = log(<<"Added animator ", Name/binary>>),
            {Log, Pid}
    end.

% Hack
get_animator(<<"animator1 ", Name/binary>>) when Name /= <<"">> ->
    {ok, ws_anim_animator, Name};
get_animator(Bin) ->
    case binary:split(Bin, <<" ">>) of
        [Bin1] ->
            {error, Bin1};
        [Bin1, Bin2] ->
            {error, Bin1, Bin2}
    end.

type(<<"log">>) -> log;
type(<<"draw">>) -> draw;
type(_) -> undefined.

log(Bin) ->
    <<"{\"log\": \"", Bin/binary, "\"}">>.
