-module(ws_anim_channel).

-behaviour(gen_server).

-export([start/1]).
-export([join/1]).
-export([leave/1]).
-export([animator_list/1]).
-export([add_animator/2]).
-export([animator_set_field_value/2]).
-export([sub/2]).
-export([subs/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(FRAME_MILLIS, 3000).

-record(state, {id = "no ID set",
                sockets = [],
                animators = [],
                subs = [],
                buffer = []}).

start(Id) ->
    gen_server:start(?MODULE, _Args = Id, _Opts = []).

join(ChannelPid) ->
    gen_server:call(ChannelPid, join).

leave(ChannelPid) ->
    gen_server:call(ChannelPid, leave).

animator_list(ChannelPid) ->
    gen_server:call(ChannelPid, animator_list).

add_animator(ChannelPid, Name) ->
    gen_server:call(ChannelPid, {add_animator, Name}).

animator_set_field_value(ChannelPid, AnimatorFieldValue) ->
    gen_server:call(ChannelPid, {animator_set_field_value, AnimatorFieldValue}).

sub(ChannelPid, Type) ->
    gen_server:call(ChannelPid, {sub, Type}).

subs(ChannelPid) ->
    gen_server:call(ChannelPid, subs).

init(Id) ->
    erlang:send_after(?FRAME_MILLIS, self(), send_buffer),
    {ok, #state{id = Id}}.

handle_call(join, {From, _}, State = #state{id = Id, sockets = Sockets, subs = Subs}) ->
    Log = ws_anim_utils:log(<<"Joined ", Id/binary>>),
    {reply, Log, State#state{sockets = [From | Sockets], subs = [{From, log} | Subs]}};
handle_call(leave, {From, _}, State = #state{id = Id, sockets = Sockets, subs = Subs}) ->
    Log = ws_anim_utils:log(<<"Left ", Id/binary>>),
    Filter = fun({From_, _}) when From_ == From -> false; (_) -> true end,
    NewSubs = lists:filter(Filter, Subs),
    NewSockets = lists:delete(From, Sockets),
    {reply, Log, State#state{sockets = NewSockets, subs = NewSubs}};
handle_call(animator_list, _From, State) ->
    Names = animator_names(),
    Info = ws_anim_utils:info(#{animators => Names}),
    {reply, Info, State#state{animators = Names}};
handle_call({add_animator, Spec}, _From, State = #state{animators = Animators}) ->
    {Info, Pid, Name} = add_animator_(Spec, State),
    {reply, Info, State#state{animators = [{Name, Pid} | Animators]}};
handle_call({animator_set_field_value, AnimatorFieldValue}, _From, State) ->
    Log = set_animator_field(AnimatorFieldValue, State),
    {reply, Log, State};
handle_call({sub, TypeBin}, {From, _}, State = #state{subs = Subs}) ->
    Type = type(TypeBin),
    case {Type, lists:member({From, Type}, Subs)} of
        {undefined, _} ->
            Log = ws_anim_utils:log(<<"Invalid type: ", TypeBin/binary>>),
            {reply, Log, State};
        {_, true} ->
            Log = ws_anim_utils:log(<<"Already subbed to ", TypeBin/binary>>),
            {reply, Log, State};
        {_, false} ->
            NewSubs = [{From, Type} | Subs],
            NewState = State#state{subs = NewSubs},
            new_sub(Type, NewState),
            Log = ws_anim_utils:log(<<"Subbed to ", TypeBin/binary>>),
            {reply, Log, NewState}
    end;
handle_call(subs, {From, _}, State = #state{subs = Subs}) ->
    io:format(user, "From = ~p~n", [From]),
    io:format(user, "Subs = ~p~n", [Subs]),
    Types = [atom_to_binary(Type) || {Socket, Type} <- Subs, Socket == From],
    io:format(user, "Types = ~p~n", [Types]),
    IoList = [<<"Subbed to [">>, lists:join(<<", ">>, Types), <<"]">>],
    Log = ws_anim_utils:log(iolist_to_binary(IoList)),
    {reply, Log, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({send, Type, Text}, State = #state{subs = Subs}) ->
    [Socket ! {send, Text} || {Socket, Type_} <- Subs, Type_ == Type],
    {noreply, State};
handle_info({buffer, Type, Bin}, State = #state{buffer = Buffer}) ->
    {noreply, State#state{buffer = [{Type, Bin} | Buffer]}};
handle_info(send_buffer, State = #state{subs = Subs, buffer = Buffer}) ->
    %io:format("Send buffer~n"),
    [Socket ! {send, Bin} || {Socket, SubType} <- Subs,
                             {MessageType, Bin} <- [{draw, draw_clear_json()} | Buffer],
                             SubType == MessageType],
    erlang:send_after(?FRAME_MILLIS, self(), send_buffer),
    {noreply, State#state{buffer = []}};
handle_info(Info, State) ->
    io:format("Received erlang message: ~n~p~n", [Info]),
    {ok, State}.

%% TODO check for animator with same user-assigned name
add_animator_(Spec, State) ->
    case decode_animator_add_spec(Spec) of
        {error, Bin} ->
            Error = <<"Invalid animator add command \"", Bin/binary, "\"">>,
            Log = ws_anim_utils:log(Error),
            {Log, State};
        {error, Bin1, _Bin2} ->
            Error = <<"Could not find animator \"", Bin1/binary, "\"">>,
            Log = ws_anim_utils:log(Error),
            {Log, State};
        {ok, AnimatorModule, Name} ->
            {ok, Pid} = AnimatorModule:start(Name),
            Info = ws_anim_utils:info(#{animator_name => Name}),
            {Info, Pid, Name}
    end.

% Hack
decode_animator_add_spec(<<"animator1 ", Name/binary>>) when Name /= <<"">> ->
    {ok, ws_anim_animator, Name};
decode_animator_add_spec(Bin) ->
    case binary:split(Bin, <<" ">>) of
        [Bin1] ->
            {error, Bin1};
        [Bin1, Bin2] ->
            {error, Bin1, Bin2}
    end.

set_animator_field(Spec, #state{animators = Animators}) ->
    io:format(user, "Animators = ~p~n", [Animators]),
    case decode_animator_set_spec(Spec) of
        {error, Bin} ->
            Error = <<"Invalid animator set command \"", Bin/binary, "\"">>,
            ws_anim_utils:log(Error);
        {ok, Animator, Field, Value} ->
            %io:format(user, "Animator = ~p~n", [Animator]),
            %io:format(user, "Field = ~p~n", [Field]),
            %io:format(user, "Value = ~p~n", [Value]),
            set_animator_field(proplists:get_value(Animator, Animators), Field, Value),
            ws_anim_utils:log(<<"Setting ", Animator/binary, " field ", Field/binary, " to ", Value/binary>>)
    end.

set_animator_field(Pid, Field, Value) ->
    io:format(user, "Pid = ~p~n", [Pid]),
    Pid ! {set, Field, Value}.

decode_animator_set_spec(Spec) ->
    case binary:split(Spec, <<" ">>, [global]) of
        [Name, Field, Value] ->
            {ok, Name, Field, Value};
        _ ->
            {error, Spec}
    end.

animator_names() ->
    [<<"animator1">>].

type(<<"log">>) -> log;
type(<<"draw">>) -> draw;
type(<<"control">>) -> control;
type(<<"info">>) -> info;
type(_) -> undefined.

new_sub(control, #state{subs = Subs, animators = Animators}) ->
    send_controls(Subs),
    [A ! send_controls || {_Name, A} <- Animators];
new_sub(_, _) ->
    ok.

send_controls(Subs) ->
    Select = #{type => <<"control">>,
               cmd => <<"select">>,
               id => <<"create_animator">>,
               name => <<"create_animator">>,
               label => <<"Create Animator">>,
               options => [#{value => <<"squares">>,
                             text => <<"Squares">>}]},
    SelectJson = json(Select),
    ClearControlsJson = control_clear_json(),

    [Socket ! {send, ClearControlsJson} || {Socket, control} <- Subs],
    [Socket ! {send, SelectJson} || {Socket, control} <- Subs].

json(Map) ->
  iolist_to_binary(json:encode(Map)).

control_clear_json() ->
  iolist_to_binary(json:encode(#{type => <<"control">>, cmd => <<"clear">>})).

draw_clear_json() ->
  iolist_to_binary(json:encode(#{type => <<"draw">>, cmd => <<"clear">>})).
