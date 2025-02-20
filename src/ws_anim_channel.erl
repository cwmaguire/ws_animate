-module(ws_anim_channel).

-include("ws_anim.hrl").

-behaviour(gen_server).

-export([start/2]).
-export([join/1]).
-export([leave/1]).
-export([animator_list/1]).
-export([add_animator/2]).
-export([animator_set_field_value/2]).
-export([animator_stop/2]).
-export([animator_start/2]).
-export([animator_freeze/2]).
-export([animator_unfreeze/2]).
-export([sub/2]).
-export([subs/1]).
-export([save/2]).
-export([load/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(FRAME_MILLIS, 2000).

-define(ANIMATOR_NAMES,
        [#{name => squares, short_name => s},
         #{name => circles, short_name => c1},
         #{name => circles2, short_name => c2},
         #{name => gradient, short_name => g},
         #{name => fps, short_name => f},
         #{name => background, short_name => bg},
         #{name => image, short_name => img}]).

-define(ANIMATORS,
        #{<<"squares">> => ws_anim_animate_squares,
          <<"circles">> => ws_anim_animate_circles,
          <<"circles2">> => ws_anim_animate_circles2,
          <<"gradient">> => ws_anim_animate_gradient,
          <<"fps">> => ws_anim_animate_frame_info,
          <<"background">> => ws_anim_animate_background,
          <<"image">> => ws_anim_animate_image}).

-record(state, {id = "no ID set",
                sockets = [],
                animators = #{},
                subs = [],
                buffer = [],
                ets_id}).

start(Id, Socket) ->
    gen_server:start(?MODULE, _Args = [Id, Socket], _Opts = []).

join(ChannelPid) ->
    gen_server:call(ChannelPid, join).

leave(ChannelPid) ->
    gen_server:call(ChannelPid, leave).

-spec save(pid(), binary()) -> {reply, list(), #state{}}.
save(ChannelPid, Filename) ->
    gen_server:call(ChannelPid, {save, Filename}).

load(ChannelPid, Filename) ->
    gen_server:call(ChannelPid, {load, Filename}).

animator_list(ChannelPid) ->
    gen_server:call(ChannelPid, animator_list).

add_animator(ChannelPid, Name) ->
    gen_server:call(ChannelPid, {add_animator, Name}).

animator_set_field_value(ChannelPid, AnimatorFieldValue) ->
    gen_server:call(ChannelPid, {animator_set_field_value, AnimatorFieldValue}).

animator_stop(ChannelPid, Name) ->
    gen_server:call(ChannelPid, {animator_stop, Name}).

animator_start(ChannelPid, Name) ->
    gen_server:call(ChannelPid, {animator_start, Name}).

animator_freeze(ChannelPid, Name) ->
    gen_server:call(ChannelPid, {animator_freeze, Name}).

animator_unfreeze(ChannelPid, Name) ->
    gen_server:call(ChannelPid, {animator_unfreeze, Name}).

sub(ChannelPid, Type) ->
    gen_server:call(ChannelPid, {sub, Type}).

subs(ChannelPid) ->
    gen_server:call(ChannelPid, subs).

init([Id, Socket]) ->
    _Ref = monitor(process, Socket),
    TableId = ets:new(undefined, [ordered_set]),
    erlang:send_after(?FRAME_MILLIS, self(), send_buffer),
    self() ! send_files,
    {ok, #state{id = Id, ets_id = TableId, sockets = [Socket]}}.

handle_call(join, {From, _}, State = #state{id = Id,
                                            sockets = Sockets,
                                            subs = Subs,
                                            animators = Animators}) ->
    monitor(process, From),
    Log = ?utils:log(<<"Joined ", Id/binary>>),
    send_animator_names(Animators, Sockets),
    {reply, [Log], State#state{sockets = [From | Sockets], subs = [{From, log} | Subs]}};
handle_call(leave, {From, _}, State = #state{id = Id, sockets = Sockets0, subs = Subs0}) ->
    case remove_socket(From, Sockets0, Subs0) of
        {[], _} ->
            Log = ?utils:log(<<"Left ", Id/binary, "; no more sockets, shutting down">>),
            {stop, normal, [Log], State#state{sockets = []}};
        {Sockets, Subs} ->
            Log = ?utils:log(<<"Left ", Id/binary>>),
            {reply, [Log], State#state{sockets = Sockets, subs = Subs}}
    end;
handle_call({save, Filename}, {_From, _}, State = #state{animators = Animators}) ->
    States = [ws_anim_animator:get_state(A) || A <- maps:values(Animators)],
    Json = json:encode(States),
    file:write_file(<<?SAVE_DIR/binary, Filename/binary>>, Json),
    Log = ?utils:log(<<"ok">>),
    self() ! send_files,
    {reply, [Log], State};
handle_call({load, Filename}, {_From, _}, State = #state{animators = Animators}) ->
    {ok, Bin} = file:read_file(<<?SAVE_DIR/binary, Filename/binary>>),
    AStates = atomize_keys(json:decode(Bin)),
    %[gen_server:stop(A) || A <- maps:values(Animators)],
    NewAnimators =
        lists:foldl(fun start_animator/2, #{}, AStates),
    Animators2 = maps:merge(NewAnimators, Animators),
    Log = ?utils:log(<<"ok">>),
    {reply, [Log], State#state{animators = Animators2}};
handle_call(animator_list, _From, State) ->
    Info = ?utils:info(#{animators => ?ANIMATOR_NAMES}),
    {reply, [Info], State};
handle_call({add_animator, Spec}, _From, State = #state{animators = Animators}) ->
    {Msgs, Pid, Name} = add_animator_(Spec, State),
    case Pid of
        undefined ->
            {reply, Msgs, State};
        _ ->
            {reply,
             Msgs,
             State#state{animators = Animators#{Name => Pid}}}
    end;
handle_call({animator_set_field_value, AnimatorFieldValue}, _From, State) ->
    Msgs = set_animator_field(AnimatorFieldValue, State),
    {reply, Msgs, State};
handle_call({animator_stop, Name}, _From, State) ->
    #{Name := Pid} = State#state.animators,
    Pid ! stop,
    Log = ?utils:log(<<"Attempted to stop ", Name/binary>>),
    {reply, [Log], State};
handle_call({animator_start, Name}, _From, State) ->
    #{Name := Pid} = State#state.animators,
    Pid ! start,
    Log = ?utils:log(<<"Attempted to start ", Name/binary>>),
    {reply, [Log], State};
handle_call({animator_freeze, Name}, _From, State) ->
    #{Name := Pid} = State#state.animators,
    Pid ! freeze,
    Log = ?utils:log(<<"Attempted to freeze ", Name/binary>>),
    {reply, [Log], State};
handle_call({animator_unfreeze, Name}, _From, State) ->
    #{Name := Pid} = State#state.animators,
    Pid ! unfreeze,
    Log = ?utils:log(<<"Attempted to unfreeze ", Name/binary>>),
    {reply, [Log], State};
handle_call({sub, TypeBin}, {From, _}, State = #state{subs = Subs}) ->
    Type = type(TypeBin),
    case {Type, lists:member({From, Type}, Subs)} of
        {undefined, _} ->
            Log = ?utils:log(<<"Invalid type: ", TypeBin/binary>>),
            {reply, [Log], State};
        {_, true} ->
            Log = ?utils:log(<<"Already subbed to ", TypeBin/binary>>),
            {reply, [Log], State};
        {_, false} ->
            NewSubs = [{From, Type} | Subs],
            NewState = State#state{subs = NewSubs},
            %io:format(user, "handle call sub: NewState = ~p~n", [NewState]),
            new_sub(Type, NewState),
            Log = ?utils:log(<<"Subbed to ", TypeBin/binary>>),
            case Type of
                info ->
                    send_files(From);
                _ ->
                    ok
            end,
            {reply, [Log], NewState}
    end;
handle_call(subs, {From, _}, State = #state{subs = Subs}) ->
    Types = [atom_to_binary(Type) || {Socket, Type} <- Subs, Socket == From],
    IoList = [<<"Subbed to [">>, lists:join(<<", ">>, Types), <<"]">>],
    Log = ?utils:log(iolist_to_binary(IoList)),
    {reply, [Log], State};
handle_call(_, _From, State) ->
    {reply, [], State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({send, Type, Text}, State = #state{subs = Subs}) ->
    [Socket ! {send, Text} || {Socket, Type_} <- Subs, Type_ == Type],
    {noreply, State};
handle_info({buffer, DrawObject}, State = #state{ets_id = EtsId}) ->
    ets:insert(EtsId, DrawObject),
    {noreply, State};
handle_info(send_buffer, State = #state{subs = Subs, ets_id = EtsId}) ->
    erlang:send_after(?FRAME_MILLIS, self(), send_buffer),
    maybe_send_buffer(ets:tab2list(EtsId), Subs),
    %% XXX causes flashing when animators don't draw fast enough
    %%ets:delete_all_objects(EtsId),
    {noreply, State#state{buffer = []}};
handle_info(send_files, State = #state{subs = Subs}) ->
    [send_files(Socket) || {Socket, info} <- Subs],
    {noreply, State};
handle_info(_Monitor = {'DOWN', _Ref, process, Pid, Info}, State = #state{sockets = Sockets0, subs = Subs0}) ->
    io:format("Received monitor message for ~p: ~n~p~n", [Pid, Info]),
    case remove_socket(Pid, Sockets0, Subs0) of
        {[], _} ->
            io:format("Channel ~p stopping because no more sockets~n", [Pid]),
            {stop, normal, State#state{sockets = []}};
        {Sockets, Subs} ->
            {noreply, State#state{sockets = Sockets, subs = Subs}}
    end;
handle_info(Info, State) ->
    io:format("Channel: Received erlang message: ~n~p~n", [Info]),
    {noreply, State}.

send_files(Socket) ->
    {ok, Filenames} = file:list_dir(?SAVE_DIR),
    InfoJson = ?utils:info(#{filenames => [list_to_binary(F) || F <- Filenames]}),
    Socket ! {send, InfoJson}.

maybe_send_buffer([], _) ->
    ok;
maybe_send_buffer(DrawCalls, Subs) ->
    %io:format(user, "maybe_send buffer: Subs = ~p~n", [Subs]),
    Clear = {0, draw_clear_json()},
    Finish = {0, finish_json()},
    Commands = [Clear | DrawCalls] ++ [Finish],
    [Socket ! {send, Json} || {Socket, draw} <- Subs,
                             {_Id, Json} <- Commands].

%% TODO check for animator with same user-assigned name
add_animator_(Spec, State) ->
    case decode_animator_add_spec(Spec) of
        {error, Bin} ->
            Error = <<"Invalid animator add command \"", Bin/binary, "\"">>,
            Log = ?utils:log(Error),
            {[Log], undefined, State};
        {error, Bin1, _Bin2} ->
            Error = <<"Could not find animator \"", Bin1/binary, "\"">>,
            Log = ?utils:log(Error),
            {[Log], undefined, State};
        {ok, AnimatorModule, Name} ->
            {ok, Pid} = ws_anim_animator:start(Name, AnimatorModule),
            %Info = ?utils:info(#{animator_name => Name}),
            {[], Pid, Name}
    end.

decode_animator_add_spec(Spec) ->
    case binary:split(Spec, <<" ">>) of
        [Type, Name] ->
            {ok, maps:get(Type, ?ANIMATORS), Name};
        [Bin1] ->
            {error, Bin1}
    end.

set_animator_field(Spec, #state{animators = Animators}) ->
    case decode_animator_set_spec(Spec) of
        {error, Bin} ->
            Error = <<"Invalid animator set command \"", Bin/binary, "\"">>,
            Log = ?utils:log(Error),
            [Log];
        {ok, Animator, Field, Value} ->
            case Animators of
                #{Animator := Pid} ->
                    set_animator_field(Pid, Field, Value),
                    Log = ?utils:log(<<"Setting ", Animator/binary, " field ", Field/binary, " to ", Value/binary>>),
                    [Log];
                _ ->
                    Log = ?utils:log(<<"Animator ", Animator/binary, " not in animators">>),
                    [Log]
            end
    end.

set_animator_field(Pid, Field, Value) ->
    Pid ! {set, Field, Value}.

decode_animator_set_spec(Spec) ->
    case binary:split(Spec, <<" ">>, [global]) of
        [Name, Field, Value] ->
            {ok, Name, Field, Value};
        _ ->
            {error, Spec}
    end.

type(<<"log">>) -> log;
type(<<"draw">>) -> draw;
type(<<"control">>) -> control;
type(<<"info">>) -> info;
type(_) -> undefined.

new_sub(control, #state{sockets = Sockets, animators = Animators}) ->
    [S ! {send, control_clear_json()} || S <- Sockets],
    [A ! send_controls || A <- maps:values(Animators)];
new_sub(_, _) ->
    ok.

control_clear_json() ->
    iolist_to_binary(json:encode(#{type => <<"control">>, cmd => <<"clear">>})).

draw_clear_json() ->
    iolist_to_binary(json:encode(#{type => <<"draw">>, cmd => <<"clear">>})).

finish_json() ->
    iolist_to_binary(json:encode(#{type => <<"draw">>, cmd => <<"finish">>})).

atomize_keys(List) when is_list(List) ->
    [atomize_keys(Map) || Map <- List];
atomize_keys(Map) ->
    maps:fold(fun atomize_keys/3, #{}, Map).

atomize_keys(K, V, Map) when is_map(V) ->
    maps:put(binary_to_atom(K), atomize_keys(V), Map);
atomize_keys(K, V, Map) ->
    maps:put(binary_to_atom(K), V, Map).

start_animator(Map = #{name := Name}, Animators) ->
    {ok, Pid} = ws_anim_animator:start(Map),
    Animators#{Name => Pid}.

remove_socket(Socket, Sockets, Subs) ->
    Filter =
        fun({Socket_, _}) when Socket_ == Socket ->
                false;
           (_) ->
                true
        end,
    {lists:delete(Socket, Sockets),
     lists:filter(Filter, Subs)}.

send_animator_names(Animators, Sockets) ->
    Clear = ?utils:info(#{info => <<"clear_animator_names">>}),
    [S ! {send, Clear} || S <- Sockets],
    io:format(user, "sending 'send_name' to Animators = ~p~n", [Animators]),
    [A ! send_name || A <- maps:values(Animators)].
