-module(ws_anim_channel).

-behaviour(gen_server).

-export([start/1]).
-export([join/1]).
-export([leave/1]).
-export([add_animator/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {id = "no ID set",
                sockets = [],
                animators = []}).

start(Id) ->
    gen_server:start(?MODULE, _Args = Id, _Opts = []).

join(ChannelPid) ->
    gen_server:call(ChannelPid, join).

leave(ChannelPid) ->
    gen_server:call(ChannelPid, leave).

add_animator(ChannelPid, Name) ->
    gen_server:call(ChannelPid, {add_animator, Name}).

init(Id) ->
    {ok, #state{id = Id}}.

handle_call(join, From, State = #state{id = Id, sockets = Sockets}) ->
    Log = "{\"log\": \"Joined " ++ Id ++ "\"}",
    {reply, Log, State#state{sockets = [From | Sockets]}};
handle_call(leave, From, State = #state{id = Id, sockets = Sockets}) ->
    Log = "{\"log\": \"Left " ++ Id ++ "\"}",
    {reply, Log, State#state{sockets = lists:delete(From, Sockets)}};
handle_call({add_animator, Name}, _From, State = #state{animators = Animators}) ->
    {Log, Animator} = add_animator_(Name, State),
    {reply, Log, State#state{animators = [Animator | Animators]}};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({send, Text}, State = #state{sockets = Sockets}) ->
    [Socket ! {send, Text} || Socket <- Sockets],
    {noreply, State};
handle_info(Info, State) ->
    io:format("Received erlang message: ~n~p~n", [Info]),
    {ok, State}.

add_animator_(Spec, State) ->
    case get_animator(Spec) of
        {error, String} ->
            Error = iolist_to_binary(io_lib:format("Invalid animator add command ~p", [String])),
            Log = <<"{\"Log\": \"", Error/binary, "\"}">>,
            {Log, State};
        {error, String1, _String2} ->
            Error = iolist_to_binary(io_lib:format("Could not find animator ~p", [String1])),
            Log = <<"{\"Log\": \"", Error/binary, "\"}">>,
            {Log, State};
        {ok, AnimatorModule, Name} ->
            {ok, Pid} = AnimatorModule:start(Name),
            Log = <<"{\"Log\": \"Added animator ", AnimatorModule/binary, "\"}">>,
            {Log, Pid}
    end.

% Hack
get_animator("animator1 " ++ Name) when Name /= ""->
    {ok, ws_anim_animator, Name};
get_animator(String) ->
    case lists:splitwith(fun($ ) -> false; (_) -> true end, String) of
        {String1, []} ->
            {error, String1};
        {String1, String2} ->
            {error, String1, String2}
    end.
