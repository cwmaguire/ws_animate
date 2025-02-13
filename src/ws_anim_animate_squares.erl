-module(ws_anim_animate_squares).

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).

-record(state, {name,
                channel = undefined,
                width = 300,
                height = 300,
                style = <<"black">>}).

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(Frame,
        State = #state{name = Name,
                       channel = Channel}) ->
    Id = {_ZIndex = 100, self(), 1},
    Square = square(State, Frame, Name),
    Channel ! {buffer, {Id, Square}},
    State.

square(State, Frame, Name) ->
    W = trunc(abs(math:sin((Frame / 100))) * State#state.width),
    H = trunc(abs(math:cos((Frame / 100))) * State#state.height),
    X = trunc(abs(math:sin((Frame / 100))) * (800 - W)),
    Y = trunc(abs(math:cos((Frame / 100))) * (700 - H)),
    Map = #{type => <<"draw">>,
            cmd => <<"square">>,
            x => X,
            y => Y,
            w => W,
            h => H,
            style => State#state.style,
            name => Name},
    ws_anim_utils:json(Map).

send_controls(State = #state{name = Name, channel = Channel}) ->
    Channel ! {send, control, textbox(Name, <<"width">>, State#state.width)},
    Channel ! {send, control, textbox(Name, <<"height">>, State#state.height)},
    Channel ! {send, control, textbox(Name, <<"style">>, State#state.style)},
    State.

textbox(AnimatorName, Field, Value) ->
    Id = <<AnimatorName/binary, "_", Field/binary, "_textbox">>,
    Textbox = #{type => <<"control">>,
                cmd => <<"textbox">>,
                id => Id,
                name => Id,
                animator => AnimatorName,
                field => Field,
                value => Value,
                label => <<AnimatorName/binary, " ", Field/binary>>},
    ws_anim_utils:json(Textbox).

set(<<"width">>, Value, State) ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          io:format(user, "I = ~p~n", [I]),
          State#state{width = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for width">>)},
          State
  end;
set(<<"height">>, Value, State) ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          io:format(user, "I = ~p~n", [I]),
          State#state{height = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for height">>)},
          State
  end;
set(<<"style">>, Value, State) ->
    State#state{style = Value};
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.
