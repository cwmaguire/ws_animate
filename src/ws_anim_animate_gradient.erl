-module(ws_anim_animate_gradient).

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).

-record(state, {name,
                channel = undefined,
                width = 800,
                height = 700}).

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(Frame,
        State = #state{name = Name,
                       channel = Channel}) ->
    ZIndex = 0,
    Id = {ZIndex, self(), 1},
    Square = square(State, Frame, Name),
    Channel ! {buffer, {Id, Square}},
    State.

% 5 seconds per side
square(#state{width = W, height = H}, Frame, Name) ->
    {X1, Y1} =
        case Frame rem 200 of
            Top when Top =< 50 ->
                X1_ = W / 50 * Top,
                Y1_ = 0,
                {X1_, Y1_};
            Right when Right =< 100 ->
                X1_ = W,
                Y1_ = H / 50 * (Right - 50),
                {X1_, Y1_};
            Bottom when Bottom =< 150 ->
                X1_ = W - (W / 50 * (Bottom - 100)),
                Y1_ = H,
                {X1_, Y1_};
            Left ->
                X1_ = 0,
                Y1_ = H - (H / 50 * (Left - 150)),
                {X1_, Y1_}
        end,
    X2 = W - X1,
    Y2 = H - Y1,

    GradientMap =
        #{%type => <<"draw">>,
          %cmd => <<"gradient">>,
          gx1 => X1,
          gy1 => Y1,
          gx2 => X2,
          gy2 => Y2,
          stop1 => #{stop => 0, color => <<"black">>},
          stop2 => #{stop => 1, color => <<"red">>}},

    SquareMap =
        #{type => <<"draw">>,
          cmd => <<"square_gradient">>,
          x => 0,
          y => 0,
          w => W,
          h => H,
          style => GradientMap,
          name => Name},
    ws_anim_utils:json(SquareMap).

send_controls(State = #state{name = Name, channel = Channel}) ->
    Channel ! {send, control, textbox(Name, <<"width">>, State#state.width)},
    Channel ! {send, control, textbox(Name, <<"height">>, State#state.height)},
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
set(<<"style">>, _Value, State) ->
    %State#state{style = Value};
    State;
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.

