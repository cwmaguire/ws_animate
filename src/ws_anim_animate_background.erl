-module(ws_anim_animate_background).

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).

-record(state, {name,
                channel = undefined,
                width = 800,
                height = 700,
                color1 = {0, 0, 0},
                color2 = {255, 255, 255},
                color_tween_frames = 100,
                color_frame_delta = {2.55, 2.55, 2.55}}).

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
square(State = #state{width = W, height = H},
       Frame,
       Name) ->
    FrameColor = frame_color(Frame, State),
    SquareMap =
        #{type => <<"draw">>,
          cmd => <<"square_filled">>,
          x => 0,
          y => 0,
          w => W,
          h => H,
          style => FrameColor,
          name => Name},
    ws_anim_utils:json(SquareMap).

frame_color(Frame, #state{color1 = {R1, G1, B1},
                          color_tween_frames = TweenFrames,
                          color_frame_delta = {RD, GD, BD}}) ->
    TweenFrame = Frame rem TweenFrames,
    R = trunc(R1 + (TweenFrame * RD)),
    G = trunc(G1 + (TweenFrame * GD)),
    B = trunc(B1 + (TweenFrame * BD)),
    ws_anim_utils:tuple_to_color({R, G, B}).

send_controls(State = #state{name = Name,
                             channel = Channel,
                             color1 = Color1,
                             color2 = Color2}) ->

    Color1Bin = ws_anim_utils:tuple_to_color(Color1),
    Channel ! {send, control, color_picker(Name, <<"color_1">>, Color1Bin)},

    Color2Bin = ws_anim_utils:tuple_to_color(Color2),
    Channel ! {send, control, color_picker(Name, <<"color_2">>, Color2Bin)},
    State.

color_picker(AnimatorName, Field, Value) ->
    Id = <<AnimatorName/binary, "_", Field/binary, "_textbox">>,
    ColorPicker = #{type => <<"control">>,
                    cmd => <<"color_picker">>,
                    id => Id,
                    name => Id,
                    animator => AnimatorName,
                    field => Field,
                    value => Value,
                    label => <<AnimatorName/binary, " ", Field/binary>>},
    ws_anim_utils:json(ColorPicker).

set(<<"color_1">>, Value, State) ->
    Color1 = ws_anim_utils:color_to_tuple(Value),
    color_frame_delta(State#state{color1 = Color1});
set(<<"color_2">>, Value, State) ->
    Color2 = ws_anim_utils:color_to_tuple(Value),
    color_frame_delta(State#state{color2 = Color2});
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

color_frame_delta(State = #state{color1 = {R1, G1, B1},
                                 color2 = {R2, G2, B2},
                                 color_tween_frames = ColorTweenFrames}) ->

    RDelta = R2 - R1,
    GDelta = G2 - G1,
    BDelta = B2 - B1,
    RFrameDelta = RDelta / ColorTweenFrames,
    GFrameDelta = GDelta / ColorTweenFrames,
    BFrameDelta = BDelta / ColorTweenFrames,

    NewState = State#state{color_frame_delta = {RFrameDelta,
                                     GFrameDelta,
                                     BFrameDelta}},

    % io:format("OldState: ~p~n"
    %           "1:  {~p, ~p, ~p},~n"
    %           "2:  {~p, ~p, ~p},~n"
    %           "D:  {~p, ~p, ~p},~n"
    %           "FD: {~p, ~p, ~p},~n"
    %           "NewState: ~p~n",
    %           [State,
    %            R1, G1, B1,
    %            R2, G2, B2,
    %            RDelta, GDelta, BDelta,
    %            RFrameDelta, GFrameDelta, BFrameDelta,
    %            NewState]),
    NewState.
