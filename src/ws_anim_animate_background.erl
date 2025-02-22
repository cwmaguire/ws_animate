-module(ws_anim_animate_background).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined,
                x = 0,
                y = 0,
                width = 800,
                height = 700,
                color1 = {0, 0, 0},
                color2 = {255, 255, 255},
                color_tween_frames = 100,
                color_frame_delta = {2.55, 2.55, 2.55},
                is_cycling = true,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame},
        State = #state{name = Name,
                       channel = Channel}) ->
    ZIndex = 0,
    Id = {ZIndex, self(), 1},
    Square = square(State, Frame, Name),
    Channel ! {buffer, {Id, Square}},
    maybe_send_name(State),
    State.

% 5 seconds per side
square(State = #state{x = X, y = Y, width = W, height = H},
       Frame,
       Name) ->
    FrameColor = frame_color(Frame, State),
    SquareMap =
        #{type => <<"draw">>,
          cmd => <<"square_filled">>,
          x => X,
          y => Y,
          w => W,
          h => H,
          style => FrameColor,
          name => Name},
    ws_anim_utils:json(SquareMap).

maybe_send_name(#state{name = Name,
                       channel = Channel,
                       x = X,
                       y = Y,
                       is_showing_name = true}) ->
    TextMap =
        #{type => <<"draw">>,
          cmd => <<"text">>,
          x => X + ?NAME_OFFSET_X,
          y => Y + ?NAME_OFFSET_Y,
          text => Name,
          font => <<"serif">>,
          font_size => ?NAME_FONT_SIZE,
          font_color => ?NAME_FONT_COLOR,
          name => Name},

    TextJson = ws_anim_utils:json(TextMap),

    Id = {1, self(), 2},
    Channel ! {buffer, {Id, TextJson}};
maybe_send_name(_) ->
    ok.

frame_color(Frame, State = #state{color_tween_frames = TweenFrames}) ->

    {RBase, GBase, BBase} = base_color(Frame, State),
    {RDelta, GDelta, BDelta} = color_deltas(Frame, State),

    TweenFrame = Frame rem TweenFrames,
    R = trunc(RBase + (TweenFrame * RDelta)),
    G = trunc(GBase + (TweenFrame * GDelta)),
    B = trunc(BBase + (TweenFrame * BDelta)),
    ws_anim_utils:tuple_to_color({R, G, B}).

base_color(Frame, State = #state{is_cycling = true, color_tween_frames = Frames}) ->
    TweenFrame = Frame rem (Frames * 2),
    case TweenFrame < 100 of
        true ->
            State#state.color1;
        false ->
            State#state.color2
    end;
base_color(_Frame, #state{is_cycling = false, color1 = Color1}) ->
    Color1.

color_deltas(Frame, #state{is_cycling = true,
                           color_tween_frames = Frames,
                           color_frame_delta = {RD, GD, BD}}) ->
    TweenFrame = Frame rem (Frames * 2),
    case TweenFrame =< 100 of
        true ->
            {RD, GD, BD};
        false ->
            {-RD, -GD, -BD}
    end;
color_deltas(_Frame, #state{color_frame_delta = Deltas}) ->
    Deltas.
send_controls(State = #state{name = Name,
                             channel = Channel,
                             color1 = Color1,
                             color2 = Color2}) ->

    Color1Bin = ws_anim_utils:tuple_to_color(Color1),
    Color2Bin = ws_anim_utils:tuple_to_color(Color2),

    ?utils:send_input_control(Channel, Name, <<"color">>, <<"color_1">>, Color1Bin),
    ?utils:send_input_control(Channel, Name, <<"color">>, <<"color_2">>, Color2Bin),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"x">>, State#state.x),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"y">>, State#state.y),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"width">>, State#state.width),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"height">>, State#state.height),
    ?utils:send_input_control(Channel, Name, <<"checkbox">>, <<"is_cycling">>, State#state.is_cycling),
    ?utils:send_input_control(Channel, Name, <<"checkbox">>, <<"is_showing_name">>, State#state.is_showing_name),
    State.

set(<<"color_1">>, Value, State) ->
    Color1 = ws_anim_utils:color_to_tuple(Value),
    color_frame_delta(State#state{color1 = Color1});
set(<<"color_2">>, Value, State) ->
    Color2 = ws_anim_utils:color_to_tuple(Value),
    color_frame_delta(State#state{color2 = Color2});
set(<<"is_cycling">>, Value, State) ->
    case Value of
        <<"true">> ->
            State#state{is_cycling = true};
        <<"false">> ->
            State#state{is_cycling = false};
        _ ->
            State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid boolean ", Value/binary, " for is_cycling">>)},
            State
    end;
set(<<"is_showing_name">>, Value, State) ->
    case Value of
        <<"true">> ->
            State#state{is_showing_name = true};
        <<"false">> ->
            State#state{is_showing_name = false};
        _ ->
            State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid boolean ", Value/binary, " for is_showing_name">>)},
            State
    end;
set(<<"x">>, Value, State) ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          State#state{x = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for x">>)},
          State
  end;
set(<<"y">>, Value, State) ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          State#state{y = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for y">>)},
          State
  end;
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
