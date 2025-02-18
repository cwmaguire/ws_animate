-module(ws_anim_animate_image).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined,
                src = <<"cloak.png">>,
                width_scale = 1.0,
                height_scale = 1.0,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame,
          z_index := ZIndex},
        State = #state{name = Name,
                       channel = Channel}) ->
    Id = {ZIndex, self(), 1},
    {Image, X, Y} = image(State, Frame, Name),
    Channel ! {buffer, {Id, Image}},
    maybe_send_name(State, X, Y),
    State.

image(State, Frame, Name) ->
    FramesPerWidth = 100,
    CycleFrame = Frame rem (2 * FramesPerWidth),
    HalfCycleFrame = Frame rem (FramesPerWidth),
    X0 = trunc(600 / FramesPerWidth) * HalfCycleFrame,
    X = case CycleFrame of
             F when F < FramesPerWidth ->
                 X0;
             _ ->
                 600 - X0
         end,
    Y = 100,
    Map = #{type => <<"draw">>,
            cmd => <<"image">>,
            src => State#state.src,
            x => X,
            y => Y,
            w_scale => State#state.width_scale,
            h_scale => State#state.height_scale,
            name => Name},
    {ws_anim_utils:json(Map), X, Y}.

maybe_send_name(#state{name = Name,
                       channel = Channel,
                       is_showing_name = true},
                X, Y) ->
    TextMap =
        #{type => <<"draw">>,
          cmd => <<"text">>,
          x => X - ?NAME_OFFSET_X,
          y => Y - ?NAME_OFFSET_Y,
          text => Name,
          font_size => ?NAME_FONT_SIZE,
          font_color => ?NAME_FONT_COLOR,
          name => Name},

    TextJson = ws_anim_utils:json(TextMap),

    Id = {100, self(), 2},
    Channel ! {buffer, {Id, TextJson}};
maybe_send_name(_, _, _) ->
    ok.

send_controls(State = #state{name = Name, channel = Channel}) ->
    ?utils:send_input_control(Channel, Name, <<"checkbox">>, <<"is_showing_name">>, State#state.is_showing_name),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"src">>, State#state.src),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"width_scale">>, State#state.width_scale),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"height_scale">>, State#state.height_scale),
    State.

set(<<"src">>, Value, State) ->
    % TODO add validation on filename unless URL
    % (draw.js is currently hardcoded to previx the filename with 'images/')
    State#state{src = Value};
set(<<"width_scale">>, Value, State) ->
  case catch binary_to_float(Value) of
      F when is_float(F) ->
          State#state{width_scale = F};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid float ", Value/binary, " for width">>)},
          State
  end;
set(<<"height_scale">>, Value, State) ->
  case catch binary_to_float(Value) of
      F when is_float(F) ->
          State#state{height_scale = F};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for height">>)},
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
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.
