-module(ws_anim_animate_text).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined :: pid(),
                text = <<"F">>,
                font = <<"serif">>,
                font_size = <<"72px">>,
                font_color = <<"black">>,
                x = 100,
                y = 100,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame},
        State = #state{name = Name,
                       channel = Channel}) ->

    Text = transform(Frame, Name, State),

    send(Channel, Text),
    maybe_send_name(State),
    State.

transform(_Frame,
          Name,
          #state{text = Text,
                 font = Font,
                 font_size = FontSize,
                 font_color = FontColor,
                 x = X,
                 y = Y}) ->
    DrawInstruction =
        #{type => <<"draw">>,
          cmd => <<"text">>,
          x => X,
          y => Y,
          text => Text,
          font => Font,
          font_size => FontSize,
          font_color => FontColor,
          name => Name},
    {id(1), ?utils:json(DrawInstruction)}.

id(X) ->
    ZIndex = 100,
    {ZIndex, self(), X}.

send(Channel, BufferObject) ->
    Channel ! {buffer, BufferObject}.

maybe_send_name(#state{name = Name,
                       channel = Channel,
                       is_showing_name = true}) ->
    TextMap =
        #{type => <<"draw">>,
          cmd => <<"text">>,
          x => 290,
          y => 290,
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

send_controls(State = #state{name = Name, channel = Channel}) ->
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"font_size">>, State#state.font_size),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"font">>, State#state.font),
    ?utils:send_input_control(Channel, Name, <<"color">>, <<"font_color">>, State#state.font_color),
    ?utils:send_input_control(Channel, Name, <<"range">>, <<"x">>, State#state.x, #{min => 0, max => 800}),
    ?utils:send_input_control(Channel, Name, <<"range">>, <<"y">>, State#state.y, #{min => 0, max => 750}),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"text">>, State#state.text),
    State.

-define(INT_SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(INT_SETTINGS, #{<<"x">> => ?INT_SETTING(x),
                        <<"y">> => ?INT_SETTING(y)}).

set(Setting, Value, State)
  when Setting == <<"x">>;
       Setting == <<"y">> ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          #{Setting := Fun} = ?INT_SETTINGS,
          Fun(State, I);
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for ", Setting/binary>>)},
          State
  end;
set(<<"text">>, Value, State) ->
    State#state{text = Value};
set(<<"font">>, Value, State) ->
    State#state{font = Value};
set(<<"font_size">>, Value, State) ->
    State#state{font_size = Value};
set(<<"font_color">>, Value, State) ->
    State#state{font_color = Value};
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.



