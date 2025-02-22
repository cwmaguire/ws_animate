-module(ws_anim_animate_transform).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined :: pid(),
                a = 1.0,    % | a c e |
                b = 0.0,    % | b d f |
                c = 0.0,    % | 0 0 1 |
                d = 1.0,
                e = 0.0,
                f = 0.0,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame},
        State = #state{name = Name,
                       channel = Channel}) ->

    Transform = transform(Frame, Name, State),
    Square = square(Name),

    send(Channel, Transform),
    send(Channel, Square),
    maybe_send_name(State),
    State.

transform(_Frame, Name, #state{a = A, b = B, c = C, d = D, e = E, f = F}) ->
    DrawInstruction = #{type => <<"draw">>,
                        cmd => <<"transform">>,
                        name => Name,
                        transform => [A, B, C, D, E, F]},
    {id(1), ?utils:json(DrawInstruction)}.

square(Name) ->
    DrawInstruction = #{type => <<"draw">>,
                        cmd => <<"square">>,
                        name => Name,
                        style => 'red',
                        x => 0,
                        y => 0,
                        w => 800,
                        h => 750},
    {id(2), ?utils:json(DrawInstruction)}.

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
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"a">>, State#state.a, #{step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"b">>, State#state.b, #{step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"c">>, State#state.c, #{step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"d">>, State#state.d, #{step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"e">>, State#state.e, #{step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"f">>, State#state.f, #{step => 0.1}),
    State.

-define(SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(SETTINGS, #{<<"a">> => ?SETTING(a),
                    <<"b">> => ?SETTING(b),
                    <<"c">> => ?SETTING(c),
                    <<"d">> => ?SETTING(d),
                    <<"e">> => ?SETTING(e),
                    <<"f">> => ?SETTING(f)}).

set(Setting, Value, State)
  when Setting == <<"a">>;
       Setting == <<"b">>;
       Setting == <<"c">>;
       Setting == <<"d">>;
       Setting == <<"e">>;
       Setting == <<"f">> ->
  WithDecimal =
      case binary:split(Value, <<".">>) of
          [_Int] ->
              <<Value/binary, ".0">>;
          _ ->
              Value
      end,
  case catch binary_to_float(WithDecimal) of
      F when is_float(F) ->
          #{Setting := Fun} = ?SETTINGS,
          Fun(State, F);
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for ", Setting/binary>>)},
          State
  end;
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.


