-module(ws_anim_animate_convolution).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-export([apply_kernel/3]).
-export([apply_kernel_/3]).
-export([convolute/1]).
-export([kernel_binaries/2]).
-export([shift_row_down_1/2]).
-export([shift_row_up_1/2]).
-export([shift_columns_right_1/2]).
-export([shift_columns_left_1/2]).

-export([test_image/0]).
-export([id_kernel/0]).

-define(BITS, 8).
-define(PIXEL_BYTES, 4).
-define(PIXEL_BITS, (?PIXEL_BYTES * ?BITS)).

-record(state, {name,
                channel = undefined,
                device_id = <<"cde819aad3a5f7da8759721271d1d3deaf3dbdce8666ecfb5f4180f93f5e0d00">>,
                x,
                y,
                kernel = [0, 0, 0,
                          0, 1, 0,
                          0, 0, 0],
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    %% TODO wait for a device ID to be set from the web page
    DeviceId = (#state{})#state.device_id,
    InfoMsg = #{info => <<"send_image">>,
                deviceId => DeviceId,
                animatorName => Name},
    Channel ! {send, info, ?utils:info(InfoMsg)},
    #state{name = Name,
           channel = Channel}.

animate(#{data := Image},
        State = #state{name = Name,
                       channel = Channel,
                       device_id = DeviceId})
  when Image /= undefined ->
    %io:format("ws_anim_animate_convolution: image received~n"),
    WebCacheId = list_to_binary(pid_to_list(self())),
    Json = convolute_image(State, Name, WebCacheId, Image),
    Id = {_ZIndex = 100, self(), 1},
    ws_anim_channel:buffer_delete(Channel),
    Channel ! {buffer, {Id, #{json => Json, id => WebCacheId, cached => true}}},
    timer:sleep(5000),
    request_image(DeviceId, Name, Channel),
    maybe_send_name(State, _X = 0, _Y = 0),
    State;
animate(_Settings, State) ->
    %io:format("No image received~n"),
    State.

request_image(DeviceId, Name, Channel) ->
    InfoMsg = #{info => <<"send_image">>,
                deviceId => DeviceId,
                animatorName => Name},
    io:format("requesting image: ~p~n", [InfoMsg]),
    Channel ! {send, info, ?utils:info(InfoMsg)}.

convolute_image(#state{kernel = Kernel}, Name, WebCacheId, Image) ->
    Bytes =
        lists:map(fun erlang:binary_to_integer/1,
                  binary:split(Image, <<",">>, [global])),

    Bin = list_to_binary(Bytes),

    %_Bytes2 = [trunc(X / 2) || X <- Bytes],

    % Kernel = [0, 0, 0,
    %           0, 0, 1,
    %           0, 0, 0],
    % Kernel = [0, -1, 0,
    %           -1, 4, -1,
    %           0, -1, 0],
    % Kernel = [1,  1,  1,
    %           1, -8,  1,
    %           1,  1,  1],
    %Kernel = [0, 0, 0,
              %0, 1, 0,
              %0, 0, 0],
    Convoluted = apply_kernel(Kernel, Bin, 160),
    %io:format(user, "Convoluted = ~p~n", [Convoluted]),

    %List = binary_to_list(Convoluted),

    %% TODO have the web page draw this data directly
    %% onto the canvas, as opposed to creating an image
    %% DOM element and loading it from a URL object.
    %% (partly because I don't know how to load arbitrary
    %%  RGBA image data [bitmap?] into an image elemnt,
    %%  but also because I don't need to: the canvase
    %%  can just draw it.)
    Map = #{type => <<"draw">>,
            cmd => <<"bitmap">>,
            shouldCache => true,
            id => WebCacheId,
            data => Convoluted,
            x => 0,
            y => 0,
            w => 160,
            name => Name},
    ws_anim_utils:json(Map).

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
          font => <<"serif">>,
          font_size => ?NAME_FONT_SIZE,
          font_color => ?NAME_FONT_COLOR,
          name => Name},

    TextJson = ws_anim_utils:json(TextMap),

    Id = {100, self(), 2},
    Channel ! {buffer, {Id, TextJson}};
maybe_send_name(_, _, _) ->
    ok.

send_controls(State = #state{name = Name, channel = Channel, kernel = Kernel}) ->
    [K1, K2, K3,
     K4, K5, K6,
     K7, K8, K9] = Kernel,
    ?utils:send_input_control(Channel, Name, <<"checkbox">>, <<"is_showing_name">>, State#state.is_showing_name),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"x">>, State#state.x),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"y">>, State#state.y),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K1">>, K1, #{min => -20, max => 20, step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K2">>, K2, #{min => -20, max => 20, step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K3">>, K3, #{min => -20, max => 20, step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K4">>, K4, #{min => -20, max => 20, step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K5">>, K5, #{min => -20, max => 20, step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K6">>, K6, #{min => -20, max => 20, step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K7">>, K7, #{min => -20, max => 20, step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K8">>, K8, #{min => -20, max => 20, step => 0.1}),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"K9">>, K9, #{min => -20, max => 20, step => 0.1}),
    State.

set_1(State = #state{kernel = [_K1, K2, K3, K4, K5, K6, K7, K8, K9]}, I) ->
    State#state{kernel = [I, K2, K3, K4, K5, K6, K7, K8, K9]}.

set_2(State = #state{kernel = [K1, _K2, K3, K4, K5, K6, K7, K8, K9]}, I) ->
    State#state{kernel = [K1, I, K3, K4, K5, K6, K7, K8, K9]}.

set_3(State = #state{kernel = [K1, K2, _K3, K4, K5, K6, K7, K8, K9]}, I) ->
    State#state{kernel = [K1, K2, I, K4, K5, K6, K7, K8, K9]}.

set_4(State = #state{kernel = [K1, K2, K3, _K4, K5, K6, K7, K8, K9]}, I) ->
    State#state{kernel = [K1, K2, K3, I, K5, K6, K7, K8, K9]}.

set_5(State = #state{kernel = [K1, K2, K3, K4, _K5, K6, K7, K8, K9]}, I) ->
    State#state{kernel = [K1, K2, K3, K4, I, K6, K7, K8, K9]}.

set_6(State = #state{kernel = [K1, K2, K3, K4, K5, _K6, K7, K8, K9]}, I) ->
    State#state{kernel = [K1, K2, K3, K4, K5, I, K7, K8, K9]}.

set_7(State = #state{kernel = [K1, K2, K3, K4, K5, K6, _K7, K8, K9]}, I) ->
    State#state{kernel = [K1, K2, K3, K4, K5, K6, I, K8, K9]}.

set_8(State = #state{kernel = [K1, K2, K3, K4, K5, K6, K7, _K8, K9]}, I) ->
    State#state{kernel = [K1, K2, K3, K4, K5, K6, K7, I, K9]}.

set_9(State = #state{kernel = [K1, K2, K3, K4, K5, K6, K7, K8, _K9]}, I) ->
    State#state{kernel = [K1, K2, K3, K4, K5, K6, K7, K8, I]}.


-define(SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(SETTINGS, #{<<"x">> => ?SETTING(x),
                    <<"y">> => ?SETTING(y),
                    <<"K1">> => fun set_1/2,
                    <<"K2">> => fun set_2/2,
                    <<"K3">> => fun set_3/2,
                    <<"K4">> => fun set_4/2,
                    <<"K5">> => fun set_5/2,
                    <<"K6">> => fun set_6/2,
                    <<"K7">> => fun set_7/2,
                    <<"K8">> => fun set_8/2,
                    <<"K9">> => fun set_9/2}).

set(Setting, Value, State)
  when Setting == <<"x">>;
       Setting == <<"y">> ->

  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          #{Setting := Fun} = ?SETTINGS,
          Fun(State, I);
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for ", Setting/binary>>)},
          State
  end;
set(Setting, Value, State)
  when Setting == <<"K1">>;
       Setting == <<"K2">>;
       Setting == <<"K3">>;
       Setting == <<"K4">>;
       Setting == <<"K5">>;
       Setting == <<"K6">>;
       Setting == <<"K7">>;
       Setting == <<"K8">>;
       Setting == <<"K9">> ->
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
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid float ", Value/binary, " for ", Setting/binary>>)},
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

apply_kernel(Kernel, Bin, Width) ->
    Bins = kernel_binaries(Bin, Width),
    apply_kernel_(Kernel, Bins, _Pixels = []).

apply_kernel_(_, [<<>> | _], Image) ->
    Image;
apply_kernel_(Kernel, Bins, Image) ->
    {Pixels, Bins2} = lists:unzip([{Pixel, Rest} || <<Pixel:4/binary, Rest/binary>> <- Bins]),
    KernelPixels = lists:zip(Kernel, Pixels),
    Convoluted = lists:map(fun convolute/1, KernelPixels),
    P = sum_columns(Convoluted, []),
    apply_kernel_(Kernel, Bins2, Image ++ P).

sum_columns([[_A] | _], Sums) ->
    Alpha = 255,
    Sums ++ [Alpha];
sum_columns(Lists, Sums) ->
    {Heads, Tails} = lists:unzip([{hd(L), tl(L)} || L <- Lists]),
    Sum = clamp(lists:foldl(fun erlang:'+'/2, 0, Heads)),
    sum_columns(Tails, Sums ++ [Sum]).

clamp(I) ->
    max(0, min(255, I)).

convolute({K, <<R/integer, G/integer, B/integer, A/integer>>}) ->
    [K * R,
     K * G,
     K * B,
     K * A].

kernel_binaries(Bin, Width) ->
    [shift_columns_right_1(shift_row_down_1(Bin, Width), Width),
     shift_row_down_1(Bin, Width),
     shift_columns_left_1(shift_row_down_1(Bin, Width), Width),
     shift_columns_right_1(Bin, Width),
     Bin,
     shift_columns_left_1(Bin, Width),
     shift_columns_right_1(shift_row_up_1(Bin, Width), Width),
     shift_row_up_1(Bin, Width),
     shift_columns_left_1(shift_row_up_1(Bin, Width), Width)].

shift_row_down_1(Bin, Width) ->
    Size = size(Bin),
    <<Head:(Size - (Width * ?PIXEL_BYTES))/binary, _/binary>> = Bin,
    <<0:(Width * ?PIXEL_BITS), Head/binary>>.

shift_row_up_1(Bin, Width) ->
    <<_:(Width * ?PIXEL_BYTES)/binary, Tail/binary>> = Bin,
    <<Tail/binary, 0:(Width * ?PIXEL_BITS)>>.

shift_columns_right_1(Bin, Width) ->
    PixelWidth = Width * ?PIXEL_BYTES,
    << <<0:?PIXEL_BITS, Head/binary>> || <<Head:(PixelWidth - ?PIXEL_BYTES)/binary, _:?PIXEL_BYTES/binary>> <= Bin >>.

shift_columns_left_1(Bin, Width) ->
    PixelWidth = Width * ?PIXEL_BYTES,
    << <<Tail/binary, 0:?PIXEL_BITS>> || <<_:?PIXEL_BYTES/binary, Tail:(PixelWidth - ?PIXEL_BYTES)/binary>> <= Bin>>.

test_image() ->
    <<0,0,0,255,1,1,1,255,2,2,2,255,3,3,3,255,
      4,4,4,255,5,5,5,255,6,6,6,255,7,7,7,255,
      8,8,8,255,9,9,9,255,10,10,10,255,11,11,11,255,
      12,12,12,255,13,13,13,255,14,14,14,255,15,15,15,255>>.

id_kernel() ->
    [0, 0, 0,
     0, 1, 0,
     0, 0, 0].
