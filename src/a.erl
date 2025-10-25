-module(a).

-export([a/0]).
-export([rec/0]).

-record(a, {a, b, c}).

a() ->
    a.

rec() ->
    #a{a = 1, b = 2, c = 3}.
