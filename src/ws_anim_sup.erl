%%-------------------------------------------------------------------
%% @doc Not currently using a supervisor as I don't want to restart anything
%% @end
%%%-------------------------------------------------------------------

-module(ws_anim_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => fake, start => {fake, fake, []}}],
    {ok, {SupFlags, ChildSpecs}}.
