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
    SupFlags = #{},
    ChildSpecs = [#{id => registry,
                    start => {ws_anim_channel_registry, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.
