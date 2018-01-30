-module(erlivo_handler_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_child/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    SupFlags = #{strategy => simple_one_for_one
                , intensity => 1
                , period => 3},
    ChildSpecs = [#{id => ?MODULE
                   , start => {erlivo_handler, start_link, []}
                   , restart => temporary
                   , shutdown => brutal_kill
                   , type => worker
                   , modules => [erlivo_handler]}],
    {ok, {SupFlags, ChildSpecs}}.

start_child(M, F) ->
    supervisor:start_child(?MODULE, [M, F]).
