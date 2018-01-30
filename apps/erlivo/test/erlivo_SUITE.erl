-module(erlivo_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2]).
-export([call_test/1, bulk_calls_test/1]).

all() ->
    [call_test, bulk_calls_test].


init_per_testcase(_, Config) ->
    Node = freeswitch@monda,
    erlang:set_cookie(node(), 'ClueCon'),
    erlivo_handler_sup:start_link(),
    erlivo_api:start_link(Node),
    erlivo_handler_pool:start_link(),
    [{pbx, Node} | Config].

call_test(Config) ->
    Call = erlivo_call:new(?config(pbx, Config)
                          , "test", "answer\\,park/default/inline", ["loopback/"], erlivo_handler_pool),
    {ok, UUID} = erlivo_api:outbound_call(Call),
    {call, {UUID, H, _}} = erlivo_handler_pool:get_call_by_uuid(UUID),
    timer:sleep(500),
    erlivo_handler:hangup(H, "USER_BUSY"),
    finish.

%[CRIT] switch_core_session.c:2265 Throttle Error! 39
%[CRIT] mod_loopback.c:250 Failure.
%[CRIT] switch_time.c:1227 Over Session Rate of 30!
%must update auto_loadconfig/switch.xml:param[max-sessions-per-second]
%@todo not support more of 100 call by seconds, then timed out erlang handler.
bulk_calls_test(Config) ->
    Pid = self(),
    Max = 200,
    [ spawn_link(fun() -> finish = call_test(Config), wait_group_done(Pid) end) || _ <- lists:seq(0, Max)],
    wait_group(Max).

%% AUXILIARY

wait_group_done(Pid) ->
    Pid ! done.

wait_group(C) when C =:= 0 ->
                    done;
wait_group(C) ->
    receive
        done ->
            wait_group(C - 1)
    end.
    
