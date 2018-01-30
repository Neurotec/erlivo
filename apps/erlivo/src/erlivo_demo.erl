-module(erlivo_demo).
-include("erlivo_commands.hrl").
-export([start/0, start_pool/0, start_loopback/0, start_services/0]).

start() ->
    io:format("STARTED HANDLER~n"),
    Call = erlivo_call:new(freeswitch@monda, "1004", "1001", ["user/"], erlivo_handler_logger),
    io:format("CALL IN PROGRESS~n"),
    {ok, UUID} = erlivo_api:outbound_call(Call).


start_pool() ->
    erlivo_handler_sup:start_link(),
    erlivo_api:start_link(freeswitch@monda),
    erlivo_handler_pool:start_link(),
    io:format("STARTED HANDLER POOL~n"),
    Call = erlivo_call:new(freeswitch@monda, "1004", "1001", ["user/"], erlivo_handler_pool),
    io:format("CALL IN PROGRESS~n"),
    {ok, UUID} = erlivo_api:outbound_call(Call),
    {call, {UUID, Handler, _}} = erlivo_handler_pool:get_call_by_uuid(UUID),
    timer:sleep(500),
    erlivo_handler:command(Handler, #record{path="/tmp/salee.wav", limit=1}),
    timer:sleep(50000),
    erlivo_handler:hangup(Handler, "USER_BUSY").

start_services() ->
    {ok, _} = erlivo_handler_sup:start_link(),
    {ok, _} = erlivo_api:start_link(freeswitch@monda),
    {ok, _} = erlivo_handler_pool:start_link().

start_loopback() ->
    Call = erlivo_call:new(freeswitch@monda
                          , "test", "answer\\,park/default/inline", ["loopback/"], erlivo_handler_pool),
    {ok, UUID} = erlivo_api:outbound_call(Call),
    {call, {UUID, H, _}} = erlivo_handler_pool:get_call_by_uuid(UUID),
    timer:sleep(500),
    ok = erlivo_handler:hangup(H, "USER_BUSY"),
    timer:sleep(500).
