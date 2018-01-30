-module(erlivo_handler_pool).
-behaviour(gen_server).
-behaviour(erlivo_handler).

-export([start_application/1]).
-export([start_link/0, init/1,handle_call/3, handle_cast/2, terminate/2]).
-export([ start_call/3, get_call_by_uuid/1]).

start_application(Ref) ->
    {ok, Handler} = erlivo_handler_sup:start_child(erlivo_handler_pool, start_call),
    {Ref, Handler}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ets:new(calls, [set, private, named_table]),
    ets:new(handlers, [set, private, named_table]),
    {ok, []}.

-spec start_call(erlivo_call:uuid(), pid(), erlivo_call:direction()) -> any().
start_call(UUID, Pid, Direction) ->
    gen_server:cast(?MODULE, {start_call, UUID, Pid, Direction}).

get_call_by_uuid(UUID) when is_binary(UUID) ->
    get_call_by_uuid(binary_to_list(UUID));
get_call_by_uuid(UUID) ->
    gen_server:call(?MODULE, {get_call_by_uuid, UUID}).

handle_cast({start_call, UUID, Handler, Dir}, State) ->
    case ets:take(handlers, UUID) of
        [] ->
            ets:insert(calls, {UUID, Handler, Dir}),
            {noreply, State};
        [{UUID, From}] ->
            gen_server:reply(From, {call, {UUID, Handler, Dir}}),
            {noreply, State}
    end.

handle_call({get_call_by_uuid, UUID}, From, State) ->
    case ets:take(calls, UUID) of
        [] ->
            ets:insert(handlers, {UUID, From}),
            {noreply, State};
        [Item] ->
            {reply, {call, Item}, State}
    end.

terminate(_, _) ->
    ets:foldl(fun({_, Handler, _}, _Acc) ->
                      erlivo_handler:hangup(Handler, "MANAGER_REQUEST")
              end, [], calls),
    ok.
