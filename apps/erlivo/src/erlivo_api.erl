-module(erlivo_api).
-include("erlivo_commands.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_info/2, terminate/2]).
-export([outbound_call/1, create_uuid/1]).
-export([hangup/3, answer/2, preanswer/2, msleep/3
         , set/4, playback/3, play_and_get_digits/3, chat/3, record/3]).

%% API
-spec outbound_call(erlivo_call:call()) -> ok.
outbound_call(Call) ->
    SofiaUrl = erlivo_handler:dial_string(Call),
    Reply = gen_server:call(erlivo_call:node(Call), {bgapi, originate, SofiaUrl}, infinity),
    case Reply of
        {ok, Msg} ->
            UUID = extract_call_uuid(Msg),
            {ok, binary_to_list(UUID)};
        Error ->
            Error
    end.
    
create_uuid(FS) ->
    gen_server:call(FS, {bgapi, create_uuid, ""}).

chat(FS, UUID, Msg) when is_list(Msg) ->
    gen_server:call(FS, {sendmsg, UUID, execute
                        , [{"execute-app-name", "chat"}
                          ,{"execute-app-arg", Msg}]}).

record(FS, UUID, Opts)  when is_binary(UUID) ->
    record(FS, binary_to_list(UUID), Opts);
record(FS, UUID, #record{action=start, path=Path,limit=Limit}) when is_integer(Limit) ->
    gen_server:call(FS, {bgapi, uuid_record, UUID ++ " start " ++ Path ++ " " ++ integer_to_list(Limit)});
record(FS, UUID,  #record{action=stop, path=Path,limit=Limit}) when is_integer(Limit) ->
    gen_server:call(FS, {bgapi, uuid_record,UUID ++ " stop " ++ Path ++ " " ++ integer_to_list(Limit)}).

answer(FS, UUID) ->
    gen_server:call(FS, {sendmsg, UUID, execute,
                         [{"execute-app-name", "answer"}]}).

preanswer(FS, UUID) ->
    gen_server:call(FS, {sendmsg, UUID, execute, 
                         [{"execute-app-name", "preanswer"}]}).

msleep(FS, UUID, Time) when is_integer(Time) ->
    gen_server:call(FS, {sendmsg, UUID, execute,
                         [{"execute-app-name", "msleep"}
                         , {"execute-app-arg", integer_to_list(Time)}
                         , {"loops", 1}
                         ]}).

set(FS, UUID, Name, Value) ->
    gen_server:call(FS, {sendmsg, UUID, set, 
                         [{"execute-app-name", Name ++ "=" ++ Value}
                         ,{"event-lock", "true"}]}).

playback(FS, UUID, #playback{filename=Filename, loops=Loops}) ->
    gen_server:call(FS, {sendmsg, UUID,  execute,
                         [{"execute-app-name", "playback"}
                         ,{"execute-app-arg", Filename}
                         , {"event-lock", "true"}
                         , {"loops", Loops}
                         ]}).

play_and_get_digits(FS, UUID, #play_and_get_digits{min = Min
                                                  , max = Max
                                                  , tries = Tries
                                                  , timeout = Timeout
                                                  , terminators = Terminators
                                                  , file = File
                                                  , invalid_file = InvalidFile
                                                  , regexp = ValidDigits
                                })  ->
    ok = set(FS, UUID, "playback_delimiter", "!"),
    Args =  [integer_to_list(Min), integer_to_list(Max), integer_to_list(Tries), integer_to_list(Timeout)
            , Terminators
            , File, InvalidFile, "pagd_input"
            , "^(" ++ ValidDigits ++ ")$"
            , integer_to_list(5000)
            ],

    gen_server:call(FS, {sendmsg, UUID, execute,
                         [{"execute-app-name", "play_and_get_digits"}
                         , {"execute-app-arg", string:join(Args, " ")}
                         , {"loops", "1"}]}).

hangup(FS, UUID, Cause) ->
    gen_server:call(FS, {sendmsg, UUID, execute,
                         [{"execute-app-name", "hangup"}
                         , {"execute-app-arg", Cause}
                         , {"loops", "1"}]}).

%% BEHAVIOUR

start_link(Node) ->
    gen_server:start_link({local, Node}, ?MODULE, [Node], []).

init([Node]) ->
    Table = ets:new(bgapi_reply, [set, private]),
    {event, Node} ! register_event_handler,
    {event, Node} ! {event, 'CHANNEL_EXECUTE_COMPLETE'},

    {ok, {Node, queue:new(), Table}}.
    
terminate(_, _) ->
    ok.

handle_call({sendmsg, UUID, Action, Args}, From, {Node, Queue, Table}) ->
    case (catch api_sendmsg(Node, UUID, Action, Args)) of
        {ok, SUUID} ->
            ets:insert(Table, {{sendmsg, SUUID}, From}),
            io:format("SENDMSG ~p ~p UUID: ~p~n", [Action, Args, SUUID]),
            {noreply, {Node, Queue, Table}};
        Error ->
            io:format("SENDMSG ERROR: ~p~n", [Error]),
            {reply, Error, {Node, Queue, Table}}
    end;
handle_call({bgapi, Action, Args}, From, {Node, Queue, Table}) ->
    case (catch bgapi(Node, Action, Args)) of
        ok ->
            io:format("bgapi: ~p~n", [Action]),
            Queue1 = queue:in({bgapi, From}, Queue),
            {noreply, {Node, Queue1, Table}};
        Error ->
            {reply, Error, {Node, Queue, Table}}
    end.

handle_info({bgok, Job, Resp}, {_, _, Table} = State) ->
    [{{_, Job}, From}] = ets:take(Table, {bgapi, Job}),
    gen_server:reply(From, {ok, Resp}),
    {noreply, State};
handle_info({bgerror, Job, Resp}, {_, _, Table} = State) ->
    [{Job, From}] = ets:take(Table, {bgapi, Job}),
    gen_server:reply(From, {error, Resp}),
    {noreply, State};
handle_info({event, Event}, {Node, Queue, Table}) ->
    [_UUID | Headers] = Event,
    ApplicationUUID = proplists:get_value("Application-UUID", Headers),
    ApplicationResponse = proplists:get_value("Application-Response", Headers),
    io:format("SENDMSG RESPONSE: ~p:~p~n", [ApplicationUUID, ApplicationResponse]),
    case ets:take(Table, {sendmsg, ApplicationUUID}) of
        [{_, From}] ->
            Response = case ApplicationResponse of
                           "_none_" ->
                               ok;
                           Msg ->
                               {ok, Msg}
                       end,
            gen_server:reply(From, Response),
            {noreply, {Node, Queue, Table}};
        Data ->
            io:format("SENDMSG RESPONS UNKNOW: ~p~n", [Data]),
            {noreply, {Node, Queue, Table}}
    end;
handle_info(ok, State) ->
    {noreply, State};
handle_info(Resp, {Node, Queue, Table}) ->
    io:format("RESPONSE: ~p~n", [Resp]),
    {{value, {bgapi, From}}, Queue1} = queue:out(Queue),
    case Resp of
        {ok, Job} ->
            ets:insert(Table, {{bgapi, Job}, From});
        _ ->
            gen_server:reply(From, Resp)
    end,
    {noreply, {Node, Queue1, Table}}.



%% HELPERS
extract_call_uuid(Msg)  when is_list(Msg) ->
    extract_call_uuid(list_to_binary(Msg));
extract_call_uuid(<<"+OK ", Rest/binary>>) ->
    binary:replace(Rest, <<"\n">>, <<>>).

api_sendmsg(FS, UUID, Command, Args) ->
    SUUID = integer_to_list(erlang:unique_integer([positive])),
    Url = {sendmsg, UUID, [{"call-command", atom_to_list(Command)}
                          , {"Event-UUID", SUUID}] ++ args_to_list(Args)},
    {sendmsg, FS} ! Url,
    {ok, SUUID}.

bgapi(FS, Action, Args) ->
    {api, FS} ! {bgapi, Action, Args},
    ok.


args_to_list(Args) ->
    args_to_list(Args, []).
args_to_list([{Name, Value} | Args], Acc) when is_integer(Value) ->
    args_to_list(Args, Acc ++ [{Name, integer_to_list(Value)}]);
args_to_list([{Name, Value} | Args], Acc) when is_atom(Value)  ->
    args_to_list(Args, Acc ++ [{Name, atom_to_list(Value)}]);
args_to_list([Arg | Args], Acc) ->
    args_to_list(Args, Acc ++ [Arg]);
args_to_list([], Acc) ->
    Acc.
