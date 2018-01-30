-module(erlivo_handler).
-behaviour(gen_statem).
-include("erlivo_commands.hrl").

-export([start_link/2, init/1, callback_mode/0, terminate/3, stop/1]).
-export([channel_create/3, channel_answer/3, channel_bridge/3, 
         channel_hangup/3, play_and_get_digits/3]).
-export([command/2, answer/1, preanswer/1, hangup/2]).
-export([dial_string/1]).
-callback start_application(reference()) -> {reference(), pid()}.

%% API
stop(Pid) ->
    gen_statem:stop(Pid).

answer(Pid) ->
    gen_statem:call(Pid, answer).

preanswer(Pid) ->
    gen_statem:call(Pid, presanswer).

hangup(Pid, Cause) ->
    gen_statem:call(Pid, {hangup, Cause}).

command(Pid, #playback{} = Opts) ->
    gen_statem:call(Pid, {playback, Opts});
command(Pid, #play_and_get_digits{} = Opts) ->
    gen_statem:call(Pid, {play_and_get_digits, Opts});
command(Pid, #record{} = Opts) ->
    gen_statem:call(Pid, {record, Opts}).




%% BEHAVIOUR
start_link(M,F) ->
    gen_statem:start_link(?MODULE, [M,F], []).

callback_mode() ->
    state_functions.

init([M,F]) ->
    Table = ets:new(reply, [set, private]),
    {ok, channel_create, #{handler => {M, F}, table => Table}}.

terminate(_, _, _) ->
    io:format("TERMINATE~n"),
    ok.

channel_create(info, {call, Call}, #{handler := {M, F}} = Data) ->
    {event, [UUID | Headers]} = Call,
    io:format("NEW CALL DETECTED ~p~n", [UUID]),
    FS = list_to_atom(proplists:get_value("variable_erlivo_freeswich_node", Headers)),
    Direction = list_to_atom(proplists:get_value("variable_direction", Headers)),
    spawn_link(M, F, [UUID, self(), Direction]),
    erlang:monitor_node(FS, true),
    {keep_state, Data#{uuid => UUID, node => FS}};
channel_create(info, {call_event, Event}, Data) ->
    {event, [_UUID | Headers]} = Event,  
    Direction = proplists:get_value("Caller-Logical-Direction", Headers),
    case  Direction of
        "inbound" ->
            {next_state, channel_answer, Data};
        "outbound" ->
            {next_state, channel_bridge, Data}
    end.

channel_answer(State, Action, Data) ->
    case handle_event(State, Action, Data) of
        {keep_state, _} ->
            handle_answer(State, Action, Data);
        Reply ->
            Reply
    end.


channel_bridge(State, Action, Data) ->
    io:format("CALL BRIDGED~n"),
    case handle_event(State, Action, Data) of
        {keep_state, _} ->
            handle_bridge(State, Action, Data);
        Reply ->
            Reply
    end.

play_and_get_digits(State, Action, Data) ->
    case handle_event(State, Action, Data) of
        {keep_state, _} ->
            handle_play_and_get_digits(State, Action, Data);
        Reply ->
            Reply
    end.

channel_hangup(cast, Cause, #{uuid := UUID, node := FS}) ->
    erlivo_api:hangup(FS, UUID, Cause),
    {stop, normal};
channel_hangup(info, _, Data) ->
    {keep_state, Data}.

handle_answer({call, From}, Action, #{uuid := UUID, node := FS} = Data) ->
    case Action of
        answer ->
            erlivo_api:answer(FS, UUID),
            {next_state, channel_bridge, Data, [{reply, From, ok}]};
        preanswer ->
            erlivo_api:preanswer(FS, UUID),
            {next_state, channel_bridge, Data, [{reply, From, ok}]};
        _ ->
            {keep_state, Data, [{reply, From, {error, invalid_answer}}]}
    end;
handle_answer(_, _, Data) ->
    {keep_state, Data}.

handle_bridge({call, From}, {playback, Opts}, #{uuid := UUID, node := FS} = Data) ->
    Reply = erlivo_api:playback(FS, UUID, Opts),
    {keep_state, Data, [{reply, From, Reply}]};
handle_bridge({call, From}, {record, Opts}, #{uuid := UUID, node := FS} = Data) ->
    Reply = erlivo_api:record(FS, UUID, Opts),
    {keep_state, Data, [{reply, From, Reply}]};
handle_bridge({call, From}, {play_and_get_digits, #play_and_get_digits{} = Opts}, #{uuid := UUID, node := FS, table := Table} = Data) ->
    ets:insert(Table, {UUID, From}),
    ok = erlivo_api:play_and_get_digits(FS, UUID, Opts),
    {next_state, play_and_get_digits, Data};
handle_bridge({call, From}, {hangup, Cause}, Data) ->
    gen_statem:cast(self(), Cause),
    {next_state, channel_hangup, Data, [{reply, From, ok}]};
handle_bridge(cast, _, Data) ->
    {keep_state, Data};
handle_bridge(info, {call_event, _event}, Data) ->
    {keep_state, Data}.

handle_play_and_get_digits(_, {call_event, Event}, #{table := Table} = Data) ->
    {event, [UUID | Headers]} = Event,
    EventName = proplists:get_value("Event-Name", Headers),
    io:format("EVENT: ~p~n", [EventName]),
    case EventName of
        "CHANNEL_EXECUTE_COMPLETE" ->
            [{_, From}] = ets:take(Table, UUID),
            Digits = proplists:get_value("variable_pagd_input", Headers),
            case Digits of
                undefined ->
                    gen_server:reply(From, timeout);
                Digits ->
                    gen_server:reply(From, {digits, Digits})
            end,
            {next_state, channel_bridge, Data};
        _ ->
            {keep_state, Data}
    end.

handle_event(_, {call_event, Event}, Data) ->
    {event, [_UUID | Headers]} = Event,
    EventName = proplists:get_value("Event-Name", Headers),
    case EventName of
        "CHANNEL_HANGUP_COMPLETE" ->
            Cause = proplists:get_value("variable_hangup_cause", Headers),
            gen_statem:cast(self(), Cause),
            {next_state, channel_hangup, Data};
        _ ->
            {keep_state, Data}
    end;
handle_event(_, {call_hangup, Event}, Data) ->
    {event, [_UUID | Headers]} = Event,
    Cause = proplists:get_value("variable_hangup_cause", Headers),
    gen_statem:cast(self(), Cause),
    {next_state, channel_hangup, Data};
handle_event(_, {nodedown, _}, _) ->
    {stop, nodedown};
handle_event(_, _, Data) ->
    {keep_state, Data}.




    
%% AUXILIAR
-spec sofia_url(erlivo_call:call(), string()) -> binary().
sofia_url(Call, Suffix) ->
    Gateways = [Gateway ++ erlivo_call:to(Call) || Gateway <- erlivo_call:gateways(Call)],
    Vars = [
            {"erlivo_freeswich_node", atom_to_list(erlivo_call:node(Call))}
           , {"origination_caller_id_number", erlivo_call:from(Call)}
           , {"destination_number", erlivo_call:to(Call)}],
    VarString = [Name ++ "='" ++ Value ++ "'" || {Name, Value} <- Vars],
    erlang:list_to_binary(
      "{" ++ lists:join(",", VarString) ++ "}" ++ lists:join("|", Gateways)
      ++ " " ++ Suffix
     ).

-spec dial_string(erlivo_call:call()) -> binary().
dial_string(Call) ->
    {M,F, N} = case erlivo_call:application(Call) of
                   {M1, N1} ->
                       {M1, start_application, N1};
                   M1 when is_atom(M1) ->
                       {M1, start_application, node()};
                   _ ->
                       throw(bad_handler)
               end,
    SN = atom_to_list(N),
    Append = " '&erlang(" ++ atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ " " ++ SN ++ ")'",
    sofia_url(Call, Append).
