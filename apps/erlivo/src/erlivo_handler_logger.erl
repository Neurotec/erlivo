-module(erlivo_handler_logger).
-include("erlivo_commands.hrl").
-export([start_application/1, handle_call/3]).

%%@bug 2016-12-30 00:39:14.426014 [WARNING] ei_helpers.c:85 Failed to link to process on local@monda
%%debe estar corriendo el servicio erlivo_handler_sup
start_application(Ref) ->
    {ok , Handler} = erlivo_handler_sup:start_child(erlivo_handler_logger, handle_call),
    {Ref, Handler}.

handle_call(_UUID, Pid, _Direction) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("handle_call logger~n"),
    loop([Pid]).

loop([Pid] = State) ->
    receive
        {'EXIT', _, _} ->
            {stop, normal};
        Msg ->
            error_logger:info_msg(Msg)
        after 3000 ->
                error_logger:info_msg("START APPLICATION LOGGER~n"),
                %%ok = erlivo_handler:command(Pid, #playback{filename="/tmp/audio.wav", loops=3}),
                Val = erlivo_handler:command(Pid, #play_and_get_digits{file="/tmp/demo.wav", min=2}),
                io:format("GET DIGITS: ~p~n", [Val]),
                erlivo_handler:hangup(Pid, "normal-clearing"),
                loop(State)
        end.


