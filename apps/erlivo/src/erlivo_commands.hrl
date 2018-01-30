-record(playback, {filename
              , loops = 1
              }).
-type playback() :: #playback{}.
-type uuid() :: string() | atom().

-record(play_and_get_digits, {min = 1
                             , max = 99
                             , tries = 1
                             , timeout = 5000
                             , terminators = "#"
                             , file
                             , invalid_file = "silence_stream://250"
                             , var_name = "pagd_input"
                             , regexp = "[0123456789#*]+"
                             , delimiter = "!"
                             }).
-type play_and_get_digits() :: #play_and_get_digits{}.

-record(record, {action = start
                ,path
                , limit = 10}).
-type record() :: #record{}.
