-module(all_exported).
-compile({parse_transform, export_all}).

test() ->
    ok.
