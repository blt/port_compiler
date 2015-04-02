-module(pc_util).

-export([
         replace_extension/2, replace_extension/3,
         target_type/1
        ]).
-export_type([]).

%%%===================================================================
%%% API
%%%===================================================================

replace_extension(File, NewExt) ->
    OldExt = filename:extension(File),
    replace_extension(File, OldExt, NewExt).
replace_extension(File, OldExt, NewExt) ->
    filename:rootname(File, OldExt) ++ NewExt.

target_type(Target)  -> target_type1(filename:extension(Target)).
target_type1(".so")  -> drv;
target_type1(".dll") -> drv;
target_type1("")     -> exe;
target_type1(".exe") -> exe.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
