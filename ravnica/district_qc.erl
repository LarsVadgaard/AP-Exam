-module(district_qc).

-export([territory/0, setup_world/1]).
-export([prop_activate/0, prop_take_action/0]).

-include_lib("eqc/include/eqc.hrl").

territory() ->
    undefined.

setup_world(_) ->
    undefined.

prop_activate() ->
    false.

prop_take_action() ->
    false.
