-module(district).

-export([create/1,
         get_description/1,
         connect/3,
         activate/1,
         options/1,
         enter/2,
         take_action/3,
         shutdown/2,
         trigger/2]).

-type passage() :: pid().
-type creature_ref() :: reference().
-type creature_stats() :: map().
-type creature() :: {creature_ref(), creature_stats()}.
-type trigger() :: fun((entering | leaving, creature(), [creature()])
                                                   -> {creature(), [creature()]}).


-spec create(string()) -> {ok, passage()} | {error, any()}.
create(_) ->
    undefined.

-spec get_description(passage()) -> {ok, string()} | {error, any()}.
get_description(_) ->
    undefined.

-spec connect(passage(), atom(), passage()) -> ok | {error, any()}.
connect(_, _, _) ->
    undefined.

-spec activate(passage()) -> active | under_activation | impossible.
activate(_) ->
    undefined.


-spec options(passage()) -> {ok, [atom()]} | none.
options(_) ->
    undefined.

-spec enter(passage(), creature()) -> ok | {error, any()}.
enter(_, _) ->
    undefined.

-spec take_action(passage(), creature_ref(), atom()) -> {ok, passage()} | {error, any()}.
take_action(_, _, _) ->
    undefined.

-spec shutdown(passage(), pid()) -> ok.
shutdown(_, _) ->
    undefined.

-spec trigger(passage(), trigger()) -> ok | {error, any()} | not_supported.
trigger(_, _) ->
    not_supported.
