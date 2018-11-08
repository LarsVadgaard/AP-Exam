-module(the_diamond_path).
-export([a_love_story/0]).

% Print stuff
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

a_love_story() ->
  % Defining a dimond-shaped territory.
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  {ok, D} = district:create("D"),
  district:connect(A, a, A),
  district:connect(A, b, B),
  district:connect(A, c, C),
  district:connect(B, d, D),
  district:connect(C, d, D),
  district:connect(D, a, A),

  ?PRINT(sys:get_state(A)),
  ?PRINT(sys:get_state(B)),
  ?PRINT(sys:get_state(C)),
  ?PRINT(sys:get_state(D)),
  % Activating the districts.
  % Since there is a path from A to every other district, this will suffice:
  district:activate(A),

  % Two players without stats.
  {BobRef, _}   = Bob   = {make_ref(), #{}},
  {AliceRef, _} = Alice = {make_ref(), #{}},

  % Bob and Alice entered the same district:
  district:enter(A, Bob),
  district:enter(A, Alice),

  ?PRINT(sys:get_state(A)),
  ?PRINT(sys:get_state(B)),
  ?PRINT(sys:get_state(C)),
  ?PRINT(sys:get_state(D)),

  % But on that day, they choose to follow different paths.
  district:take_action(A, BobRef, b),
  district:take_action(A, AliceRef, c),

  % But fortunately, there is no way to get lost in the diamond path.
  district:take_action(B, BobRef, d),
  district:take_action(C, AliceRef, d),  % <------------- | changed in ver. 1.0.1 |

  ?PRINT(sys:get_state(A)),
  ?PRINT(sys:get_state(B)),
  ?PRINT(sys:get_state(C)),
  ?PRINT(sys:get_state(D)),

  district:shutdown(A, self()).

% THE END
