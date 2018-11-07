-module(test).
-export([test/0]).

% Print stuff
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

test() ->
  % Defining a dimond-shaped territory.
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  {ok, D} = district:create("D"),
  district:connect(A, b, B),
  district:connect(A, c, C),
  district:connect(B, d, D),
  district:connect(C, d, D),

  OptA = district:options(A),
  OptB = district:options(B),
  OptC = district:options(C),
  OptD = district:options(D),

  DesA = district:get_description(A),
  DesB = district:get_description(B),
  DesC = district:get_description(C),
  DesD = district:get_description(D),

  ?PRINT(OptA),
  ?PRINT(OptB),
  ?PRINT(OptC),
  ?PRINT(OptD),
  ?PRINT(DesA),
  ?PRINT(DesB),
  ?PRINT(DesC),
  ?PRINT(DesD),

  % Activating the districts.
  % Since there is a path from A to every other district, this will suffice:
  district:activate(A).


% THE END
