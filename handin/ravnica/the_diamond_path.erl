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
%  district:connect(A, a, A),
  district:connect(A, b, B),
  district:connect(A, c, C),
  district:connect(B, d, D),
  district:connect(C, d, D),
%  district:connect(A, f, A),
%  district:connect(D, g, B),

  district:trigger(A, fun (leaving,{Cr,S},Cs)  -> {{Cr,maps:put("leave",5,S)},Cs};
                          (entering,{Cr,S},Cs) -> {{Cr,maps:put("enter",0,S)},Cs}
                      end),

  % Activating the districts.
  % Since there is a path from A to every other district, this will suffice:
  district:activate(A),

  % Two players without stats.
  {BobRef, _}   = Bob   = {make_ref(), #{}},
  {AliceRef, _} = Alice = {make_ref(), #{}},

  % Bob and Alice entered the same district:
  district:enter(A, Bob),
  district:enter(A, Alice),

  % But on that day, they choose to follow different paths.
  district:take_action(A, BobRef, b),
  district:take_action(A, AliceRef, c),

  % But fortunately, there is no way to get lost in the diamond path.
  district:take_action(B, BobRef, d),
  district:take_action(C, AliceRef, d),  % <------------- | changed in ver. 1.0.1 |

  district:shutdown(A, self()).

% THE END
