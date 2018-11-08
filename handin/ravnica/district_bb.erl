-module(district_bb).
-include_lib("eunit/include/eunit.hrl").


%%================================
%% Getting description
%%================================

% simple
get_description1_test() ->
  {ok, A} = district:create("A"),
  {ok, Desc} = district:get_description(A),
  ?assert(Desc =:= "A").

% from district C just moved to
get_description2_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),

  active = district:activate(A),

  {CRef, _} = C = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, To} = district:take_action(A, CRef, b),

  {ok, Desc} = district:get_description(To),

  ?assert(Desc =:= "B").


%%================================
%% Connecting districts
%%================================

% simple
connect1_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B).

% two to same
connect2_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),
  ok = district:connect(A, c, B).

% two to different
connect3_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),

  ok = district:connect(A, b, B),
  ok = district:connect(A, c, C).

% same action twice
connect4_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),
  {error,_} = district:connect(A, b, B).


%%================================
%% Activation
%%================================

% single
activate1_test() ->
  {ok, A} = district:create("A"),
  active = district:activate(A).

% simple connection
activate2_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  ok = district:connect(A, b, B),
  active = district:activate(A).

% cycle
activate3_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, a, A),
  active = district:activate(A).

% self loop in root
activate4_test() ->
  {ok, A} = district:create("A"),
  ok = district:connect(A, a, A),
  active = district:activate(A).

% self loop somewhere
activate5_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, c, C),
  ok = district:connect(C, a, A),
  active = district:activate(A).

% multiple self loops in root
activate6_test() ->
  {ok, A} = district:create("A"),
  ok = district:connect(A, a, A),
  ok = district:connect(A, b, A),
  active = district:activate(A).

% multiple self loops somewhere
activate7_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, c, C),
  ok = district:connect(C, d, C),
  ok = district:connect(C, a, A),
  active = district:activate(A).


%%================================
%% Getting options
%%================================

% none
options1_test() ->
  {ok, A} = district:create("A"),
  {ok, O} = district:options(A),
  ?assert(O =:= []).

% single
options2_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  ok = district:connect(A, b, B),
  {ok, O} = district:options(A),
  ?assert(O =:= [b]).

% multiple
options3_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(A, c, C),
  {ok, O} = district:options(A),
  ?assert(O =:= [b,c]).

% self loop
options4_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, a, A),
  ok = district:connect(A, b, B),
  ok = district:connect(A, c, C),
  {ok, O} = district:options(A),
  ?assert(O =:= [a,b,c]).


%%================================
%% Entering districts
%%================================

% single
enter1_test() ->
  {ok, A} = district:create("A"),
  active = district:activate(A),
  C = {make_ref(), #{}},
  ok = district:enter(A, C).

% entering twice
enter2_test() ->
  {ok, A} = district:create("A"),
  active = district:activate(A),
  C = {make_ref(), #{}},
  ok = district:enter(A, C),
  {error, _} = district:enter(A, C).

% entering twice when not active
enter3_test() ->
  {ok, A} = district:create("A"),
  C = {make_ref(), #{}},
  {error, _} = district:enter(A, C).


%%================================
%% Taking action
%%================================

% single
take_action1_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),

  active = district:activate(A),

  {CRef, _} = C = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, To} = district:take_action(A, CRef, b),

  ?assert(To =:= B).

% self loop
take_action2_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, a, A),
  ok = district:connect(A, b, B),

  active = district:activate(A),

  {CRef, _} = C = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, To} = district:take_action(A, CRef, a),

  ?assert(To =:= A).

% already exists in To
take_action3_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),

  active = district:activate(A),

  {CRef, _} = C = {make_ref(), #{}},

  ok = district:enter(A, C),
  ok = district:enter(B, C),

  {error, _} = district:take_action(A, CRef, b).

% forth and back again
take_action4_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),
  ok = district:connect(B, a, A),

  active = district:activate(A),

  {CRef, _} = C = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, To1} = district:take_action(A, CRef, b),
  {ok, To2} = district:take_action(B, CRef, a),
  ?assert((To1 =:= B) and (To2 =:= A)).

% try the same action twice
take_action5_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),

  active = district:activate(A),

  {CRef, _} = C = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, _} = district:take_action(A, CRef, b),
  {error, _} = district:take_action(A, CRef, b).


%%================================
%% Shutting down when under config
%%================================

% single
shutdown1_test() ->
  {ok, A} = district:create("A"),
  ok = district:shutdown(A,self()).

% simple connection
shutdown2_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  ok = district:connect(A, b, B),
  ok = district:shutdown(A,self()).

% cycle
shutdown3_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, a, A),
  ok = district:shutdown(A,self()).

% self loop in root
shutdown4_test() ->
  {ok, A} = district:create("A"),
  ok = district:connect(A, a, A),
  ok = district:shutdown(A,self()).

% self loop somewhere
shutdown5_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, c, C),
  ok = district:connect(C, a, A),
  ok = district:shutdown(A,self()).

% multiple self loops in root
shutdown6_test() ->
  {ok, A} = district:create("A"),
  ok = district:connect(A, a, A),
  ok = district:connect(A, b, A),
  ok = district:shutdown(A,self()).

% multiple self loops somewhere
shutdown7_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, c, C),
  ok = district:connect(C, d, C),
  ok = district:connect(C, a, A),
  ok = district:shutdown(A,self()).


%%================================
%% Shutting down when active
%%================================

% single
shutdown8_test() ->
  {ok, A} = district:create("A"),
  active = district:activate(A),
  ok = district:shutdown(A,self()).

% simple connection
shutdown9_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  ok = district:connect(A, b, B),
  active = district:activate(A),
  ok = district:shutdown(A,self()).

% cycle
shutdown10_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, a, A),
  active = district:activate(A),
  ok = district:shutdown(A,self()).

% self loop in root
shutdown11_test() ->
  {ok, A} = district:create("A"),
  ok = district:connect(A, a, A),
  ok = district:shutdown(A,self()).

% self loop somewhere
shutdown12_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, c, C),
  ok = district:connect(C, a, A),
  active = district:activate(A),
  ok = district:shutdown(A,self()).

% multiple self loops in root
shutdown13_test() ->
  {ok, A} = district:create("A"),
  ok = district:connect(A, a, A),
  ok = district:connect(A, b, A),
  active = district:activate(A),
  ok = district:shutdown(A,self()).

% multiple self loops somewhere
shutdown14_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  {ok, C} = district:create("C"),
  ok = district:connect(A, b, B),
  ok = district:connect(B, c, C),
  ok = district:connect(C, c, C),
  ok = district:connect(C, d, C),
  ok = district:connect(C, a, A),
  active = district:activate(A),
  ok = district:shutdown(A,self()).


%%===================================
%% Getting message when shutting down
%%===================================

% single
shutdown15_test() ->
  {ok, A} = district:create("A"),
  active = district:activate(A),

  C = {make_ref(), #{}},

  ok = district:enter(A, C),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, Cs} ->
      case process_info(A) of
        undefined -> ?assert(Cs =:= [C]);
        _ -> ?assert(false)
      end
  after
      5000 -> ?assert(false)
  end.

% two
shutdown16_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),

  active = district:activate(A),

  C1 = {make_ref(), #{}},
  C2 = {make_ref(), #{}},

  ok = district:enter(A, C1),
  ok = district:enter(B, C2),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, Cs1} ->
      case process_info(A) of
        undefined ->
          receive
            {shutting_down, B, Cs2} -> ?assert((Cs1 =:= [C1]) and (Cs2 =:= [C2]))
          after
              5000 -> ?assert(false)
          end;
        _ -> ?assert(false)
      end
  after
      5000 -> ?assert(false)
  end.

% after taking action
shutdown17_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),

  active = district:activate(A),

  {CRef,_} = C1 = {make_ref(), #{}},
  C2 = {make_ref(), #{}},

  ok = district:enter(A, C1),
  ok = district:enter(B, C2),

  {ok, _} = district:take_action(A, CRef, b),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, Cs1} ->
      case process_info(A) of
        undefined ->
          receive
            {shutting_down, B, Cs2} ->
              case process_info(B) of
                undefined -> ?assert((Cs1 =:= []) and (Cs2 =:= [C1,C2]));
                _ -> ?assert(false)
              end
          after
              5000 -> ?assert(false)
          end;
        _ -> ?assert(false)
      end
  after
      5000 -> ?assert(false)
  end.

% after forth and back again
shutdown18_test() ->
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),

  ok = district:connect(A, b, B),
  ok = district:connect(B, a, A),

  active = district:activate(A),

  {CRef,_} = C1 = {make_ref(), #{}},
  C2 = {make_ref(), #{}},

  ok = district:enter(A, C1),
  ok = district:enter(B, C2),

  {ok, _} = district:take_action(A, CRef, b),
  {ok, _} = district:take_action(B, CRef, a),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, Cs1} ->
      case process_info(A) of
        undefined ->
          receive
            {shutting_down, B, Cs2} ->
              case process_info(B) of
                undefined -> ?assert((Cs1 =:= [C1]) and (Cs2 =:= [C2]));
                _ -> ?assert(false)
              end
          after
              5000 -> ?assert(false)
          end;
        _ -> ?assert(false)
      end
  after
      5000 -> ?assert(false)
  end.


%%=====================
%% Triggers
%%=====================

% no trigger for leaving, should ignore
trigger1(entering, {CRef,CStat}, Cs) ->
  NCStat = maps:update_with("entered", fun (V) -> V + 1 end, 1, CStat),
  {{CRef, NCStat}, Cs}.

% count enterings (1)
trigger1_test() ->
  {ok, A} = district:create("A"),

  ok = district:trigger(A, fun trigger1/3),

  active = district:activate(A),

  C = {CRef, _} = {make_ref(), #{}},

  ok = district:enter(A, C),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, [{CRef,CStat1}]} ->
      case process_info(A) of
        undefined ->
          case maps:get("entered",CStat1,none) of
            R -> ?assert(R =:= 1)
          end;
        _ -> ?assert(false)
      end
  after
      5000 -> ?assert(false)
  end.

% count enterings (2)
trigger2_test() ->
  {ok, A} = district:create("A"),

  ok = district:trigger(A, fun trigger1/3),

  ok = district:connect(A, a, A),

  active = district:activate(A),

  C = {CRef, _} = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, _} = district:take_action(A, CRef, a),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, [{CRef,CStat1}]} ->
      case process_info(A) of
        undefined ->
          case CStat1 of
            #{"entered" := 2} -> ?assert(true);
            #{} -> ?assert(false)
          end;
        _ -> ?assert(false)
      end
  after
      5000 -> ?assert(false)
  end.

% now also trigger for leaving
trigger2(entering, {CRef,CStat}, Cs) ->
  NCStat = maps:update_with("entered", fun (V) -> V + 1 end, 1, CStat),
  {{CRef, NCStat}, Cs};
trigger2(leaving, {CRef,CStat}, Cs) ->
  NCStat = maps:update_with("left", fun (V) -> V + 1 end, 1, CStat),
  {{CRef, NCStat}, Cs}.

% count enterings and leavings (2,1)
trigger3_test() ->
  {ok, A} = district:create("A"),

  ok = district:trigger(A, fun trigger2/3),

  ok = district:connect(A, a, A),

  active = district:activate(A),

  C = {CRef, _} = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, _} = district:take_action(A, CRef, a),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, [{CRef,CStat1}]} ->
      case process_info(A) of
        undefined ->
          case CStat1 of
            #{"entered" := 2, "left" := 1} -> ?assert(true);
            #{} -> ?assert(false)
          end;
        _ -> ?assert(false)
      end
  after
      5000 -> ?assert(false)
  end.

% wrong output
trigger3(entering, C, _) ->
  {C, none};
trigger3(leaving, _, _) ->
  also_none.

% should not change C
trigger4_test() ->
  {ok, A} = district:create("A"),

  ok = district:trigger(A, fun trigger3/3),

  ok = district:connect(A, a, A),

  active = district:activate(A),

  C = {CRef, _} = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, _} = district:take_action(A, CRef, a),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, [C]} -> ?assert(true)
  after
      5000 -> ?assert(false)
  end.

% throw
trigger4(entering, _, _) ->
  throw("THROW").

% should not change C
trigger5_test() ->
  {ok, A} = district:create("A"),

  ok = district:trigger(A, fun trigger4/3),

  ok = district:connect(A, a, A),

  active = district:activate(A),

  C = {CRef, _} = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, _} = district:take_action(A, CRef, a),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, [C]} -> ?assert(true)
  after
      5000 -> ?assert(false)
  end.

% error
trigger5(entering, _, _) ->
  error("ERROR").

% should not change C
trigger6_test() ->
  {ok, A} = district:create("A"),

  ok = district:trigger(A, fun trigger5/3),

  ok = district:connect(A, a, A),

  active = district:activate(A),

  C = {CRef, _} = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, _} = district:take_action(A, CRef, a),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, [C]} -> ?assert(true)
  after
      5000 -> ?assert(false)
  end.

% exit
trigger6(entering, _, _) ->
  exit("EXIT").

% should not change C
trigger7_test() ->
  {ok, A} = district:create("A"),

  ok = district:trigger(A, fun trigger6/3),

  ok = district:connect(A, a, A),

  active = district:activate(A),

  C = {CRef, _} = {make_ref(), #{}},

  ok = district:enter(A, C),

  {ok, _} = district:take_action(A, CRef, a),

  ok = district:shutdown(A,self()),

  receive
    {shutting_down, A, [C]} -> ?assert(true)
  after
      5000 -> ?assert(false)
  end.
