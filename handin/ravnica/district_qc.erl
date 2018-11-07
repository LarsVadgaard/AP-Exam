-module(district_qc).

-export([territory/0, setup_territory/1]).
-export([prop_activate/0, prop_shutdown/0, prop_take_action/0]).

-include_lib("eqc/include/eqc.hrl").

%%%===================================================================
%%% Generators
%%%===================================================================

atom()     -> elements([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,x,y,z]).
key()      -> ?SIZED(Size,choose(0,Size)).
creature() -> {make_ref(), #{}}.

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

% Generating the map
territory() ->
  ?LET(L, list({key(),list({atom(),key()})}),
       maps:from_list(L)).

% Setting up the territory
setup_territory(Map) ->
  NMap = maps:map(fun (_,V) -> maps:from_list(V) end, Map),
  Fun = fun (_,V,Acc) -> case maps:is_key(V,NMap) of
                           true -> Acc;
                           false -> [V|Acc]
                         end
        end,
  NotInMap = maps:fold(fun (_,V,Acc) ->
                           lists:append( Acc
                                       , maps:fold(Fun, [], V))
                       end, [], NMap),
  NNMap = lists:foldl(fun (K,Acc) -> maps:put(K, #{}, Acc) end, NMap, NotInMap),
  Ds = maps:map(fun (K,_) ->
                    {ok, District} = district:create(integer_to_list(K)),
                    District
                end, NNMap),
  maps:map(fun (K,V) ->
               maps:map(fun (K1,V1) ->
                            district:connect(maps:get(K,Ds),K1,maps:get(V1,Ds))
                        end, V)
           end, NNMap),
  maps:values(Ds).


%%%===================================================================
%%% Properties
%%%===================================================================

% Activation property
prop_activate() ->
    ?FORALL(T, ?SUCHTHAT(T1, territory(), maps:size(T1) > 0),
    ?LET(T1, setup_territory(T),
    ?FORALL(D, elements(T1),
    ?IMPLIES(
         district:activate(D) == active
       , is_active(D) %lists:all(fun is_active/1, neighbours(D))
    )))).

is_active(D) ->
  case sys:get_state(D) of
    {active, _} -> true;
    _ -> false
  end.


% Shutdown property
prop_shutdown() ->
    ?FORALL(T, ?SUCHTHAT(T1, territory(), maps:size(T1) > 0),
    ?LET(T1, setup_territory(T),
    ?FORALL(D, elements(T1),
    ?IMPLIES(district:activate(D) == active,
      ?IMPLIES(
           district:shutdown(D,self()) == ok
         , is_shut_down(D)
    ))))).

is_shut_down(D) ->
  try sys:get_state(D) of
    {shutting_down,_} -> true;
    _ -> false
  catch
    _   -> true;
    _:_ -> true
  end.


% Take_action property
prop_take_action() ->
    ?FORALL(T, ?SUCHTHAT(T1, territory(), maps:size(T1) > 0),
    ?LET(T1, setup_territory(T),
    ?LET(D, elements(T1),
    ?IMPLIES(district:activate(D) == active,
      ?LET({CRef,CStat}, creature(),
      ?IMPLIES(district:enter(D,{CRef,CStat}) == ok,
        ?LET({ok,O}, district:options(D),
        ?IMPLIES(length(O) > 0,
          ?LET(Act, elements(O),
          ?LET({ok,To}, district:take_action(D, CRef, Act),
          ?IMPLIES(To =/= D,
                   is_in_district(CRef, To) and not(is_in_district(CRef, D))
    ))))))))))).


is_in_district(CRef,D) ->
  case sys:get_state(D) of
    {_, {_,C,_,_}} -> maps:is_key(CRef,C);
    _ -> false
  end.
