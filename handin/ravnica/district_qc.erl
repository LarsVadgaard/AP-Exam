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

% Generating the map
territory() ->
  ?LET(L, list({key(),list({atom(),key()})}),
       make_nonexisting_neighbours(maps:from_list(L))).

% Make sure that all neighbours exist
make_nonexisting_neighbours(Map) ->
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
  lists:foldl(fun (K,Acc) -> maps:put(K, #{}, Acc) end, NMap, NotInMap).


% Setting up the territory
setup_territory(Map) ->
  Ds = maps:map(fun (K,_) ->
                    {ok, District} = district:create(integer_to_list(K)),
                    District
                end, Map),
  maps:map(fun (K,V) ->
               maps:map(fun (K1,V1) ->
                            district:connect(maps:get(K,Ds),K1,maps:get(V1,Ds))
                        end, V)
           end, Map),
  maps:values(Ds).


%%%===================================================================
%%% Properties
%%%===================================================================

% Activation property
prop_activate() ->
    ?FORALL(T, ?SUCHTHAT(T1, territory(), maps:size(T1) > 0),
    ?LET(T1, setup_territory(T),
    ?FORALL(D, elements(T1),
    ?LET({ok,O}, district:options(D),
    ?IMPLIES(length(O) > 0,
    ?IMPLIES(district:activate(D) == active,
        ?FORALL(Act, elements(O),
        ?LET({CRef,CStat}, creature(),
        ?IMPLIES(district:enter(D,{CRef,CStat}) == ok,
          ?LET({ok,To}, district:take_action(D,CRef,Act),
          is_active(To)
    )))))))))).

% use system call to check if active
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
    ?LET({ok,O}, district:options(D),
    ?IMPLIES(length(O) > 0,
    ?IMPLIES(district:activate(D) == active,
        ?FORALL(Act, elements(O),
        ?LET({CRef,CStat}, creature(),
        ?IMPLIES(district:enter(D,{CRef,CStat}) == ok,
          ?LET({ok,To}, district:take_action(D,CRef,Act),
          ?IMPLIES(district:shutdown(D,self()) == ok,
            (is_shut_down(D)) and (is_shut_down(To))
    ))))))))))).

is_shut_down(D) ->
  case process_info(D) of
    undefined -> true;
    _         -> false
  end.


% Take_action property
prop_take_action() ->
    ?FORALL(T, ?SUCHTHAT(T1, territory(), maps:size(T1) > 0),
    ?LET(T1, setup_territory(T),
    ?FORALL(D, elements(T1),
    ?LET({ok,O}, district:options(D),
    ?IMPLIES(length(O) > 0,
    ?IMPLIES(district:activate(D) == active,
        ?FORALL(Act, elements(O),
        ?LET({CRef,CStat}, creature(),
        ?IMPLIES(district:enter(D,{CRef,CStat}) == ok,
          ?LET({ok,To}, district:take_action(D,CRef,Act),
          ?IMPLIES(To /= D,
            district:take_action(D,CRef,Act) /= ok
    ))))))))))).
