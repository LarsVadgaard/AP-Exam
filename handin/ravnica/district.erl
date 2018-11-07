-module(district).
-behaviour(gen_statem).

% API exports
-export([create/1,
         get_description/1,
         connect/3,
         activate/1,
         options/1,
         enter/2,
         take_action/3,
         shutdown/2,
         trigger/2]).

% Callback exports
-export([init/1, callback_mode/0, code_change/4, terminate/3]).

% State exports
-export([under_configuration/3, under_activation/3, active/3, shutting_down/3]).

% types
-type passage() :: pid().
-type creature_ref() :: reference().
-type creature_stats() :: map().
-type creature() :: {creature_ref(), creature_stats()}.
-type trigger() :: fun((entering | leaving, creature(), [creature()])
                                                   -> {creature(), [creature()]}).


%%%===================================================================
%%% API (State must be a neighbors/creatures/triggers tuple)
%%%===================================================================

name() -> ?MODULE.

-spec create(string()) -> {ok, passage()} | {error, any()}.
create(Desc) -> gen_statem:start(name(), Desc, []).

-spec get_description(passage()) -> {ok, string()} | {error, any()}.
get_description(District) -> gen_statem:call(District, get_description).

% TODO: error from the state function if the action is used for another connection
% and or the district is not under configuration
-spec connect(passage(), atom(), passage()) -> ok | {error, any()}.
connect(From, Action, To) -> gen_statem:call(From, {connect, Action, To}). % puts (Action, To) in the map state

% TODO: handle ref in state function
-spec activate(passage()) -> active | under_activation | impossible.
activate(District) -> repeat([District]).

repeat(N) ->
      A = lists:map(fun (T) -> gen_statem:call(T, activate) end, N),
      case lists:any(fun ({O,_}) -> O =:= impossible end, A) of
        true ->
          lists:foreach(fun (T) -> gen_statem:cast(T, cancel) end, N),
          impossible;
        false ->
          M = lists:map(fun ({_,Ns}) -> Ns end, A),
          Ns = lists:append(M),
          NoDup = lists:usort(Ns),
          case NoDup of
            [] ->
              lists:foreach(fun (T) -> gen_statem:cast(T, conclude) end, lists:usort(lists:append(N,Ns))),
              active;
            _  -> repeat(lists:append(N,NoDup))
          end
      end.


% returns all actions of the connections (keys)
% none if the district is shutting down
-spec options(passage()) -> {ok, [atom()]} | none.
options(District) -> gen_statem:call(District, options).

% district must be active and the reference must be unique
-spec enter(passage(), creature()) -> ok | {error, any()}.
enter(District, Creature) -> gen_statem:call(District, {enter, Creature}).

% from and to must be active, creature must be in from, and the creature can be moved to to
-spec take_action(passage(), creature_ref(), atom()) -> {ok, passage()} | {error, any()}.
take_action(From, CRef, Action) -> gen_statem:call(From, {take_action, CRef, Action}).

% sender en (shutting_down, district, creatures) besked ud til NextPlane
% lukker så ned for sine naboer
% stopper når alle er shut down eller shutting down
% simply returns okay if district is already shutting down
-spec shutdown(passage(), pid()) -> ok.
shutdown(District, NextPlane) -> gen_statem:call(District, {shutdown, NextPlane}).

% register trigger
% a trigger is a function that is called everytime a creature enters or leaves the district
% the trigger takes an even (entering|leaving), the creature that is entering/leaving, and
% a list of all creatures in the district
% only one trigger per district
-spec trigger(passage(), trigger()) -> ok | {error, any()} | not_supported.
trigger(District, Trigger) -> gen_statem:call(District, {trigger, Trigger}).



%%%===================================================================
%%% Callback functions
%%%===================================================================

callback_mode() -> state_functions.

init(Desc) -> {ok, under_configuration, {#{},#{},fun (_,C,Cs) -> {C,Cs} end,Desc}}.

terminate(_Reason, _State, _Data) -> void.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.


%%%===================================================================
%%% State functions
%%%===================================================================

% under configuration

under_configuration({call, From}, get_description, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {ok, D}}]};

under_configuration({call, From}, {connect, Action, To}, {N,C,T,D}) ->
  case maps:get(Action, N, none) of
    none -> {keep_state, {maps:put(Action, To, N),C,T,D}, [{reply, From, ok}]};
    _    -> {keep_state, {N,C,T,D}, [{reply, From, {error, action_already_exists}}]}
  end;

% TODO: få det her til at virke
% sæt hver nabo i gang, og gå så til under_activation
% vent så på at de caster et kald tilbage
under_configuration({call, From}, activate, {N,C,T,D}) ->
  {next_state, under_activation, {N,C,T,D}, [{reply, From, {under_activation, maps:values(N)}}]};

under_configuration({call, From}, options, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {ok, maps:keys(N)}}]};

under_configuration({call, From}, {enter, _}, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {error, not_active}}]};

under_configuration({call, From}, {trigger, Trigger}, {N,C,_,D}) ->
  {keep_state, {N,C,Trigger,D}, [{reply, From, ok}]}.



% under activation

under_activation({call, From}, activate, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {under_activation, []}}]};
under_activation(cast, cancel, {N,C,T,D}) ->
  {next_state, under_configuration, {N,C,T,D}};
under_activation(cast, conclude, {N,C,T,D}) ->
  {next_state, active, {N,C,T,D}};

under_activation({call, From}, {enter, _}, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {error, not_active}}]};

under_activation({call, From}, {trigger, _}, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {error, not_under_configuration}}]}.



% active

active({call, From}, activate, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, active}]};

active({call, From}, options, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {ok, maps:keys(N)}}]};

active({call, From}, {enter, {CRef,CStat}}, {N,C,T,D}) ->
  case maps:get(CRef, C, none) of
    none -> {keep_state, {N,maps:put(CRef,CStat,C),T,D}, [{reply, From, ok}]};
    _    -> {keep_state, {N,C,T,D}, [{reply, From, {error, ref_already_exists}}]}
  end;

active({call, From}, {take_action, CRef, Action}, {N,C,T,D}) ->
  % check if is in C
  % delete from C
  % check if action is avail
  % enter through action
  case maps:get(CRef, C, none) of
    none  -> {keep_state, {N,C,T,D}, [{reply, From, {error, no_such_creature}}]};
    CStat ->
      C1 = maps:remove(CRef, C),
      case maps:get(Action, N, none) of
        none -> {keep_state, {N,C,T,D}, [{reply, From, {error, no_such_action}}]};
        To   ->
          case enter(To, {CRef,CStat}) of
            {error, Reason} -> {keep_state, {N,C,T,D}, [{reply, From, {error, Reason}}]};
            ok              -> {keep_state, {N,C1,T,D}, [{reply, From, {ok, To}}]}
          end
      end
  end;

% TODO: fix this also
active({call, From}, {shutdown, NextPlane}, {N,C,T,D}) ->
  NextPlane ! {shutting_down, self(), C},
  case maps:size(N) of
    0 -> {next_state, shutting_down, {#{},C,T,D}, [{reply, From, ok}]};
    _ ->
      maps:map(fun (To) -> shutdown(To, NextPlane) end, N),
      {next_state, shutting_down, {N,C,T,D}, [{reply, From, ok}]}
  end;

active({call, From}, {trigger, _}, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {error, not_under_configuration}}]}.


% shutting down

shutting_down({call, From}, activate, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, impossible}]};

shutting_down({call, From}, options, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, none}]};

shutting_down({call, From}, {enter, _}, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {error, not_active}}]};

shutting_down({call, From}, {shutdown, _, _}, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, ok}]};

shutting_down({call, From}, {trigger, _}, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {error, not_under_configuration}}]}.
