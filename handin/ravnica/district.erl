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


%%%=======================================================================
%%% API (State must be a neighbours/creatures/trigger/description tuple)
%%%=======================================================================

-spec create(string()) -> {ok, passage()} | {error, any()}.
create(Desc) -> gen_statem:start(?MODULE, Desc, []).

-spec get_description(passage()) -> {ok, string()} | {error, any()}.
get_description(District) -> gen_statem:call(District, get_description).

-spec connect(passage(), atom(), passage()) -> ok | {error, any()}.
connect(From, Action, To) -> gen_statem:call(From, {connect, Action, To}).

-spec activate(passage()) -> active | under_activation | impossible.
activate(District) -> activate1(District, []).
activate1(District, Visited) -> gen_statem:call(District, {activate, Visited}).

-spec options(passage()) -> {ok, [atom()]} | none.
options(District) -> gen_statem:call(District, options).

-spec enter(passage(), creature()) -> ok | {error, any()}.
enter(District, Creature) -> gen_statem:call(District, {enter, Creature}).

-spec take_action(passage(), creature_ref(), atom()) -> {ok, passage()} | {error, any()}.
take_action(From, CRef, Action) -> gen_statem:call(From, {take_action, CRef, Action}).

-spec shutdown(passage(), pid()) -> ok.
shutdown(District, NextPlane) -> shutdown1(District, NextPlane, []).
shutdown1(District, NextPlane, Visited) ->
  gen_statem:call(District, {shutdown, NextPlane, Visited}),
  gen_statem:stop(District).

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

% Under configuration

% simply return the description
under_configuration({call, From}, get_description, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {ok, D}}]};

% insert the (atom,To) pair into the map over neighbours if possible
under_configuration({call, From}, {connect, Action, To}, {N,C,T,D}) ->
  case maps:is_key(Action, N) of
    true  -> {keep_state, {N,C,T,D}, [{reply, From, {error, action_already_exists}}]};
    false -> {keep_state, {maps:put(Action, To, N),C,T,D}, [{reply, From, ok}]}
  end;

% activate each neighbour and wait for a response
% if any response is impossible, don't activate
under_configuration({call, From}, {activate, Visited}, Data) ->
  {next_state, under_activation, Data, [{next_event, internal, {From, activate,[self()|Visited]}}]};

% simply return the keys of the (atom,To) pairs in the map over neighbours
under_configuration({call, From}, options, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {ok, maps:keys(N)}}]};

% replace the current trigger in the test data
under_configuration({call, From}, {trigger, Trigger}, {N,C,_,D}) ->
  {keep_state, {N,C,Trigger,D}, [{reply, From, ok}]};

% call the shut_down helper function, see below
under_configuration({call, From}, {shutdown, NextPlane, Visited}, {N,C,T,D}) ->
  NextPlane ! {shutting_down, self(), maps:to_list(C)},
  {next_state, shutting_down, {N,C,T,D}, [{next_event, internal, {From, shutdown, NextPlane, [self()|Visited]}}]};

% handle other events generically
under_configuration({call, From}, _, Data) ->
  {keep_state, Data, [{reply, From, {error, not_valid}}]};
under_configuration(_, _, _) ->
  keep_state_and_data.



% Under activation

% simply return the state
under_activation({call, From}, {activate,_}, Data) ->
  {keep_state, Data, [{reply, From, under_activation}]};
% activate neighbours that haven't been visited
under_activation(internal, {From, activate, Visited}, {N,C,T,D}) ->
  Ns = lists:filter(fun (To) -> not(lists:member(To,Visited)) end, maps:values(N)),
  Act = lists:map(fun (To) -> activate1(To,Visited) end, Ns),
  case lists:any(fun (A) -> A == impossible end, Act) of
    true  -> {next_state, under_configuration, {N,C,T,D}, [{reply, From, impossible}]};
    false -> {next_state, active, {N,C,T,D}, [{reply, From, active}]}
  end;

% handle other events generically
under_activation({call, From}, _, Data) ->
  {keep_state, Data, [{reply, From, {error, not_valid}}]};
under_activation(_, _, _) ->
  keep_state_and_data.



% Active

% same as in under_configuration
active({call, From}, get_description, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {ok, D}}]};

% if already active, simply return this atom
active({call, From}, {activate,_}, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, active}]};

% same as in under_configuration
active({call, From}, options, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {ok, maps:keys(N)}}]};

% if the creature is not in the district, insert it and apply the trigger
active({call, From}, {enter, {CRef,CStat}}, {N,C,T,D}) ->
  case maps:is_key(CRef, C) of
    true  -> {keep_state, {N,C,T,D}, [{reply, From, {error, ref_already_exists}}]};
    false ->
      {{CRef,CStat1},Cs1} = apply_trigger(T,entering,{CRef,CStat},maps:to_list(C)),
      {keep_state, {N,maps:put(CRef,CStat1,maps:from_list(Cs1)),T,D}, [{reply, From, ok}]}
  end;

% check if the given action and the given creature exist
% if so, apply the trigger and move it if possible
% if it's a connection to the same district, apply the trigger locally
% and return as to avoid blocking
active({call, From}, {take_action, CRef, Action}, {N,C,T,D}) ->
  case C of
    #{CRef := CStat} ->
      case N of
        #{Action := To} ->
          {C1,Cs1} = apply_trigger(T,leaving,{CRef,CStat},maps:to_list(maps:remove(CRef, C))),
          case To == self() of
            true -> % going back to the same district, simply apply the trigger locally
              {{CRef,CStat2},Cs2} = apply_trigger(T,entering,C1,Cs1),
              {keep_state, {N,maps:put(CRef, CStat2, maps:from_list(Cs2)),T,D}, [{reply, From, {ok, To}}]};
            false -> % else, it will be handled when entering the other district
              case enter(To, C1) of
                {error, Reason} -> {keep_state, {N,C,T,D}, [{reply, From, {error, Reason}}]};
                ok              -> {keep_state, {N,maps:from_list(Cs1),T,D}, [{reply, From, {ok, To}}]}
              end
          end;
        #{} -> {keep_state, {N,C,T,D}, [{reply, From, {error, no_such_action}}]}
      end;
    #{}  -> {keep_state, {N,C,T,D}, [{reply, From, {error, no_such_creature}}]}
  end;

% same as in under_configuration
active({call, From}, {shutdown, NextPlane, Visited}, {N,C,T,D}) ->
  NextPlane ! {shutting_down, self(), maps:to_list(C)},
  {next_state, shutting_down, {N,C,T,D}, [{next_event, internal, {From, shutdown, NextPlane, [self()|Visited]}}]};

% handle other events generically
active({call, From}, _, Data) ->
  {keep_state, Data, [{reply, From, {error, not_valid}}]};
active(_, _, _) ->
  keep_state_and_data.

% applying the trigger: spawn a process and await a response
% if not well-formed, return the input data
apply_trigger(T, Event, {CRef,CStat}, Cs) ->
  Me = self(),
  Pid = spawn(fun () -> Me ! {self(), check_trigger(T,Event,{CRef,CStat},Cs)} end),
  receive
    {Pid,{{CRef,CStat1},Cs1}} ->
      case same_creatures(Cs1,Cs) of
        true  -> {{CRef,CStat1},Cs1};
        false -> {{CRef,CStat},Cs}
      end;
    {Pid, _} -> {{CRef,CStat},Cs}
  after
    2000 -> {{CRef,CStat}, Cs}
  end.

% checking the trigger
check_trigger(T,Event,C,Cs) ->
  try T(Event,C,Cs) of
    Res -> Res
  catch
    _:_ -> error
  end.

% check if the creatures are the same
same_creatures(Cs1, Cs2) ->
  case is_list(Cs1) and is_list(Cs1) of
    true ->
      Cs11 = lists:map(fun ({Ref,_}) -> Ref;
                           (Other) -> Other
                       end, Cs1),
      Cs22 = lists:map(fun ({Ref,_}) -> Ref;
                           (Other) -> Other
                       end, Cs2),
      lists:sort(Cs11) == lists:sort(Cs22);
    false -> false
  end.



% shutting down

% same as in under_configuration
shutting_down({call, From}, get_description, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, {ok, D}}]};

% impossible to active district that is shutting down
shutting_down({call, From}, {activate,_}, Data) ->
  {keep_state, Data, [{reply, From, impossible}]};

% no options when shutting down
shutting_down({call, From}, options, {N,C,T,D}) ->
  {keep_state, {N,C,T,D}, [{reply, From, none}]};

% already shutting down, so whatever
shutting_down({call, From}, {shutdown, _, _}, Data) ->
  {keep_state, Data, [{reply, From, ok}]};
% shut down neighbours if they haven't already been stopped
shutting_down(internal, {From, shutdown, NextPlane, Visited}, {N,C,T,D}) ->
  Ns = lists:filter(fun (To) -> not(lists:member(To,Visited)) end, maps:values(N)),
  lists:foreach(fun (To) ->
                    case process_info(To) of
                      undefined -> ok;
                      _ -> shutdown1(To,NextPlane,Visited)
                    end
                end, Ns),
  {keep_state, {N,C,T,D}, [{reply, From, ok}]};

% handle other events generically
shutting_down({call, From}, _, Data) ->
  {keep_state, Data, [{reply, From, {error, not_valid}}]};
shutting_down(_, _, _) ->
  keep_state_and_data.
