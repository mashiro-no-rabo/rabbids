%% Memory attack in order to trigger alarms based on memeory in RabbitMQ
%% This is archived by holding a large binary in the gen_server state

-module(rabbids_mem_attack).

-vsn(1).

-export([start/0, attack/1, withdraw/0]).
-export([init/1, handle_cast/2]).

-behaviour(gen_server).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [nil], []).

%% Start a memory attack aiming to increase memory usage by the amount in arguments (in megabytes)
attack(NumMegabytes) ->
  gen_server:cast(?MODULE, {start_attack, NumMegabytes}).

%% List running attacks
list() ->
  [].

%% Stop a specific attack
withdraw() ->
  gen_server:stop(?MODULE).

init(_) ->
  {ok, nil}.

handle_cast({start_attack, NumMegabytes}, _) ->
  NewState=list_to_binary(lists:duplicate(NumMegabytes, list_to_binary(lists:duplicate(1024*1024, <<"a">>)))),
  {noreply, NewState}.
