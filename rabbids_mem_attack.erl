%% Memory attack in order to trigger alarms based on memeory in RabbitMQ
%% This is archived by holding a large binary in the gen_server state

-module(rabbids_mem_attack).

-vsn(1).

-export([prepare/0, teardown/0, attack/1, info/0, withdraw/0]).
-export([init/1, handle_cast/2, handle_call/3]).

-behaviour(gen_server).

%%%%%
%% APIs

prepare() ->
  gen_server:start(
    {local, ?MODULE}, % register a local name
    ?MODULE,
    [nil],
    % configure to run fullsweep GC all the time, in order to quickly get rid of stale refc binaries
    [{spawn_opt, [{fullsweep_after, 0}]}]
  ).

%% Start a memory attack by using additional memory as specified by the argument (in MiB, aka. 2^20 bytes)
attack(NumMebibytes) ->
  gen_server:cast(?MODULE, {attack, NumMebibytes}).

%% Query the current running attack
info() ->
  case gen_server:call(?MODULE, info) of
    0 -> io:fwrite("No memory attack running.\n");
    NumMebibytes -> io:fwrite("Memory attack running with ~b MiB payload.\n", [NumMebibytes])
  end.

%% Stop a specific attack
withdraw() ->
  gen_server:cast(?MODULE, withdraw).

%% Teardown the attack process
teardown() ->
  gen_server:stop(?MODULE).

%%%%%
%% callbacks

init(_) ->
  {ok, nil}.

handle_cast({attack, NumMebibytes}, _) ->
  LargeBinary=list_to_binary(lists:duplicate(NumMebibytes, list_to_binary(lists:duplicate(1024*1024, <<"r">>)))),
  {noreply, {NumMebibytes, LargeBinary}};

handle_cast(withdraw, _) ->
  {noreply, nil}.

handle_call(info, _, nil) ->
  {reply, 0, nil};

handle_call(info, _, {NumMebibytes, _} = State) ->
  {reply, NumMebibytes, State}.
