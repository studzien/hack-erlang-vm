-module(hevm_emu).

-behaviour(gen_server).

%% API
-export([start_link/0,
         code/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

code(Module, Function, Arity) ->
    gen_server:call(?MODULE, {code, Module, Function, Arity}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({code, Module, Function, Arity}, _From, State) ->
    Reply = handle_code(Module, Function, Arity),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_code(M, F, A) ->
    MIndex = hevm_atoms:get(M),
    FIndex = hevm_atoms:get(F),
    Entry = hevm_exports:get({MIndex, FIndex, A}),
    {Code, _} = hevm_code:get(MIndex),
    lists:nthtail(Entry-1, tuple_to_list(Code)).
