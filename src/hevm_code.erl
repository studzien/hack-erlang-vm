%%%===================================================================
%%% The code table's internal state is a simple mapping between
%%% module name atom and the module's code in binary
%%% It is expressed by an Erlang map and is temporary :)
%%%===================================================================
-module(hevm_code).

-behaviour(gen_server).

-compile({no_auto_import, [get/1]}).

%% API
-export([start_link/0,
         get/1,
         add/2,
         list/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Module, Code) ->
    gen_server:call(?MODULE, {add, Module, Code}).

get(Module) ->
    gen_server:call(?MODULE, {get, Module}).

list() ->
    gen_server:call(?MODULE, list).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #{}}.

handle_call(list, _From, State) ->
    {reply, State, State};
handle_call({get, Module}, _From, State) ->
    Reply = handle_get(Module, State),
    {reply, Reply, State};
handle_call({add, Module, Code}, _From, State) ->
    State1 = handle_add(Module, Code, State),
    {reply, ok, State1};
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
handle_add(Module, Code, State) ->
    maps:put(Module, Code, State).

handle_get(Module, State) ->
    maps:get(Module, State).
