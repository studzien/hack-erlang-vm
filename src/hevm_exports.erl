%%%===================================================================
%%% The export table's internal state consists of a mapping between
%%% 3-tuples {module's atom index, function's atom index, arity}
%%% and an internal (for a module from which it's exported) 
%%% label of this function. This is temporary :)
%%%===================================================================
-module(hevm_exports).

-behaviour(gen_server).

-compile({no_auto_import, [put/2, get/1]}).

%% API
-export([start_link/0,
         put/2,
         get/1,
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

put(MFA, Value) ->
    gen_server:call(?MODULE, {put, MFA, Value}).

get(MFA) ->
    gen_server:call(?MODULE, {get, MFA}).

list() ->
    gen_server:call(?MODULE, list).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #{}}.

handle_call(list, _From, State) ->
    {reply, State, State};
handle_call({put, MFA, Value}, _From, State) ->
    State1 = handle_put(MFA, Value, State),
    {reply, ok, State1};
handle_call({get, MFA}, _From, State) ->
    {reply, handle_get(MFA, State), State};
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
handle_get(MFA, State) ->
    case maps:find(MFA, State) of
        {ok, Value} ->
            Value;
        error ->
            undef
    end.

handle_put(MFA, Value, State) ->
    maps:put(MFA, Value, State).
