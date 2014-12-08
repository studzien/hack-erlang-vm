%%%===================================================================
%%% The atom table's internal state consists of two mappings:
%%% a) from atom binary to atom's index
%%% b) from atom's index to its binary
%%% both are represented by an Erlang map
%%% The state also contains atom counter (next atom's index)
%%%===================================================================
-module(hevm_atoms).

-behaviour(gen_server).

%% API
-export([start_link/0,
         list/0,
         add/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {atom_to_index, index_to_atom, next}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list() ->
    gen_server:call(?MODULE, list).

add(Atom) ->
    gen_server:call(?MODULE, {add, Atom}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{atom_to_index = #{},
                index_to_atom = #{},
                next = 1}}.

handle_call(list, _From, State) ->
    {reply, handle_list(State), State};
handle_call({add, Atom}, _From, State) ->
    {Index, NewState} = handle_add(Atom, State),
    {reply, Index, NewState};
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
handle_add(Atom, State=#state{atom_to_index = AtoI,
                              index_to_atom = ItoA,
                              next = Next}) ->
    case maps:find(Atom, AtoI) of
        {ok, Index} ->
            {Index, State};
        error ->
            AtoI1 = maps:put(Atom, Next, AtoI),
            ItoA1 = maps:put(Next, Atom, ItoA),
            State1 = State#state{atom_to_index = AtoI1,
                                 index_to_atom = ItoA1,
                                 next = Next+1},
            {Next, State1}
    end.

handle_list(#state{index_to_atom = ItoA}) ->
    List = maps:to_list(ItoA),
    lists:keysort(1, List).
