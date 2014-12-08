-module(hevm_loader).

-behaviour(gen_server).

%% API
-export([start_link/0,
         load/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {atoms, exports, code}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load(Path) ->
    gen_server:call(?MODULE, {load, Path}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({load, Path}, _From, State) ->
    {reply, handle_load(Path), State};
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
handle_load(Path) ->
    {ok, File} = file:read_file(Path),
    do_load(File).

do_load(<<"FOR1", _Size:32, "BEAM", Chunks/binary>>) ->
    State1 = load_atom_chunk(Chunks, #state{}),
    State2 = load_exports_chunk(Chunks, State1),
    State3 = load_code_chunk(Chunks, State2).

load_atom_chunk(<<"Atom", Size:32, Rest/binary>>, State) ->
    {AtomChunk, _} = erlang:split_binary(Rest, chunk_size(Size)),
    <<_:32, Atoms/binary>> = AtomChunk,
    LocalAtoms = load_atoms(Atoms, 1, #{}),
    State#state{atoms = LocalAtoms};
load_atom_chunk(<<_:8, Rest/binary>>, State) ->
    load_atom_chunk(Rest, State).

load_atoms(<<>>, _, Acc) ->
    Acc;
load_atoms(<<0:8,_/binary>>, _, Acc) ->
    Acc;
load_atoms(<<Len:8, Rest/binary>>, LocalIndex, Acc) ->
    {Atom, Atoms} = erlang:split_binary(Rest, Len),
    GlobalIndex = hevm_atoms:add(Atom),
    Acc1 = maps:put(LocalIndex, GlobalIndex, Acc),
    load_atoms(Atoms, LocalIndex+1, Acc1).

load_exports_chunk(<<"ExpT", Size:32, Rest/binary>>, #state{atoms=Atoms}=State) ->
    {ExpChunk, _} = erlang:split_binary(Rest, chunk_size(Size)),
    <<_:32, Exports/binary>> = ExpChunk,
    LocalExports = load_exports(Exports, 1, Atoms, #{}),
    State#state{exports = LocalExports};
load_exports_chunk(<<_:8, Rest/binary>>, State) ->
    load_exports_chunk(Rest, State).

load_exports(<<>>, _, _, Acc) ->
    Acc;
load_exports(<<F:32, Arity:32, L:32, Rest/binary>>, LocalIndex, Atoms, Acc) ->
    Module = maps:get(1, Atoms),
    Function = maps:get(F, Atoms),
    MFA = {Module, Function, Arity},
    ok = hevm_exports:put(MFA, L),
    Acc1 = maps:put(LocalIndex, MFA, Acc), 
    load_exports(Rest, LocalIndex+1, Atoms, Acc1);
load_exports(_Other, _, _, Acc) ->
    Acc.

load_code_chunk(<<"Code", Size:32, Rest/binary>>, #state{atoms=Atoms}=State) ->
    {CodeChunk, _} = erlang:split_binary(Rest, chunk_size(Size)),
    <<16:32,0:32,_Opcode:32,Labels:32,Exported:32,Code/binary>> = CodeChunk,
    ok = hevm_code:add(maps:get(1, Atoms), Code),
    State#state{code = Code};
load_code_chunk(<<_:8, Rest/binary>>, State) ->
    load_code_chunk(Rest, State).

chunk_size(Size) ->
    case Size rem 4 of
        0 -> Size;
        R -> Size+(4-R)
    end.
