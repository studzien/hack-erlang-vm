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

-record(state, {atoms, exports, imports, literals, code, labels}).

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
    State3 = load_imports_chunk(Chunks, State2),
    State4 = load_literals_chunk(Chunks, State3),
    State5 = load_code_chunk(Chunks, State4),
    State6 = load_labels(State5),
    State7 = replace_jumps(State6),
    State8 = replace_atoms(State7),
    do_load_exports(State8),
    do_load_code(State8).

do_load_exports(#state{exports=Exports, labels=Labels}) ->
    maps:map(fun(MFA, Label) ->
                Position = maps:get(Label, Labels),
                ok = hevm_exports:put(MFA, Position)
        end, Exports).

do_load_code(#state{code=Code, atoms=Atoms, imports=Imports}) ->
    #{1 := Module} = Atoms,
    ok = hevm_code:add(Module, {list_to_tuple(Code), Imports}).

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

load_exports_chunk(<<"ExpT", Size:32, Rest/binary>>,
                   #state{atoms=Atoms}=State) ->
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
    Acc1 = maps:put(MFA, L, Acc), 
    load_exports(Rest, LocalIndex+1, Atoms, Acc1);
load_exports(_Other, _, _, Acc) ->
    Acc.

load_imports_chunk(<<"ImpT", Size:32, Rest/binary>>,
                   #state{atoms=Atoms}=State) ->
    {ImpChunk, _} = erlang:split_binary(Rest, chunk_size(Size)),
    <<_:32, Imports/binary>> = ImpChunk,
    ImportMFAs = load_imports(Imports, 1, Atoms, #{}),
    State#state{imports = ImportMFAs};
load_imports_chunk(<<_:8, Rest/binary>>, State) ->
    load_imports_chunk(Rest, State).

load_imports(<<>>, _, _, Acc) ->
    Acc;
load_imports(<<M:32, F:32, A:32, Rest/binary>>, LocalIndex, Atoms, Acc) ->
    MFA = {maps:get(M, Atoms), maps:get(F, Atoms), A},
    Acc1 = maps:put(LocalIndex, MFA, Acc),
    load_imports(Rest, LocalIndex+1, Atoms, Acc1);
load_imports(_Other, _, _, Acc) ->
    Acc.

load_literals_chunk(<<>>, State) ->
    State;
load_literals_chunk(<<"LitT", Size:32, Rest/binary>>, State) ->
    {LitChunk, _} = erlang:split_binary(Rest, chunk_size(Size)),
    Literals = load_literals(LitChunk),
    State#state{literals = Literals};
load_literals_chunk(<<_:8, Rest/binary>>, State) ->
    load_literals_chunk(Rest, State).

load_literals(<<_Size:32, LitT/binary>>) ->
    <<_N:32, LitT1/binary>> = zlib:uncompress(LitT),
    load_literals(LitT1, 0, #{}).

load_literals(<<>>, _, Acc) ->
    Acc;
load_literals(<<Size:32, Rest/binary>>, Index, Acc) ->
    {Literal, Rest1} = erlang:split_binary(Rest, Size),
    Acc1 = maps:put(Index, binary_to_term(Literal), Acc),
    load_literals(Rest1, Index+1, Acc1).

load_code_chunk(<<"Code", Size:32, Rest/binary>>,
                #state{literals=Literals}=State) ->
    {CodeChunk, _} = erlang:split_binary(Rest, chunk_size(Size)),
    <<16:32,0:32,_Opcode:32,_Labels:32,_Exported:32,Code/binary>> = CodeChunk,
    InterpretableCode = transform_code(Code, Literals, []),
    State#state{code = InterpretableCode};
load_code_chunk(<<_:8, Rest/binary>>, State) ->
    load_code_chunk(Rest, State).

load_labels(State=#state{code=Code}) ->
    Labels = load_labels(Code, 1, #{}),
    State#state{labels = Labels}.

load_labels([], _, Acc) ->
    Acc;
load_labels([label, {u,Index}|Tail], Position, Acc) ->
    Acc1 = maps:put(Index, Position+2, Acc),
    load_labels(Tail, Position+2, Acc1);
load_labels([_|Tail], Position, Acc) ->
    load_labels(Tail, Position+1, Acc).

replace_jumps(State=#state{code=Code, labels=Labels}) ->
    Code1 = replace_jumps(Code, Labels, []),
    State#state{code=Code1}.

replace_jumps([], _Labels, Acc) ->
    lists:reverse(Acc);
replace_jumps([{f, 0}|Tail], Labels, Acc) ->
    replace_jumps(Tail, Labels, [{f, 0}|Acc]);
replace_jumps([{f,Label}|Tail], Labels, Acc) ->
    Acc1 = [{f, maps:get(Label, Labels)}|Acc],
    replace_jumps(Tail, Labels, Acc1);
replace_jumps([Head|Tail], Labels, Acc) ->
    replace_jumps(Tail, Labels, [Head|Acc]).

replace_atoms(State=#state{code=Code, atoms=Atoms}) ->
    Code1 = replace_atoms(Code, Atoms, []),
    State#state{code=Code1}.

replace_atoms([], _Atoms, Acc) ->
    lists:reverse(Acc);
replace_atoms([{a, Index}|Tail], Atoms, Acc) ->
    Acc1 = [{a, maps:get(Index, Atoms)}|Acc],
    replace_atoms(Tail, Atoms, Acc1);
replace_atoms([Head|Tail], Atoms, Acc) ->
    replace_atoms(Tail, Atoms, [Head|Acc]).

transform_code(<<>>, _Literals, Acc) ->
    lists:reverse(Acc);
transform_code(<<Op:8, Rest/binary>>, Literals, Acc) ->
    {Name, Arity} = beam_opcodes:opname(Op),
    {Args, Rest1} = transform_args(Rest, Arity, Literals, []),
    Acc1 = Args ++ [Name | Acc],
    transform_code(Rest1, Literals, Acc1).

transform_args(Rest, 0, _Literals, Acc) ->
    {Acc, Rest};
transform_args(Binary, N, Literals, Acc) ->
    {Arg, Rest} = transform_arg(Binary, Literals),
    transform_args(Rest, N-1, Literals, [Arg|Acc]).

transform_arg(<<A:8, Rest/binary>>, Literals) ->
    Tag = tag(A band 2#111),
    {Value, Rest1} = value(Tag, A, Rest, Literals),
    {{Tag, Value}, Rest1}.

tag(0) -> u;
tag(1) -> i;
tag(2) -> a;
tag(3) -> x;
tag(4) -> y;
tag(5) -> f;
tag(7) -> z.

value(z, B, Bs, Literals) ->
    N = B bsr 4,
    %% N may be 0 (float), 1 (list), 2, 3, but we only need 4 (literal)
    case N of
        4 ->
            {{u, LitIndex}, RestBs} = transform_arg(Bs, Literals),
            {maps:get(LitIndex, Literals), RestBs}
    end;
value(_Tag, B, Bs, _Literals) ->
    value_int(B, Bs).

value_int(B, Bs) when (B band 16#08) =:= 0 ->
    N = B bsr 4,
    {N, Bs};
value_int(B1, <<B2:8, Bs/binary>>) when (B1 band 16#10) =:= 0 ->
    Val0 = B1 band 2#11100000,
    N = (Val0 bsl 3) bor B2,
    {N, Bs}.

%% here should be another clause that handles ints that need more that 11 bits
%% but we don't need it right now
chunk_size(Size) ->
    case Size rem 4 of
        0 -> Size;
        R -> Size+(4-R)
    end.
