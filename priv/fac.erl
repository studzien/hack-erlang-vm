-module(fac).
-export([fac/1]).

-record(state, {acc}).

fac(N) ->
    fac(N, #state{acc=1}).

fac(0, #state{acc=Acc}) ->
    Acc;
fac(N, #state{acc=Acc}=State) ->
    fac(N-1, State#state{acc=Acc*N}).
