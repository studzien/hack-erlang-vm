-module(fac2).
-export([fac/1]).

fac(1) ->
    1;
fac(N) ->
    fac(N-1)*N.
