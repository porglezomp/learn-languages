-module(first).
-export([fib/1, convertToUnitless/1, convert/2, list_length/1]).

fib(0, A, _) ->
    A;
fib(N, A, B) ->
    fib(N - 1, B, A + B).
fib(N) ->
    fib(N, 0, 1).

unitlessMeters(M) ->
    M.

convertToUnitless({M, ft}) ->
    convertToUnitless({M * 12, in});
convertToUnitless({M, in}) ->
    unitlessMeters(M / 39.3701);
convertToUnitless({M, cm}) ->
    unitlessMeters(M / 100);
convertToUnitless({M, km}) ->
    unitlessMeters(M * 1000);
convertToUnitless({M, m}) ->
    unitlessMeters(M).

rawMetersFromUnitless(M) ->
    M.

convertFromUnitless(M, ft) ->
    {rawMetersFromUnitless(M) / 3.28084, ft};
convertFromUnitless(M, in) ->
    {rawMetersFromUnitless(M) / 39.3701, in};
convertFromUnitless(M, cm) ->
    {rawMetersFromUnitless(M) * 100, cm};
convertFromUnitless(M, km) ->
    {rawMetersFromUnitless(M) / 1000, km};
convertFromUnitless(M, m) ->
    {rawMetersFromUnitless(M), m}.

convert(From, ToUnit) ->
    convertFromUnitless(convertToUnitless(From), ToUnit).

list_length([], N) ->
    N;
list_length([_ | XS], N) ->
    list_length(XS, N + 1).

list_length(L) ->
    list_length(L, 0).
