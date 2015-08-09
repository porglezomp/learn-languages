-module(euler).
-export([problem_21/0, problem_23/0, abundant_number/1]).

%% Problem 21: Amicable numbers
divisors(N) ->
    lists:filter(fun(X) -> N rem X == 0 end, lists:seq(1, N div 2)).
is_amicable(N) ->
    A = lists:sum(divisors(N)),
    (A /= N) andalso (N == lists:sum(divisors(A))).
problem_21() ->
    NumbersBelow10K = lists:seq(1, 9999),
    AmicableNumbers = lists:filter(fun is_amicable/1, NumbersBelow10K),
    lists:sum(AmicableNumbers).

%% Problem 23: Non-abundant sums
perfect_number(N) ->
    lists:sum(divisors(N)) == N.
deficient_number(N) ->
    lists:sum(divisors(N)) < N.
abundant_number(N) ->
    lists:sum(divisors(N)) > N.
is_sum(N, Sources) ->
    

problem_23() ->
    AbundantNumbers = lists:filter(fun abundant_number/1, lists:seq(12, 28123)),
    
                     
    
