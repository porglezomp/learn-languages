-module(ann).
-export([perceptron/3, sigmoid/1, dot_prod/2, feed_forward/2,
        replace_input/2, convert_to_list/1, connect/2]).

%% Learning this from:
%% https://erlangcentral.org/wiki/index.php?title=Erlang_and_Neural_Networks
%% You can follow along there too!

sigmoid(X) ->
    1 / (1 + math:exp(-X)).

dot_prod(A, B) ->
    lists:sum(lists:zipwith(fun(X, Y) -> X*Y end, A, B)).

feed_forward(Weights, Inputs) ->
    sigmoid(dot_prod(Weights, Inputs)).

perceptron(Weights, Inputs, Output_PIDs) ->
    receive
        {stimulate, Input} ->
            % add Input to Inputs to get New_Inputs...
            New_inputs = replace_input(Inputs, Input),
            
            % calculate output of perceptron...
            Output = feed_forward(Weights, convert_to_list(New_inputs)),
            
            % stimulate the perceptron my output is connected to
            if Output_PIDs =/= [] ->
                    lists:foreach(fun(Output_PID) ->
                                          Output_PID ! {stimulate, {self(), Output}}
                                  end,
                                  Output_PIDs);
               Output_PIDs =:= [] ->
                    io:format("~w outputs: ~w~n", [self(), Output])
            end,
            perceptron(Weights, New_inputs, Output_PIDs);
        {connect_to_output, Receiver_PID} ->
            Combined_output = [Receiver_PID | Output_PIDs],
            io:format("~w output connected to ~w: ~w~n",
                      [self(), Receiver_PID, Combined_output]),
            perceptron(Weights, Inputs, Combined_output);
        {connect_to_input, Sender_PID} ->
            Combined_input = [{Sender_PID, 0.5} | Inputs],
            io:format("~w inputs connected to ~w: ~w~n",
                      [self(), Sender_PID, Combined_input]),
            perceptron([0.5 | Weights], Combined_input, Output_PIDs);
        {pass, Input_value} ->
            lists:foreach(fun(Output_PID) ->
                                  io:format("Stimulating ~w with ~w~n",
                                            [Output_PID, Input_value]),
                                  Output_PID ! {stimulate, {self(), Input_value}}
                          end,
                          Output_PIDs)
    end.

connect(Sender_PID, Receiver_PID) ->
    Sender_PID ! {connect_to_output, Receiver_PID},
    Receiver_PID ! {connect_to_input, Sender_PID}.

replace_input(Inputs, Input) ->
    {Input_PID, _} = Input,
    lists:keyreplace(Input_PID, 1, Inputs, Input).

convert_to_list(Inputs) ->
    lists:map(fun({_, Val}) -> Val end, Inputs).

