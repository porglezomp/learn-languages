-module(ann).
-export([perceptron/3, connect/2, run/0]).

%% Learning this from:
%% https://erlangcentral.org/wiki/index.php?title=Erlang_and_Neural_Networks
%% You can follow along there too!

dot_prod(A, B) ->
    lists:sum(lists:zipwith(fun(X, Y) -> X*Y end, A, B)).

%% vect_sub(A, B) ->
%%     lists:zipwith(fun(X, Y) -> X-Y end, A, B).

%% vect_norm2(A) ->
%%     dot_prod(A, A).

feed_forward(Sigmoid, Weights, Inputs) ->
    Sigmoid(dot_prod(Weights, Inputs)).

perceptron(Weights, Inputs, Sensitivities) ->
    Sigmoid = fun(X) ->
                      1 / (1 + math:exp(-X))
              end,
    
    Sigmoid_prime = fun(X) ->
                            math:exp(-X) / (1 + math:exp(-2 * X))
                    end,
    receive
        {learn, Backprop} ->
            Learning_rate = 0.5,
            
            % Calculate the correct sensitivies
            New_sensitivities = add_sensitivity(Sensitivities, Backprop),
            Output_value = feed_forward(Sigmoid, Weights, convert_to_values(Inputs)),
            Deriv_value = feed_forward(Sigmoid_prime, Weights, convert_to_values(Inputs)),
            Sensitivity = calculate_sensitivity(Backprop, Inputs, New_sensitivities,
                                                Output_value, Deriv_value),
            io:format("(~w) New Sensitivies: ~w~n", [self(), New_sensitivities]),
            io:format("(~w) Calculated Sensitivies: ~w~n", [self(), Sensitivity]),

            % Adjust all the weights
            Weight_adjustment = lists:map(fun(Input) ->
                                                  Learning_rate * Sensitivity * Input
                                          end,
                                          convert_to_values(Inputs)),
            New_weights = lists:zip_with(fun(W, D) -> W + D end, Weights, Weight_adjustment),
            io:format("(~w) Adjusted Weights: ~w~n", [self(), Weights]),

            % Propogate sensitivies and associated weights back to the previous layer
            lists:zip_with(fun(Weight, Input_PID) ->
                                   Input_PID ! {learn, {self(), Sensitivity * Weight}}
                           end,
                           New_weights,
                           convert_to_keys(Inputs)),
            
            perceptron(New_weights, Inputs, New_sensitivities);
        {stimulate, Input} ->
            New_inputs = replace_input(Inputs, Input),
            Output_value = feed_forward(Sigmoid, Weights, convert_to_values(New_inputs)),
            if Sensitivities =/= [] ->
                    lists:foreach(fun(Output_PID) ->
                                          Output_PID ! {stimulate, {self(), Output_value}}
                                  end,
                                  convert_to_keys(Sensitivities));
               Sensitivities =:= [] ->
                    io:format("~w outputs: ~w~n", [self(), Output_value])
            end,
            perceptron(Weights, New_inputs, Sensitivities);
        {connect_to_output, Receiver_PID} ->
            New_sensitivies = add_sensitivity(Sensitivities, {Receiver_PID, 1}),
            io:format("~w output connected to ~w: ~w~n",
                      [self(), Receiver_PID, New_sensitivies]),
            perceptron(Weights, Inputs, New_sensitivies);
        {connect_to_input, Sender_PID} ->
            Combined_input = [{Sender_PID, 0.5} | Inputs],
            io:format("~w inputs connected to ~w: ~w~n",
                      [self(), Sender_PID, Combined_input]),
            perceptron([0.5 | Weights], Combined_input, Sensitivities);
        {pass, Input_value} ->
           lists:foreach(fun(Output_PID) ->
                                  io:format("Stimulating ~w with ~w~n",
                                            [Output_PID, Input_value]),
                                  Output_PID ! {stimulate, {self(), Input_value}}
                          end,
                         convert_to_keys(Sensitivities));
        {status} ->
            io:format("~w~n", [Sensitivities])
    end.

connect(Sender_PID, Receiver_PID) ->
    Sender_PID ! {connect_to_output, Receiver_PID},
    Receiver_PID ! {connect_to_input, Sender_PID}.

replace_input(Inputs, Input) ->
    {Input_PID, _} = Input,
    lists:keyreplace(Input_PID, 1, Inputs, Input).

convert_to_values(Tuple_list) ->
    lists:map(fun({_, Val}) -> Val end, Tuple_list).

convert_to_keys(Tuple_list) ->
    lists:map(fun({Key, _}) -> Key end, Tuple_list).

add_sensitivity(Sensitivies, Backprop) when Sensitivies =/= [] ->
    replace_input(Sensitivies, Backprop);
add_sensitivity(Sensitivies, _) when Sensitivies =:= [] ->
    [].

calculate_sensitivity(_, Inputs, Sensitivities, _, _)
  when Sensitivities =/= [], Inputs =:= [] ->
    null;
calculate_sensitivity(Backprop, Inputs, Sensitivities, Output_value, Deriv_value)
  when Sensitivities =:= [], Inputs =/= [] ->
    {_, Training_value} = Backprop,
    (Training_value - Output_value) * Deriv_value;
calculate_sensitivity(_, Inputs, Sensitivities, _, Deriv_value)
  when Sensitivities =/= [], Inputs =/= [] ->
    Deriv_value * lists:sum(convert_to_values(Sensitivities)).

run() ->
    X1_pid = spawn(ann, perceptron, [[],[],[]]),
    X2_pid = spawn(ann, perceptron, [[],[],[]]),
    H1_pid = spawn(ann, perceptron, [[],[],[]]),
    H2_pid = spawn(ann, perceptron, [[],[],[]]),

    O_pid = spawn(ann, perceptron, [[],[],[]]),

    ann:connect(X1_pid, H1_pid),
    ann:connect(X1_pid, H2_pid),
    
    ann:connect(X2_pid, H1_pid),
    ann:connect(X2_pid, H2_pid),
    
    ann:connect(X1_pid, O_pid),
    ann:connect(H2_pid, O_pid),
    
    X1_pid ! {status},
    X2_pid ! {status},
    H1_pid ! {status},
    H2_pid ! {status},
    O_pid ! {status},

    X1_pid ! {pass, 1.8},
    X2_pid ! {pass, 1.3}.
