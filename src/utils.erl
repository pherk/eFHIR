-module(utils).

-export([keys/1,binary_to_boolean/2,binary_to_boolean/3]).

%%
%%-------------------------------------------------------------------------
%%

-spec keys(Props :: list()) -> list(). 
keys(Props) -> 
    keys(Props,[]).
keys([],L) -> lists:reverse(L);
keys([{K,_}|T],Acc) ->
    NewAcc = [K] ++ Acc,
    keys(T,NewAcc).

-spec boolean_to_binary(Bool :: boolean()) -> binary().
boolean_to_binary(Bool) ->
    case Bool of
        true -> <<"1">>;
        false -> <<"0">>
    end.

-spec binary_to_boolean(Binary :: binary(), DefaultResult :: any()) -> any().
binary_to_boolean(Binary, DefaultResult) ->
    binary_to_boolean(Binary, DefaultResult, error).

-spec binary_to_boolean(Binary :: binary(), DefaultResult :: any(), InvalidResult :: any()) -> any().
binary_to_boolean(Binary, DefaultResult, InvalidResult) ->
    case Binary of
        <<"1">> -> true;
        <<"0">> -> false;
        <<"true">> -> true;
        <<"false">> -> false;
        undefined -> DefaultResult;
        _ -> InvalidResult
    end.
