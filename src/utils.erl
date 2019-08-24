-module(utils).

-export([type_to_fun/1, binary_join/2, 
         keys/1,binary_to_boolean/2,binary_to_boolean/3]).

%%
%%-------------------------------------------------------------------------
%%
type_to_fun(S) ->
    Parts = binary:split(S,<<".">>),
    Mod = string:lowercase(hd(Parts)),
    FPs = binary_join([decap(P) || P <- Parts],<<"_">>),
    {binary_to_atom(Mod,latin1),binary_to_atom(<<"to_", FPs/binary>>,latin1)}.

decap(B) -> <<H:1/binary, T/binary>> =B,
            NH = string:lowercase(H),
            <<NH/binary,T/binary>>.

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
   <<>>;
binary_join([Part], _Sep) ->
   Part;
binary_join(List, Sep) ->
   lists:foldr(fun (A, B) ->
     if
       bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
       true -> A
     end
   end, <<>>, List).

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
