-module(utils).

-export([rec_to_prop/3,binary_to_boolean/2,binary_to_boolean/3]).

check_value(Field, Value, RecInfo) ->
	FieldInfo = proplists:get_value(Field,RecInfo),
	case Value of
          undefined  -> false;
	  _          -> {true, {Field, Value}}
	end.	

rec_to_prop(PropName, Rec, DT) ->
    RecName = element(1,Rec),
	Info = maps:get(atom_to_binary(RecName,utf8), DT),
	FL = lists:zip(proplists:keys(Info), tl(tuple_to_list(Rec))), 
	PropList = lists:filtermap(fun({Key,Value}) -> check_value(Key,Value,Info) end, FL),
	io:format("~p~n", [PropList]),
	{[{ PropName, {PropList}}]}.

patient_to_proplist(Rec) ->
    L = [{resourceType, element(1,Rec)}] ++
        lists:zip(complex:record_info(fields, <<"Patient">>), tl(tuple_to_list(Rec))),
    J = jiffy:encode({L}),
    io:format("~p~n", [J]),
    J.
%
%-------------------------------------------------------------------------

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
