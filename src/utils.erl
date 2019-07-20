-module(utils).

-export([rec_to_proplist/1,binary_to_boolean/2,binary_to_boolean/3]).

check_value(Field, Value, RecInfo) ->
	FieldInfo = proplists:get_value(Field,RecInfo),
	case Value of
          undefined  -> false;
	  _          -> {true, {Field, Value}}
	end.	

rec_to_proplist(Rec) ->
    RecName = element(1,Rec),
    io:format("r2p-0: ~p~n",[Rec]),
    XSDType  = complex:get_type(RecName),
    io:format("r2p-1: ~p~n",[XSDType]),
    Info = complex:record_info(XSDType),
    io:format("r2p-2: ~p~n",[Info]),
	FL = lists:zip(Info, tl(tuple_to_list(Rec))), 
	io:format("r2p-3: ~p~n", [FL]),
	PropList = lists:filtermap(fun({Key,Value}) -> check_value(Key,Value,Info) end, FL),
	io:format("r2p-4: ~p~n", [PropList]),
	{[{ [{<<"resource_type">>, RecName}] ++ PropList}]}.

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
