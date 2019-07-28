-module(encode).

-compile(export_all).

-include("primitives.hrl").
-include("fhir_400.hrl").

-export([to_proplist/1]).
%%====================================================================
%% Internal functions
%%====================================================================
-spec resolve_base(Base :: binary()) -> list().
resolve_base(Base) -> 
    resolve_base(Base,[]).
resolve_base(<<>>, L) -> L;
resolve_base(<<"BackboneElement">>, L) -> L;
resolve_base(Base, L) -> 
    {NewBase, BI, Attrs, Restrictions} = xsd_info(Base),
    resolve_base(NewBase, BI++L).

erlang_to_fhir(<<"reference_">>) -> <<"reference">>;
erlang_to_fhir(<<"when_">>) -> <<"when">>;
erlang_to_fhir(<<"start_">>) -> <<"start">>;
erlang_to_fhir(<<"end_">>) -> <<"end">>;
erlang_to_fhir(Key) -> Key.

fhir_to_erlang(<<"reference">>) -> <<"reference_">>;
fhir_to_erlang(<<"when">>) -> <<"when_">>;
fhir_to_erlang(<<"start">>) -> <<"start_">>;
fhir_to_erlang(<<"end">>) -> <<"end_">>;
fhir_to_erlang(Key) -> Key.

xsd_info(Key) -> maps:get(Key,?fhir_xsd).

get_type(bundle) -> <<"Bundle">>;
get_type(patient) -> <<"Patient">>.

rec_info(XSDType) -> 
    {Base,FI,Attrs,Restrictions} = xsd_info(XSDType), 
    BFI = resolve_base(Base,FI),
    io:format("r_i: ~p~n",[BFI]),
    keys(BFI).

-spec keys(Props :: list()) -> list(). 
keys(Props) -> 
    keys(Props,[]).
keys([],L) -> lists:reverse(L);
keys([{K,_}|T],Acc) ->
    NewAcc = [K] ++ Acc,
    keys(T,NewAcc).

%%
%% Serialization back to ejson
%%
check_value(Field, Value, RecInfo) ->
	FieldInfo = proplists:get_value(Field,RecInfo),
	case Value of
          undefined  -> false;
          []         -> false;
          V when is_list(V) ->  {true, {Field, []}};
          R when is_tuple(R) -> {true, {Field, to_proplist(R)}};
          _          -> {true, {Field, Value}}
	end.	

to_proplist(Rec) ->
    RecName = element(1,Rec),
    io:format("r2p-0: ~p~n",[Rec]),
    XSDType  = get_type(RecName),
    io:format("r2p-1: ~p~n",[XSDType]),
    Info = rec_info(XSDType),
    io:format("r2p-2: ~p~n",[Info]),
	FL = lists:zip(Info, tl(tuple_to_list(Rec))), 
	io:format("r2p-3: ~p~n", [FL]),
	PropList = lists:filtermap(fun({Key,Value}) -> check_value(Key,Value,Info) end, FL),
	io:format("r2p-4: ~p~n", [PropList]),
	[{<<"resourceType">>, get_type(RecName)}] ++ PropList.

patient_to_proplist(Rec) ->
    L = [{resourceType, element(1,Rec)}] ++
        lists:zip(types:record_info(fields, <<"Patient">>), tl(tuple_to_list(Rec))),
    J = jiffy:encode({L}),
    io:format("~p~n", [J]),
    J.
%%%
%%% EUnit
%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


-endif.

