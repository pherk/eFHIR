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

get_type(bundle) ->          {<<"DomainResource">>, <<"Bundle">>};
get_type(bundle_link) ->     {<<"BackboneElement">>, <<"Bundle.Link">>};
get_type(bundle_entry) ->    {<<"BackboneElement">>, <<"Bundle.Entry">>};
get_type(bundle_search) ->   {<<"BackboneElement">>, <<"Bundle.Search">>};
get_type(bundle_request) ->  {<<"BackboneElement">>, <<"Bundle.Request">>};
get_type(bundle_response) -> {<<"BackboneElement">>, <<"Bundle.Response">>};
get_type(extension) ->       {<<"Element">>, <<"Extension">>};
get_type(meta) ->            {<<"Element">>, <<"Meta">>};
get_type(narrative) ->       {<<"Element">>, <<"Narrative">>};
get_type(patient) ->         {<<"DomainResource">>, <<"Patient">>};
get_type(patient_contact) -> {<<"BackboneElement">>, <<"Patient.Contact">>};
get_type(patient_communication) -> {<<"BackboneElement">>, <<"Patient.Communication">>};
get_type(patient_link) ->    {<<"BackboneElement">>, <<"Patient.Link">>}.

rec_info(XSDType) -> 
    {Base,FI,Attrs,Restrictions} = xsd_info(XSDType), 
    BFI = resolve_base(Base,FI),
%    io:format("r_i: ~p~n",[BFI]),
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
          V when is_list(V) -> {true, {Field, lists:map(fun to_proplist/1, V)}};
          V                 -> {true, {Field, to_proplist(V)}}
	end.	

to_proplist(Rec) when is_tuple(Rec) ->
    RecName = element(1,Rec),
%    io:format("r2p-0: ~p~n",[Rec]),
    {XSDBaseType, XSDType}  = get_type(RecName),
%    io:format("r2p-1: ~s <= ~s~n",[XSDType,XSDBaseType]),
    Info = rec_info(XSDType),
%    io:format("r2p-2: ~p~n",[Info]),
	FL = lists:zip(Info, tl(tuple_to_list(Rec))), 
%	io:format("r2p-3: ~p~n", [FL]),
	PropList = lists:filtermap(fun({Key,Value}) -> check_value(Key,Value,Info) end, FL),
%	io:format("r2p-4: ~p~n", [PropList]),
    case XSDBaseType of
        <<"DomainResource">> -> {[{<<"resourceType">>, XSDType}] ++ PropList};
        _                    -> {PropList}
    end;
to_proplist(Value) -> Value.

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

