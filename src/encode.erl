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
resolve_base(<<"BackboneElement">>, L) ->
    {NewBase, BI, Attrs, Restrictions} = xsd_info(<<"BackboneElement">>),
    resolve_base(NewBase, Attrs++BI++L);
resolve_base(Base, L) -> 
    {NewBase, BI, Attrs, Restrictions} = xsd_info(Base),
    resolve_base(NewBase, Attrs++BI++L).

erlang_to_fhir(<<"when_">>) -> <<"when">>;
erlang_to_fhir(Key) -> Key.

fhir_to_erlang(<<"when">>) -> <<"when_">>;
fhir_to_erlang(Key) -> Key.

xsd_info(Key) -> maps:get(Key,?fhir_xsd).

%% TODO more elegant via compiled XSD?
get_type(<<"Extension">>) ->   {<<"Element">>, <<"Extension">>};
get_type(<<"Meta">>)      ->   {<<"Element">>, <<"Meta">>};
get_type(<<"Narrative">>) ->   {<<"Element">>, <<"Narrative">>};
get_type(<<"Reference">>) ->   {<<"Element">>, <<"Reference">>};
get_type(ResourceName) ->
    case binary:split(ResourceName, <<".">>,[global]) of    
        [Base]       -> {<<"DomainResource">>,  Base};
        [Base | BBs] -> {<<"BackboneElement">>, ResourceName}
    end.

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
          V when is_list(V) -> {true, {Field, lists:map(fun to_proplist/1, V)}};
          V                 -> {true, {Field, to_proplist(V)}}
	end.	

to_proplist(Rec) when is_tuple(Rec) ->
    RecName = atom_to_binary(element(1,Rec),latin1),
    io:format("r2p-0: ~p~n",[Rec]),
    {XSDBaseType, XSDType}  = get_type(RecName),
    io:format("r2p-1: ~s <= ~s~n",[XSDType,XSDBaseType]),
    Info = [<<"attribs">> | rec_info(XSDType)],
    io:format("r2p-2: ~p~n",[Info]),
	FL = lists:zip(Info, tl(tuple_to_list(Rec))), 
%	io:format("r2p-3: ~p~n", [FL]),
	PropList = lists:filtermap(fun({Key,Value}) -> check_value(Key,Value,Info) end, FL),
%	io:format("r2p-4: ~p~n", [PropList]),
    case XSDBaseType of
        <<"DomainResource">> -> {[{<<"resourceType">>, XSDType}] ++ PropList};
        _                    -> {PropList}
    end;
to_proplist(Value) -> 
    io:format("r2p2-1: ~p~n",[Value]),
    Value.

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

