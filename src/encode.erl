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
get_type(<<"CodeableConcept">>) ->   {<<"Element">>, <<"CodeableConcept">>};
get_type(<<"Coding">>)    ->   {<<"Element">>, <<"Coding">>};
get_type(<<"ContactPoint">>)   ->   {<<"Element">>, <<"ContactPoint">>};
get_type(<<"HumanName">>)  ->  {<<"Element">>, <<"HumanName">>};
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
    % io:format("r_i: ~p~n",[BFI]),
    BFI.

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
check_value(Field, Value, FieldInfo) ->
	case Value of
        undefined  -> false;
        []         -> false;
        V when is_list(V) ->       % choice elements only 0..1
            {true, {Field, lists:map(fun to_proplist/1, V)}};
        V                 -> 
            FI = proplists:get_value(Field,FieldInfo),
            case FI of
                Choice when is_list(Choice) ->
                    F = tagged_field(Field, Value),
                    io:format("cv: ~p: ~p: ~p~n", [Field, Value, FI]),
                    io:format("cv: type: ~p",[F]), 
                    {true, F};
                {_Type, _Occurs} ->
                    {true, {Field, to_proplist(V)}}
            end
	end.	
%%
%% primitive datatypes are tagged
%% complex are tagged by theirself
%%
tagged_field(F, {<<"Base64Binary">>, V}) -> {<<F/binary, "Base64Binary">>, V};
tagged_field(F, {<<"Boolean">>, V}) -> {<<F/binary, "Boolean">>, V};
tagged_field(F, {<<"Canonical">>, V}) -> {<<F/binary, "Canonical">>, V};
tagged_field(F, {<<"Code">>, V}) -> {<<F/binary, "Code">>, V};
tagged_field(F, {<<"Date">>, V}) -> {<<F/binary, "Date">>, V};
tagged_field(F, {<<"DateTime">>, V}) -> {<<F/binary, "DateTime">>, V};
tagged_field(F, {<<"Decimal">>, V}) -> {<<F/binary, "Decimal">>, V};
tagged_field(F, {<<"Id">>, V}) -> {<<F/binary, "Id">>, V};
tagged_field(F, {<<"Instant">>, V}) -> {<<F/binary, "Instant">>, V};
tagged_field(F, {<<"Integer">>, V}) -> {<<F/binary, "Integer">>, V};
tagged_field(F, {<<"Markdown">>, V}) -> {<<F/binary, "Markdown">>, V};
tagged_field(F, {<<"Oid">>, V}) -> {<<F/binary, "Oid">>, V};
tagged_field(F, {<<"PositiveInt">>, V}) -> {<<F/binary, "PositiveInt">>, V};
tagged_field(F, {<<"String">>, V}) -> {<<F/binary, "String">>, V};
tagged_field(F, {<<"Time">>, V}) -> {<<F/binary, "Time">>, V};
tagged_field(F, {<<"UnsignedInt">>, V}) -> {<<F/binary, "UnsignedInt">>, V};
tagged_field(F, {<<"Uri">>, V}) -> {<<F/binary, "Uri">>, V};
tagged_field(F, {<<"Url">>, V}) -> {<<F/binary, "Url">>, V};
tagged_field(F, {<<"Uuid">>, V}) -> {<<F/binary, "Uuid">>, V};
tagged_field(F, {_, V}) ->
    T = atom_to_binary(element(1,V),latin1),
    {<<F/binary, T/binary>>, to_proplist(V)}.

to_proplist(Rec) when is_tuple(Rec) ->
    RecName = atom_to_binary(element(1,Rec),latin1),
     io:format("r2p-0: ~p~n",[Rec]),
    {XSDBaseType, XSDType}  = get_type(RecName),
     io:format("r2p-1: ~s <= ~s~n",[XSDType,XSDBaseType]),
    RI = rec_info(XSDType),
    FieldNames = [<<"attribs">> | keys(RI)],
     io:format("r2p-2: ~p~n",[FieldNames]),
	FL = lists:zip(FieldNames, tl(tuple_to_list(Rec))), 
%	io:format("r2p-3: ~p~n", [FL]),
	PropList = lists:filtermap(fun({Key,Value}) -> check_value(Key,Value,RI) end, FL),
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

