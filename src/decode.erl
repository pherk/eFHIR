-module(decode).
-compile(export_all).
-include("primitives.hrl").
-include("fhir_400.hrl").
-include("codes.hrl").
%%
%% API exports
%%
-export_type([decimal/0,uri/0,url/0,canonical/0,base64Binary/0]).
-export_type([instant/0,date/0,dateTime/0,yearMonth/0,year/0,dow/0,time/0]).
-export_type([code/0,oid/0,id/0,markdown/0,positiveInt/0,unsignedInt/0,uuid/0,ucum/0]).

%%
%%====================================================================
%% Primitive Data Types
%%====================================================================
%%
to_uri({Props}) -> to_uri(Props);
to_uri(Props) ->
    proplists:get_value(<<"uri">>, Props).

to_boolean({Props}) -> to_boolean(Props);
to_boolean(Props) ->
    proplists:get_value(<<"uri">>, Props).

to_binary({Bin}) -> Bin;
to_binary(Bin) -> Bin.

to_code({Bin}) -> Bin;
to_code(Bin) -> Bin.

to_date({Bin}) -> Bin;
to_date(Bin) -> Bin.

to_dateTime({Bin}) -> Bin;
to_dateTime(Bin) -> Bin.

to_string({Bin}) -> Bin;
to_string(Bin) -> Bin.

to_time({Bin}) -> Bin;
to_time(Bin) -> Bin.


value(Key, Props, {Base,FI,Attrs,Restriction}=DT) when is_list(Props)->
    % io:format("value0: ~s~n",[Key]),
    BFI = resolve_base(Base,FI),
    % io:format("value1: ~p~n",[BFI]),
    PropInfo = proplists:get_value(Key, BFI),
    analyse_propinfo(PropInfo, Key, Props);
value(Key, Props, {Base,FI,Attrs,Restriction}=DT) ->
    throw(<<"proplists error, input malformat">>).

analyse_propinfo(Choice, Key, Props) when is_list(Choice) ->
    io:format("api1: ~p~n",[Choice]),
    ChoiceKeys = proplists:get_keys(Choice),
    CVs = check_choices(ChoiceKeys, Props),
    io:format("api4: ~p~n",[CVs]),
    case CVs of
        []         -> undefined;
        [{K, V}|_] ->
            {Type,optional} = proplists:get_value(K, Choice),
            match_propinfo(Type, optional, V);
        [_,_|_]    ->
            throw(<<"error in fhir:decode(): more than one of a choice type in resource">>)
    end;
analyse_propinfo({Type, Occurs}, Key, Props) ->
    Value = proplists:get_value(erlang_to_fhir(Key), Props),
    match_propinfo(Type, Occurs, Value).

check_choices(Keys, Props) ->
    lists:filtermap(fun(K) -> case proplists:get_value(K, Props) of
                                  undefined -> false;
                                  V         -> {true, {K, V}}
                              end end, Keys).

match_propinfo(Type, Occurs, Value) ->
    case {Value,Occurs} of
                {undefined, optional}       -> undefined;
                {undefined, required}       -> error;
                {undefined, list}           -> [];
                {undefined, non_empty_list} -> error;
                {Value,     optional}       -> validate(Type,Value);
                {Value,     required}       -> 
            io:format("mpi_required: ~p : ~p~n",[Type, Value]), validate(Type,Value);
                {Value,     list}           -> Fun = get_fun(Type), lists:map(Fun, Value);
                {Value,     non_empty_list} -> Fun = get_fun(Type), lists:map(Fun, Value)
    end.

resourceType({EJson}) -> resourceType(EJson);
resourceType(EJson) ->
     proplists:get_value(<<"resourceType">>,EJson).

attrs(Props, {Base,FI,Attrs,Restriction}=DT) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================
-spec resolve_base(Base :: binary()) -> list().
resolve_base(Base) -> 
    resolve_base(Base,[]).
resolve_base(<<>>, L) -> L;
resolve_base(<<"BackboneElement">>, L) ->
    {NewBase, BI, Attrs, Restrictions} = xsd_info(<<"BackboneElement">>),
    resolve_base(NewBase, BI++L);
resolve_base(Base, L) -> 
    {NewBase, BI, Attrs, Restrictions} = xsd_info(Base),
    resolve_base(NewBase, BI++L).

validate({primitive, <<"base64Binary">>},   Value) -> Value;
validate({primitive, <<"boolean">>},   Value) -> Value; % utils:binary_to_boolean(Value,error);
validate({primitive, <<"canonical">>},   Value) -> Value;
validate({primitive, <<"code">>},   Value) -> Value;
validate({primitive, <<"date">>},   Value) -> Value;
validate({primitive, <<"dateTime">>},   Value) -> Value;
validate({primitive, <<"decimal">>},   Value) -> Value;
validate({primitive, <<"id">>},   Value) -> Value;
validate({primitive, <<"instant">>},   Value) -> Value;
validate({primitive, <<"integer">>},   Value) -> Value;
validate({primitive, <<"makdown">>},   Value) -> Value;
validate({primitive, <<"oid">>},   Value) -> Value;
validate({primitive, <<"positiveInt">>},   Value) -> Value;
validate({primitive, <<"string">>},   Value) -> Value;
validate({primitive, <<"time">>},   Value) -> Value;
validate({primitive, <<"uuid">>},   Value) -> Value;
validate({primitive, <<"unsignedInt">>},   Value) -> Value;
validate({primitive, <<"uri">>},   Value) -> Value;
validate({primitive, <<"url">>},   Value) -> Value;
validate({primitive, <<"xhtml">>},   Value) -> Value;
validate({code, Type},   Value) -> 
    List = maps:get(Type,?fhir_codes),
%    io:format("code: ~s in ~p~n",[Value,List]),
    Value;
validate({complex, <<"Address">>},        Value) -> complex:to_address(Value);
validate({complex, <<"Age">>},            Value) -> complex:to_age(Value);
validate({complex, <<"Annotation">>},     Value) -> complex:to_annotation(Value);
validate({complex, <<"Attachment">>},     Value) -> complex:to_attachment(Value);
validate({complex, <<"Coding">>},         Value) -> complex:to_coding(Value);
validate({complex, <<"CodeableConcept">>}, Value) -> complex:to_codeableConcept(Value);
validate({complex, <<"ContactPoint">>},   Value) -> complex:to_contactPoint(Value);
validate({complex, <<"Count">>},          Value) -> complex:to_count(Value);
validate({complex, <<"Distance">>},       Value) -> complex:to_distance(Value);
validate({complex, <<"Duration">>},       Value) -> complex:to_duration(Value);
validate({complex, <<"HumanName">>},      Value) -> complex:to_humanName(Value);
validate({complex, <<"Identifier">>},     Value) -> complex:to_identifier(Value);
validate({complex, <<"Money">>},          Value) -> complex:to_money(Value);
validate({complex, <<"MoneyQuantity">>},  Value) -> complex:to_moneyQuantity(Value);
validate({complex, <<"Narrative">>},      Value) -> complex:to_narrative(Value);
validate({complex, <<"Period">>},         Value) -> complex:to_period(Value);
validate({complex, <<"Range">>},          Value) -> complex:to_range(Value);
validate({complex, <<"Ratio">>},          Value) -> complex:to_ratio(Value);
validate({complex, <<"Quantity">>},       Value) -> complex:to_quantity(Value);
validate({complex, <<"ResourceContainer">>}, Value) -> resource:to_resource(Value); 
validate({complex, <<"Signature">>},      Value) -> complex:to_signature(Value);
validate({complex, <<"SimpleQuantity">>}, Value) -> complex:to_simpleQuantity(Value);
validate({complex, <<"Timing">>}, Value)         -> complex:to_timing(Value);
validate({metadata, <<"RelatedArtifakt">>}, Value) -> metadata:to_relatedArtifakt(Value);
validate({special, <<"xhtml">>},          Value) -> Value;
validate({special, <<"Extension">>},      Value) -> extensions:to_extension_list(Value);
validate({special, <<"Meta">>},           Value) -> special:to_meta(Value);
validate({special, <<"Narrative">>},      Value) -> special:to_narrative(Value);
validate({special, <<"Reference">>},      Value) -> special:to_reference(Value);
validate({bbelement, Resource},   Value) ->
    R = string:lowercase(Resource),
    Mod = hd(binary:split(R, <<".">>)),
    Fun = list_to_binary([<<"to_">>,binary:replace(R,<<".">>,<<"_">>)]),
%    io:format("validate: apply: ~s:~s(~p)~n",[Mod,Fun,Value]),
    apply(binary_to_atom(Mod,utf8),binary_to_atom(Fun,utf8),[Value]).


get_fun({primitive, <<"binary">>})     -> fun to_binary/1; % not used?!
get_fun({primitive, <<"code">>})       -> fun to_code/1;
get_fun({primitive, <<"date">>})       -> fun to_date/1;
get_fun({primitive, <<"dateTime">>})   -> fun to_dateTime/1;
get_fun({primitive, <<"string">>})     -> fun to_string/1;
get_fun({primitive, <<"time">>})       -> fun to_time/1;
get_fun({complex, <<"Address">>}) -> fun complex:to_address/1;
get_fun({complex, <<"Age">>}) -> fun complex:to_age/1;
get_fun({complex, <<"Attachment">>}) -> fun complex:to_attachment/1;
get_fun({complex, <<"Coding">>})     -> fun complex:to_coding/1;
get_fun({complex, <<"ContactPoint">>}) -> fun complex:to_contactPoint/1;
get_fun({complex, <<"Count">>}) -> fun complex:to_count/1;
get_fun({complex, <<"Distance">>}) -> fun complex:to_distance/1;
get_fun({complex, <<"Duration">>}) -> fun complex:to_duration/1;
get_fun({complex, <<"HumanName">>})  -> fun complex:to_humanName/1;
get_fun({complex, <<"Identifier">>}) -> fun complex:to_identifier/1;
get_fun({complex, <<"Money">>}) -> fun complex:to_money/1;
get_fun({complex, <<"Period">>}) -> fun complex:to_period/1;
get_fun({complex, <<"Quantity">>}) -> fun complex:to_quantity/1;
get_fun({complex, <<"Range">>}) -> fun complex:to_range/1;
get_fun({complex, <<"Ratio">>}) -> fun complex:to_ration/1;
get_fun({complex, <<"SampledData">>}) -> fun complex:to_sampled_data/1;
get_fun({complex, <<"Signature">>}) -> fun complex:to_signature/1;
get_fun({complex, <<"Timing">>}) -> fun complex:to_timing/1;
get_fun({complex, <<"ContactDetail">>}) -> fun complex:to_contact_details/1;
get_fun({complex, <<"Contributor">>}) -> fun complex:to_contributor/1;
get_fun({complex, <<"DataRequirement">>}) -> fun complex:to_data_requirement/1;
get_fun({complex, <<"Expression">>}) -> fun complex:to_expression/1;
get_fun({complex, <<"ParameterDefinition">>}) -> fun complex:to_parameter_definition/1;
get_fun({complex, <<"RelatedArtifact">>}) -> fun complex:to_related_artifiact/1;
get_fun({complex, <<"TriggerDefinition">>}) -> fun complex:to_trigger_definition/1;
get_fun({complex, <<"UsageContext">>}) -> fun complex:to_usage_context/1;
get_fun({complex, <<"Dosage">>}) -> fun complex:to_dosage/1;
get_fun({special,   <<"Extension">>})  -> fun extensions:to_extension_list/1;
get_fun({bbelement, <<"Bundle.Entry">>}) -> fun bundle:to_bundle_entry/1;
get_fun({bbelement, <<"Bundle.Link">>})  -> fun bundle:to_bundle_link/1;
get_fun({bbelement, Resource}) ->
     R = string:lowercase(Resource),
     Mod = hd(binary:split(R, <<".">>)),
     Fun = list_to_binary([<<"to_">>,binary:replace(R,<<".">>,<<"_">>)]),
%    io:format("validate: apply: ~s:~s(~p)~n",[Mod,Fun]),
     fun(V) -> apply(binary_to_atom(Mod,utf8),binary_to_atom(Fun,utf8),[V]) end.

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

get_type(patient) -> <<"Patient">>.

record_info(XSDType) -> 
    {Base,FI,Attrs,Restrictions} = xsd_info(XSDType), 
    BFI = resolve_base(Base,FI),
%    io:format("r_i: ~p~n",[BFI]),
    utils:keys(BFI).


%%%
%%% EUnit
%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


-endif.

