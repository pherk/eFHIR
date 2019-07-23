-module(decode).
-compile(export_all).
-include("primitives.hrl").
-include("fhir_400.hrl").
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


value(Key, Props, {Base,FI,Attrs,Restriction}=DT) ->
    io:format("get_value: ~s~n",[Key]),
    BFI = resolve_base(Base,FI),
%    io:format("get_value: ~p~n",[BFI]),
    {Type,Occurs} = proplists:get_value(Key, BFI),
    io:format("get_value: ~s: ~p~n",[Key, {Type,Occurs}]),
    Value = proplists:get_value(erlang_to_fhir(Key), Props),
    io:format("get_value: ~p~n",[Value]),
    case {Value,Occurs} of
        {undefined, optional}       -> undefined;
        {undefined, required}       -> error;
        {undefined, list}           -> [];
        {undefined, non_empty_list} -> error;
        {Value,     optional}       -> validate(Type,Value);
        {Value,     required}       -> validate(Type,Value);
        {Value,     list}           -> Fun = get_fun(Type), lists:map(Fun, Value);
        {Value,     non_empty_list} -> Fun = get_fun(Type), lists:map(Fun, Value)
    end.

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

validate({primitive, <<"boolean">>},   Value) -> utils:binary_to_boolean(Value,error);
validate({primitive, <<"canonical">>},   Value) -> Value;
validate({primitive, <<"code">>},   Value) -> Value;
validate({primitive, <<"dateTime">>},   Value) -> Value;
validate({primitive, <<"id">>},   Value) -> Value;
validate({primitive, <<"instant">>},   Value) -> Value;
validate({primitive, <<"period">>},   Value) -> Value;
validate({primitive, <<"string">>},   Value) -> Value;
validate({primitive, <<"uri">>},   Value) -> Value;
validate({code, Type},   Value) -> Value.


get_fun({primitive, <<"binary">>})     -> fun to_binary/1; % not used?!
get_fun({primitive, <<"code">>})       -> fun to_code/1;
get_fun({primitive, <<"date">>})       -> fun to_date/1;
get_fun({primitive, <<"dateTime">>})   -> fun to_dateTime/1;
get_fun({primitive, <<"string">>})     -> fun to_string/1;
get_fun({primitive, <<"time">>})       -> fun to_time/1;
get_fun({complex,   <<"Coding">>})     -> fun complex:to_coding/1;
get_fun({complex,   <<"Extension">>})  -> fun extensions:to_extension/1.

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
    io:format("r_i: ~p~n",[BFI]),
    utils:keys(BFI).


%%%
%%% EUnit
%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


-endif.

