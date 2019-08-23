-module(extensions).
-include("fhir.hrl").
-include("primitives.hrl").
-include("fhir_400.hrl").

-export([to_extension/1, to_extension_list/1]).
-export_type(['Extension'/0]).


%% not used
-define(ext_info, {<<"Element">>, [
                   {<<"value">>, {extensionValue, optional}} ],
                  [ {<<"url">>, {primitive, <<"uri">>}, required}} ],
                  [] }).

-record('Extension', {
      url :: binary()
    , value :: extensionValue()
    }).
-opaque 'Extension'() :: #'Extension'{}.


-record(valueBase64Binary, {value :: base64Binary()}).
-record(valueBoolean,  {value :: boolean()}).
-record(valueCode,     {value :: binary()}).
-record(valueDate,     {value :: date()}). 
-record(valueDateTime, {value :: dateTime()}). 
-record(valueDecimal,  {value :: decimal()}).
-record(valueId,       {value :: binary()}).
-record(valueInstant,  {value :: instant()}).
-record(valueInteger,  {value :: integer()}).
-record(valueMarkdown, {value :: markdown()}).
-record(valueOid,      {value :: oid()}).
-record(valuePositiveInt, {value :: positiveInt()}).
-record(valueString,   {value :: binary()}).
-record(valueTime,     {value :: time()}).
-record(valueUnsignedInt, {value :: unsignedInt()}).
-record(valueUri,      {value :: uri()}).
-record(valueUrl,      {value :: url()}).
-record(valueUuid,     {value :: uuid()}). 
-record(valueAddress,         {value :: complex:'Address'()}).
-record(valueAnnotation,      {value :: complex:'Annotation'()}).
-record(valueCodeableConcept, {value :: complex:'CodeableConcept'()}).
-record(valueCoding,          {value :: complex:'Coding'()}).
-record(valueReference,       {value :: special:'Reference'()}).

-type extensionValue() :: 
      'Extension'()
    | #valueBase64Binary{}
    | #valueBoolean{}
    | #valueCode{}
    | #valueDate{}
    | #valueDateTime{}
    | #valueDecimal{}
    | #valueId{}
    | #valueInstant{}
    | #valueInteger{}
    | #valueMarkdown{}
    | #valueOid{}
    | #valuePositiveInt{}
    | #valueString{}
    | #valueTime{}
    | #valueUnsignedInt{}
    | #valueUri{}
    | #valueUrl{}
    | #valueUuid{}
    | #valueAnnotation{}
    | #valueCodeableConcept{}
    | #valueCoding{}
    | #valueReference{}
    | #valueAddress{}.
%% ValueAge Age
%% ValueAttachment Attachment
%% ValueContactPoint ContactPoint
%% ValueCount Count
%% ValueDistance Distance
%% ValueDuration Duration
%% ValueHumanName HumanName
%% ValueIdentifier Identifier
%% ValueMoney Money
%% ValuePeriod Period
%% ValueQuantity Quantity
%% ValueRange Range
%% ValueRatio Ratio
%% ValueSampledData SampledData
%% ValueSignature Signature
%% ValueTiming Timing
%% ValueContactDetail ContactDetail
%% ValueContributor Contributor
%% ValueDataRequirement DataRequirement
%% ValueExpression Expression
%% ValueParameterDefinition ParameterDefinition
%% ValueRelatedArtifact RelatedArtifact
%% ValueTriggerDefinition TriggerDefinition
%% ValueUsageContext UsageContext
%% ValueDosage Dosage

%% ValueCanonical canonical

%%
%% API exports
-export([to_extension/1, to_extension_list/1]).

%%====================================================================
%% API functions
%%
%%[{[{<<"url">>,<<"ombCategory">>},
%%   {<<"valueCoding">>,
%%      {[{<<"system">>, <<"urn:oid:2.16.840.1.113883.6.238">>},
%%        {<<"code">>,<<"2054-5">>},
%%        {<<"display">>, <<"Black or African American">>}]}
%%      }]},
%% {[{<<"url">>,<<"text">>},
%%   {<<"valueString">>,<<"Black or African American">>}]}
%%]

%%====================================================================
to_extension_list({List}) -> to_extension(List);
to_extension_list(List) ->
    [ to_extension(P) || P <- List].

to_extension({Props}) ->    to_extension(Props);
to_extension(Props) ->
    % DT = ?ext_info,
    Keys = proplists:get_keys(Props),
    [ValueType] = lists:delete(<<"url">>,Keys),
    Value = proplists:get_value(ValueType,Props),
    case Value of
      {Val} ->   % Object
          #'Extension'{ url    = proplists:get_value(<<"url">>, Props),
                        value  = to_extensionValue(ValueType,Value)};
      Val ->     % simple value
          #'Extension'{ url    = proplists:get_value(<<"url">>, Props),
                        value  = to_extensionValue(ValueType,Value)}
    end.

%% TODO replace by makeTuple
%%====================================================================
%% Internal functions
%%====================================================================

to_extensionValue(<<"extension">>, Value) ->
    to_extension_list(Value);
to_extensionValue(<<"valueBase64Binary">>, Value) ->
    #valueBase64Binary{ value = Value };
to_extensionValue(<<"valueBoolean">>, Value ) ->
    #valueBoolean{ value = Value };
to_extensionValue(<<"valueCode">>, Value ) ->
    #valueCode{ value = Value };
to_extensionValue(<<"valueDate">>, Value ) ->
    #valueDate{ value = Value };
to_extensionValue(<<"valueDateTime">>, Value ) ->
    #valueDateTime{ value = Value };
to_extensionValue(<<"valueDecimal">>, Value ) ->
    #valueDecimal{ value = Value };
to_extensionValue(<<"valueId">>, Value) ->
    #valueId{ value = Value };
to_extensionValue(<<"valueInstant">>, Value) ->
    #valueInstant{ value = Value };
to_extensionValue(<<"valueInteger">>, Value) ->
    #valueInteger{ value = Value };
to_extensionValue(<<"valueMarkdown">>, Value) ->
    #valueMarkdown{ value = Value };
to_extensionValue(<<"valueOid">>, Value) ->
    #valueOid{ value = Value };
to_extensionValue(<<"valuePositiveInt">>, Value) ->
    #valuePositiveInt{ value = Value };
to_extensionValue(<<"valueString">>, Value) ->
    #valueString{ value = Value };
to_extensionValue(<<"valueTime">>, Value) ->
    #valueTime{ value = Value };
to_extensionValue(<<"valueUnsignedInt">>, Value) ->
    #valueUnsignedInt{ value = Value };
to_extensionValue(<<"valueUri">>, Value) ->
    #valueUri{ value = Value };
to_extensionValue(<<"valueUrl">>, Value) ->
    #valueUrl{ value = Value };
to_extensionValue(<<"valueUuid">>, Value) ->
    #valueUuid{ value = Value };
to_extensionValue(<<"valueAddress">>, Props) ->
    #valueAddress{ value = complex:to_address(Props) };
to_extensionValue(<<"valueAnnotation">>, Props) ->
    #valueAnnotation{ value = complex:to_annotation(Props) };
to_extensionValue(<<"valueCodeableConcept">>, Props) ->
    #valueCodeableConcept{ value = complex:to_codeableConcept(Props) };
to_extensionValue(<<"valueCoding">>, Props) ->
    #valueCoding{ value = complex:to_coding(Props) };
to_extensionValue(<<"valueReference">>, Props) ->
    #valueReference{ value = special:to_reference(Props) }.
    

%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrte(A, B), ?assertEqual(B, extensions:to_extension(A))).
-define(asrtelist(A, B), ?assertEqual(B, extensions:to_extension_list(A))).


extensions_type_test() ->
    ?asrte({[{<<"url">>,<<"text">>},
             {<<"valueString">>,<<"Black or African American">>}]},
           {'Extension',<<"text">>,
                        {valueString,<<"Black or African American">>}}
          ),
    ?asrte({[{<<"url">>,<<"ombCategory">>},
               {<<"valueCoding">>,
                 {[{<<"system">>, <<"urn:oid:2.16.840.1.113883.6.238">>},
                   {<<"code">>,<<"2054-5">>},
                   {<<"display">>, <<"Black or African American">>}]}
                 }]},
           {'Extension',<<"ombCategory">>,
                        {valueCoding, 
                            {'Coding',<<"urn:oid:2.16.840.1.113883.6.238">>, undefined,<<"2054-5">>, <<"Black or African American">>,undefined}}}
          ).


extensions_list_test() ->
    ?asrtelist([{[{<<"url">>,<<"ombCategory">>},
                {<<"valueCoding">>,
                   {[{<<"system">>, <<"urn:oid:2.16.840.1.113883.6.238">>},
                     {<<"code">>,<<"2054-5">>},
                     {<<"display">>, <<"Black or African American">>}]}
                }]},
                {[{<<"url">>,<<"text">>},
                  {<<"valueString">>,<<"Black or African American">>}]}
               ], 
               [{'Extension',<<"ombCategory">>,
                      {valueCoding,
                          {'Coding',<<"urn:oid:2.16.840.1.113883.6.238">>, undefined,<<"2054-5">>, <<"Black or African American">>,undefined}}},
                {'Extension',<<"text">>,
                      {valueString,<<"Black or African American">>}}
               ]).

extension_recursive_test() ->
    ?asrtelist({[{<<"url">>, <<"http://hl7.org/fhir/us/core/StructureDefinition/us-core-race">>},
                   {<<"extension">>,
                     [{[{<<"url">>, <<"ombCategory">>},
                        {<<"valueCoding">>,
                          {[{<<"system">>, <<"urn:oid:2.16.840.1.113883.6.238">>},
                            {<<"code">>, <<"2054-5">>},
                            {<<"display">>, <<"Black or African American">>}]}}]},
                      {[{<<"url">>, <<"text">>},
                        {<<"valueString">>, <<"Black or African American">>}]}]}]},
              {'Extension', <<"http://hl7.org/fhir/us/core/StructureDefinition/us-core-race">>,
                     [{'Extension',<<"ombCategory">>,
                          {valueCoding,
                              {'Coding',
                                  <<"urn:oid:2.16.840.1.113883.6.238">>,
                                  undefined,<<"2054-5">>,
                                  <<"Black or African American">>,undefined}}},
                      {'Extension',<<"text">>,
                          {valueString,<<"Black or African American">>}}]}).


-endif.

