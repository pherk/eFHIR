-module(extensions).
-include("primitives.hrl").

-export_type([extension/0]).
-exports([to_extension/1]).

-define(ext_info, {<<"Element">>, [
                   {<<"value">>, {extensionValue, optional}} ],
                  [ {<<"url">>, {primitive, <<"uri">>}, required}} ],
                  [] }).

-record(extension, {
      url :: binary()
    , value :: extensionValue()
    }).
-opaque extension() :: #extension{}.


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
-record(valueAnnotation,      {value :: complex:annotation()}).
-record(valueCodeableConcept, {value :: complex:codeableConcept()}).
-record(valueCoding,          {value :: complex:coding()}).
-record(valueReference,       {value :: complex:reference()}).

-type extensionValue() :: 
      #valueBase64Binary{}
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
    | #valueReference{}.


%% ValueAddress Address
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
-export([to_extension/1]).

%%====================================================================
%% API functions
%%====================================================================
to_extension({Props}) ->    to_extension(Props);
to_extension(Props) ->
    % DT = ?ext_info,
    [ValueType] = lists:delete(<<"url">>,proplists:get_keys(Props)),
    {Value} = proplists:get_value(ValueType,Props),
    io:format("extensions: ~s: ~p~n",[ValueType, Value]),
    #extension{
        url    = proplists:get_value(<<"url">>, Props)
      , value  = to_extensionValue(ValueType,Value)
      }.

%% TODO replace by makeTuple
%%====================================================================
%% Internal functions
%%====================================================================

to_extensionValue(<<"valueBase64Binary">>, Props) ->
    #valueBase64Binary{ value = proplists:get_value(<<"valueBase64Binary">>, Props) };
to_extensionValue(<<"valueBoolean">>, Props) ->
    #valueBoolean{ value = proplists:get_value(<<"valueBoolean">>, Props) };
to_extensionValue(<<"valueCode">>, Props) ->
    #valueCode{ value = proplists:get_value(<<"valueCode">>, Props) };
to_extensionValue(<<"valueDate">>, Props) ->
    #valueDate{ value = proplists:get_value(<<"valueDate">>, Props) };
to_extensionValue(<<"valueDateTime">>, Props) ->
    #valueDateTime{ value = proplists:get_value(<<"valueDateTime">>, Props) };
to_extensionValue(<<"valueDecimal">>, Props) ->
    #valueDecimal{ value = proplists:get_value(<<"valueDecimal">>, Props) };
to_extensionValue(<<"valueId">>, Props) ->
    #valueId{ value = proplists:get_value(<<"valueId">>, Props) };
to_extensionValue(<<"valueInstant">>, Props) ->
    #valueInstant{ value = proplists:get_value(<<"valueInstant">>, Props) };
to_extensionValue(<<"valueInteger">>, Props) ->
    #valueInteger{ value = proplists:get_value(<<"valueInteger">>, Props) };
to_extensionValue(<<"valueMarkdown">>, Props) ->
    #valueMarkdown{ value = proplists:get_value(<<"valueMarkdown">>, Props) };
to_extensionValue(<<"valueOid">>, Props) ->
    #valueOid{ value = proplists:get_value(<<"valueOid">>, Props) };
to_extensionValue(<<"valuePositiveInt">>, Props) ->
    #valuePositiveInt{ value = proplists:get_value(<<"valuePositiveInt">>, Props) };
to_extensionValue(<<"valueString">>, Props) ->
    #valueString{ value = proplists:get_value(<<"valueString">>, Props) };
to_extensionValue(<<"valueTime">>, Props) ->
    #valueTime{ value = proplists:get_value(<<"valueTime">>, Props) };
to_extensionValue(<<"valueUnsignedInt">>, Props) ->
    #valueUnsignedInt{ value = proplists:get_value(<<"valueUnsignedInt">>, Props) };
to_extensionValue(<<"valueUri">>, Props) ->
    #valueUri{ value = proplists:get_value(<<"valueUri">>, Props) };
to_extensionValue(<<"valueUrl">>, Props) ->
    #valueUrl{ value = proplists:get_value(<<"valueUrl">>, Props) };
to_extensionValue(<<"valueUuid">>, Props) ->
    #valueUuid{ value = proplists:get_value(<<"valueUuid">>, Props) };
to_extensionValue(<<"valueAnnotation">>, Props) ->
    #valueAnnotation{ value = complex:to_annotation(Props) };
to_extensionValue(<<"valueCodeableConcept">>, Props) ->
    #valueCodeableConcept{ value = complex:to_codeableConcept(Props) };
to_extensionValue(<<"valueCoding">>, Props) ->
    #valueCoding{ value = complex:to_coding(Props) };
to_extensionValue(<<"valueReference">>, Props) ->
    #valueReference{ value = complex:to_reference(Props) }.
    
