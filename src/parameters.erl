-module(parameters).
%%%
%%% FHIR 4.0.0 Parameters
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('Parameters', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , parameter :: ['Parameters.Parameter'()]
    }).
-type 'Parameters'() :: #'Parameters'{}.


-record('Parameters.Parameter', { anyAttribs :: anyAttribs(),
      name :: string()
    , valueBase64Binary :: base64Binary() | undefined
    , valueBoolean :: boolean() | undefined
    , valueCanonical :: canonical() | undefined
    , valueCode :: code() | undefined
    , valueDate :: date() | undefined
    , valueDateTime :: dateTime() | undefined
    , valueDecimal :: decimal() | undefined
    , valueId :: id() | undefined
    , valueInstant :: instant() | undefined
    , valueInteger :: integer() | undefined
    , valueMarkdown :: markdown() | undefined
    , valueOid :: oid() | undefined
    , valuePositiveInt :: positiveInt() | undefined
    , valueString :: string() | undefined
    , valueTime :: time() | undefined
    , valueUnsignedInt :: unsignedInt() | undefined
    , valueUri :: uri() | undefined
    , valueUrl :: url() | undefined
    , valueUuid :: uuid() | undefined
    , valueAddress :: complex:'Address'() | undefined
    , valueAge :: complex:'Age'() | undefined
    , valueAnnotation :: complex:'Annotation'() | undefined
    , valueAttachment :: complex:'Attachment'() | undefined
    , valueCodeableConcept :: complex:'CodeableConcept'() | undefined
    , valueCoding :: complex:'Coding'() | undefined
    , valueContactPoint :: complex:'ContactPoint'() | undefined
    , valueCount :: complex:'Count'() | undefined
    , valueDistance :: complex:'Distance'() | undefined
    , valueDuration :: complex:'Duration'() | undefined
    , valueHumanName :: complex:'HumanName'() | undefined
    , valueIdentifier :: complex:'Identifier'() | undefined
    , valueMoney :: complex:'Money'() | undefined
    , valuePeriod :: complex:'Period'() | undefined
    , valueQuantity :: complex:'Quantity'() | undefined
    , valueRange :: complex:'Range'() | undefined
    , valueRatio :: complex:'Ratio'() | undefined
    , valueReference :: special:'Reference'() | undefined
    , valueSampledData :: complex:'SampledData'() | undefined
    , valueSignature :: complex:'Signature'() | undefined
    , valueTiming :: complex:'Timing'() | undefined
    , valueContactDetail :: metadata:'ContactDetail'() | undefined
    , valueContributor :: metadata:'Contributor'() | undefined
    , valueDataRequirement :: metadata:'DataRequirement'() | undefined
    , valueExpression :: metadata:'Expression'() | undefined
    , valueParameterDefinition :: metadata:'ParameterDefinition'() | undefined
    , valueRelatedArtifact :: metadata:'RelatedArtifact'() | undefined
    , valueTriggerDefinition :: metadata:'TriggerDefinition'() | undefined
    , valueUsageContext :: metadata:'UsageContext'() | undefined
    , valueDosage :: metadata:'Dosage'() | undefined
    , resource :: resource:'ResourceContainer'() | undefined
    , part :: ['Parameters.Parameter'()]
    }).
-type 'Parameters.Parameter'() :: #'Parameters.Parameter'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_parameters({Props}) -> to_parameters(Props);
to_parameters(Props) -> 
    DT = decode:xsd_info(<<"Parameters">>),
    #'Parameters'{
      anyAttribs = decode:attrs(Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , meta = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
    , language = decode:value(<<"language">>, Props, DT)
    , parameter = decode:value(<<"parameter">>, Props, DT)
    }.

to_parameters_parameter({Props}) -> to_parameters_parameter(Props);
to_parameters_parameter(Props) -> 
    DT = decode:xsd_info(<<"Parameters.Parameter">>),
    #'Parameters.Parameter'{
      anyAttribs = decode:attrs(Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , valueBase64Binary = decode:value(<<"valueBase64Binary">>, Props, DT)
    , valueBoolean = decode:value(<<"valueBoolean">>, Props, DT)
    , valueCanonical = decode:value(<<"valueCanonical">>, Props, DT)
    , valueCode = decode:value(<<"valueCode">>, Props, DT)
    , valueDate = decode:value(<<"valueDate">>, Props, DT)
    , valueDateTime = decode:value(<<"valueDateTime">>, Props, DT)
    , valueDecimal = decode:value(<<"valueDecimal">>, Props, DT)
    , valueId = decode:value(<<"valueId">>, Props, DT)
    , valueInstant = decode:value(<<"valueInstant">>, Props, DT)
    , valueInteger = decode:value(<<"valueInteger">>, Props, DT)
    , valueMarkdown = decode:value(<<"valueMarkdown">>, Props, DT)
    , valueOid = decode:value(<<"valueOid">>, Props, DT)
    , valuePositiveInt = decode:value(<<"valuePositiveInt">>, Props, DT)
    , valueString = decode:value(<<"valueString">>, Props, DT)
    , valueTime = decode:value(<<"valueTime">>, Props, DT)
    , valueUnsignedInt = decode:value(<<"valueUnsignedInt">>, Props, DT)
    , valueUri = decode:value(<<"valueUri">>, Props, DT)
    , valueUrl = decode:value(<<"valueUrl">>, Props, DT)
    , valueUuid = decode:value(<<"valueUuid">>, Props, DT)
    , valueAddress = decode:value(<<"valueAddress">>, Props, DT)
    , valueAge = decode:value(<<"valueAge">>, Props, DT)
    , valueAnnotation = decode:value(<<"valueAnnotation">>, Props, DT)
    , valueAttachment = decode:value(<<"valueAttachment">>, Props, DT)
    , valueCodeableConcept = decode:value(<<"valueCodeableConcept">>, Props, DT)
    , valueCoding = decode:value(<<"valueCoding">>, Props, DT)
    , valueContactPoint = decode:value(<<"valueContactPoint">>, Props, DT)
    , valueCount = decode:value(<<"valueCount">>, Props, DT)
    , valueDistance = decode:value(<<"valueDistance">>, Props, DT)
    , valueDuration = decode:value(<<"valueDuration">>, Props, DT)
    , valueHumanName = decode:value(<<"valueHumanName">>, Props, DT)
    , valueIdentifier = decode:value(<<"valueIdentifier">>, Props, DT)
    , valueMoney = decode:value(<<"valueMoney">>, Props, DT)
    , valuePeriod = decode:value(<<"valuePeriod">>, Props, DT)
    , valueQuantity = decode:value(<<"valueQuantity">>, Props, DT)
    , valueRange = decode:value(<<"valueRange">>, Props, DT)
    , valueRatio = decode:value(<<"valueRatio">>, Props, DT)
    , valueReference = decode:value(<<"valueReference">>, Props, DT)
    , valueSampledData = decode:value(<<"valueSampledData">>, Props, DT)
    , valueSignature = decode:value(<<"valueSignature">>, Props, DT)
    , valueTiming = decode:value(<<"valueTiming">>, Props, DT)
    , valueContactDetail = decode:value(<<"valueContactDetail">>, Props, DT)
    , valueContributor = decode:value(<<"valueContributor">>, Props, DT)
    , valueDataRequirement = decode:value(<<"valueDataRequirement">>, Props, DT)
    , valueExpression = decode:value(<<"valueExpression">>, Props, DT)
    , valueParameterDefinition = decode:value(<<"valueParameterDefinition">>, Props, DT)
    , valueRelatedArtifact = decode:value(<<"valueRelatedArtifact">>, Props, DT)
    , valueTriggerDefinition = decode:value(<<"valueTriggerDefinition">>, Props, DT)
    , valueUsageContext = decode:value(<<"valueUsageContext">>, Props, DT)
    , valueDosage = decode:value(<<"valueDosage">>, Props, DT)
    , resource = decode:value(<<"resource">>, Props, DT)
    , part = decode:value(<<"part">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, parameters:to_parameters(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
