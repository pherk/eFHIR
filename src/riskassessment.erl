-module(riskassessment).
%%%
%%% FHIR 4.0.0 RiskAssessment
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('RiskAssessment', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , 'identifier' :: [complex:'Identifier'()]
    , basedOn :: special:'Reference'() | undefined
    , parent :: special:'Reference'() | undefined
    , status :: code()
    , method :: complex:'CodeableConcept'() | undefined
    , code :: complex:'CodeableConcept'() | undefined
    , subject :: special:'Reference'()
    , encounter :: special:'Reference'() | undefined
    , occurrenceDateTime :: dateTime() | undefined
    , occurrencePeriod :: complex:'Period'() | undefined
    , condition :: special:'Reference'() | undefined
    , performer :: special:'Reference'() | undefined
    , reasonCode :: [complex:'CodeableConcept'()]
    , reasonReference :: [special:'Reference'()]
    , basis :: [special:'Reference'()]
    , prediction :: ['RiskAssessment.Prediction'()]
    , mitigation :: string() | undefined
    , note :: [complex:'Annotation'()]
    }).
-type 'RiskAssessment'() :: #'RiskAssessment'{}.


-record('RiskAssessment.Prediction', { anyAttribs :: anyAttribs(),
      outcome :: complex:'CodeableConcept'() | undefined
    , probabilityDecimal :: decimal() | undefined
    , probabilityRange :: complex:'Range'() | undefined
    , qualitativeRisk :: complex:'CodeableConcept'() | undefined
    , relativeRisk :: decimal() | undefined
    , whenPeriod :: complex:'Period'() | undefined
    , whenRange :: complex:'Range'() | undefined
    , rationale :: string() | undefined
    }).
-type 'RiskAssessment.Prediction'() :: #'RiskAssessment.Prediction'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_riskAssessment({Props}) -> to_riskAssessment(Props);
to_riskAssessment(Props) -> 
    DT = decode:xsd_info(<<"RiskAssessment">>),
    #'RiskAssessment'{
      anyAttribs = decode:attrs(Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , meta = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
    , language = decode:value(<<"language">>, Props, DT)
    , text = decode:value(<<"text">>, Props, DT)
    , contained = decode:value(<<"contained">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , identifier = decode:value(<<"identifier">>, Props, DT)
    , basedOn = decode:value(<<"basedOn">>, Props, DT)
    , parent = decode:value(<<"parent">>, Props, DT)
    , status = decode:value(<<"status">>, Props, DT)
    , method = decode:value(<<"method">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , subject = decode:value(<<"subject">>, Props, DT)
    , encounter = decode:value(<<"encounter">>, Props, DT)
    , occurrenceDateTime = decode:value(<<"occurrenceDateTime">>, Props, DT)
    , occurrencePeriod = decode:value(<<"occurrencePeriod">>, Props, DT)
    , condition = decode:value(<<"condition">>, Props, DT)
    , performer = decode:value(<<"performer">>, Props, DT)
    , reasonCode = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference = decode:value(<<"reasonReference">>, Props, DT)
    , basis = decode:value(<<"basis">>, Props, DT)
    , prediction = decode:value(<<"prediction">>, Props, DT)
    , mitigation = decode:value(<<"mitigation">>, Props, DT)
    , note = decode:value(<<"note">>, Props, DT)
    }.

to_riskAssessment_prediction({Props}) -> to_riskAssessment_prediction(Props);
to_riskAssessment_prediction(Props) -> 
    DT = decode:xsd_info(<<"RiskAssessment.Prediction">>),
    #'RiskAssessment.Prediction'{
      anyAttribs = decode:attrs(Props, DT)
    , outcome = decode:value(<<"outcome">>, Props, DT)
    , probabilityDecimal = decode:value(<<"probabilityDecimal">>, Props, DT)
    , probabilityRange = decode:value(<<"probabilityRange">>, Props, DT)
    , qualitativeRisk = decode:value(<<"qualitativeRisk">>, Props, DT)
    , relativeRisk = decode:value(<<"relativeRisk">>, Props, DT)
    , whenPeriod = decode:value(<<"whenPeriod">>, Props, DT)
    , whenRange = decode:value(<<"whenRange">>, Props, DT)
    , rationale = decode:value(<<"rationale">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, riskassessment:to_riskAssessment(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
