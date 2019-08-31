-module(clinicalimpression).
%%%
%%% FHIR 4.0.0 ClinicalImpression
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('ClinicalImpression', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , 'identifier' :: [complex:'Identifier'()]
    , status :: code()
    , statusReason :: complex:'CodeableConcept'() | undefined
    , code :: complex:'CodeableConcept'() | undefined
    , description :: string() | undefined
    , subject :: special:'Reference'()
    , encounter :: special:'Reference'() | undefined
    , effectiveDateTime :: dateTime() | undefined
    , effectivePeriod :: complex:'Period'() | undefined
    , date :: dateTime() | undefined
    , assessor :: special:'Reference'() | undefined
    , previous :: special:'Reference'() | undefined
    , problem :: [special:'Reference'()]
    , investigation :: ['ClinicalImpression.Investigation'()]
    , protocol :: [uri()]
    , summary :: string() | undefined
    , finding :: ['ClinicalImpression.Finding'()]
    , prognosisCodeableConcept :: [complex:'CodeableConcept'()]
    , prognosisReference :: [special:'Reference'()]
    , supportingInfo :: [special:'Reference'()]
    , note :: [complex:'Annotation'()]
    }).
-type 'ClinicalImpression'() :: #'ClinicalImpression'{}.


-record('ClinicalImpression.Investigation', { anyAttribs :: anyAttribs(),
      code :: complex:'CodeableConcept'()
    , item :: [special:'Reference'()]
    }).
-type 'ClinicalImpression.Investigation'() :: #'ClinicalImpression.Investigation'{}.


-record('ClinicalImpression.Finding', { anyAttribs :: anyAttribs(),
      itemCodeableConcept :: complex:'CodeableConcept'() | undefined
    , itemReference :: special:'Reference'() | undefined
    , basis :: string() | undefined
    }).
-type 'ClinicalImpression.Finding'() :: #'ClinicalImpression.Finding'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_clinicalImpression({Props}) -> to_clinicalImpression(Props);
to_clinicalImpression(Props) -> 
    DT = decode:xsd_info(<<"ClinicalImpression">>),
    #'ClinicalImpression'{
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
    , status = decode:value(<<"status">>, Props, DT)
    , statusReason = decode:value(<<"statusReason">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , subject = decode:value(<<"subject">>, Props, DT)
    , encounter = decode:value(<<"encounter">>, Props, DT)
    , effectiveDateTime = decode:value(<<"effectiveDateTime">>, Props, DT)
    , effectivePeriod = decode:value(<<"effectivePeriod">>, Props, DT)
    , date = decode:value(<<"date">>, Props, DT)
    , assessor = decode:value(<<"assessor">>, Props, DT)
    , previous = decode:value(<<"previous">>, Props, DT)
    , problem = decode:value(<<"problem">>, Props, DT)
    , investigation = decode:value(<<"investigation">>, Props, DT)
    , protocol = decode:value(<<"protocol">>, Props, DT)
    , summary = decode:value(<<"summary">>, Props, DT)
    , finding = decode:value(<<"finding">>, Props, DT)
    , prognosisCodeableConcept = decode:value(<<"prognosisCodeableConcept">>, Props, DT)
    , prognosisReference = decode:value(<<"prognosisReference">>, Props, DT)
    , supportingInfo = decode:value(<<"supportingInfo">>, Props, DT)
    , note = decode:value(<<"note">>, Props, DT)
    }.

to_clinicalImpression_investigation({Props}) -> to_clinicalImpression_investigation(Props);
to_clinicalImpression_investigation(Props) -> 
    DT = decode:xsd_info(<<"ClinicalImpression.Investigation">>),
    #'ClinicalImpression.Investigation'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , item = decode:value(<<"item">>, Props, DT)
    }.

to_clinicalImpression_finding({Props}) -> to_clinicalImpression_finding(Props);
to_clinicalImpression_finding(Props) -> 
    DT = decode:xsd_info(<<"ClinicalImpression.Finding">>),
    #'ClinicalImpression.Finding'{
      anyAttribs = decode:attrs(Props, DT)
    , itemCodeableConcept = decode:value(<<"itemCodeableConcept">>, Props, DT)
    , itemReference = decode:value(<<"itemReference">>, Props, DT)
    , basis = decode:value(<<"basis">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, clinicalimpression:to_clinicalImpression(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
