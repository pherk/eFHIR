-module(healthcareservice).
%%%
%%% FHIR 4.0.0 HealthcareService
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('HealthcareService', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , 'identifier' :: [complex:'Identifier'()]
    , active :: boolean() | undefined
    , providedBy :: special:'Reference'() | undefined
    , category :: [complex:'CodeableConcept'()]
    , type :: [complex:'CodeableConcept'()]
    , specialty :: [complex:'CodeableConcept'()]
    , location :: [special:'Reference'()]
    , name :: string() | undefined
    , comment :: string() | undefined
    , extraDetails :: markdown() | undefined
    , photo :: complex:'Attachment'() | undefined
    , telecom :: [complex:'ContactPoint'()]
    , coverageArea :: [special:'Reference'()]
    , serviceProvisionCode :: [complex:'CodeableConcept'()]
    , eligibility :: ['HealthcareService.Eligibility'()]
    , program :: [complex:'CodeableConcept'()]
    , characteristic :: [complex:'CodeableConcept'()]
    , communication :: [complex:'CodeableConcept'()]
    , referralMethod :: [complex:'CodeableConcept'()]
    , appointmentRequired :: boolean() | undefined
    , availableTime :: ['HealthcareService.AvailableTime'()]
    , notAvailable :: ['HealthcareService.NotAvailable'()]
    , availabilityExceptions :: string() | undefined
    , endpoint :: [special:'Reference'()]
    }).
-type 'HealthcareService'() :: #'HealthcareService'{}.


-record('HealthcareService.Eligibility', { anyAttribs :: anyAttribs(),
      code :: complex:'CodeableConcept'() | undefined
    , comment :: markdown() | undefined
    }).
-type 'HealthcareService.Eligibility'() :: #'HealthcareService.Eligibility'{}.


-record('HealthcareService.NotAvailable', { anyAttribs :: anyAttribs(),
      description :: string()
    , during :: complex:'Period'() | undefined
    }).
-type 'HealthcareService.NotAvailable'() :: #'HealthcareService.NotAvailable'{}.


-record('HealthcareService.AvailableTime', { anyAttribs :: anyAttribs(),
      daysOfWeek :: [code()]
    , allDay :: boolean() | undefined
    , availableStartTime :: time() | undefined
    , availableEndTime :: time() | undefined
    }).
-type 'HealthcareService.AvailableTime'() :: #'HealthcareService.AvailableTime'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_healthcareService({Props}) -> to_healthcareService(Props);
to_healthcareService(Props) -> 
    DT = decode:xsd_info(<<"HealthcareService">>),
    #'HealthcareService'{
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
    , active = decode:value(<<"active">>, Props, DT)
    , providedBy = decode:value(<<"providedBy">>, Props, DT)
    , category = decode:value(<<"category">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , specialty = decode:value(<<"specialty">>, Props, DT)
    , location = decode:value(<<"location">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , comment = decode:value(<<"comment">>, Props, DT)
    , extraDetails = decode:value(<<"extraDetails">>, Props, DT)
    , photo = decode:value(<<"photo">>, Props, DT)
    , telecom = decode:value(<<"telecom">>, Props, DT)
    , coverageArea = decode:value(<<"coverageArea">>, Props, DT)
    , serviceProvisionCode = decode:value(<<"serviceProvisionCode">>, Props, DT)
    , eligibility = decode:value(<<"eligibility">>, Props, DT)
    , program = decode:value(<<"program">>, Props, DT)
    , characteristic = decode:value(<<"characteristic">>, Props, DT)
    , communication = decode:value(<<"communication">>, Props, DT)
    , referralMethod = decode:value(<<"referralMethod">>, Props, DT)
    , appointmentRequired = decode:value(<<"appointmentRequired">>, Props, DT)
    , availableTime = decode:value(<<"availableTime">>, Props, DT)
    , notAvailable = decode:value(<<"notAvailable">>, Props, DT)
    , availabilityExceptions = decode:value(<<"availabilityExceptions">>, Props, DT)
    , endpoint = decode:value(<<"endpoint">>, Props, DT)
    }.

to_healthcareService_eligibility({Props}) -> to_healthcareService_eligibility(Props);
to_healthcareService_eligibility(Props) -> 
    DT = decode:xsd_info(<<"HealthcareService.Eligibility">>),
    #'HealthcareService.Eligibility'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , comment = decode:value(<<"comment">>, Props, DT)
    }.

to_healthcareService_notAvailable({Props}) -> to_healthcareService_notAvailable(Props);
to_healthcareService_notAvailable(Props) -> 
    DT = decode:xsd_info(<<"HealthcareService.NotAvailable">>),
    #'HealthcareService.NotAvailable'{
      anyAttribs = decode:attrs(Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , during = decode:value(<<"during">>, Props, DT)
    }.

to_healthcareService_availableTime({Props}) -> to_healthcareService_availableTime(Props);
to_healthcareService_availableTime(Props) -> 
    DT = decode:xsd_info(<<"HealthcareService.AvailableTime">>),
    #'HealthcareService.AvailableTime'{
      anyAttribs = decode:attrs(Props, DT)
    , daysOfWeek = decode:value(<<"daysOfWeek">>, Props, DT)
    , allDay = decode:value(<<"allDay">>, Props, DT)
    , availableStartTime = decode:value(<<"availableStartTime">>, Props, DT)
    , availableEndTime = decode:value(<<"availableEndTime">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, healthcareservice:to_healthcareService(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
