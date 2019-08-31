-module(adverseevent).
%%%
%%% FHIR 4.0.0 AdverseEvent
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('AdverseEvent', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , 'identifier' :: complex:'Identifier'() | undefined
    , actuality :: code()
    , category :: [complex:'CodeableConcept'()]
    , event :: complex:'CodeableConcept'() | undefined
    , subject :: special:'Reference'()
    , encounter :: special:'Reference'() | undefined
    , date :: dateTime() | undefined
    , detected :: dateTime() | undefined
    , recordedDate :: dateTime() | undefined
    , resultingCondition :: [special:'Reference'()]
    , location :: special:'Reference'() | undefined
    , seriousness :: complex:'CodeableConcept'() | undefined
    , severity :: complex:'CodeableConcept'() | undefined
    , outcome :: complex:'CodeableConcept'() | undefined
    , recorder :: special:'Reference'() | undefined
    , contributor :: [special:'Reference'()]
    , suspectEntity :: ['AdverseEvent.SuspectEntity'()]
    , subjectMedicalHistory :: [special:'Reference'()]
    , referenceDocument :: [special:'Reference'()]
    , study :: [special:'Reference'()]
    }).
-type 'AdverseEvent'() :: #'AdverseEvent'{}.


-record('AdverseEvent.SuspectEntity', { anyAttribs :: anyAttribs(),
      instance :: special:'Reference'()
    , causality :: ['AdverseEvent.Causality'()]
    }).
-type 'AdverseEvent.SuspectEntity'() :: #'AdverseEvent.SuspectEntity'{}.


-record('AdverseEvent.Causality', { anyAttribs :: anyAttribs(),
      assessment :: complex:'CodeableConcept'() | undefined
    , productRelatedness :: string() | undefined
    , author :: special:'Reference'() | undefined
    , method :: complex:'CodeableConcept'() | undefined
    }).
-type 'AdverseEvent.Causality'() :: #'AdverseEvent.Causality'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_adverseEvent({Props}) -> to_adverseEvent(Props);
to_adverseEvent(Props) -> 
    DT = decode:xsd_info(<<"AdverseEvent">>),
    #'AdverseEvent'{
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
    , actuality = decode:value(<<"actuality">>, Props, DT)
    , category = decode:value(<<"category">>, Props, DT)
    , event = decode:value(<<"event">>, Props, DT)
    , subject = decode:value(<<"subject">>, Props, DT)
    , encounter = decode:value(<<"encounter">>, Props, DT)
    , date = decode:value(<<"date">>, Props, DT)
    , detected = decode:value(<<"detected">>, Props, DT)
    , recordedDate = decode:value(<<"recordedDate">>, Props, DT)
    , resultingCondition = decode:value(<<"resultingCondition">>, Props, DT)
    , location = decode:value(<<"location">>, Props, DT)
    , seriousness = decode:value(<<"seriousness">>, Props, DT)
    , severity = decode:value(<<"severity">>, Props, DT)
    , outcome = decode:value(<<"outcome">>, Props, DT)
    , recorder = decode:value(<<"recorder">>, Props, DT)
    , contributor = decode:value(<<"contributor">>, Props, DT)
    , suspectEntity = decode:value(<<"suspectEntity">>, Props, DT)
    , subjectMedicalHistory = decode:value(<<"subjectMedicalHistory">>, Props, DT)
    , referenceDocument = decode:value(<<"referenceDocument">>, Props, DT)
    , study = decode:value(<<"study">>, Props, DT)
    }.

to_adverseEvent_suspectEntity({Props}) -> to_adverseEvent_suspectEntity(Props);
to_adverseEvent_suspectEntity(Props) -> 
    DT = decode:xsd_info(<<"AdverseEvent.SuspectEntity">>),
    #'AdverseEvent.SuspectEntity'{
      anyAttribs = decode:attrs(Props, DT)
    , instance = decode:value(<<"instance">>, Props, DT)
    , causality = decode:value(<<"causality">>, Props, DT)
    }.

to_adverseEvent_causality({Props}) -> to_adverseEvent_causality(Props);
to_adverseEvent_causality(Props) -> 
    DT = decode:xsd_info(<<"AdverseEvent.Causality">>),
    #'AdverseEvent.Causality'{
      anyAttribs = decode:attrs(Props, DT)
    , assessment = decode:value(<<"assessment">>, Props, DT)
    , productRelatedness = decode:value(<<"productRelatedness">>, Props, DT)
    , author = decode:value(<<"author">>, Props, DT)
    , method = decode:value(<<"method">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, adverseevent:to_adverseEvent(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
