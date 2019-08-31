-module(familymemberhistory).
%%%
%%% FHIR 4.0.0 FamilyMemberHistory
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('FamilyMemberHistory', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , 'identifier' :: [complex:'Identifier'()]
    , instantiatesCanonical :: [canonical()]
    , instantiatesUri :: [uri()]
    , status :: code()
    , dataAbsentReason :: complex:'CodeableConcept'() | undefined
    , patient :: special:'Reference'()
    , date :: dateTime() | undefined
    , name :: string() | undefined
    , relationship :: complex:'CodeableConcept'()
    , sex :: complex:'CodeableConcept'() | undefined
    , bornPeriod :: complex:'Period'() | undefined
    , bornDate :: date() | undefined
    , bornString :: string() | undefined
    , ageAge :: complex:'Age'() | undefined
    , ageRange :: complex:'Range'() | undefined
    , ageString :: string() | undefined
    , estimatedAge :: boolean() | undefined
    , deceasedBoolean :: boolean() | undefined
    , deceasedAge :: complex:'Age'() | undefined
    , deceasedRange :: complex:'Range'() | undefined
    , deceasedDate :: date() | undefined
    , deceasedString :: string() | undefined
    , reasonCode :: [complex:'CodeableConcept'()]
    , reasonReference :: [special:'Reference'()]
    , note :: [complex:'Annotation'()]
    , condition :: ['FamilyMemberHistory.Condition'()]
    }).
-type 'FamilyMemberHistory'() :: #'FamilyMemberHistory'{}.


-record('FamilyMemberHistory.Condition', { anyAttribs :: anyAttribs(),
      code :: complex:'CodeableConcept'()
    , outcome :: complex:'CodeableConcept'() | undefined
    , contributedToDeath :: boolean() | undefined
    , onsetAge :: complex:'Age'() | undefined
    , onsetRange :: complex:'Range'() | undefined
    , onsetPeriod :: complex:'Period'() | undefined
    , onsetString :: string() | undefined
    , note :: [complex:'Annotation'()]
    }).
-type 'FamilyMemberHistory.Condition'() :: #'FamilyMemberHistory.Condition'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_familyMemberHistory({Props}) -> to_familyMemberHistory(Props);
to_familyMemberHistory(Props) -> 
    DT = decode:xsd_info(<<"FamilyMemberHistory">>),
    #'FamilyMemberHistory'{
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
    , instantiatesCanonical = decode:value(<<"instantiatesCanonical">>, Props, DT)
    , instantiatesUri = decode:value(<<"instantiatesUri">>, Props, DT)
    , status = decode:value(<<"status">>, Props, DT)
    , dataAbsentReason = decode:value(<<"dataAbsentReason">>, Props, DT)
    , patient = decode:value(<<"patient">>, Props, DT)
    , date = decode:value(<<"date">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , relationship = decode:value(<<"relationship">>, Props, DT)
    , sex = decode:value(<<"sex">>, Props, DT)
    , bornPeriod = decode:value(<<"bornPeriod">>, Props, DT)
    , bornDate = decode:value(<<"bornDate">>, Props, DT)
    , bornString = decode:value(<<"bornString">>, Props, DT)
    , ageAge = decode:value(<<"ageAge">>, Props, DT)
    , ageRange = decode:value(<<"ageRange">>, Props, DT)
    , ageString = decode:value(<<"ageString">>, Props, DT)
    , estimatedAge = decode:value(<<"estimatedAge">>, Props, DT)
    , deceasedBoolean = decode:value(<<"deceasedBoolean">>, Props, DT)
    , deceasedAge = decode:value(<<"deceasedAge">>, Props, DT)
    , deceasedRange = decode:value(<<"deceasedRange">>, Props, DT)
    , deceasedDate = decode:value(<<"deceasedDate">>, Props, DT)
    , deceasedString = decode:value(<<"deceasedString">>, Props, DT)
    , reasonCode = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference = decode:value(<<"reasonReference">>, Props, DT)
    , note = decode:value(<<"note">>, Props, DT)
    , condition = decode:value(<<"condition">>, Props, DT)
    }.

to_familyMemberHistory_condition({Props}) -> to_familyMemberHistory_condition(Props);
to_familyMemberHistory_condition(Props) -> 
    DT = decode:xsd_info(<<"FamilyMemberHistory.Condition">>),
    #'FamilyMemberHistory.Condition'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , outcome = decode:value(<<"outcome">>, Props, DT)
    , contributedToDeath = decode:value(<<"contributedToDeath">>, Props, DT)
    , onsetAge = decode:value(<<"onsetAge">>, Props, DT)
    , onsetRange = decode:value(<<"onsetRange">>, Props, DT)
    , onsetPeriod = decode:value(<<"onsetPeriod">>, Props, DT)
    , onsetString = decode:value(<<"onsetString">>, Props, DT)
    , note = decode:value(<<"note">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, familymemberhistory:to_familyMemberHistory(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
