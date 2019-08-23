-module(allergyintolerance).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('AllergyIntolerance.Reaction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	substance :: complex:'CodeableConcept'() | undefined,
	manifestation :: [complex:'CodeableConcept'()],
	description :: string() | undefined,
	onset :: dateTime() | undefined,
	severity :: complex:'AllergyIntoleranceSeverity'() | undefined,
	exposureRoute :: complex:'CodeableConcept'() | undefined,
	note :: [complex:'Annotation'()] | undefined}).

-type 'AllergyIntolerance.Reaction'() :: #'AllergyIntolerance.Reaction'{}.


-record('AllergyIntolerance', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	clinicalStatus :: complex:'CodeableConcept'() | undefined,
	verificationStatus :: complex:'CodeableConcept'() | undefined,
	type :: complex:'AllergyIntoleranceType'() | undefined,
	category :: [complex:'AllergyIntoleranceCategory'()] | undefined,
	criticality :: complex:'AllergyIntoleranceCriticality'() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	patient :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	choice :: string() | complex:'Range'() | complex:'Period'() | dateTime() | complex:'Age'() | undefined,
	recordedDate :: dateTime() | undefined,
	recorder :: special:'Reference'() | undefined,
	asserter :: special:'Reference'() | undefined,
	lastOccurrence :: dateTime() | undefined,
	note :: [complex:'Annotation'()] | undefined,
	reaction :: [complex:'AllergyIntolerance.Reaction'()] | undefined}).

%%====================================================================
%% API functions
%%====================================================================
to_allergyIntolerance({Props}) -> to_allergyIntolerance(Props);
to_allergyIntolerance(Props) ->
  DT = decode:xsd_info(<<"AllergyIntolerance">>),
  #'AllergyIntolerance'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
    , clinicalStatus  = decode:value(<<"clinicalStatus">>, Props, DT) 
    , verificationStatus  = decode:value(<<"verificationStatus">>, Props, DT) 
    , type  = decode:value(<<"type">>, Props, DT) 
    , category  = decode:value(<<"category">>, Props, DT) 
    , criticality  = decode:value(<<"criticality">>, Props, DT) 
    , code  = decode:value(<<"code">>, Props, DT) 
    , patient  = decode:value(<<"patient">>, Props, DT) 
    , encounter  = decode:value(<<"encounter">>, Props, DT) 
    , choice  = decode:value(<<"choice">>, Props, DT) 
    , recordedDate  = decode:value(<<"recordedDate">>, Props, DT) 
    , recorder  = decode:value(<<"recorder">>, Props, DT) 
    , asserter  = decode:value(<<"asserter">>, Props, DT) 
    , lastOccurrence  = decode:value(<<"lastOccurrence">>, Props, DT) 
    , note  = decode:value(<<"note">>, Props, DT) 
    , reaction  = decode:value(<<"reaction">>, Props, DT) 
    }.


%%====================================================================
%% Internal functions
%%====================================================================

to_allergyIntolerance_reaction({Props}) -> to_allergyIntolerance_reaction(Props);
to_allergyIntolerance_reaction(Props) ->
  DT = decode:xsd_info(<<"AllergyIntolerance.Reaction">>),
  #'AllergyIntolerance.Reaction'{ 
      anyAttribs = decode:attrs(Props, DT)
    , id               = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , substance  = decode:value(<<"substance">>, Props, DT) 
    , manifestation  = decode:value(<<"manifestation">>, Props, DT) 
    , description  = decode:value(<<"description">>, Props, DT) 
    , onset  = decode:value(<<"onset">>, Props, DT) 
    , severity  = decode:value(<<"severity">>, Props, DT) 
    , exposureRoute  = decode:value(<<"exposureRoute">>, Props, DT) 
    , note  = decode:value(<<"note">>, Props, DT) 
    }.




text(#'AllergyIntolerance'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, allergyintolerance:to_allergyintolerance(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

allergyintolerance_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'AllergyIntolerance',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
allergyintolerance_toprop_test() ->
    ?asrtp({'AllergyIntolerance',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"AllergyIntolerance">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

allergyintolerance_json_test() ->
    ?asrtjson({'AllergyIntolerance',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"AllergyIntolerance\",\"id\":\"p-21666\"}">>).

-endif.


