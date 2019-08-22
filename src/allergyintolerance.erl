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


to_allergyIntolerance({Props}) -> to_allergyIntolerance(Props);
to_allergyIntolerance(Props) ->
  DT = decode:xsd_info(<<"AllergyIntolerance">>),
  #'AllergyIntolerance'{ 
      anyAttribs :: anyAttribs(),
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
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
    }.


%%====================================================================
%% Internal functions
%%====================================================================

to_allergyIntolerance_reaction({Props}) -> to_allergyIntolerance_reaction(Props);
to_allergyIntolerance_reaction(Props) ->
  DT = decode:xsd_info(<<"AllergyIntolerance.Reakion">>),
  #'AllergyIntolerance.Reaktion'{ 
      anyAttribs :: anyAttribs(),
    , id               = decode:value(<<"id">>, Props, DT)
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	substance :: complex:'CodeableConcept'() | undefined,
	manifestation :: [complex:'CodeableConcept'()],
	description :: string() | undefined,
	onset :: dateTime() | undefined,
	severity :: complex:'AllergyIntoleranceSeverity'() | undefined,
	exposureRoute :: complex:'CodeableConcept'() | undefined,
	note :: [complex:'Annotation'()] | undefined}).
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


