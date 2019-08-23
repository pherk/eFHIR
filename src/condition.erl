-module(condition).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Condition.Evidence', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	detail :: [special:'Reference'()] | undefined}).

-type 'Condition.Evidence'() :: #'Condition.Evidence'{}.


-record('Condition.Stage', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	summary :: complex:'CodeableConcept'() | undefined,
	assessment :: [special:'Reference'()] | undefined,
	type :: complex:'CodeableConcept'() | undefined}).

-type 'Condition.Stage'() :: #'Condition.Stage'{}.


-record('Condition', {anyAttribs :: anyAttribs(),
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
	category :: [complex:'CodeableConcept'()] | undefined,
	severity :: complex:'CodeableConcept'() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	bodySite :: [complex:'CodeableConcept'()] | undefined,
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	choice :: string() | complex:'Range'() | complex:'Period'() | dateTime() | complex:'Age'() | undefined,
	choice1 :: string() | complex:'Range'() | complex:'Period'() | dateTime() | complex:'Age'() | undefined,
	recordedDate :: dateTime() | undefined,
	recorder :: special:'Reference'() | undefined,
	asserter :: special:'Reference'() | undefined,
	stage :: [complex:'Condition.Stage'()] | undefined,
	evidence :: [complex:'Condition.Evidence'()] | undefined,
	note :: [complex:'Annotation'()] | undefined}).

-type 'Condition'() :: #'Condition'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_condition({Props}) -> to_condition(Props);
to_condition(Props) ->
  DT = decode:xsd_info(<<"Condition">>),
  #'Condition'{ 
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
    , category  = decode:value(<<"category">>, Props, DT)
    , severity  = decode:value(<<"severity">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , bodySite  = decode:value(<<"bodySite">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , choice1  = decode:value(<<"choice1">>, Props, DT)
    , recordedDate  = decode:value(<<"recordedDate">>, Props, DT)
    , recorder  = decode:value(<<"recorder">>, Props, DT)
    , asserter  = decode:value(<<"asserter">>, Props, DT)
    , stage  = decode:value(<<"stage">>, Props, DT)
    , evidence  = decode:value(<<"evidence">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_condition_evidence({Props}) ->  to_condition_evidence(Props);
to_condition_evidence(Props) -> 
  DT = decode:xsd_info(<<"Condition.Evidence">>),
  #'Condition.Evidence'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , detail  = decode:value(<<"detail">>, Props, DT)
    }.

to_condition_stage({Props}) ->  to_condition_stage(Props);
to_condition_stage(Props) -> 
  DT = decode:xsd_info(<<"Condition.Stage">>),
  #'Condition.Stage'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , summary  = decode:value(<<"summary">>, Props, DT)
    , assessment  = decode:value(<<"assessment">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    }.

text(#'Condition'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, condition:to_condition(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

condition_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Condition',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
condition_toprop_test() ->
    ?asrtp({'Condition',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Condition">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

condition_json_test() ->
    ?asrtjson({'Condition',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Condition\",\"id\":\"p-21666\"}">>).

-endif.


