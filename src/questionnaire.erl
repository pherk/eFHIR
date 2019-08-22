-module(questionnaire).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('Questionnaire.Initial', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	choice :: uri() | time() | string() | complex:'Reference'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | integer() | decimal() | dateTime() | date() | complex:'Coding'() | boolean() | complex:'Attachment'()}).

-type 'Questionnaire.Initial'() :: #'Questionnaire.Initial'{}.


-record('Questionnaire.AnswerOption', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	choice :: time() | string() | complex:'Reference'() | integer() | date() | complex:'Coding'(),
	initialSelected :: boolean() | undefined}).

-type 'Questionnaire.AnswerOption'() :: #'Questionnaire.AnswerOption'{}.


-record('Questionnaire.EnableWhen', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	question :: string(),
	operator :: complex:'QuestionnaireItemOperator'(),
	choice :: time() | string() | complex:'Reference'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | integer() | decimal() | dateTime() | date() | complex:'Coding'() | boolean()}).

-type 'Questionnaire.EnableWhen'() :: #'Questionnaire.EnableWhen'{}.


-record('Questionnaire.Item', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	linkId :: string(),
	definition :: uri() | undefined,
	code :: [complex:'Coding'()] | undefined,
	prefix :: string() | undefined,
	text :: string() | undefined,
	type :: complex:'QuestionnaireItemType'(),
	enableWhen :: [complex:'Questionnaire.EnableWhen'()] | undefined,
	enableBehavior :: complex:'EnableWhenBehavior'() | undefined,
	required :: boolean() | undefined,
	repeats :: boolean() | undefined,
	readOnly :: boolean() | undefined,
	maxLength :: integer() | undefined,
	answerValueSet :: canonical() | undefined,
	answerOption :: [complex:'Questionnaire.AnswerOption'()] | undefined,
	initial :: [complex:'Questionnaire.Initial'()] | undefined,
	item :: [complex:'Questionnaire.Item'()] | undefined}).

-type 'Questionnaire.Item'() :: #'Questionnaire.Item'{}.


-record('Questionnaire', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	url :: uri() | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	version :: string() | undefined,
	name :: string() | undefined,
	title :: string() | undefined,
	derivedFrom :: [canonical()] | undefined,
	status :: complex:'PublicationStatus'(),
	experimental :: boolean() | undefined,
	subjectType :: [code()] | undefined,
	date :: dateTime() | undefined,
	publisher :: string() | undefined,
	contact :: [complex:'ContactDetail'()] | undefined,
	description :: markdown() | undefined,
	useContext :: [complex:'UsageContext'()] | undefined,
	jurisdiction :: [complex:'CodeableConcept'()] | undefined,
	purpose :: markdown() | undefined,
	copyright :: markdown() | undefined,
	approvalDate :: date() | undefined,
	lastReviewDate :: date() | undefined,
	effectivePeriod :: complex:'Period'() | undefined,
	code :: [complex:'Coding'()] | undefined,
	item :: [complex:'Questionnaire.Item'()] | undefined}).

-type 'Questionnaire'() :: #'Questionnaire'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_questionnaire({Props}) -> to_questionnaire(Props);
to_questionnaire(Props) ->
  DT = decode:xsd_info(<<"Questionnaire">>),
  #'Questionnaire'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
	url :: uri() | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	version :: string() | undefined,
	name :: string() | undefined,
	title :: string() | undefined,
	derivedFrom :: [canonical()] | undefined,
	status :: complex:'PublicationStatus'(),
	experimental :: boolean() | undefined,
	subjectType :: [code()] | undefined,
	date :: dateTime() | undefined,
	publisher :: string() | undefined,
	contact :: [complex:'ContactDetail'()] | undefined,
	description :: markdown() | undefined,
	useContext :: [complex:'UsageContext'()] | undefined,
	jurisdiction :: [complex:'CodeableConcept'()] | undefined,
	purpose :: markdown() | undefined,
	copyright :: markdown() | undefined,
	approvalDate :: date() | undefined,
	lastReviewDate :: date() | undefined,
	effectivePeriod :: complex:'Period'() | undefined,
	code :: [complex:'Coding'()] | undefined,
	item :: [complex:'Questionnaire.Item'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_questionnaire.Initial({Props}) -> to_questionnaire.Initial(Props);
to_questionnaire.Initial(Props) ->
  DT = decode:xsd_info(<<"Questionnaire.Initial">>),
  #'Questionnaire.Initial'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	choice :: uri() | time() | string() | complex:'Reference'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | integer() | decimal() | dateTime() | date() | complex:'Coding'() | boolean() | complex:'Attachment'()}).
    }.


to_questionnaire.AnswerOption({Props}) -> to_questionnaire.AnswerOption(Props);
to_questionnaire.AnswerOption(Props) ->
  DT = decode:xsd_info(<<"Questionnaire.AnswerOption">>),
  #'Questionnaire.AnswerOption'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	choice :: time() | string() | complex:'Reference'() | integer() | date() | complex:'Coding'(),
	initialSelected :: boolean() | undefined}).
    }.


to_questionnaire.EnableWhen({Props}) -> to_questionnaire.EnableWhen(Props);
to_questionnaire.EnableWhen(Props) ->
  DT = decode:xsd_info(<<"Questionnaire.EnableWhen">>),
  #'Questionnaire.EnableWhen'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	question :: string(),
	operator :: complex:'QuestionnaireItemOperator'(),
	choice :: time() | string() | complex:'Reference'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | integer() | decimal() | dateTime() | date() | complex:'Coding'() | boolean()}).
    }.


to_questionnaire.Item({Props}) -> to_questionnaire.Item(Props);
to_questionnaire.Item(Props) ->
  DT = decode:xsd_info(<<"Questionnaire.Item">>),
  #'Questionnaire.Item'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	linkId :: string(),
	definition :: uri() | undefined,
	code :: [complex:'Coding'()] | undefined,
	prefix :: string() | undefined,
	text :: string() | undefined,
	type :: complex:'QuestionnaireItemType'(),
	enableWhen :: [complex:'Questionnaire.EnableWhen'()] | undefined,
	enableBehavior :: complex:'EnableWhenBehavior'() | undefined,
	required :: boolean() | undefined,
	repeats :: boolean() | undefined,
	readOnly :: boolean() | undefined,
	maxLength :: integer() | undefined,
	answerValueSet :: canonical() | undefined,
	answerOption :: [complex:'Questionnaire.AnswerOption'()] | undefined,
	initial :: [complex:'Questionnaire.Initial'()] | undefined,
	item :: [complex:'Questionnaire.Item'()] | undefined}).
    }.


text(#'Questionnaire'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, questionnaire:to_questionnaire(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

questionnaire_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Questionnaire',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
questionnaire_toprop_test() ->
    ?asrtp({'Questionnaire',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Questionnaire">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

questionnaire_json_test() ->
    ?asrtjson({'Questionnaire',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Questionnaire\",\"id\":\"p-21666\"}">>).

-endif.


