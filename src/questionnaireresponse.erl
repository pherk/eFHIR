-module(questionnaireresponse).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('QuestionnaireResponse.Answer', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	choice :: uri() | time() | string() | complex:'Reference'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | integer() | decimal() | dateTime() | date() | complex:'Coding'() | boolean() | complex:'Attachment'() | undefined,
	item :: [complex:'QuestionnaireResponse.Item'()] | undefined}).

-type 'QuestionnaireResponse.Answer'() :: #'QuestionnaireResponse.Answer'{}.


-record('QuestionnaireResponse.Item', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	linkId :: string(),
	definition :: uri() | undefined,
	text :: string() | undefined,
	answer :: [complex:'QuestionnaireResponse.Answer'()] | undefined,
	item :: [complex:'QuestionnaireResponse.Item'()] | undefined}).

-type 'QuestionnaireResponse.Item'() :: #'QuestionnaireResponse.Item'{}.


-record('QuestionnaireResponse', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: complex:'Identifier'() | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	partOf :: [special:'Reference'()] | undefined,
	questionnaire :: canonical() | undefined,
	status :: complex:'QuestionnaireResponseStatus'(),
	subject :: special:'Reference'() | undefined,
	encounter :: special:'Reference'() | undefined,
	authored :: dateTime() | undefined,
	author :: special:'Reference'() | undefined,
	source :: special:'Reference'() | undefined,
	item :: [complex:'QuestionnaireResponse.Item'()] | undefined}).

-type 'QuestionnaireResponse'() :: #'QuestionnaireResponse'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_questionnaireResponse({Props}) -> to_questionnaireResponse(Props);
to_questionnaireResponse(Props) ->
  DT = decode:xsd_info(<<"QuestionnaireResponse">>),
  #'QuestionnaireResponse'{ 
      anyAttribs       = decode:attrs(Props, DT)
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , partOf  = decode:value(<<"partOf">>, Props, DT)
    , questionnaire  = decode:value(<<"questionnaire">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , authored  = decode:value(<<"authored">>, Props, DT)
    , author  = decode:value(<<"author">>, Props, DT)
    , source  = decode:value(<<"source">>, Props, DT)
    , item  = decode:value(<<"item">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_questionnaireResponse_answer({Props}) -> to_questionnaireResponse_answer(Props);
to_questionnaireResponse_answer(Props) ->
  DT = decode:xsd_info(<<"QuestionnaireResponse.Answer">>),
  #'QuestionnaireResponse.Answer'{
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , item  = decode:value(<<"item">>, Props, DT)
    }.


to_questionnaireResponse_item({Props}) -> to_questionnaireResponse_item(Props);
to_questionnaireResponse_item(Props) ->
  DT = decode:xsd_info(<<"QuestionnaireResponse.Item">>),
  #'QuestionnaireResponse.Item'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , linkId  = decode:value(<<"linkId">>, Props, DT)
    , definition  = decode:value(<<"definition">>, Props, DT)
    , text  = decode:value(<<"text">>, Props, DT)
    , answer  = decode:value(<<"answer">>, Props, DT)
    , item  = decode:value(<<"item">>, Props, DT)
    }.



text(#'QuestionnaireResponse'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, questionnaireresponse:to_questionnaireResponse(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

questionnaireResponse_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>},{<<"status">>,<<"completed">>}],
        {'QuestionnaireResponse',[],<<"p-21666">>,undefined,
                     undefined,undefined,undefined,[],[],[],undefined,[],[],
                     undefined,<<"completed">>,undefined,undefined,undefined,undefined,
                     undefined,[]}).

questionnaireResponse_toprop_test() ->
    ?asrtp(
        {'QuestionnaireResponse',[],<<"p-21666">>,undefined,
                     undefined,undefined,undefined,[],[],[],undefined,[],[],
                     undefined,<<"completed">>,undefined,undefined,undefined,undefined,
                     undefined,[]},
           {[{<<"resourceType">>,<<"QuestionnaireResponse">>},
              {<<"id">>,<<"p-21666">>},
              {<<"status">>,<<"completed">>}
            ]}).

questionnaireResponse_json_test() ->
    ?asrtjson({'QuestionnaireResponse',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"QuestionnaireResponse\",\"id\":\"p-21666\"}">>).

-endif.



