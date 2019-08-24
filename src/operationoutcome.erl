-module(operationoutcome).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('OperationOutcome.Issue', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	severity :: code(),
	code :: code(),
	details :: complex:'CodeableConcept'() | undefined,
	diagnostics :: string() | undefined,
	location :: [string()] | undefined,
	expression :: [string()] | undefined}).

-type 'OperationOutcome.Issue'() :: #'OperationOutcome.Issue'{}.


-record('OperationOutcome', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	issue :: ['OperationOutcome.Issue'()]}).

-type 'OperationOutcome'() :: #'OperationOutcome'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_operationOutcome({Props}) -> to_operationOutcome(Props);
to_operationOutcome(Props) ->
  DT = decode:xsd_info(<<"OperationOutcome">>),
  #'OperationOutcome'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
	, issue            = decode:value(<<"issue">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_operationOutcome_issue({Props}) -> to_operationOutcome_issue(Props);
to_operationOutcome_issue(Props) -> 
  DT = decode:xsd_info(<<"OperationOutcome.Issue">>),
  #'OperationOutcome.Issue'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , severity  = decode:value(<<"severity">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , details  = decode:value(<<"details">>, Props, DT)
    , diagnostics  = decode:value(<<"diagnostics">>, Props, DT)
    , location  = decode:value(<<"location">>, Props, DT)
    , expression  = decode:value(<<"expression">>, Props, DT)
    }.


text(#'OperationOutcome'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, operationoutcome:to_operationOutcome(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

operationOutcome_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}, 
             {<<"issue">>, [{[{<<"severity">>, <<"fatal">>},
                             {<<"code">>, <<"Rest API">>}]}]} 
            ],
            {'OperationOutcome',[],<<"p-21666">>,undefined,undefined,
                     undefined,undefined,[],[],[],
                     [{'OperationOutcome.Issue',[],undefined,[],[],
                          <<"fatal">>,<<"Rest API">>,undefined,undefined,[],
                          []}]}
           ).

operationOutcome_toprop_test() ->
    ?asrtp(
            {'OperationOutcome',[],<<"p-21666">>,undefined,undefined,
                     undefined,undefined,[],[],[],
                     [{'OperationOutcome.Issue',[],undefined,[],[],
                          <<"fatal">>,<<"Rest API">>,undefined,undefined,[],
                          []}]},
            {[{<<"resourceType">>,<<"OperationOutcome">>},
                   {<<"id">>,<<"p-21666">>},
                   {<<"issue">>,
                    [{[{<<"severity">>,<<"fatal">>},
                       {<<"code">>,<<"Rest API">>}]}]}]}
      ).

operationOutcome_json_test() ->
    ?asrtjson(
            {'OperationOutcome',[],<<"p-21666">>,undefined,undefined,
                     undefined,undefined,[],[],[],
                     [{'OperationOutcome.Issue',[],undefined,[],[],
                          <<"fatal">>,<<"Rest API">>,undefined,undefined,[],
                          []}]},
            <<"{\"resourceType\":\"OperationOutcome\",\"id\":\"p-21666\",\"issue\":[{\"severity\":\"fatal\",\"code\":\"Rest API\"}]}">>
      ).

-endif.


