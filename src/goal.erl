-module(goal).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('Goal.Target', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	measure :: complex:'CodeableConcept'() | undefined,
	choice :: string() | complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | integer() | complex:'CodeableConcept'() | boolean() | undefined,
	choice1 :: complex:'Duration'() | date() | undefined}).

-type 'Goal.Target'() :: #'Goal.Target'{}.


-record('Goal', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	lifecycleStatus :: complex:'GoalLifecycleStatus'(),
	achievementStatus :: complex:'CodeableConcept'() | undefined,
	category :: [complex:'CodeableConcept'()] | undefined,
	priority :: complex:'CodeableConcept'() | undefined,
	description :: complex:'CodeableConcept'(),
	subject :: special:'Reference'(),
	choice :: date() | complex:'CodeableConcept'() | undefined,
	target :: [complex:'Goal.Target'()] | undefined,
	statusDate :: date() | undefined,
	statusReason :: string() | undefined,
	expressedBy :: special:'Reference'() | undefined,
	addresses :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	outcomeCode :: [complex:'CodeableConcept'()] | undefined,
	outcomeReference :: [special:'Reference'()] | undefined}).

-type 'Goal'() :: #'Goal'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_goal({Props}) -> to_goal(Props);
to_goal(Props) ->
  DT = decode:xsd_info(<<"Goal">>),
  #'Goal'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	lifecycleStatus :: complex:'GoalLifecycleStatus'(),
	achievementStatus :: complex:'CodeableConcept'() | undefined,
	category :: [complex:'CodeableConcept'()] | undefined,
	priority :: complex:'CodeableConcept'() | undefined,
	description :: complex:'CodeableConcept'(),
	subject :: special:'Reference'(),
	choice :: date() | complex:'CodeableConcept'() | undefined,
	target :: [complex:'Goal.Target'()] | undefined,
	statusDate :: date() | undefined,
	statusReason :: string() | undefined,
	expressedBy :: special:'Reference'() | undefined,
	addresses :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	outcomeCode :: [complex:'CodeableConcept'()] | undefined,
	outcomeReference :: [special:'Reference'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
goal.Target({Props}) -> goal.Target(Props); 
goal.Target(Props) -> 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	measure :: complex:'CodeableConcept'() | undefined,
	choice :: string() | complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | integer() | complex:'CodeableConcept'() | boolean() | undefined,
	choice1 :: complex:'Duration'() | date() | undefined}).
    }.



text(#'Goal'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, goal:to_goal(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

goal_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Goal',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
goal_toprop_test() ->
    ?asrtp({'Goal',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Goal">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

goal_json_test() ->
    ?asrtjson({'Goal',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Goal\",\"id\":\"p-21666\"}">>).

-endif.


