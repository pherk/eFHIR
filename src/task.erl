-module(task).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('Task.Output', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	choice :: uuid() | complex:'UsageContext'() | url() | uri() | unsignedInt() | complex:'TriggerDefinition'() | complex:'Timing'() | time() | string() | complex:'Signature'() | complex:'SampledData'() | complex:'RelatedArtifact'() | complex:'Reference'() | complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | positiveInt() | complex:'Period'() | complex:'ParameterDefinition'() | oid() | complex:'Money'() | markdown() | integer() | instant() | complex:'Identifier'() | id() | complex:'HumanName'() | complex:'Expression'() | complex:'Duration'() | complex:'Dosage'() | complex:'Distance'() | decimal() | dateTime() | date() | complex:'DataRequirement'() | complex:'Count'() | complex:'Contributor'() | complex:'ContactPoint'() | complex:'ContactDetail'() | complex:'Coding'() | complex:'CodeableConcept'() | code() | canonical() | boolean() | base64Binary() | complex:'Attachment'() | complex:'Annotation'() | complex:'Age'() | complex:'Address'()}).

-type 'Task.Output'() :: #'Task.Output'{}.


-record('Task.Input', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	choice :: uuid() | complex:'UsageContext'() | url() | uri() | unsignedInt() | complex:'TriggerDefinition'() | complex:'Timing'() | time() | string() | complex:'Signature'() | complex:'SampledData'() | complex:'RelatedArtifact'() | complex:'Reference'() | complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | positiveInt() | complex:'Period'() | complex:'ParameterDefinition'() | oid() | complex:'Money'() | markdown() | integer() | instant() | complex:'Identifier'() | id() | complex:'HumanName'() | complex:'Expression'() | complex:'Duration'() | complex:'Dosage'() | complex:'Distance'() | decimal() | dateTime() | date() | complex:'DataRequirement'() | complex:'Count'() | complex:'Contributor'() | complex:'ContactPoint'() | complex:'ContactDetail'() | complex:'Coding'() | complex:'CodeableConcept'() | code() | canonical() | boolean() | base64Binary() | complex:'Attachment'() | complex:'Annotation'() | complex:'Age'() | complex:'Address'()}).

-type 'Task.Input'() :: #'Task.Input'{}.


-record('Task.Restriction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	repetitions :: positiveInt() | undefined,
	period :: complex:'Period'() | undefined,
	recipient :: [special:'Reference'()] | undefined}).

-type 'Task.Restriction'() :: #'Task.Restriction'{}.


-record('Task', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	instantiatesCanonical :: canonical() | undefined,
	instantiatesUri :: uri() | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	groupIdentifier :: complex:'Identifier'() | undefined,
	partOf :: [special:'Reference'()] | undefined,
	status :: complex:'TaskStatus'(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	businessStatus :: complex:'CodeableConcept'() | undefined,
	intent :: complex:'TaskIntent'(),
	priority :: complex:'RequestPriority'() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	description :: string() | undefined,
	focus :: special:'Reference'() | undefined,
	for :: special:'Reference'() | undefined,
	encounter :: special:'Reference'() | undefined,
	executionPeriod :: complex:'Period'() | undefined,
	authoredOn :: dateTime() | undefined,
	lastModified :: dateTime() | undefined,
	requester :: special:'Reference'() | undefined,
	performerType :: [complex:'CodeableConcept'()] | undefined,
	owner :: special:'Reference'() | undefined,
	location :: special:'Reference'() | undefined,
	reasonCode :: complex:'CodeableConcept'() | undefined,
	reasonReference :: special:'Reference'() | undefined,
	insurance :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	relevantHistory :: [special:'Reference'()] | undefined,
	restriction :: complex:'Task.Restriction'() | undefined,
	input :: [complex:'Task.Input'()] | undefined,
	output :: [complex:'Task.Output'()] | undefined}).

-type 'Task'() :: #'Task'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_task({Props}) -> to_task(Props);
to_task(Props) ->
  DT = decode:xsd_info(<<"Task">>),
  #'Task'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	instantiatesCanonical :: canonical() | undefined,
	instantiatesUri :: uri() | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	groupIdentifier :: complex:'Identifier'() | undefined,
	partOf :: [special:'Reference'()] | undefined,
	status :: complex:'TaskStatus'(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	businessStatus :: complex:'CodeableConcept'() | undefined,
	intent :: complex:'TaskIntent'(),
	priority :: complex:'RequestPriority'() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	description :: string() | undefined,
	focus :: special:'Reference'() | undefined,
	for :: special:'Reference'() | undefined,
	encounter :: special:'Reference'() | undefined,
	executionPeriod :: complex:'Period'() | undefined,
	authoredOn :: dateTime() | undefined,
	lastModified :: dateTime() | undefined,
	requester :: special:'Reference'() | undefined,
	performerType :: [complex:'CodeableConcept'()] | undefined,
	owner :: special:'Reference'() | undefined,
	location :: special:'Reference'() | undefined,
	reasonCode :: complex:'CodeableConcept'() | undefined,
	reasonReference :: special:'Reference'() | undefined,
	insurance :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	relevantHistory :: [special:'Reference'()] | undefined,
	restriction :: 'Task.Restriction'() | undefined,
	input :: ['Task.Input'()] | undefined,
	output :: ['Task.Output'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_task.Output({Props}) -> to_task.Output(Props);
to_task.Output(Props) ->
  DT = decode:xsd_info(<<"Task.Output">>),
  #'Task.Output'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	choice :: uuid() | complex:'UsageContext'() | url() | uri() | unsignedInt() | complex:'TriggerDefinition'() | complex:'Timing'() | time() | string() | complex:'Signature'() | complex:'SampledData'() | complex:'RelatedArtifact'() | complex:'Reference'() | complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | positiveInt() | complex:'Period'() | complex:'ParameterDefinition'() | oid() | complex:'Money'() | markdown() | integer() | instant() | complex:'Identifier'() | id() | complex:'HumanName'() | complex:'Expression'() | complex:'Duration'() | complex:'Dosage'() | complex:'Distance'() | decimal() | dateTime() | date() | complex:'DataRequirement'() | complex:'Count'() | complex:'Contributor'() | complex:'ContactPoint'() | complex:'ContactDetail'() | complex:'Coding'() | complex:'CodeableConcept'() | code() | canonical() | boolean() | base64Binary() | complex:'Attachment'() | complex:'Annotation'() | complex:'Age'() | complex:'Address'()}).
    }.


to_task.Input({Props}) -> to_task.Input(Props);
to_task.Input(Props) ->
  DT = decode:xsd_info(<<"Task.Input">>),
  #'Task.Input'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'CodeableConcept'(),
	choice :: uuid() | complex:'UsageContext'() | url() | uri() | unsignedInt() | complex:'TriggerDefinition'() | complex:'Timing'() | time() | string() | complex:'Signature'() | complex:'SampledData'() | complex:'RelatedArtifact'() | complex:'Reference'() | complex:'Ratio'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | positiveInt() | complex:'Period'() | complex:'ParameterDefinition'() | oid() | complex:'Money'() | markdown() | integer() | instant() | complex:'Identifier'() | id() | complex:'HumanName'() | complex:'Expression'() | complex:'Duration'() | complex:'Dosage'() | complex:'Distance'() | decimal() | dateTime() | date() | complex:'DataRequirement'() | complex:'Count'() | complex:'Contributor'() | complex:'ContactPoint'() | complex:'ContactDetail'() | complex:'Coding'() | complex:'CodeableConcept'() | code() | canonical() | boolean() | base64Binary() | complex:'Attachment'() | complex:'Annotation'() | complex:'Age'() | complex:'Address'()}).
    }.


to_task.Restriction({Props}) -> to_task.Restriction(Props);
to_task.Restriction(Props) ->
  DT = decode:xsd_info(<<"Task.Restriction">>),
  #'Task.Restriction'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	repetitions :: positiveInt() | undefined,
	period :: complex:'Period'() | undefined,
	recipient :: [special:'Reference'()] | undefined}).
    }.



text(#'Task'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, task:to_task(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

task_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Task',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
task_toprop_test() ->
    ?asrtp({'Task',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Task">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

task_json_test() ->
    ?asrtjson({'Task',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Task\",\"id\":\"p-21666\"}">>).

-endif.


