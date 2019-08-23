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
    , instantiatesCanonical  = decode:value(<<"instantiatesCanonical">>, Props, DT)
    , instantiatesUri  = decode:value(<<"instantiatesUri">>, Props, DT)
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , groupIdentifier  = decode:value(<<"groupIdentifier">>, Props, DT)
    , partOf  = decode:value(<<"partOf">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , statusReason  = decode:value(<<"statusReason">>, Props, DT)
    , businessStatus  = decode:value(<<"businessStatus">>, Props, DT)
    , intent  = decode:value(<<"intent">>, Props, DT)
    , priority  = decode:value(<<"priority">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , description  = decode:value(<<"description">>, Props, DT)
    , focus  = decode:value(<<"focus">>, Props, DT)
    , for  = decode:value(<<"for">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , executionPeriod  = decode:value(<<"executionPeriod">>, Props, DT)
    , authoredOn  = decode:value(<<"authoredOn">>, Props, DT)
    , lastModified  = decode:value(<<"lastModified">>, Props, DT)
    , requester  = decode:value(<<"requester">>, Props, DT)
    , performerType  = decode:value(<<"performerType">>, Props, DT)
    , owner  = decode:value(<<"owner">>, Props, DT)
    , location  = decode:value(<<"location">>, Props, DT)
    , reasonCode  = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference  = decode:value(<<"reasonReference">>, Props, DT)
    , insurance  = decode:value(<<"insurance">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    , relevantHistory  = decode:value(<<"relevantHistory">>, Props, DT)
    , restriction  = decode:value(<<"restriction">>, Props, DT)
    , input  = decode:value(<<"input">>, Props, DT)
    , output  = decode:value(<<"output">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_task_output({Props}) -> to_task_output(Props);
to_task_output(Props) ->
  DT = decode:xsd_info(<<"Task.Output">>),
  #'Task.Output'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    }.


to_task_input({Props}) -> to_task_input(Props);
to_task_input(Props) ->
  DT = decode:xsd_info(<<"Task.Input">>),
  #'Task.Input'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    }.


to_task_restriction({Props}) -> to_task_restriction(Props);
to_task_restriction(Props) ->
  DT = decode:xsd_info(<<"Task.Restriction">>),
  #'Task.Restriction'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , repetitions  = decode:value(<<"repetitions">>, Props, DT)
    , period  = decode:value(<<"period">>, Props, DT)
    , recipient  = decode:value(<<"recipient">>, Props, DT)
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
    ?asrtto([{<<"id">>, <<"p-21666">>},{<<"status">>,<<"requested">>},{<<"intent">>,<<"order">>}],
{'Task',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],
           [],undefined,undefined,[], undefined,
           [],<<"requested">>,undefined,undefined, <<"order">>,
           undefined,undefined,undefined,undefined, undefined,
           undefined,undefined,undefined,undefined, undefined,
           [],undefined,undefined,undefined,undefined,
           [],[],[],undefined,[],
           []}).

task_toprop_test() ->
    ?asrtp(
{'Task',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],
           [],undefined,undefined,[], undefined,
           [],<<"requested">>,undefined,undefined, <<"order">>,
           undefined,undefined,undefined,undefined, undefined,
           undefined,undefined,undefined,undefined, undefined,
           [],undefined,undefined,undefined,undefined,
           [],[],[],undefined,[],
           []},
           {[{<<"resourceType">>,<<"Task">>},
             {<<"id">>,<<"p-21666">>},
             {<<"status">>,<<"requested">>},
             {<<"intent">>,<<"order">>}]}).

task_json_test() ->
    ?asrtjson(
{'Task',[],<<"p-21666">>,undefined,undefined,undefined,undefined,[],[],[],
           [],undefined,undefined,[], undefined,
           [],<<"requested">>,undefined,undefined, <<"order">>,
           undefined,undefined,undefined,undefined, undefined,
           undefined,undefined,undefined,undefined, undefined,
           [],undefined,undefined,undefined,undefined,
           [],[],[],undefined,[],
           []},
           <<"{\"resourceType\":\"Task\",\"id\":\"p-21666\",\"status\":\"requested\",\"intent\":\"order\"}">>).

-endif.


