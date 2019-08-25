-module(consent).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('ConsentState', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined}).

-type 'ConsentState'() :: #'ConsentState'{}.


-record('ConsentDataMeaning', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined}).

-type 'ConsentDataMeaning'() :: #'ConsentDataMeaning'{}.


-record('ConsentProvisionType', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	value :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined}).

-type 'ConsentProvisionType'() :: #'ConsentProvisionType'{}.


-record('Consent.Data', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	meaning :: 'ConsentDataMeaning'(),
	reference :: special:'Reference'()}).

-type 'Consent.Data'() :: #'Consent.Data'{}.


-record('Consent.Actor', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	role :: complex:'CodeableConcept'(),
	reference :: special:'Reference'()}).

-type 'Consent.Actor'() :: #'Consent.Actor'{}.


-record('Consent.Provision', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: code() | undefined,
	period :: complex:'Period'() | undefined,
	actor :: ['Consent.Actor'()] | undefined,
	action :: [complex:'CodeableConcept'()] | undefined,
	securityLabel :: [complex:'Coding'()] | undefined,
	purpose :: [complex:'Coding'()] | undefined,
	class :: [complex:'Coding'()] | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	dataPeriod :: complex:'Period'() | undefined,
	data :: ['Consent.Data'()] | undefined,
	provision :: ['Consent.Provision'()] | undefined}).

-type 'Consent.Provision'() :: #'Consent.Provision'{}.


-record('Consent.Verification', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	verified :: boolean(),
	verifiedWith :: special:'Reference'() | undefined,
	verificationDate :: dateTime() | undefined}).

-type 'Consent.Verification'() :: #'Consent.Verification'{}.


-record('Consent.Policy', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	authority :: uri() | undefined,
	uri :: uri() | undefined}).

-type 'Consent.Policy'() :: #'Consent.Policy'{}.


-record('Consent', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: code(),
	scope :: complex:'CodeableConcept'(),
	category :: [complex:'CodeableConcept'()],
	patient :: special:'Reference'() | undefined,
	dateTime :: dateTime() | undefined,
	performer :: [special:'Reference'()] | undefined,
	organization :: [special:'Reference'()] | undefined,
	source :: special:'Reference'() | complex:'Attachment'() | undefined,
	policy :: ['Consent.Policy'()] | undefined,
	policyRule :: complex:'CodeableConcept'() | undefined,
	verification :: ['Consent.Verification'()] | undefined,
	provision :: 'Consent.Provision'() | undefined}).

-type 'Consent'() :: #'Consent'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_consent({Props}) -> to_consent(Props);
to_consent(Props) ->
  DT = decode:xsd_info(<<"Consent">>),
  #'Consent'{ 
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
    , status  = decode:value(<<"status">>, Props, DT)
    , scope  = decode:value(<<"scope">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , patient  = decode:value(<<"patient">>, Props, DT)
    , dateTime  = decode:value(<<"dateTime">>, Props, DT)
    , performer  = decode:value(<<"performer">>, Props, DT)
    , organization  = decode:value(<<"organization">>, Props, DT)
    , source  = decode:value(<<"source">>, Props, DT)
    , policy  = decode:value(<<"policy">>, Props, DT)
    , policyRule  = decode:value(<<"policyRule">>, Props, DT)
    , verification  = decode:value(<<"verification">>, Props, DT)
    , provision  = decode:value(<<"provision">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_consent_data({Props}) ->  to_consent_data(Props);
to_consent_data(Props) -> 
  DT = decode:xsd_info(<<"Consent.Data">>),
  #'Consent.Data'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , meaning  = decode:value(<<"meaning">>, Props, DT)
    , reference  = decode:value(<<"reference">>, Props, DT)
    }.

to_consent_actor({Props}) ->  to_consent_actor(Props);
to_consent_actor(Props) -> 
  DT = decode:xsd_info(<<"Consent.Actor">>),
  #'Consent.Actor'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , role  = decode:value(<<"role">>, Props, DT)
    , reference  = decode:value(<<"reference">>, Props, DT)
    }.

to_consent_provision({Props}) ->  to_consent_provision(Props);
to_consent_provision(Props) -> 
  DT = decode:xsd_info(<<"Consent.Provision">>),
  #'Consent.Provision'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , type  = decode:value(<<"type">>, Props, DT)
    , period  = decode:value(<<"period">>, Props, DT)
    , actor  = decode:value(<<"actor">>, Props, DT)
    , action  = decode:value(<<"action">>, Props, DT)
    , securityLabel  = decode:value(<<"securityLabel">>, Props, DT)
    , purpose  = decode:value(<<"purpose">>, Props, DT)
    , class  = decode:value(<<"class">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , dataPeriod  = decode:value(<<"dataPeriod">>, Props, DT)
    , data  = decode:value(<<"data">>, Props, DT)
    , provision  = decode:value(<<"provision">>, Props, DT)
    }.


to_consent_verification({Props}) ->  to_consent_verification(Props);
to_consent_verification(Props) -> 
  DT = decode:xsd_info(<<"Consent.Verification">>),
  #'Consent.Verification'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , verified  = decode:value(<<"verified">>, Props, DT)
    , verifiedWith  = decode:value(<<"verifiedWith">>, Props, DT)
    , verificationDate  = decode:value(<<"verificationDate">>, Props, DT)
    }.


to_consent_policy({Props}) ->  to_consent_policy(Props);
to_consent_policy(Props) -> 
  DT = decode:xsd_info(<<"Consent.policy">>),
  #'Consent.Policy'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , authority  = decode:value(<<"authority">>, Props, DT)
    , uri  = decode:value(<<"uri">>, Props, DT)
    }.


text(#'Consent'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, consent:to_consent(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

consent_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}, {<<"status">>, <<"active">>},
             {<<"scope">>, {[{<<"coding">>, [{[{<<"code">>, <<"treatment">>}]}]}]}},
             {<<"category">>, [{[{<<"coding">>, [{[{<<"code">>, <<"ICOL">>}]}]}]}]}
            ],
            {'Consent',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],<<"active">>,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"treatment">>,undefined,undefined}],
                 undefined},
             [{'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"ICOL">>,undefined,undefined}],
                 undefined}],
             undefined,undefined,[],[],undefined,[],undefined,[], undefined}
           ).

consent_toprop_test() ->
    ?asrtp(
            {'Consent',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],<<"active">>,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"treatment">>,undefined,undefined}],
                 undefined},
             [{'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"ICOL">>,undefined,undefined}],
                 undefined}],
             undefined,undefined,[],[],undefined,[],undefined,[], undefined},
            {[{<<"resourceType">>,<<"Consent">>},
              {<<"id">>,<<"p-21666">>},
              {<<"status">>,<<"active">>},
              {<<"scope">>,
                    {[{<<"coding">>,[{[{<<"code">>,<<"treatment">>}]}]}]}},
              {<<"category">>,
                    [{[{<<"coding">>,[{[{<<"code">>,<<"ICOL">>}]}]}]}]}]}
            ).

consent_json_test() ->
    ?asrtjson(
            {'Consent',[],<<"p-21666">>,undefined,undefined,undefined, undefined,[],[],[],
             [],<<"active">>,
             {'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"treatment">>,undefined,undefined}],
                 undefined},
             [{'CodeableConcept',[],undefined,[],
                 [{'Coding',[],undefined,[],undefined,undefined, <<"ICOL">>,undefined,undefined}],
                 undefined}],
             undefined,undefined,[],[],undefined,[],undefined,[], undefined},
            <<"{\"resourceType\":\"Consent\",\"id\":\"p-21666\",\"status\":\"active\",\"scope\":{\"coding\":[{\"code\":\"treatment\"}]},\"category\":[{\"coding\":[{\"code\":\"ICOL\"}]}]}">>
      ).

-endif.


