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
	meaning :: complex:'ConsentDataMeaning'(),
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
	type :: complex:'ConsentProvisionType'() | undefined,
	period :: complex:'Period'() | undefined,
	actor :: [complex:'Consent.Actor'()] | undefined,
	action :: [complex:'CodeableConcept'()] | undefined,
	securityLabel :: [complex:'Coding'()] | undefined,
	purpose :: [complex:'Coding'()] | undefined,
	class :: [complex:'Coding'()] | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	dataPeriod :: complex:'Period'() | undefined,
	data :: [complex:'Consent.Data'()] | undefined,
	provision :: [complex:'Consent.Provision'()] | undefined}).

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
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'ConsentState'(),
	scope :: complex:'CodeableConcept'(),
	category :: [complex:'CodeableConcept'()],
	patient :: special:'Reference'() | undefined,
	dateTime :: dateTime() | undefined,
	performer :: [special:'Reference'()] | undefined,
	organization :: [special:'Reference'()] | undefined,
	choice :: special:'Reference'() | complex:'Attachment'() | undefined,
	policy :: [complex:'Consent.Policy'()] | undefined,
	policyRule :: complex:'CodeableConcept'() | undefined,
	verification :: [complex:'Consent.Verification'()] | undefined,
	provision :: complex:'Consent.Provision'() | undefined}).

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
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	status :: complex:'ConsentState'(),
	scope :: complex:'CodeableConcept'(),
	category :: [complex:'CodeableConcept'()],
	patient :: special:'Reference'() | undefined,
	dateTime :: dateTime() | undefined,
	performer :: [special:'Reference'()] | undefined,
	organization :: [special:'Reference'()] | undefined,
	choice :: special:'Reference'() | complex:'Attachment'() | undefined,
	policy :: [complex:'Consent.Policy'()] | undefined,
	policyRule :: complex:'CodeableConcept'() | undefined,
	verification :: [complex:'Consent.Verification'()] | undefined,
	provision :: complex:'Consent.Provision'() | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_consent.Data({Props}) ->  to_consent.Data(Props);
to_consent.Data(Props) -> 
  DT = decode:xsd_info(<<"Consent.Data">>),
  #'Consent.Data'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	meaning :: complex:'ConsentDataMeaning'(),
	reference :: special:'Reference'()}).
    }.

to_consent.Actor({Props}) ->  to_consent.Actor(Props);
to_consent.Actor(Props) -> 
  DT = decode:xsd_info(<<"Consent.Actor">>),
  #'Consent.Actor'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	role :: complex:'CodeableConcept'(),
	reference :: special:'Reference'()}).
    }.

to_consent.Provision({Props}) ->  to_consent.Provision(Props);
to_consent.Provision(Props) -> 
  DT = decode:xsd_info(<<"Consent.Provision">>),
  #'Consent.Provision'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: complex:'ConsentProvisionType'() | undefined,
	period :: complex:'Period'() | undefined,
	actor :: [complex:'Consent.Actor'()] | undefined,
	action :: [complex:'CodeableConcept'()] | undefined,
	securityLabel :: [complex:'Coding'()] | undefined,
	purpose :: [complex:'Coding'()] | undefined,
	class :: [complex:'Coding'()] | undefined,
	code :: [complex:'CodeableConcept'()] | undefined,
	dataPeriod :: complex:'Period'() | undefined,
	data :: [complex:'Consent.Data'()] | undefined,
	provision :: [complex:'Consent.Provision'()] | undefined}).
    }.


to_consent.Verification({Props}) ->  to_consent.Verification(Props);
to_consent.Verification(Props) -> 
  DT = decode:xsd_info(<<"Consent.Verification">>),
  #'Consent.Verification'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	verified :: boolean(),
	verifiedWith :: special:'Reference'() | undefined,
	verificationDate :: dateTime() | undefined}).
    }.


to_consent.Policy({Props}) ->  to_consent.Policy(Props);
to_consent.Policy(Props) -> 
  DT = decode:xsd_info(<<"Consent.policy">>),
  #'Consent.Policy'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	authority :: uri() | undefined,
	uri :: uri() | undefined}).
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
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Consent',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
consent_toprop_test() ->
    ?asrtp({'Consent',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Consent">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

consent_json_test() ->
    ?asrtjson({'Consent',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Consent\",\"id\":\"p-21666\"}">>).

-endif.


