-module(capabilities).

-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").
-include("erlsom_types.hrl").

-record('CapabilityStatement.Document', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	mode :: code(),
	documentation :: markdown() | undefined,
	profile :: canonical()}).

-type 'CapabilityStatement.Document'() :: #'CapabilityStatement.Document'{}.


-record('CapabilityStatement.SupportedMessage', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	mode :: code(),
	definition :: canonical()}).

-type 'CapabilityStatement.SupportedMessage'() :: #'CapabilityStatement.SupportedMessage'{}.


-record('CapabilityStatement.Endpoint', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	protocol :: complex:'Coding'(),
	address :: url()}).

-type 'CapabilityStatement.Endpoint'() :: #'CapabilityStatement.Endpoint'{}.


-record('CapabilityStatement.Messaging', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	endpoint :: [complex:'CapabilityStatement.Endpoint'()] | undefined,
	reliableCache :: unsignedInt() | undefined,
	documentation :: markdown() | undefined,
	supportedMessage :: [complex:'CapabilityStatement.SupportedMessage'()] | undefined}).

-type 'CapabilityStatement.Messaging'() :: #'CapabilityStatement.Messaging'{}.


-record('CapabilityStatement.Interaction1', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	code :: code(),
	documentation :: markdown() | undefined}).

-type 'CapabilityStatement.Interaction1'() :: #'CapabilityStatement.Interaction1'{}.


-record('CapabilityStatement.Operation', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	name :: string(),
	definition :: canonical(),
	documentation :: markdown() | undefined}).

-type 'CapabilityStatement.Operation'() :: #'CapabilityStatement.Operation'{}.


-record('CapabilityStatement.SearchParam', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	name :: string(),
	definition :: canonical() | undefined,
	type :: code(),
	documentation :: markdown() | undefined}).

-type 'CapabilityStatement.SearchParam'() :: #'CapabilityStatement.SearchParam'{}.


-record('CapabilityStatement.Interaction', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	code :: code(),
	documentation :: markdown() | undefined}).

-type 'CapabilityStatement.Interaction'() :: #'CapabilityStatement.Interaction'{}.


-record('CapabilityStatement.Resource', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	type :: code(),
	profile :: canonical() | undefined,
	supportedProfile :: [canonical()] | undefined,
	documentation :: markdown() | undefined,
	interaction :: [complex:'CapabilityStatement.Interaction'()] | undefined,
	versioning :: code() | undefined,
	readHistory :: boolean() | undefined,
	updateCreate :: boolean() | undefined,
	conditionalCreate :: boolean() | undefined,
	conditionalRead :: code() | undefined,
	conditionalUpdate :: boolean() | undefined,
	conditionalDelete :: code() | undefined,
	referencePolicy :: [code()] | undefined,
	searchInclude :: [string()] | undefined,
	searchRevInclude :: [string()] | undefined,
	searchParam :: [complex:'CapabilityStatement.SearchParam'()] | undefined,
	operation :: [complex:'CapabilityStatement.Operation'()] | undefined}).

-type 'CapabilityStatement.Resource'() :: #'CapabilityStatement.Resource'{}.


-record('CapabilityStatement.Security', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	cors :: boolean() | undefined,
	service :: [complex:'CodeableConcept'()] | undefined,
	description :: markdown() | undefined}).

-type 'CapabilityStatement.Security'() :: #'CapabilityStatement.Security'{}.


-record('CapabilityStatement.Rest', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	mode :: code(),
	documentation :: markdown() | undefined,
	security :: complex:'CapabilityStatement.Security'() | undefined,
	resource :: [complex:'CapabilityStatement.Resource'()] | undefined,
	interaction :: [complex:'CapabilityStatement.Interaction1'()] | undefined,
	searchParam :: [complex:'CapabilityStatement.SearchParam'()] | undefined,
	operation :: [complex:'CapabilityStatement.Operation'()] | undefined,
	compartment :: [canonical()] | undefined}).

-type 'CapabilityStatement.Rest'() :: #'CapabilityStatement.Rest'{}.


-record('CapabilityStatement.Implementation', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	description :: string(),
	url :: url() | undefined,
	custodian :: special:'Reference'() | undefined}).

-type 'CapabilityStatement.Implementation'() :: #'CapabilityStatement.Implementation'{}.


-record('CapabilityStatement.Software', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	name :: string(),
	version :: string() | undefined,
	releaseDate :: dateTime() | undefined}).

-type 'CapabilityStatement.Software'() :: #'CapabilityStatement.Software'{}.


-record('CapabilityStatement', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	url :: uri() | undefined,
	version :: string() | undefined,
	name :: string() | undefined,
	title :: string() | undefined,
	status :: code() | undefined,
	experimental :: boolean() | undefined,
	date :: dateTime(),
	publisher :: string() | undefined,
	contact :: [metadata:'ContactDetail'()] | undefined,
	description :: markdown() | undefined,
	useContext :: [code()] | undefined,
	jurisdiction :: [complex:'CodeableConcept'()] | undefined,
	purpose :: markdown() | undefined,
	copyright :: markdown() | undefined,
	kind :: code(),
	instantiates :: [canonical()] | undefined,
	imports :: [canonical()] | undefined,
	software :: complex:'CapabilityStatement.Software'() | undefined,
	implementation :: complex:'CapabilityStatement.Implementation'() | undefined,
	fhirVersion :: code(),
	format :: [code()],
	patchFormat :: [code()] | undefined,
	implementationGuide :: [canonical()] | undefined,
	rest :: [complex:'CapabilityStatement.Rest'()] | undefined,
	messaging :: [complex:'CapabilityStatement.Messaging'()] | undefined,
	document :: [complex:'CapabilityStatement.Document'()] | undefined}).

-type 'CapabilityStatement'() :: #'CapabilityStatement'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_capabilityStatement({Props}) -> to_capabilityStatement(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement">>),
  #'CapabilityStatement'{ 
      anyAttribs = decode:attrs(Props, DT) 
    , id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , url  = decode:value(<<"url">>, Props, DT) 
    , version  = decode:value(<<"version">>, Props, DT) 
    , name  = decode:value(<<"name">>, Props, DT) 
    , title  = decode:value(<<"title">>, Props, DT) 
    , status  = decode:value(<<"status">>, Props, DT) 
    , experimental  = decode:value(<<"experimental">>, Props, DT) 
    , date  = decode:value(<<"date">>, Props, DT) 
    , publisher  = decode:value(<<"publisher">>, Props, DT) 
    , contact  = decode:value(<<"contact">>, Props, DT) 
    , description  = decode:value(<<"description">>, Props, DT) 
    , useContext  = decode:value(<<"useContext">>, Props, DT) 
    , jurisdiction  = decode:value(<<"jurisdiction">>, Props, DT) 
    , purpose  = decode:value(<<"purpose">>, Props, DT) 
    , copyright  = decode:value(<<"copyright">>, Props, DT) 
    , kind  = decode:value(<<"kind">>, Props, DT) 
    , instantiates  = decode:value(<<"instantiates">>, Props, DT) 
    , imports  = decode:value(<<"imports">>, Props, DT) 
    , software  = decode:value(<<"software">>, Props, DT) 
    , implementation  = decode:value(<<"implementation">>, Props, DT) 
    , fhirVersion  = decode:value(<<"fhirVersion">>, Props, DT) 
    , format  = decode:value(<<"format">>, Props, DT) 
    , patchFormat  = decode:value(<<"patchFormat">>, Props, DT) 
    , implementationGuide  = decode:value(<<"implementationGuide">>, Props, DT) 
    , rest  = decode:value(<<"rest">>, Props, DT) 
    , messaging  = decode:value(<<"messaging">>, Props, DT) 
    , document  = decode:value(<<"document">>, Props, DT) 
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_capabilityStatement.Document({Props}) -> to_capabilityStatement.Document(Props);
to_capabilityStatement.Document(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Document">>),
  #'CapabilityStatement.Document'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , mode  = decode:value(<<"mode">>, Props, DT) 
    , documentation  = decode:value(<<"documentation">>, Props, DT) 
    , profile  = decode:value(<<"profile">>, Props, DT) 
    }.



to_capabilityStatement.SupportedMessage({Props}) -> to_capabilityStatement.SupportedMessage(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.SupportedMessage">>),
  #'CapabilityStatement.SupportedMessage'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , mode  = decode:value(<<"mode">>, Props, DT) 
    , definition  = decode:value(<<"definition">>, Props, DT) 
    }.



to_capabilityStatement.Endpoint({Props}) -> to_capabilityStatement.Endpoint(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Endpoint">>),
  #'CapabilityStatement.Endpoint'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , protocol  = decode:value(<<"protocol">>, Props, DT) 
    , address  = decode:value(<<"address">>, Props, DT) 
    }.



to_capabilityStatement.Messaging({Props}) -> to_capabilityStatement.Messaging(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Messaging">>),
  #'CapabilityStatement.Messaging'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , endpoint  = decode:value(<<"endpoint">>, Props, DT) 
    , reliableCache  = decode:value(<<"reliableCache">>, Props, DT) 
    , documentation  = decode:value(<<"documentation">>, Props, DT) 
    , supportedMessage  = decode:value(<<"supportedMessage">>, Props, DT) 
    }.


to_capabilityStatement.Interaction1({Props}) -> to_capabilityStatement.Interaction1(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Interaction1">>),
  #'CapabilityStatement.Interaction1'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , code  = decode:value(<<"code">>, Props, DT) 
    , documentation  = decode:value(<<"documentation">>, Props, DT) 
    }.



to_capabilityStatement.Operation({Props}) -> to_capabilityStatement.Operation(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Operation">>),
  #'CapabilityStatement.Operation'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , name  = decode:value(<<"name">>, Props, DT) 
    , definition  = decode:value(<<"definition">>, Props, DT) 
    , documentation  = decode:value(<<"documentation">>, Props, DT) 
    }.



to_capabilityStatement.SearchParam({Props}) -> to_capabilityStatement.SearchParam(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.SearchParam">>),
  #'CapabilityStatement.SearchParam'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , name  = decode:value(<<"name">>, Props, DT) 
    , definition  = decode:value(<<"definition">>, Props, DT) 
    , type  = decode:value(<<"type">>, Props, DT) 
    , documentation  = decode:value(<<"documentation">>, Props, DT) 


to_capabilityStatement.Interaction({Props}) -> to_capabilityStatement.Interaction(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Interaction">>),
  #'CapabilityStatement.Interaction'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , code  = decode:value(<<"code">>, Props, DT) 
    , documentation  = decode:value(<<"documentation">>, Props, DT) 
    }.



to_capabilityStatement.Resource({Props}) -> to_capabilityStatement.Resource(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Resource">>),
  #'CapabilityStatement.Resource'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , type  = decode:value(<<"type">>, Props, DT) 
    , profile  = decode:value(<<"profile">>, Props, DT) 
    , supportedProfile  = decode:value(<<"supportedProfile">>, Props, DT) 
    , documentation  = decode:value(<<"documentation">>, Props, DT) 
    , interaction  = decode:value(<<"interaction">>, Props, DT) 
    , versioning  = decode:value(<<"versioning">>, Props, DT) 
    , readHistory  = decode:value(<<"readHistory">>, Props, DT) 
    , updateCreate  = decode:value(<<"updateCreate">>, Props, DT) 
    , conditionalCreate  = decode:value(<<"conditionalCreate">>, Props, DT) 
    , conditionalRead  = decode:value(<<"conditionalRead">>, Props, DT) 
    , conditionalUpdate  = decode:value(<<"conditionalUpdate">>, Props, DT) 
    , conditionalDelete  = decode:value(<<"conditionalDelete">>, Props, DT) 
    , referencePolicy  = decode:value(<<"referencePolicy">>, Props, DT) 
    , searchInclude  = decode:value(<<"searchInclude">>, Props, DT) 
    , searchRevInclude  = decode:value(<<"searchRevInclude">>, Props, DT) 
    , searchParam  = decode:value(<<"searchParam">>, Props, DT) 
    , operation  = decode:value(<<"operation">>, Props, DT) 
    }.



to_capabilityStatement.Security({Props}) -> to_capabilityStatement.Security(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Security">>),
  #'CapabilityStatement.Security'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , cors  = decode:value(<<"cors">>, Props, DT) 
    , service  = decode:value(<<"service">>, Props, DT) 
    , description  = decode:value(<<"description">>, Props, DT) 
    }.



to_capabilityStatement.Rest({Props}) -> to_capabilityStatement.Rest(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Rest">>),
  #'CapabilityStatement.Rest'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , mode  = decode:value(<<"mode">>, Props, DT) 
    , documentation  = decode:value(<<"documentation">>, Props, DT) 
    , security  = decode:value(<<"security">>, Props, DT) 
    , resource  = decode:value(<<"resource">>, Props, DT) 
    , interaction  = decode:value(<<"interaction">>, Props, DT) 
    , searchParam  = decode:value(<<"searchParam">>, Props, DT) 
    , operation  = decode:value(<<"operation">>, Props, DT) 
    , compartment  = decode:value(<<"compartment">>, Props, DT) 
    }.



to_capabilityStatement.Implementation({Props}) -> to_capabilityStatement.Implementation(Props);
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Implementation">>),
  #'CapabilityStatement.Implementation'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , description  = decode:value(<<"description">>, Props, DT) 
    , url  = decode:value(<<"url">>, Props, DT) 
    , custodian  = decode:value(<<"custodian">>, Props, DT) 
    }.



to_capabilityStatement.Software({Props}) -> to_capabilityStatement.Software({Props});
to_capabilityStatement(Props) ->
  DT = decode:xsd_info(<<"CapabilityStatement.Software">>),
  #'CapabilityStatement.Software'{ 
    anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT) 
    , extension  = decode:value(<<"extension">>, Props, DT) 
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT) 
    , name  = decode:value(<<"name">>, Props, DT) 
    , version  = decode:value(<<"version">>, Props, DT) 
    , releaseDate  = decode:value(<<"releaseDate">>, Props, DT) 
    }.





text(#'CapabilityStatement'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, capabilityStatement:to_capabilityStatement(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

capabilityStatement_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'CapabilityStatement',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
capabilityStatement_toprop_test() ->
    ?asrtp({'CapabilityStatement',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"CapabilityStatement">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

capabilityStatement_json_test() ->
    ?asrtjson({'CapabilityStatement',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"CapabilityStatement\",\"id\":\"p-21666\"}">>).

-endif.


