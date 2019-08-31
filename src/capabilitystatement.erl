-module(capabilitystatement).
%%%
%%% FHIR 4.0.0 CapabilityStatement
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('CapabilityStatement', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , url :: uri() | undefined
    , version :: string() | undefined
    , name :: string() | undefined
    , title :: string() | undefined
    , status :: code()
    , experimental :: boolean() | undefined
    , date :: dateTime()
    , publisher :: string() | undefined
    , contact :: [complex:'ContactDetail'()]
    , description :: markdown() | undefined
    , useContext :: [complex:'UsageContext'()]
    , jurisdiction :: [complex:'CodeableConcept'()]
    , purpose :: markdown() | undefined
    , copyright :: markdown() | undefined
    , kind :: code()
    , instantiates :: [canonical()]
    , imports :: [canonical()]
    , software :: 'CapabilityStatement.Software'() | undefined
    , implementation :: 'CapabilityStatement.Implementation'() | undefined
    , fhirVersion :: code()
    , format :: [code()]
    , patchFormat :: [code()]
    , implementationGuide :: [canonical()]
    , rest :: ['CapabilityStatement.Rest'()]
    , messaging :: ['CapabilityStatement.Messaging'()]
    , document :: ['CapabilityStatement.Document'()]
    }).
-type 'CapabilityStatement'() :: #'CapabilityStatement'{}.


-record('CapabilityStatement.Implementation', { anyAttribs :: anyAttribs(),
      description :: string()
    , url :: url() | undefined
    , custodian :: special:'Reference'() | undefined
    }).
-type 'CapabilityStatement.Implementation'() :: #'CapabilityStatement.Implementation'{}.


-record('CapabilityStatement.Rest', { anyAttribs :: anyAttribs(),
      mode :: code()
    , documentation :: markdown() | undefined
    , security :: 'CapabilityStatement.Security'() | undefined
    , resource :: ['CapabilityStatement.Resource'()]
    , interaction :: ['CapabilityStatement.Interaction1'()]
    , searchParam :: ['CapabilityStatement.SearchParam'()]
    , operation :: ['CapabilityStatement.Operation'()]
    , compartment :: [canonical()]
    }).
-type 'CapabilityStatement.Rest'() :: #'CapabilityStatement.Rest'{}.


-record('CapabilityStatement.Document', { anyAttribs :: anyAttribs(),
      mode :: code()
    , documentation :: markdown() | undefined
    , profile :: canonical()
    }).
-type 'CapabilityStatement.Document'() :: #'CapabilityStatement.Document'{}.


-record('CapabilityStatement.Software', { anyAttribs :: anyAttribs(),
      name :: string()
    , version :: string() | undefined
    , releaseDate :: dateTime() | undefined
    }).
-type 'CapabilityStatement.Software'() :: #'CapabilityStatement.Software'{}.


-record('CapabilityStatement.Messaging', { anyAttribs :: anyAttribs(),
      endpoint :: ['CapabilityStatement.Endpoint'()]
    , reliableCache :: unsignedInt() | undefined
    , documentation :: markdown() | undefined
    , supportedMessage :: ['CapabilityStatement.SupportedMessage'()]
    }).
-type 'CapabilityStatement.Messaging'() :: #'CapabilityStatement.Messaging'{}.


-record('CapabilityStatement.SupportedMessage', { anyAttribs :: anyAttribs(),
      mode :: code()
    , definition :: canonical()
    }).
-type 'CapabilityStatement.SupportedMessage'() :: #'CapabilityStatement.SupportedMessage'{}.


-record('CapabilityStatement.Endpoint', { anyAttribs :: anyAttribs(),
      protocol :: complex:'Coding'()
    , address :: url()
    }).
-type 'CapabilityStatement.Endpoint'() :: #'CapabilityStatement.Endpoint'{}.


-record('CapabilityStatement.Interaction1', { anyAttribs :: anyAttribs(),
      code :: code()
    , documentation :: markdown() | undefined
    }).
-type 'CapabilityStatement.Interaction1'() :: #'CapabilityStatement.Interaction1'{}.


-record('CapabilityStatement.Operation', { anyAttribs :: anyAttribs(),
      name :: string()
    , definition :: canonical()
    , documentation :: markdown() | undefined
    }).
-type 'CapabilityStatement.Operation'() :: #'CapabilityStatement.Operation'{}.


-record('CapabilityStatement.Security', { anyAttribs :: anyAttribs(),
      cors :: boolean() | undefined
    , service :: [complex:'CodeableConcept'()]
    , description :: markdown() | undefined
    }).
-type 'CapabilityStatement.Security'() :: #'CapabilityStatement.Security'{}.


-record('CapabilityStatement.Interaction', { anyAttribs :: anyAttribs(),
      code :: code()
    , documentation :: markdown() | undefined
    }).
-type 'CapabilityStatement.Interaction'() :: #'CapabilityStatement.Interaction'{}.


-record('CapabilityStatement.SearchParam', { anyAttribs :: anyAttribs(),
      name :: string()
    , definition :: canonical() | undefined
    , type :: code()
    , documentation :: markdown() | undefined
    }).
-type 'CapabilityStatement.SearchParam'() :: #'CapabilityStatement.SearchParam'{}.


-record('CapabilityStatement.Resource', { anyAttribs :: anyAttribs(),
      type :: code()
    , profile :: canonical() | undefined
    , supportedProfile :: [canonical()]
    , documentation :: markdown() | undefined
    , interaction :: ['CapabilityStatement.Interaction'()]
    , versioning :: code() | undefined
    , readHistory :: boolean() | undefined
    , updateCreate :: boolean() | undefined
    , conditionalCreate :: boolean() | undefined
    , conditionalRead :: code() | undefined
    , conditionalUpdate :: boolean() | undefined
    , conditionalDelete :: code() | undefined
    , referencePolicy :: [code()]
    , searchInclude :: [string()]
    , searchRevInclude :: [string()]
    , searchParam :: ['CapabilityStatement.SearchParam'()]
    , operation :: ['CapabilityStatement.Operation'()]
    }).
-type 'CapabilityStatement.Resource'() :: #'CapabilityStatement.Resource'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_capabilityStatement({Props}) -> to_capabilityStatement(Props);
to_capabilityStatement(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement">>),
    #'CapabilityStatement'{
      anyAttribs = decode:attrs(Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , meta = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
    , language = decode:value(<<"language">>, Props, DT)
    , text = decode:value(<<"text">>, Props, DT)
    , contained = decode:value(<<"contained">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , url = decode:value(<<"url">>, Props, DT)
    , version = decode:value(<<"version">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , title = decode:value(<<"title">>, Props, DT)
    , status = decode:value(<<"status">>, Props, DT)
    , experimental = decode:value(<<"experimental">>, Props, DT)
    , date = decode:value(<<"date">>, Props, DT)
    , publisher = decode:value(<<"publisher">>, Props, DT)
    , contact = decode:value(<<"contact">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , useContext = decode:value(<<"useContext">>, Props, DT)
    , jurisdiction = decode:value(<<"jurisdiction">>, Props, DT)
    , purpose = decode:value(<<"purpose">>, Props, DT)
    , copyright = decode:value(<<"copyright">>, Props, DT)
    , kind = decode:value(<<"kind">>, Props, DT)
    , instantiates = decode:value(<<"instantiates">>, Props, DT)
    , imports = decode:value(<<"imports">>, Props, DT)
    , software = decode:value(<<"software">>, Props, DT)
    , implementation = decode:value(<<"implementation">>, Props, DT)
    , fhirVersion = decode:value(<<"fhirVersion">>, Props, DT)
    , format = decode:value(<<"format">>, Props, DT)
    , patchFormat = decode:value(<<"patchFormat">>, Props, DT)
    , implementationGuide = decode:value(<<"implementationGuide">>, Props, DT)
    , rest = decode:value(<<"rest">>, Props, DT)
    , messaging = decode:value(<<"messaging">>, Props, DT)
    , document = decode:value(<<"document">>, Props, DT)
    }.

to_capabilityStatement_implementation({Props}) -> to_capabilityStatement_implementation(Props);
to_capabilityStatement_implementation(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Implementation">>),
    #'CapabilityStatement.Implementation'{
      anyAttribs = decode:attrs(Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , url = decode:value(<<"url">>, Props, DT)
    , custodian = decode:value(<<"custodian">>, Props, DT)
    }.

to_capabilityStatement_rest({Props}) -> to_capabilityStatement_rest(Props);
to_capabilityStatement_rest(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Rest">>),
    #'CapabilityStatement.Rest'{
      anyAttribs = decode:attrs(Props, DT)
    , mode = decode:value(<<"mode">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    , security = decode:value(<<"security">>, Props, DT)
    , resource = decode:value(<<"resource">>, Props, DT)
    , interaction = decode:value(<<"interaction">>, Props, DT)
    , searchParam = decode:value(<<"searchParam">>, Props, DT)
    , operation = decode:value(<<"operation">>, Props, DT)
    , compartment = decode:value(<<"compartment">>, Props, DT)
    }.

to_capabilityStatement_document({Props}) -> to_capabilityStatement_document(Props);
to_capabilityStatement_document(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Document">>),
    #'CapabilityStatement.Document'{
      anyAttribs = decode:attrs(Props, DT)
    , mode = decode:value(<<"mode">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    , profile = decode:value(<<"profile">>, Props, DT)
    }.

to_capabilityStatement_software({Props}) -> to_capabilityStatement_software(Props);
to_capabilityStatement_software(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Software">>),
    #'CapabilityStatement.Software'{
      anyAttribs = decode:attrs(Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , version = decode:value(<<"version">>, Props, DT)
    , releaseDate = decode:value(<<"releaseDate">>, Props, DT)
    }.

to_capabilityStatement_messaging({Props}) -> to_capabilityStatement_messaging(Props);
to_capabilityStatement_messaging(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Messaging">>),
    #'CapabilityStatement.Messaging'{
      anyAttribs = decode:attrs(Props, DT)
    , endpoint = decode:value(<<"endpoint">>, Props, DT)
    , reliableCache = decode:value(<<"reliableCache">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    , supportedMessage = decode:value(<<"supportedMessage">>, Props, DT)
    }.

to_capabilityStatement_supportedMessage({Props}) -> to_capabilityStatement_supportedMessage(Props);
to_capabilityStatement_supportedMessage(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.SupportedMessage">>),
    #'CapabilityStatement.SupportedMessage'{
      anyAttribs = decode:attrs(Props, DT)
    , mode = decode:value(<<"mode">>, Props, DT)
    , definition = decode:value(<<"definition">>, Props, DT)
    }.

to_capabilityStatement_endpoint({Props}) -> to_capabilityStatement_endpoint(Props);
to_capabilityStatement_endpoint(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Endpoint">>),
    #'CapabilityStatement.Endpoint'{
      anyAttribs = decode:attrs(Props, DT)
    , protocol = decode:value(<<"protocol">>, Props, DT)
    , address = decode:value(<<"address">>, Props, DT)
    }.

to_capabilityStatement_interaction1({Props}) -> to_capabilityStatement_interaction1(Props);
to_capabilityStatement_interaction1(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Interaction1">>),
    #'CapabilityStatement.Interaction1'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    }.

to_capabilityStatement_operation({Props}) -> to_capabilityStatement_operation(Props);
to_capabilityStatement_operation(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Operation">>),
    #'CapabilityStatement.Operation'{
      anyAttribs = decode:attrs(Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , definition = decode:value(<<"definition">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    }.

to_capabilityStatement_security({Props}) -> to_capabilityStatement_security(Props);
to_capabilityStatement_security(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Security">>),
    #'CapabilityStatement.Security'{
      anyAttribs = decode:attrs(Props, DT)
    , cors = decode:value(<<"cors">>, Props, DT)
    , service = decode:value(<<"service">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    }.

to_capabilityStatement_interaction({Props}) -> to_capabilityStatement_interaction(Props);
to_capabilityStatement_interaction(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Interaction">>),
    #'CapabilityStatement.Interaction'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    }.

to_capabilityStatement_searchParam({Props}) -> to_capabilityStatement_searchParam(Props);
to_capabilityStatement_searchParam(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.SearchParam">>),
    #'CapabilityStatement.SearchParam'{
      anyAttribs = decode:attrs(Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , definition = decode:value(<<"definition">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    }.

to_capabilityStatement_resource({Props}) -> to_capabilityStatement_resource(Props);
to_capabilityStatement_resource(Props) -> 
    DT = decode:xsd_info(<<"CapabilityStatement.Resource">>),
    #'CapabilityStatement.Resource'{
      anyAttribs = decode:attrs(Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , profile = decode:value(<<"profile">>, Props, DT)
    , supportedProfile = decode:value(<<"supportedProfile">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    , interaction = decode:value(<<"interaction">>, Props, DT)
    , versioning = decode:value(<<"versioning">>, Props, DT)
    , readHistory = decode:value(<<"readHistory">>, Props, DT)
    , updateCreate = decode:value(<<"updateCreate">>, Props, DT)
    , conditionalCreate = decode:value(<<"conditionalCreate">>, Props, DT)
    , conditionalRead = decode:value(<<"conditionalRead">>, Props, DT)
    , conditionalUpdate = decode:value(<<"conditionalUpdate">>, Props, DT)
    , conditionalDelete = decode:value(<<"conditionalDelete">>, Props, DT)
    , referencePolicy = decode:value(<<"referencePolicy">>, Props, DT)
    , searchInclude = decode:value(<<"searchInclude">>, Props, DT)
    , searchRevInclude = decode:value(<<"searchRevInclude">>, Props, DT)
    , searchParam = decode:value(<<"searchParam">>, Props, DT)
    , operation = decode:value(<<"operation">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, capabilitystatement:to_capabilityStatement(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
