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
	endpoint :: ['CapabilityStatement.Endpoint'()] | undefined,
	reliableCache :: unsignedInt() | undefined,
	documentation :: markdown() | undefined,
	supportedMessage :: ['CapabilityStatement.SupportedMessage'()] | undefined}).

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
	interaction :: ['CapabilityStatement.Interaction'()] | undefined,
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
	searchParam :: ['CapabilityStatement.SearchParam'()] | undefined,
	operation :: ['CapabilityStatement.Operation'()] | undefined}).

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
	security :: 'CapabilityStatement.Security'() | undefined,
	resource :: ['CapabilityStatement.Resource'()] | undefined,
	interaction :: ['CapabilityStatement.Interaction1'()] | undefined,
	searchParam :: ['CapabilityStatement.SearchParam'()] | undefined,
	operation :: ['CapabilityStatement.Operation'()] | undefined,
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
	meta :: complex:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: complex:'Narrative'() | undefined,
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
	software :: 'CapabilityStatement.Software'() | undefined,
	implementation :: 'CapabilityStatement.Implementation'() | undefined,
	fhirVersion :: code(),
	format :: [code()],
	patchFormat :: [code()] | undefined,
	implementationGuide :: [canonical()] | undefined,
	rest :: ['CapabilityStatement.Rest'()] | undefined,
	messaging :: ['CapabilityStatement.Messaging'()] | undefined,
	document :: ['CapabilityStatement.Document'()] | undefined}).

-type 'CapabilityStatement'() :: #'CapabilityStatement'{}.


