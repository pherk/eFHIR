-module(metadata).
%-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-export([
         to_contactDetail/1,
         to_dataRequirement/1,
         to_dataRequirement_sort/1,
         to_dataRequirement_codeFilter/1,
         to_dataRequirement_dateFilter/1, 
         to_expression/1, 
         to_parameterDefinition/1,
         to_productShelfLife/1,
         to_relatedArtifact/1,
         to_triggerDefinition/1,
         to_usageContext/1
        ]).
-export_type([
              'ContactDetail'/0,
              'DataRequirement'/0,
              'Expression'/0, 
              'ParameterDefinition'/0,
              'ProductShelfLife'/0,
              'RelatedArtifact'/0,
              'TriggerDefinition'/0,
              'UsageContext'/0
             ]).
%%
%% API exports
%%
-record('Expression', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	description :: string() | undefined,
	name :: id() | undefined,
	language :: code(),
	expression :: string() | undefined,
	reference :: uri() | undefined}).

-type 'Expression'() :: #'Expression'{}.

-record('UsageContext', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	code :: complex:'Coding'(),
	value :: special:'Reference'() | complex:'Range'() | complex:'Quantity'() | complex:'Duration'() | complex:'Age'() | complex:'Distance'() | complex:'Count'() | complex:'CodeableConcept'()}).

-type 'UsageContext'() :: #'UsageContext'{}.


-record('ContactDetail', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	name :: string() | undefined,
	telecom :: [complex:'ContactPoint'()] | undefined}).

-type 'ContactDetail'() :: #'ContactDetail'{}.


-record('ProductShelfLife', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: complex:'Identifier'() | undefined,
	type :: complex:'CodeableConcept'(),
	period :: complex:'Quantity'(),
	specialPrecautionsForStorage :: [complex:'CodeableConcept'()] | undefined}).

-type 'ProductShelfLife'() :: #'ProductShelfLife'{}.


-record('RelatedArtifact', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	type :: code(),
	label :: string() | undefined,
	display :: string() | undefined,
	citation :: markdown() | undefined,
	url :: url() | undefined,
	document :: complex:'Attachment'() | undefined,
	resource :: canonical() | undefined}).

-opaque 'RelatedArtifact'() :: #'RelatedArtifact'{}.

-record('TriggerDefinition', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extension:'Extension'()] | undefined,
	type :: code(),
	name :: string() | undefined,
	timing :: complex:'Timing'() | special:'Reference'() | dateTime() | date() | undefined,
	data :: [complex:'DataRequirement'()] | undefined,
	condition :: metadata:'Expression'() | undefined}).

-type 'TriggerDefinition'() :: #'TriggerDefinition'{}.

-record('DataRequirement.Sort', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	path :: string(),
	direction :: code()}).

-type 'DataRequirement.Sort'() :: #'DataRequirement.Sort'{}.


-record('DataRequirement.DateFilter', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	path :: string() | undefined,
	searchParam :: string() | undefined,
	value :: complex:'Period'() | complex:'Duration'() | dateTime() | undefined}).

-type 'DataRequirement.DateFilter'() :: #'DataRequirement.DateFilter'{}.


-record('DataRequirement.CodeFilter', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	path :: string() | undefined,
	searchParam :: string() | undefined,
	valueSet :: canonical() | undefined,
	code :: [complex:'Coding'()] | undefined}).

-type 'DataRequirement.CodeFilter'() :: #'DataRequirement.CodeFilter'{}.


-record('DataRequirement', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	type :: code(),
	profile :: [canonical()] | undefined,
	subject :: special:'Reference'() | complex:'CodeableConcept'() | undefined,
	mustSupport :: [string()] | undefined,
	codeFilter :: ['DataRequirement.CodeFilter'()] | undefined,
	dateFilter :: ['DataRequirement.DateFilter'()] | undefined,
	limit :: positiveInt() | undefined,
	sort :: ['DataRequirement.Sort'()] | undefined}).

-type 'DataRequirement'() :: #'DataRequirement'{}.

-record('ParameterDefinition', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	name :: code() | undefined,
	use :: code(),
	min :: integer() | undefined,
	max :: string() | undefined,
	documentation :: string() | undefined,
	type :: code(),
	profile :: canonical() | undefined}).

-type 'ParameterDefinition'() :: #'ParameterDefinition'{}.

%%====================================================================
%% API functions
%%====================================================================
to_expression({Props}) -> to_expression(Props);
to_expression(Props) ->
    DT = xsd:get_info(<<"Expression">>),
    #'Expression'{
      anyAttribs = decode:attrs(<<"anyAttribs">>, Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , language = decode:value(<<"language">>, Props, DT)
    , expression = decode:value(<<"expression">>, Props, DT)
    , reference = decode:value(<<"reference">>, Props, DT)
  }.

to_usageContext({Props}) -> to_usageContext(Props);
to_usageContext(Props) ->
    DT = xsd:get_info(<<"UsageContext">>),
    #'UsageContext'{
      anyAttribs = decode:attrs(<<"anyAttribs">>, Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , value = decode:value(<<"value">>, Props, DT)
  }.

to_contactDetail({Props}) -> to_contactDetail(Props);
to_contactDetail(Props) ->
    DT = xsd:get_info(<<"ContactDetail">>),
    #'ContactDetail'{
      anyAttribs = decode:attrs(<<"anyAttribs">>, Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , telecom = decode:value(<<"telecom">>, Props, DT)
  }.

to_productShelfLife({Props}) -> to_productShelfLife(Props);
to_productShelfLife(Props) ->
    DT = xsd:get_info(<<"ProductShelfLife">>),
    #'ProductShelfLife'{
       anyAttribs = decode:attrs(<<"anyAttribs">>, Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , identifier = decode:value(<<"identifier">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , period = decode:value(<<"period">>, Props, DT)
    , specialPrecautionsForStorage = decode:value(<<"specialPrecautionsForStorage">>, Props, DT)
  }.

to_relatedArtifact({Props}) -> to_relatedArtifact(Props);
to_relatedArtifact(Props) ->
    DT = xsd:get_info(<<"RelatedArtifact">>),
    #'RelatedArtifact'{
      anyAttribs = decode:attrs(<<"anyAttribs">>, Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , label = decode:value(<<"label">>, Props, DT)
    , display = decode:value(<<"display">>, Props, DT)
    , citation = decode:value(<<"citation">>, Props, DT)
    , url = decode:value(<<"url">>, Props, DT)
    , document = decode:value(<<"document">>, Props, DT)
    , resource = decode:value(<<"resource">>, Props, DT)
  }.

to_triggerDefinition({Props}) -> to_triggerDefinition(Props);
to_triggerDefinition(Props) ->
    DT = xsd:get_info(<<"TriggerDefinition">>),
    #'TriggerDefinition'{
      anyAttribs = decode:attrs(<<"anyAttribs">>, Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , timing = decode:value(<<"timing">>, Props, DT)
    , data = decode:value(<<"data">>, Props, DT)
    , condition = decode:value(<<"condition">>, Props, DT)
  }.


to_dataRequirement({Props}) -> to_dataRequirement(Props);
to_dataRequirement(Props) ->
    DT = xsd:get_info(<<"DataRequirement">>),
    #'DataRequirement'{
      anyAttribs = decode:attrs(<<"anyAttribs">>, Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , profile = decode:value(<<"profile">>, Props, DT)
    , subject = decode:value(<<"subject">>, Props, DT)
    , mustSupport = decode:value(<<"mustSupport">>, Props, DT)
    , codeFilter = decode:value(<<"codeFilter">>, Props, DT)
    , dateFilter = decode:value(<<"dateFilter">>, Props, DT)
    , limit = decode:value(<<"limit">>, Props, DT)
    , sort = decode:value(<<"sort">>, Props, DT)
  }.


to_parameterDefinition({Props}) -> to_parameterDefinition(Props);
to_parameterDefinition(Props) ->
    DT = xsd:get_info(<<"ParameterDefinition">>),
    #'ParameterDefinition'{
      anyAttribs = decode:attrs(<<"anyAttribs">>, Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , use = decode:value(<<"use">>, Props, DT)
    , min = decode:value(<<"min">>, Props, DT)
    , max = decode:value(<<"max">>, Props, DT)
    , documentation = decode:value(<<"documentation">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , profile = decode:value(<<"profile">>, Props, DT)
  }.

%%
%% Internal
%%
to_dataRequirement_sort({Props}) -> to_dataRequirement_sort(Props);
to_dataRequirement_sort(Props) ->
    DT = xsd:get_info(<<"DataRequirement.Sort">>),
    #'DataRequirement.Sort'{
      id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , path = decode:value(<<"path">>, Props, DT)
    , direction = decode:value(<<"direction">>, Props, DT)
  }.


to_dataRequirement_dateFilter({Props}) -> to_dataRequirement_dateFilter(Props);
to_dataRequirement_dateFilter(Props) ->
    DT = xsd:get_info(<<"DataRequirement.DateFilter">>),
    #'DataRequirement.DateFilter'{
      id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , path = decode:value(<<"path">>, Props, DT)
    , searchParam = decode:value(<<"searchParam">>, Props, DT)
    , value = decode:value(<<"value">>, Props, DT)
  }.

to_dataRequirement_codeFilter({Props}) -> to_dataRequirement_codeFilter(Props);
to_dataRequirement_codeFilter(Props) ->
    DT = xsd:get_info(<<"DataRequirement.CodeFilter">>),
    #'DataRequirement.CodeFilter'{
      id = decode:value(<<"id">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , path = decode:value(<<"path">>, Props, DT)
    , searchParam = decode:value(<<"searchParam">>, Props, DT)
    , valueSet = decode:value(<<"valueSet">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
  }.

%%%
%%% EUnit
%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, A)).
-define(asrtpr(A, B), ?assertEqual(B, fhir_utils:rec_to_prop(A))).

-endif.

