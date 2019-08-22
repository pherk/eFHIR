-module(imagingstudy).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").

-record('ImagingStudy.Instance', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	uid :: id(),
	sopClass :: complex:'Coding'(),
	number :: unsignedInt() | undefined,
	title :: string() | undefined}).

-type 'ImagingStudy.Instance'() :: #'ImagingStudy.Instance'{}.


-record('ImagingStudy.Performer', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	function :: complex:'CodeableConcept'() | undefined,
	actor :: special:'Reference'()}).

-type 'ImagingStudy.Performer'() :: #'ImagingStudy.Performer'{}.


-record('ImagingStudy.Series', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	uid :: id(),
	number :: unsignedInt() | undefined,
	modality :: complex:'Coding'(),
	description :: string() | undefined,
	numberOfInstances :: unsignedInt() | undefined,
	endpoint :: [special:'Reference'()] | undefined,
	bodySite :: complex:'Coding'() | undefined,
	laterality :: complex:'Coding'() | undefined,
	specimen :: [special:'Reference'()] | undefined,
	started :: dateTime() | undefined,
	performer :: [complex:'ImagingStudy.Performer'()] | undefined,
	instance :: [complex:'ImagingStudy.Instance'()] | undefined}).

-type 'ImagingStudy.Series'() :: #'ImagingStudy.Series'{}.


-record('ImagingStudy', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	status :: complex:'ImagingStudyStatus'(),
	modality :: [complex:'Coding'()] | undefined,
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	started :: dateTime() | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	referrer :: special:'Reference'() | undefined,
	interpreter :: [special:'Reference'()] | undefined,
	endpoint :: [special:'Reference'()] | undefined,
	numberOfSeries :: unsignedInt() | undefined,
	numberOfInstances :: unsignedInt() | undefined,
	procedureReference :: special:'Reference'() | undefined,
	procedureCode :: [complex:'CodeableConcept'()] | undefined,
	location :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	description :: string() | undefined,
	series :: [complex:'ImagingStudy.Series'()] | undefined}).

-type 'ImagingStudy'() :: #'ImagingStudy'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_imagingStudy({Props}) -> to_imagingStudy(Props);
to_imagingStudy(Props) ->
  DT = decode:xsd_info(<<"ImagingStudy">>),
  #'ImagingStudy'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	status :: complex:'ImagingStudyStatus'(),
	modality :: [complex:'Coding'()] | undefined,
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	started :: dateTime() | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	referrer :: special:'Reference'() | undefined,
	interpreter :: [special:'Reference'()] | undefined,
	endpoint :: [special:'Reference'()] | undefined,
	numberOfSeries :: unsignedInt() | undefined,
	numberOfInstances :: unsignedInt() | undefined,
	procedureReference :: special:'Reference'() | undefined,
	procedureCode :: [complex:'CodeableConcept'()] | undefined,
	location :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	description :: string() | undefined,
	series :: [complex:'ImagingStudy.Series'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_imagingStudy.Instance({Props}) -> to_imagingStudy.Instance(Props);
to_imagingStudy.Instance(Props) ->
  DT = decode:xsd_info(<<"ImagingStudy.Instance">>),
  #'ImagingStudy.Instance'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	uid :: id(),
	sopClass :: complex:'Coding'(),
	number :: unsignedInt() | undefined,
	title :: string() | undefined}).
    }.



to_imagingStudy.Performer({Props}) -> to_imagingStudy.Performer(Props);
to_imagingStudy.Performer(Props) ->
  DT = decode:xsd_info(<<"ImagingStudy.Performer">>),
  #'ImagingStudy.Performer'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	function :: complex:'CodeableConcept'() | undefined,
	actor :: special:'Reference'()}).
    }.


to_imagingStudy.Series({Props}) -> to_imagingStudy.Series(Props);
to_imagingStudy.Series(Props) ->
  DT = decode:xsd_info(<<"ImagingStudy.Series">>),
  #'ImagingStudy.Series'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	uid :: id(),
	number :: unsignedInt() | undefined,
	modality :: complex:'Coding'(),
	description :: string() | undefined,
	numberOfInstances :: unsignedInt() | undefined,
	endpoint :: [special:'Reference'()] | undefined,
	bodySite :: complex:'Coding'() | undefined,
	laterality :: complex:'Coding'() | undefined,
	specimen :: [special:'Reference'()] | undefined,
	started :: dateTime() | undefined,
	performer :: [complex:'ImagingStudy.Performer'()] | undefined,
	instance :: [complex:'ImagingStudy.Instance'()] | undefined}).
    }.



text(#'ImagingStudy'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, imagingStudy:to_imagingStudy(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

imagingStudy_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'ImagingStudy',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
imagingStudy_toprop_test() ->
    ?asrtp({'ImagingStudy',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"ImagingStudy">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

imagingStudy_json_test() ->
    ?asrtjson({'ImagingStudy',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"ImagingStudy\",\"id\":\"p-21666\"}">>).

-endif.



