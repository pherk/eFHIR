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
    , modality  = decode:value(<<"modality">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , started  = decode:value(<<"started">>, Props, DT)
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , referrer  = decode:value(<<"referrer">>, Props, DT)
    , interpreter  = decode:value(<<"interpreter">>, Props, DT)
    , endpoint  = decode:value(<<"endpoint">>, Props, DT)
    , numberOfSeries  = decode:value(<<"numberOfSeries">>, Props, DT)
    , numberOfInstances  = decode:value(<<"numberOfInstances">>, Props, DT)
    , procedureReference  = decode:value(<<"procedureReference">>, Props, DT)
    , procedureCode  = decode:value(<<"procedureCode">>, Props, DT)
    , location  = decode:value(<<"location">>, Props, DT)
    , reasonCode  = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference  = decode:value(<<"reasonReference">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
    , description  = decode:value(<<"description">>, Props, DT)
    , series  = decode:value(<<"series">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_imagingStudy.Instance({Props}) -> to_imagingStudy.Instance(Props);
to_imagingStudy.Instance(Props) ->
  DT = decode:xsd_info(<<"ImagingStudy.Instance">>),
  #'ImagingStudy.Instance'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , uid  = decode:value(<<"uid">>, Props, DT)
    , sopClass  = decode:value(<<"sopClass">>, Props, DT)
    , number  = decode:value(<<"number">>, Props, DT)
    , title  = decode:value(<<"title">>, Props, DT)
    }.



to_imagingStudy.Performer({Props}) -> to_imagingStudy.Performer(Props);
to_imagingStudy.Performer(Props) ->
  DT = decode:xsd_info(<<"ImagingStudy.Performer">>),
  #'ImagingStudy.Performer'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , function  = decode:value(<<"function">>, Props, DT)
    , actor  = decode:value(<<"actor">>, Props, DT)
    }.


to_imagingStudy.Series({Props}) -> to_imagingStudy.Series(Props);
to_imagingStudy.Series(Props) ->
  DT = decode:xsd_info(<<"ImagingStudy.Series">>),
  #'ImagingStudy.Series'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , uid  = decode:value(<<"uid">>, Props, DT)
    , number  = decode:value(<<"number">>, Props, DT)
    , modality  = decode:value(<<"modality">>, Props, DT)
    , description  = decode:value(<<"description">>, Props, DT)
    , numberOfInstances  = decode:value(<<"numberOfInstances">>, Props, DT)
    , endpoint  = decode:value(<<"endpoint">>, Props, DT)
    , bodySite  = decode:value(<<"bodySite">>, Props, DT)
    , laterality  = decode:value(<<"laterality">>, Props, DT)
    , specimen  = decode:value(<<"specimen">>, Props, DT)
    , started  = decode:value(<<"started">>, Props, DT)
    , performer  = decode:value(<<"performer">>, Props, DT)
    , instance  = decode:value(<<"instance">>, Props, DT)
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



