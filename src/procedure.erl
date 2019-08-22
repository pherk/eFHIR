-module(procedure).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").


-record('Procedure.FocalDevice', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	action :: complex:'CodeableConcept'() | undefined,
	manipulated :: special:'Reference'()}).

-type 'Procedure.FocalDevice'() :: #'Procedure.FocalDevice'{}.


-record('Procedure.Performer', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	function :: complex:'CodeableConcept'() | undefined,
	actor :: special:'Reference'(),
	onBehalfOf :: special:'Reference'() | undefined}).

-type 'Procedure.Performer'() :: #'Procedure.Performer'{}.


-record('Procedure', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	partOf :: [special:'Reference'()] | undefined,
	status :: complex:'EventStatus'(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	category :: complex:'CodeableConcept'() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	choice :: string() | complex:'Range'() | complex:'Period'() | dateTime() | complex:'Age'() | undefined,
	recorder :: special:'Reference'() | undefined,
	asserter :: special:'Reference'() | undefined,
	performer :: [complex:'Procedure.Performer'()] | undefined,
	location :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	bodySite :: [complex:'CodeableConcept'()] | undefined,
	outcome :: complex:'CodeableConcept'() | undefined,
	report :: [special:'Reference'()] | undefined,
	complication :: [complex:'CodeableConcept'()] | undefined,
	complicationDetail :: [special:'Reference'()] | undefined,
	followUp :: [complex:'CodeableConcept'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	focalDevice :: [complex:'Procedure.FocalDevice'()] | undefined,
	usedReference :: [special:'Reference'()] | undefined,
	usedCode :: [complex:'CodeableConcept'()] | undefined}).

-type 'Procedure'() :: #'Procedure'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_procedure({Props}) -> to_procedure(Props);
to_procedure(Props) ->
  DT = decode:xsd_info(<<"Procedure">>),
  #'Procedure'{ 
      id               = decode:value(<<"id">>, Props, DT)
    , meta             = decode:value(<<"meta">>, Props, DT)
    , implicitRules    = decode:value(<<"implicitRules">>, Props, DT)
    , language         = decode:value(<<"language">>, Props, DT)
    , text             = decode:value(<<"text">>, Props, DT)
    , contained        = decode:value(<<"contained">>, Props, DT)
    , extension        = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , 'identifier'      = decode:value(<<"identifier">>, Props, DT)
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	partOf :: [special:'Reference'()] | undefined,
	status :: complex:'EventStatus'(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	category :: complex:'CodeableConcept'() | undefined,
	code :: complex:'CodeableConcept'() | undefined,
	subject :: special:'Reference'(),
	encounter :: special:'Reference'() | undefined,
	choice :: string() | complex:'Range'() | complex:'Period'() | dateTime() | complex:'Age'() | undefined,
	recorder :: special:'Reference'() | undefined,
	asserter :: special:'Reference'() | undefined,
	performer :: [complex:'Procedure.Performer'()] | undefined,
	location :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	bodySite :: [complex:'CodeableConcept'()] | undefined,
	outcome :: complex:'CodeableConcept'() | undefined,
	report :: [special:'Reference'()] | undefined,
	complication :: [complex:'CodeableConcept'()] | undefined,
	complicationDetail :: [special:'Reference'()] | undefined,
	followUp :: [complex:'CodeableConcept'()] | undefined,
	note :: [complex:'Annotation'()] | undefined,
	focalDevice :: [complex:'Procedure.FocalDevice'()] | undefined,
	usedReference :: [special:'Reference'()] | undefined,
	usedCode :: [complex:'CodeableConcept'()] | undefined}).
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_procedure.FocalDevice({Props}) -> to_procedure.FocalDevice(Props);
to_procedure.FocalDevice(Props) ->
  DT = decode:xsd_info(<<"Procedure.FocalDevice">>),
  #'Procedure.FocalDevice'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	action :: complex:'CodeableConcept'() | undefined,
	manipulated :: special:'Reference'()}).
    }.

to_procedure.Performer({Props}) -> to_procedure.Performer(Props);
to_procedure.Performer(Props) ->
  DT = decode:xsd_info(<<"Procedure.Performer">>),
  #'Procedure.Performer'{ 
    anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	function :: complex:'CodeableConcept'() | undefined,
	actor :: special:'Reference'(),
	onBehalfOf :: special:'Reference'() | undefined}).
    }.


text(#'ActivityDefinition'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, procedure:to_procedure(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

procedure_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'ActivityDefinition',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
procedure_toprop_test() ->
    ?asrtp({'ActivityDefinition',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"ActivityDefinition">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

procedure_json_test() ->
    ?asrtjson({'Procedure',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Procedure\",\"id\":\"p-21666\"}">>).

-endif.



