-module(communication).
-compile(export_all).
-include("fhir.hrl").
-include("primitives.hrl").


-record('Communication', {anyAttribs :: anyAttribs(),
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
	inResponseTo :: [special:'Reference'()] | undefined,
	status :: complex:'EventStatus'(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	category :: [complex:'CodeableConcept'()] | undefined,
	priority :: complex:'RequestPriority'() | undefined,
	medium :: [complex:'CodeableConcept'()] | undefined,
	subject :: special:'Reference'() | undefined,
	topic :: complex:'CodeableConcept'() | undefined,
	about :: [special:'Reference'()] | undefined,
	encounter :: special:'Reference'() | undefined,
	sent :: dateTime() | undefined,
	received :: dateTime() | undefined,
	recipient :: [special:'Reference'()] | undefined,
	sender :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	payload :: [complex:'Communication.Payload'()] | undefined,
	note :: [complex:'Annotation'()] | undefined}).
-type 'Communication'() :: #'Communication'{}.


%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================


to_communication({Props}) -> to_communication(Props);
to_communication(Props) ->
  DT = decode:xsd_info(<<"Communication">>),
  #'Communication'{ 
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
	inResponseTo :: [special:'Reference'()] | undefined,
	status :: complex:'EventStatus'(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	category :: [complex:'CodeableConcept'()] | undefined,
	priority :: complex:'RequestPriority'() | undefined,
	medium :: [complex:'CodeableConcept'()] | undefined,
	subject :: special:'Reference'() | undefined,
	topic :: complex:'CodeableConcept'() | undefined,
	about :: [special:'Reference'()] | undefined,
	encounter :: special:'Reference'() | undefined,
	sent :: dateTime() | undefined,
	received :: dateTime() | undefined,
	recipient :: [special:'Reference'()] | undefined,
	sender :: special:'Reference'() | undefined,
	reasonCode :: [complex:'CodeableConcept'()] | undefined,
	reasonReference :: [special:'Reference'()] | undefined,
	payload :: [complex:'Communication.Payload'()] | undefined,
	note :: [complex:'Annotation'()] | undefined}).
  }.

%%
%% API exports
%%-export([]).


%%====================================================================
%% Internal functions
%%====================================================================

text(#'Communication'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, communication:to_communication(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

communication_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'Communication',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
communication_toprop_test() ->
    ?asrtp({'Communication',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"Communication">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

communication_json_test() ->
    ?asrtjson({'Communication',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"Communication\",\"id\":\"p-21666\"}">>).

-endif.


