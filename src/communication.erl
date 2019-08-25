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
	contained :: [resource:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	instantiatesCanonical :: [canonical()] | undefined,
	instantiatesUri :: [uri()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	partOf :: [special:'Reference'()] | undefined,
	inResponseTo :: [special:'Reference'()] | undefined,
	status :: code(),
	statusReason :: complex:'CodeableConcept'() | undefined,
	category :: [complex:'CodeableConcept'()] | undefined,
	priority :: code() | undefined,
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
    , instantiatesCanonical  = decode:value(<<"instantiatesCanonical">>, Props, DT)
    , instantiatesUri  = decode:value(<<"instantiatesUri">>, Props, DT)
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , partOf  = decode:value(<<"partOf">>, Props, DT)
    , inResponseTo  = decode:value(<<"inResponseTo">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , statusReason  = decode:value(<<"statusReason">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , priority  = decode:value(<<"priority">>, Props, DT)
    , medium  = decode:value(<<"medium">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , topic  = decode:value(<<"topic">>, Props, DT)
    , about  = decode:value(<<"about">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , sent  = decode:value(<<"sent">>, Props, DT)
    , received  = decode:value(<<"received">>, Props, DT)
    , recipient  = decode:value(<<"recipient">>, Props, DT)
    , sender  = decode:value(<<"sender">>, Props, DT)
    , reasonCode  = decode:value(<<"reasonCode">>, Props, DT)
    , reasonReference  = decode:value(<<"reasonReference">>, Props, DT)
    , payload  = decode:value(<<"payload">>, Props, DT)
    , note  = decode:value(<<"note">>, Props, DT)
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
    ?asrtto([{<<"id">>, <<"p-21666">>}, {<<"status">>, <<"completed">>}],
            {'Communication',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             [],[],[],[],[],[],<<"completed">>,undefined,[],undefined,
             [],undefined,undefined,[],undefined, undefined,undefined,[],undefined,
             [],[],[],[]}
           ).

communication_toprop_test() ->
    ?asrtp(
            {'Communication',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             [],[],[],[],[],[],<<"completed">>,undefined,[],undefined,
             [],undefined,undefined,[],undefined, undefined,undefined,[],undefined,
             [],[],[],[]},
           {[{<<"resourceType">>,<<"Communication">>},
              {<<"id">>,<<"p-21666">>},
              {<<"status">>, <<"completed">>}
            ]}).

communication_json_test() ->
    ?asrtjson(
            {'Communication',[],<<"p-21666">>,undefined,undefined, undefined,undefined,[],[],[],
             [],[],[],[],[],[],<<"completed">>,undefined,[],undefined,
             [],undefined,undefined,[],undefined, undefined,undefined,[],undefined,
             [],[],[],[]},
            <<"{\"resourceType\":\"Communication\",\"id\":\"p-21666\",\"status\":\"completed\"}">>
      ).

-endif.


