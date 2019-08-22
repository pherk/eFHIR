-module(diagnosticreport).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").

-record('DiagnosticReport.Media', {anyAttribs :: anyAttribs(),
	id :: string() | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	comment :: string() | undefined,
	link :: special:'Reference'()}).

-type 'DiagnosticReport.Media'() :: #'DiagnosticReport.Media'{}.


-record('DiagnosticReport', {anyAttribs :: anyAttribs(),
	id :: id() | undefined,
	meta :: special:'Meta'() | undefined,
	implicitRules :: uri() | undefined,
	language :: code() | undefined,
	text :: special:'Narrative'() | undefined,
	contained :: [complex:'ResourceContainer'()] | undefined,
	extension :: [extensions:'Extension'()] | undefined,
	modifierExtension :: [extensions:'Extension'()] | undefined,
	identifier :: [complex:'Identifier'()] | undefined,
	basedOn :: [special:'Reference'()] | undefined,
	status :: complex:'DiagnosticReportStatus'(),
	category :: [complex:'CodeableConcept'()] | undefined,
	code :: complex:'CodeableConcept'(),
	subject :: special:'Reference'() | undefined,
	encounter :: special:'Reference'() | undefined,
	choice :: complex:'Period'() | dateTime() | undefined,
	issued :: instant() | undefined,
	performer :: [special:'Reference'()] | undefined,
	resultsInterpreter :: [special:'Reference'()] | undefined,
	specimen :: [special:'Reference'()] | undefined,
	result :: [special:'Reference'()] | undefined,
	imagingStudy :: [special:'Reference'()] | undefined,
	media :: [complex:'DiagnosticReport.Media'()] | undefined,
	conclusion :: string() | undefined,
	conclusionCode :: [complex:'CodeableConcept'()] | undefined,
	presentedForm :: [complex:'Attachment'()] | undefined}).

-type 'DiagnosticReport'() :: #'DiagnosticReport'{}.



%%
%% API exports
%%-export([]).

%%====================================================================
%% API functions
%%====================================================================
to_diagnosticReport({Props}) -> to_diagnosticReport(Props);
to_diagnosticReport(Props) ->
  DT = decode:xsd_info(<<"DiagnosticReport">>),
  #'DiagnosticReport'{ 
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
    , basedOn  = decode:value(<<"basedOn">>, Props, DT)
    , status  = decode:value(<<"status">>, Props, DT)
    , category  = decode:value(<<"category">>, Props, DT)
    , code  = decode:value(<<"code">>, Props, DT)
    , subject  = decode:value(<<"subject">>, Props, DT)
    , encounter  = decode:value(<<"encounter">>, Props, DT)
    , choice  = decode:value(<<"choice">>, Props, DT)
    , issued  = decode:value(<<"issued">>, Props, DT)
    , performer  = decode:value(<<"performer">>, Props, DT)
    , resultsInterpreter  = decode:value(<<"resultsInterpreter">>, Props, DT)
    , specimen  = decode:value(<<"specimen">>, Props, DT)
    , result  = decode:value(<<"result">>, Props, DT)
    , imagingStudy  = decode:value(<<"imagingStudy">>, Props, DT)
    , media  = decode:value(<<"media">>, Props, DT)
    , conclusion  = decode:value(<<"conclusion">>, Props, DT)
    , conclusionCode  = decode:value(<<"conclusionCode">>, Props, DT)
    , presentedForm  = decode:value(<<"presentedForm">>, Props, DT)
    }.


%%====================================================================
%% Internal functions
%%====================================================================
to_diagnosticReport.Media({Props}) -> to_diagnosticReport.Media(Props);
to_diagnosticReport.Media(Props) ->
  DT = decode:xsd_info(<<"DiagnosticReport.Media">>),
  #'DiagnosticReport.Media'{ 
      anyAttribs  = decode:attrs(Props, DT)
    , id  = decode:value(<<"id">>, Props, DT)
    , extension  = decode:value(<<"extension">>, Props, DT)
    , modifierExtension  = decode:value(<<"modifierExtension">>, Props, DT)
    , comment  = decode:value(<<"comment">>, Props, DT)
    , link  = decode:value(<<"link">>, Props, DT)
    }.


text(#'DiagnosticReport'{text=N}) -> 
    special:narrative(N).

%%
%% EUnit Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, diagnosticReport:to_diagnosticReport(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).

diagnosticReport_to_test() ->
    ?asrtto([{<<"id">>, <<"p-21666">>}],
         {'DiagnosticReport',<<"p-21666">>,undefined,undefined, undefined, 
                  undefined,[], [], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined,[]}).
diagnosticReport_toprop_test() ->
    ?asrtp({'DiagnosticReport',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           {[{<<"resourceType">>,<<"DiagnosticReport">>},
              {<<"id">>,<<"p-21666">>}
            ]}).

diagnosticReport_json_test() ->
    ?asrtjson({'DiagnosticReport',<<"p-21666">>,undefined,undefined,undefined, 
                  undefined, [],[], [],
                          [],undefined,[],[],undefined,undefined,
                          undefined,undefined,[],undefined,undefined,
                          undefined,[],[],[],[],undefined, []},
           <<"{\"resourceType\":\"DiagnosticReport\",\"id\":\"p-21666\"}">>).

-endif.


