-module(complex).
-compile(export_all).
-include("primitives.hrl").
%%
%% API exports
%%
-export_type([coding/0,codeableConcept/0]).
-export_type([fhir_identifier/0,reference_/0]).
-export_type([humanName/0,address/0,contactPoint/0]).
-export_type([relatedArtifact/0,attachment/0]).
-export_type([quantity/0,range/0,ratio/0]).
-export_type([period/0,repeat/0,timing/0]).
-export_type([annotation/0,signature/0]).
-export_type([narrative/0]).
-export_type([meta/0]).


-record(address, {
       use                    :: binary()     %% home | work | temp | old
     , type                   :: binary()     %% postal | physical | both
     , text                   :: binary()
     , line                   :: [binary()]
     , city                   :: binary()
     , district               :: binary()
     , state                  :: binary()
     , postalCode             :: binary()
     , country                :: binary()
     , period                 :: period()
}).
-opaque address() :: #address{}.

-record(annotation, {
      authorReference :: reference_()
    , time :: date()
    , text :: binary()
    }).
-type annotation() :: #annotation{}.

-record(attachment, { 
      contentType :: binary()
    , language :: binary()
    , data :: base64Binary()
    , url :: binary()
    , size :: integer()
    , hash :: base64Binary()
    , title :: binary()
    , creation :: date()
    }).
-opaque attachment() :: #attachment{}.

-record(coding, {
      system       :: uri()
    , version      :: binary()
    , code         :: binary()
    , display      :: binary()
    , userSelected :: boolean()
    }).
-opaque coding() :: #coding{}.


-record(codeableConcept, {
      coding       :: [coding()]      %% Coding Code defined by a terminology system
    , text         :: binary()        %% Plain text representation of the concept
}).
-opaque codeableConcept() :: #codeableConcept{}.

-record(contactPoint, {
      use                    :: binary()     %% home | work | temp | old | mobile
    , system                 :: binary()     %% phone | fax | email | pager | other
    , value                  :: binary()
    , rank                   :: non_neg_integer()
    , period                 :: period()
}).
-opaque contactPoint() :: #contactPoint{}.

-record(humanName, {
       use       = <<"official">>   :: binary()  %% usual | official | temp | nickname | anonymous | old | maiden
     , text                   :: binary()
     , family                 :: [binary()]
     , given                  :: [binary()]
     , prefix                 :: [binary()]
     , suffix                 :: [binary()]
     , period                 :: period()
}).
-opaque humanName() :: #humanName{}.

-record(identifier, {
       use = <<"official">> :: binary()    %% usual | official | temp | secondary 
     , type           :: codeableConcept() %% Description of fhir_identifier
     , system         :: uri()             %% The namespace for the fhir_identifier
     , value          :: binary()          %% The value that is unique
     , period         :: period()          %% Time period when id is/was valid for use
     , assigner       :: reference_()             %% Organization that issued id (may be just text)
}).
-opaque fhir_identifier()   :: #identifier{}.

-record(period, {
       start_      :: dateTime()
     , end_        :: dateTime()
}).
-opaque period() :: #period{}.

-record(quantity, {
      value :: float()
    , comparator :: binary()
    , unit :: binary()
    , system :: binary()
    , code :: binary()
    }).
-opaque quantity() :: #quantity{}.

-record(range, {
      low :: quantity()
    , high :: quantity()
    }).
-opaque range() :: #range{}.

-record(ratio, {
      numerator :: quantity()
    , denominator :: quantity()
    }).
-opaque ratio() :: #ratio{}.

-record(repeat, {
      boundsPeriod :: period()
    , count :: integer()
    , countMax :: integer()
    , duration :: float()
    , durationMax :: float()
    , durationUnit :: ucum()
    , frequency :: integer()
    , frequencyMax :: integer()
    , period :: float()
    , periodMax :: float()
    , periodUnit :: binary()
    , dayOfWeek :: [binary()]
    , timeOfDay :: [binary()]
    , when_ :: [binary()]
    , offset :: integer()
    }).
-opaque repeat() :: #repeat{}.

-record(signature, {
      type :: [coding]
    , when_ :: binary()
    , whoReference :: reference_()
    , onBehalfOfReference :: reference_()
    , contentType :: binary()
    , blob :: base64Binary()
    }).
-opaque signature() :: #signature{}.

-record(timing, {
      event :: [binary()]
    , repeat :: repeat()
    , code :: codeableConcept()
    }).
-opaque timing() :: #timing{}.

%%
%%   - Special Datatypes
%%
-record(narrative, {
      status :: binary()
    , div_ :: binary()
    }).
-opaque narrative() :: #narrative{}.

-record(meta, {
       versionId = 0 :: positiveInt()
     , lastUpdated   :: dateTime()
     , source        :: binary()
     , profile       :: [uri()]
     , security      :: [coding()]
     , tag           :: [coding()]
     , extension     :: [extensions:extension()]
}).
-opaque meta()   :: #meta{}.

-record(reference, {
       reference_ :: binary()
     , display   :: binary()
}).
-opaque reference_()    :: #reference{}.

-record(relatedArtifact, {
      type :: binary()
    , display :: binary()
    , citation :: binary()
    , url :: binary()
    , document :: attachment()
    , resource :: reference_()
    }).
-opaque relatedArtifact() :: #relatedArtifact{}.

%%====================================================================
%% API functions
%%====================================================================
to_address({Props}) -> to_address(Props);
to_address(Props) -> 
    DT = types:xsd_info(<<"Address">>),
    io:format("~p~n~p~n",[Props,DT]),
    #address{
      use        = types:get_value(<<"use">>, Props, DT)
    , type       = types:get_value(<<"type">>, Props, DT) 
    , text       = types:get_value(<<"text">>, Props, DT)
    , line       = types:get_value(<<"line">>, Props, DT)
    , city       = types:get_value(<<"city">>, Props, DT) 
    , district   = types:get_value(<<"distrinct">>, Props, DT) 
    , state      = types:get_value(<<"state">>, Props, DT) 
    , postalCode = types:get_value(<<"postalCode">>, Props, DT) 
    , country    = types:get_value(<<"country">>, Props, DT) 
    , period     = types:get_value(<<"period">>, Props, DT)
    }.

to_annotation({Props}) -> to_annotation(Props);
to_annotation(Props) ->
    DT = types:xsd_info(<<"Annotation">>),
    io:format("~p~n~p~n",[Props,DT]),
    #annotation{
      authorReference = types:get_value(<<"authorReference">>, Props, DT)
    , time = types:get_value(<<"time">>, Props, DT)
    , text = types:get_value(<<"text">>, Props, DT)
    }.

to_attachment({Props}) -> to_attachment(Props);
to_attachment(Props) -> 
    DT = types:xsd_info(<<"Attachment">>),
    io:format("~p~n~p~n",[Props,DT]),
    #attachment{
      contentType = types:get_value(<<"contentType">>, Props, DT)
    , language    = types:get_value(<<"language">>, Props, DT)
    , data        = types:get_value(<<"data">>, Props, DT)
    , url         = types:get_value(<<"url">>, Props, DT)
    , size        = types:get_value(<<"size">>, Props, DT)
    , hash        = types:get_value(<<"hash">>, Props, DT)
    , title       = types:get_value(<<"title">>, Props, DT)
    , creation    = types:get_value(<<"creation">>, Props, DT)
    }.

to_coding({Props}) -> to_coding(Props);
to_coding(Props) ->
    DT = types:xsd_info(<<"Coding">>),
    io:format("~p~n~p~n",[Props,DT]),
    #coding{
        system  = types:get_value(<<"system">>, Props, DT)
      , version = types:get_value(<<"version">>, Props, DT)
      , code    = types:get_value(<<"code">>, Props, DT)
      , display = types:get_value(<<"display">>, Props, DT)
      , userSelected = types:get_value(<<"userSelected">>, Props, DT)
      }.

to_codeableConcept({Props}) -> to_codeableConcept(Props);
to_codeableConcept(Props) ->
    DT = types:xsd_info(<<"CodeableConcept">>),
    io:format("~p~n~p~n",[Props,DT]),
    #codeableConcept{
        coding  = types:get_value(<<"coding">>, Props, DT)
      , text = types:get_value(<<"text">>, Props, DT)
      }.

to_contactPoint({Props}) -> to_contactPoint(Props);
to_contactPoint(Props) -> 
    DT = types:xsd_info(<<"ContactPoint">>),
    io:format("~p~n~p~n",[Props,DT]),
    #contactPoint{
      use    = types:get_value(<<"use">>, Props, DT)
    , system = types:get_value(<<"system">>, Props, DT)
    , value  = types:get_value(<<"value">>, Props, DT)
    , rank   = types:get_value(<<"rank">>, Props, DT)
    , period = types:get_value(<<"period">>, Props, DT)
    }.

to_humanName({Props}) -> to_humanName(Props);
to_humanName(Props) ->
    DT = types:xsd_info(<<"HumanName">>),
    #humanName{
       use     = types:get_value(<<"use">>, Props, DT) 
     , text    = types:get_value(<<"text">>, Props, DT) 
     , family  = types:get_value(<<"family">>, Props, DT) 
     , given   = types:get_value(<<"given">>, Props, DT) 
     , prefix  = types:get_value(<<"prefix">>, Props, DT) 
     , suffix  = types:get_value(<<"suffix">>, Props, DT) 
     , period  = types:get_value(<<"period">>, Props, DT)
    }.

to_identifier({Props}) -> to_identifier(Props);
to_identifier(Props) ->
    DT = types:xsd_info(<<"Identifier">>),
    #identifier{
        use  = types:get_value(<<"use">>, Props, DT)
      , type = types:get_value(<<"type">>, Props, DT)
      , system = types:get_value(<<"system">>, Props, DT)
      , value  = types:get_value(<<"value">>, Props, DT)
      , period   = types:get_value(<<"period">>, Props, DT)
      , assigner = types:get_value(<<"assigner">>, Props, DT)
      }.

to_period({Props}) -> to_period(Props);
to_period(Props) ->
    DT = types:xsd_info(<<"Period">>),
    #period{
        start_  = types:get_value(<<"start">>, Props, DT)
      , end_    = types:get_value(<<"end">>, Props, DT)
      }.

to_quantity({Props}) -> to_quantity(Props);
to_quantity(Props) ->
    DT = types:xsd_info(<<"Quantity">>),
    #quantity{
        value = types:get_value(<<"value">>, Props, DT)
      , comparator = types:get_value(<<"comparator">>, Props, DT)
      , unit = types:get_value(<<"unit">>, Props, DT)
      , system = types:get_value(<<"system">>, Props, DT)
      , code = types:get_value(<<"code">>, Props, DT)
      }.

to_range({Props}) -> to_range(Props);
to_range(Props) ->
    DT = types:xsd_info(<<"Range">>),
    #range{
      low = types:get_value(<<"low">>, Props, DT)
    , high = types:get_value(<<"high">>, Props, DT)
    }.

to_ratio({Props}) -> to_ratio(Props);
to_ratio(Props) ->
    DT = types:xsd_info(<<"Ratio">>),
    #ratio{
      numerator = types:get_value(<<"numerator">>, Props, DT)
    , denominator = types:get_value(<<"denominator">>, Props, DT)
    }.

to_repeat({Props}) -> to_repeat(Props);
to_repeat(Props) ->
    DT = types:xsd_info(<<"Repeat">>),
    #repeat{
      boundsPeriod = types:get_value(<<"boundsPeriod">>, Props, DT)
    , count = types:get_value(<<"count">>, Props, DT)
    , countMax = types:get_value(<<"countMax">>, Props, DT)
    , duration = types:get_value(<<"duration">>, Props, DT)
    , durationMax = types:get_value(<<"durationMax">>, Props, DT)
    , durationUnit = types:get_value(<<"durationUnit">>, Props, DT)
    , frequency = types:get_value(<<"frequency">>, Props, DT)
    , frequencyMax = types:get_value(<<"frequencyMax">>, Props, DT)
    , period = types:get_value(<<"period">>, Props, DT)
    , periodMax = types:get_value(<<"periodMax">>, Props, DT)
    , periodUnit = types:get_value(<<"periodUnit">>, Props, DT)
    , dayOfWeek = types:get_value(<<"dayOfWeek">>, Props, DT)
    , timeOfDay = types:get_value(<<"timeOfDay">>, Props, DT)
    , when_ = types:get_value(<<"when_">>, Props, DT)
    , offset = types:get_value(<<"offset">>, Props, DT)
    }.

to_signature({Props}) -> to_signature(Props);
to_signature(Props) ->
    DT = types:xsd_info(<<"Signature">>),
    #signature{
      type = types:get_value(<<"type">>, Props, DT)
    , when_ = types:get_value(<<"when_">>, Props, DT)
    , whoReference = types:get_value(<<"whoReference">>, Props, DT)
    , onBehalfOfReference = types:get_value(<<"onBehalfOfReference">>, Props, DT)
    , contentType = types:get_value(<<"contentType">>, Props, DT)
    , blob = types:get_value(<<"blob">>, Props, DT)
    }.

to_timing({Props}) -> to_timing(Props);
to_timing(Props) ->
    DT = types:xsd_info(<<"Timing">>),
    #timing{
      event = types:get_value(<<"event">>, Props, DT)
    , repeat = types:get_value(<<"repeat">>, Props, DT)
    , code = types:get_value(<<"code">>, Props, DT)
    }.

%%
%%====================================================================
%% Special data types
%%====================================================================
%%
to_narrative({Props}) -> to_narrative(Props);
to_narrative(Props) ->
    DT = types:xsd_info(<<"Narrative">>),
    io:format("~p~n~p~n",[Props,DT]),
    #narrative{
        status = types:get_value(<<"status">>, Props, DT)
      , div_   = types:get_value(<<"div">>, Props, DT)
      }.

to_meta({Props}) -> to_meta(Props);
to_meta(Props) ->
    DT = types:xsd_info(<<"Meta">>),
    io:format("~p~n~p~n",[Props,DT]),
    #meta{
        versionId    = types:get_value(<<"versionId">>, Props, DT)
      , lastUpdated  = types:get_value(<<"lastUpdated">>, Props, DT)
      , source       = types:get_value(<<"source">>, Props, DT)
      , profile      = types:get_value(<<"profile">>, Props, DT) 
      , security     = types:get_value(<<"security">>, Props, DT)
      , tag          = types:get_value(<<"tag">>, Props, DT)
      , extension    = types:get_value(<<"extension">>, Props, DT)
      }.

to_reference({Props}) -> to_reference(Props);
to_reference(Props) ->
    DT = types:xsd_info(<<"Reference">>),
    io:format("~p~n~p~n",[Props,DT]),
    #reference{
        reference_ = types:get_value(<<"reference">>, Props, DT)
      , display    = types:get_value(<<"display">>, Props, DT)
      }.

%%%
%%% EUnit
%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, A)).
-define(asrtpr(A, B), ?assertEqual(B, utils:rec_to_prop(A))).

complex_to_test() ->
    ?asrtto(complex:to_coding({[{<<"code">>, <<"test">>}]}),
            {coding,undefined,undefined,<<"test">>,undefined,undefined}),
    ?asrtto(complex:to_coding({[{<<"userSelected">>, <<"false">>}]}),
            {coding,undefined,undefined,undefined,undefined, false}),
    ?asrtto(complex:to_coding({[{<<"system">>,<<"http://eNahar.org/test">>}, {<<"code">>, <<"test">>},{<<"display">>,<<"test">>}]}),
            {coding,<<"http://eNahar.org/test">>,undefined,<<"test">>,<<"test">>,undefined}),
    ?asrtto(complex:to_humanName({[{<<"use">>, <<"official">>}]}),
            {humanName,<<"official">>,undefined,undefined,[],[],[],undefined}),
    ?asrtto(complex:to_humanName({[{<<"use">>, <<"official">>},{<<"family">>,<<"Sokolow">>},{<<"given">>,[<<"Nicolai">>]}]}),
            {humanName,<<"official">>,undefined,<<"Sokolow">>,[<<"Nicolai">>],[],[],undefined}).

complex_meta_test() ->
    ?asrtto(complex:to_meta({[{<<"versionId">>, <<"999">>},
                              {<<"lastUpdated">>,<<"2019-07-14T09:10:10">>},
                              {<<"tag">>,
                                   [{[{<<"system">>,<<"http://eNahar.org/test">>},
                                      {<<"code">>,<<"hello">>}]}]},
                              {<<"extension">>,
                                   [{[{<<"url">>, <<"http://eNahar.org/nabu/extension#lastUpdatedBy">>},
                                      {<<"valueReference">>,
                                             {[{<<"reference">>, <<"metis/practitioners/u-vkr">>},
                                               {<<"display">>, <<"von Kleist-Retzow, JÃ¼rgen-Christoph">>}]}}]}]}
                             ]}),
            {meta,<<"999">>,<<"2019-07-14T09:10:10">>,undefined, [], [], 
                          [{coding, <<"http://eNahar.org/test">>,undefined,<<"hello">>,undefined,undefined}],
                          [{extension,
                            <<"http://eNahar.org/nabu/extension#lastUpdatedBy">>,
                              {valueReference,
                                 {reference,<<"metis/practitioners/u-vkr">>, <<"von Kleist-Retzow, JÃ¼rgen-Christoph">>}}}]}).

complex_timing_test() ->
    ?asrtto(complex:to_timing({[{<<"event">>, [<<"2019-07-15T12:00:00">>]}]}),
            {timing,[<<"2019-07-15T12:00:00">>], undefined, undefined}).

-endif.

