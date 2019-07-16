-module(complex).
-compile(export_all).
-include("primitives.hrl").
-include("fhir_400.hrl").
%%
%% API exports
%%
-export_type([decimal/0,uri/0,url/0,canonical/0,base64Binary/0]).
-export_type([instant/0,date/0,dateTime/0,yearMonth/0,year/0,dow/0,time/0]).
-export_type([code/0,oid/0,id/0,markdown/0,positiveInt/0,unsignedInt/0,uuid/0,ucum/0]).
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
    DT = maps:get(<<"address">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #address{
      use        = get_value(<<"use">>, Props, DT)
    , type       = get_value(<<"type">>, Props, DT) 
    , text       = get_value(<<"text">>, Props, DT)
    , line       = get_value(<<"line">>, Props, DT)
    , city       = get_value(<<"city">>, Props, DT) 
    , district   = get_value(<<"distrinct">>, Props, DT) 
    , state      = get_value(<<"state">>, Props, DT) 
    , postalCode = get_value(<<"postalCode">>, Props, DT) 
    , country    = get_value(<<"country">>, Props, DT) 
    , period     = get_value(<<"period">>, Props, DT)
    }.

to_annotation({Props}) -> to_annotation(Props);
to_annotation(Props) ->
    DT = maps:get(<<"annotation">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #annotation{
      authorReference = get_value(<<"authorReference">>, Props, DT)
    , time = get_value(<<"time">>, Props, DT)
    , text = get_value(<<"text">>, Props, DT)
    }.

to_attachment({Props}) -> to_attachment(Props);
to_attachment(Props) -> 
    DT = maps:get(<<"attachment">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #attachment{
      contentType = get_value(<<"contentType">>, Props, DT)
    , language    = get_value(<<"language">>, Props, DT)
    , data        = get_value(<<"data">>, Props, DT)
    , url         = get_value(<<"url">>, Props, DT)
    , size        = get_value(<<"size">>, Props, DT)
    , hash        = get_value(<<"hash">>, Props, DT)
    , title       = get_value(<<"title">>, Props, DT)
    , creation    = get_value(<<"creation">>, Props, DT)
    }.

to_coding({Props}) -> to_coding(Props);
to_coding(Props) ->
    DT = maps:get(<<"coding">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #coding{
        system  = get_value(<<"system">>, Props, DT)
      , version = get_value(<<"version">>, Props, DT)
      , code    = get_value(<<"code">>, Props, DT)
      , display = get_value(<<"display">>, Props, DT)
      , userSelected = get_value(<<"userSelected">>, Props, DT)
      }.

to_codeableConcept({Props}) -> to_codeableConcept(Props);
to_codeableConcept(Props) ->
    DT = maps:get(<<"codeableConcept">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #codeableConcept{
        coding  = get_value(<<"coding">>, Props, DT)
      , text = get_value(<<"text">>, Props, DT)
      }.

to_contactPoint({Props}) -> to_contactPoint(Props);
to_contactPoint(Props) -> 
    DT = maps:get(<<"contactPoint">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #contactPoint{
      use    = get_value(<<"use">>, Props, DT)
    , system = get_value(<<"system">>, Props, DT)
    , value  = get_value(<<"value">>, Props, DT)
    , rank   = get_value(<<"rank">>, Props, DT)
    , period = get_value(<<"period">>, Props, DT)
    }.

to_humanName({Props}) -> to_humanName(Props);
to_humanName(Props) ->
    DT = maps:get(<<"humanName">>,?fhir_xsd),
    #humanName{
       use     = get_value(<<"use">>, Props, DT) 
     , text    = get_value(<<"text">>, Props, DT) 
     , family  = get_value(<<"family">>, Props, DT) 
     , given   = get_value(<<"given">>, Props, DT) 
     , prefix  = get_value(<<"prefix">>, Props, DT) 
     , suffix  = get_value(<<"suffix">>, Props, DT) 
     , period  = get_value(<<"period">>, Props, DT)
    }.

to_identifier({Props}) -> to_identifier(Props);
to_identifier(Props) ->
    DT = maps:get(<<"identifier">>,?fhir_xsd),
    #identifier{
        use  = get_value(<<"use">>, Props, DT)
      , type = get_value(<<"type">>, Props, DT)
      , system = get_value(<<"system">>, Props, DT)
      , value  = get_value(<<"value">>, Props, DT)
      , period   = get_value(<<"period">>, Props, DT)
      , assigner = get_value(<<"assigner">>, Props, DT)
      }.

to_period({Props}) -> to_period(Props);
to_period(Props) ->
    DT = maps:get(<<"period">>,?fhir_xsd),
    #period{
        start_  = get_value(<<"start">>, Props, DT)
      , end_    = get_value(<<"end">>, Props, DT)
      }.

to_quantity({Props}) -> to_quantity(Props);
to_quantity(Props) ->
    DT = maps:get(<<"quantity">>,?fhir_xsd),
    #quantity{
        value = get_value(<<"value">>, Props, DT)
      , comparator = get_value(<<"comparator">>, Props, DT)
      , unit = get_value(<<"unit">>, Props, DT)
      , system = get_value(<<"system">>, Props, DT)
      , code = get_value(<<"code">>, Props, DT)
      }.

to_range({Props}) -> to_range(Props);
to_range(Props) ->
    DT = maps:get(<<"range">>,?fhir_xsd),
    #range{
      low = get_value(<<"low">>, Props, DT)
    , high = get_value(<<"high">>, Props, DT)
    }.

to_ratio({Props}) -> to_ratio(Props);
to_ratio(Props) ->
    DT = maps:get(<<"ratio">>,?fhir_xsd),
    #ratio{
      numerator = get_value(<<"numerator">>, Props, DT)
    , denominator = get_value(<<"denominator">>, Props, DT)
    }.

to_repeat({Props}) -> to_repeat(Props);
to_repeat(Props) ->
    DT = maps:get(<<"repeat">>,?fhir_xsd),
    #repeat{
      boundsPeriod = get_value(<<"boundsPeriod">>, Props, DT)
    , count = get_value(<<"count">>, Props, DT)
    , countMax = get_value(<<"countMax">>, Props, DT)
    , duration = get_value(<<"duration">>, Props, DT)
    , durationMax = get_value(<<"durationMax">>, Props, DT)
    , durationUnit = get_value(<<"durationUnit">>, Props, DT)
    , frequency = get_value(<<"frequency">>, Props, DT)
    , frequencyMax = get_value(<<"frequencyMax">>, Props, DT)
    , period = get_value(<<"period">>, Props, DT)
    , periodMax = get_value(<<"periodMax">>, Props, DT)
    , periodUnit = get_value(<<"periodUnit">>, Props, DT)
    , dayOfWeek = get_value(<<"dayOfWeek">>, Props, DT)
    , timeOfDay = get_value(<<"timeOfDay">>, Props, DT)
    , when_ = get_value(<<"when_">>, Props, DT)
    , offset = get_value(<<"offset">>, Props, DT)
    }.

to_signature({Props}) -> to_signature(Props);
to_signature(Props) ->
    DT = maps:get(<<"signature">>,?fhir_xsd),
    #signature{
      type = get_value(<<"type">>, Props, DT)
    , when_ = get_value(<<"when_">>, Props, DT)
    , whoReference = get_value(<<"whoReference">>, Props, DT)
    , onBehalfOfReference = get_value(<<"onBehalfOfReference">>, Props, DT)
    , contentType = get_value(<<"contentType">>, Props, DT)
    , blob = get_value(<<"blob">>, Props, DT)
    }.

to_timing({Props}) -> to_timing(Props);
to_timing(Props) ->
    DT = maps:get(<<"timing">>,?fhir_xsd),
    #timing{
      event = get_value(<<"event">>, Props, DT)
    , repeat = get_value(<<"repeat">>, Props, DT)
    , code = get_value(<<"code">>, Props, DT)
    }.

%%
%%====================================================================
%% Primitive Data Types
%%====================================================================
%%
to_uri({Props}) -> to_uri(Props);
to_uri(Props) ->
    proplists:get_value(<<"uri">>, Props).

to_boolean({Props}) -> to_boolean(Props);
to_boolean(Props) ->
    proplists:get_value(<<"uri">>, Props).

to_binary({Bin}) -> Bin;
to_binary(Bin) -> Bin.

to_code({Bin}) -> Bin;
to_code(Bin) -> Bin.

to_date({Bin}) -> Bin;
to_date(Bin) -> Bin.

to_dateTime({Bin}) -> Bin;
to_dateTime(Bin) -> Bin.

to_time({Bin}) -> Bin;
to_time(Bin) -> Bin.

%%
%%====================================================================
%% Special data types
%%====================================================================
%%
to_narrative({Props}) -> to_narrative(Props);
to_narrative(Props) ->
    DT = maps:get(<<"narrative">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #narrative{
        status = get_value(<<"status">>, Props, DT)
      , div_   = get_value(<<"div">>, Props, DT)
      }.

to_meta({Props}) -> to_meta(Props);
to_meta(Props) ->
    DT = maps:get(<<"meta">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #meta{
        versionId    = get_value(<<"versionId">>, Props, DT)
      , lastUpdated  = get_value(<<"lastUpdated">>, Props, DT)
      , source       = get_value(<<"source">>, Props, DT)
      , profile      = get_value(<<"profile">>, Props, DT) 
      , security     = get_value(<<"security">>, Props, DT)
      , tag          = get_value(<<"tag">>, Props, DT)
      , extension    = get_value(<<"extension">>, Props, DT)
      }.

to_reference({Props}) -> to_reference(Props);
to_reference(Props) ->
    DT = maps:get(<<"reference">>,?fhir_xsd),
    io:format("~p~n~p~n",[Props,DT]),
    #reference{
        reference_ = get_value(<<"reference">>, Props, DT)
      , display    = get_value(<<"display">>, Props, DT)
      }.

%%====================================================================
%% Internal functions
%%====================================================================
get_value(Key, Props, DT) ->
    io:format("get_value: ~s: ~p~n",[Key, DT]),
    {Type,Occurs} = proplists:get_value(Key, DT),
    io:format("get_value: ~s: ~p~n",[Key, {Type,Occurs}]),
    Value = proplists:get_value(erlang_to_fhir(Key), Props),
    io:format("get_value: ~p~n",[Value]),
    case {Value,Occurs} of
        {undefined, optional}       -> undefined;
        {undefined, required}       -> error;
        {undefined, list}           -> [];
        {undefined, non_empty_list} -> error;
        {Value,     optional}       -> validate(Type,Value);
        {Value,     required}       -> validate(Type,Value);
        {Value,     list}           -> Fun = get_fun(Type), lists:map(Fun, Value);
        {Value,     non_empty_list} -> Fun = get_fun(Type), lists:map(Fun, Value)
    end.

validate(binary,   Value) -> Value;
validate(boolean,  Value) -> utils:binary_to_boolean(Value,error);
validate(dateTime, Value) -> Value;
validate(uri,      Value) -> Value;
validate(coding,  Value) -> Value;
validate(period,  Value) -> Value.


get_fun(binary)    -> fun to_binary/1;
get_fun(code)      -> fun to_code/1;
get_fun(coding)    -> fun to_coding/1;
get_fun(date)      -> fun to_time/1;
get_fun(dateTime)  -> fun to_dateTime/1;
get_fun(time)      -> fun to_time/1;
get_fun(extension) -> fun extensions:to_extension/1.

erlang_to_fhir(<<"reference_">>) -> <<"reference">>;
erlang_to_fhir(<<"when_">>) -> <<"when">>;
erlang_to_fhir(<<"start_">>) -> <<"start">>;
erlang_to_fhir(<<"end_">>) -> <<"end">>;
erlang_to_fhir(Key) -> Key.

fhir_to_erlang(<<"reference">>) -> <<"reference_">>;
fhir_to_erlang(<<"when">>) -> <<"when_">>;
fhir_to_erlang(<<"start">>) -> <<"start_">>;
fhir_to_erlang(<<"end">>) -> <<"end_">>;
fhir_to_erlang(Key) -> Key.
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

