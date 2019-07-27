-module(complex).
-compile(export_all).
-include("primitives.hrl").
%%
%% API exports
%%
-export_type([address/0,annotation/0]).
-export_type([attachment/0]).
-export_type([coding/0,codeableConcept/0]).
-export_type([identifier/0]).
-export_type([humanName/0,contactPoint/0]).
-export_type([quantity/0,range/0,ratio/0]).
-export_type([period/0,repeat/0,timing/0]).
-export_type([signature/0]).


-record(address, {
       use                    :: code()     %% home | work | temp | old
     , type                   :: code()     %% postal | physical | both
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
      authorReference :: special:reference_()
    , time :: date()
    , text :: binary()
    }).
-type annotation() :: #annotation{}.

-record(attachment, { 
      contentType :: code()
    , language :: code()
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
      use                    :: code()     %% home | work | temp | old | mobile
    , system                 :: code()     %% phone | fax | email | pager | other
    , value                  :: binary()
    , rank                   :: non_neg_integer()
    , period                 :: period()
}).
-opaque contactPoint() :: #contactPoint{}.

-record(humanName, {
       use       = <<"official">>   :: code()  %% usual | official | temp | nickname | anonymous | old | maiden
     , text                   :: binary()
     , family                 :: [binary()]
     , given                  :: [binary()]
     , prefix                 :: [binary()]
     , suffix                 :: [binary()]
     , period                 :: period()
}).
-opaque humanName() :: #humanName{}.

-record(identifier, {
       use = <<"official">> :: code()    %% usual | official | temp | secondary 
     , type           :: codeableConcept() %% Description of fhir_identifier
     , system         :: uri()             %% The namespace for the fhir_identifier
     , value          :: binary()          %% The value that is unique
     , period         :: period()          %% Time period when id is/was valid for use
     , assigner       :: special:reference_()  %% Organization that issued id (may be just text)
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
    , whoReference :: special:reference_()
    , onBehalfOfReference :: special:reference_()
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

%%====================================================================
%% API functions
%%====================================================================
to_address({Props}) -> to_address(Props);
to_address(Props) -> 
    DT = decode:xsd_info(<<"Address">>),
    io:format("~p~n~p~n",[Props,DT]),
    #address{
      use        = decode:value(<<"use">>, Props, DT)
    , type       = decode:value(<<"type">>, Props, DT) 
    , text       = decode:value(<<"text">>, Props, DT)
    , line       = decode:value(<<"line">>, Props, DT)
    , city       = decode:value(<<"city">>, Props, DT) 
    , district   = decode:value(<<"distrinct">>, Props, DT) 
    , state      = decode:value(<<"state">>, Props, DT) 
    , postalCode = decode:value(<<"postalCode">>, Props, DT) 
    , country    = decode:value(<<"country">>, Props, DT) 
    , period     = decode:value(<<"period">>, Props, DT)
    }.

to_annotation({Props}) -> to_annotation(Props);
to_annotation(Props) ->
    DT = decode:xsd_info(<<"Annotation">>),
    io:format("~p~n~p~n",[Props,DT]),
    #annotation{
      authorReference = decode:value(<<"authorReference">>, Props, DT)
    , time = decode:value(<<"time">>, Props, DT)
    , text = decode:value(<<"text">>, Props, DT)
    }.

to_attachment({Props}) -> to_attachment(Props);
to_attachment(Props) -> 
    DT = decode:xsd_info(<<"Attachment">>),
    io:format("~p~n~p~n",[Props,DT]),
    #attachment{
      contentType = decode:value(<<"contentType">>, Props, DT)
    , language    = decode:value(<<"language">>, Props, DT)
    , data        = decode:value(<<"data">>, Props, DT)
    , url         = decode:value(<<"url">>, Props, DT)
    , size        = decode:value(<<"size">>, Props, DT)
    , hash        = decode:value(<<"hash">>, Props, DT)
    , title       = decode:value(<<"title">>, Props, DT)
    , creation    = decode:value(<<"creation">>, Props, DT)
    }.

to_coding({Props}) -> to_coding(Props);
to_coding(Props) ->
    DT = decode:xsd_info(<<"Coding">>),
    io:format("~p~n~p~n",[Props,DT]),
    #coding{
        system  = decode:value(<<"system">>, Props, DT)
      , version = decode:value(<<"version">>, Props, DT)
      , code    = decode:value(<<"code">>, Props, DT)
      , display = decode:value(<<"display">>, Props, DT)
      , userSelected = decode:value(<<"userSelected">>, Props, DT)
      }.

to_codeableConcept({Props}) -> to_codeableConcept(Props);
to_codeableConcept(Props) ->
    DT = decode:xsd_info(<<"CodeableConcept">>),
    io:format("~p~n~p~n",[Props,DT]),
    #codeableConcept{
        coding  = decode:value(<<"coding">>, Props, DT)
      , text = decode:value(<<"text">>, Props, DT)
      }.

to_contactPoint({Props}) -> to_contactPoint(Props);
to_contactPoint(Props) -> 
    DT = decode:xsd_info(<<"ContactPoint">>),
    io:format("~p~n~p~n",[Props,DT]),
    #contactPoint{
      use    = decode:value(<<"use">>, Props, DT)
    , system = decode:value(<<"system">>, Props, DT)
    , value  = decode:value(<<"value">>, Props, DT)
    , rank   = decode:value(<<"rank">>, Props, DT)
    , period = decode:value(<<"period">>, Props, DT)
    }.

to_humanName({Props}) -> to_humanName(Props);
to_humanName(Props) ->
    DT = decode:xsd_info(<<"HumanName">>),
    #humanName{
       use     = decode:value(<<"use">>, Props, DT) 
     , text    = decode:value(<<"text">>, Props, DT) 
     , family  = decode:value(<<"family">>, Props, DT) 
     , given   = decode:value(<<"given">>, Props, DT) 
     , prefix  = decode:value(<<"prefix">>, Props, DT) 
     , suffix  = decode:value(<<"suffix">>, Props, DT) 
     , period  = decode:value(<<"period">>, Props, DT)
    }.

to_identifier({Props}) -> to_identifier(Props);
to_identifier(Props) ->
    DT = decode:xsd_info(<<"Identifier">>),
    #identifier{
        use  = decode:value(<<"use">>, Props, DT)
      , type = decode:value(<<"type">>, Props, DT)
      , system = decode:value(<<"system">>, Props, DT)
      , value  = decode:value(<<"value">>, Props, DT)
      , period   = decode:value(<<"period">>, Props, DT)
      , assigner = decode:value(<<"assigner">>, Props, DT)
      }.

to_period({Props}) -> to_period(Props);
to_period(Props) ->
    DT = decode:xsd_info(<<"Period">>),
    #period{
        start_  = decode:value(<<"start">>, Props, DT)
      , end_    = decode:value(<<"end">>, Props, DT)
      }.

to_quantity({Props}) -> to_quantity(Props);
to_quantity(Props) ->
    DT = decode:xsd_info(<<"Quantity">>),
    #quantity{
        value = decode:value(<<"value">>, Props, DT)
      , comparator = decode:value(<<"comparator">>, Props, DT)
      , unit = decode:value(<<"unit">>, Props, DT)
      , system = decode:value(<<"system">>, Props, DT)
      , code = decode:value(<<"code">>, Props, DT)
      }.

to_range({Props}) -> to_range(Props);
to_range(Props) ->
    DT = decode:xsd_info(<<"Range">>),
    #range{
      low = decode:value(<<"low">>, Props, DT)
    , high = decode:value(<<"high">>, Props, DT)
    }.

to_ratio({Props}) -> to_ratio(Props);
to_ratio(Props) ->
    DT = decode:xsd_info(<<"Ratio">>),
    #ratio{
      numerator = decode:value(<<"numerator">>, Props, DT)
    , denominator = decode:value(<<"denominator">>, Props, DT)
    }.

to_repeat({Props}) -> to_repeat(Props);
to_repeat(Props) ->
    DT = decode:xsd_info(<<"Repeat">>),
    #repeat{
      boundsPeriod = decode:value(<<"boundsPeriod">>, Props, DT)
    , count = decode:value(<<"count">>, Props, DT)
    , countMax = decode:value(<<"countMax">>, Props, DT)
    , duration = decode:value(<<"duration">>, Props, DT)
    , durationMax = decode:value(<<"durationMax">>, Props, DT)
    , durationUnit = decode:value(<<"durationUnit">>, Props, DT)
    , frequency = decode:value(<<"frequency">>, Props, DT)
    , frequencyMax = decode:value(<<"frequencyMax">>, Props, DT)
    , period = decode:value(<<"period">>, Props, DT)
    , periodMax = decode:value(<<"periodMax">>, Props, DT)
    , periodUnit = decode:value(<<"periodUnit">>, Props, DT)
    , dayOfWeek = decode:value(<<"dayOfWeek">>, Props, DT)
    , timeOfDay = decode:value(<<"timeOfDay">>, Props, DT)
    , when_ = decode:value(<<"when_">>, Props, DT)
    , offset = decode:value(<<"offset">>, Props, DT)
    }.

to_signature({Props}) -> to_signature(Props);
to_signature(Props) ->
    DT = decode:xsd_info(<<"Signature">>),
    #signature{
      type = decode:value(<<"type">>, Props, DT)
    , when_ = decode:value(<<"when_">>, Props, DT)
    , whoReference = decode:value(<<"whoReference">>, Props, DT)
    , onBehalfOfReference = decode:value(<<"onBehalfOfReference">>, Props, DT)
    , contentType = decode:value(<<"contentType">>, Props, DT)
    , blob = decode:value(<<"blob">>, Props, DT)
    }.

to_timing({Props}) -> to_timing(Props);
to_timing(Props) ->
    DT = decode:xsd_info(<<"Timing">>),
    #timing{
      event = decode:value(<<"event">>, Props, DT)
    , repeat = decode:value(<<"repeat">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
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

complex_timing_test() ->
    ?asrtto(complex:to_timing({[{<<"event">>, [<<"2019-07-15T12:00:00">>]}]}),
            {timing,[<<"2019-07-15T12:00:00">>], undefined, undefined}).

-endif.

