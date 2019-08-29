-module(complex).
-compile(export_all).

-include("fhir.hrl").
-include("primitives.hrl").
%%
%% API exports
%%
-export_type(['Address'/0, 'Annotation'/0, 'Attachment'/0]).
-export_type(['Coding'/0, 'CodeableConcept'/0]).
-export_type(['Identifier'/0]).
-export_type(['HumanName'/0, 'ContactPoint'/0]).
-export_type(['Quantity'/0, 'Duration'/0, 'Range'/0, 'Ratio'/0]).
-export_type(['Money'/0]).
-export_type(['Period'/0, 'Timing'/0]).
-export_type(['Signature'/0]).
% Age, Distance, Count MoneyQ, SimpleQuantity 

-record('Address', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    ,  use                    :: code()     %% home | work | temp | old
     , type                   :: code()     %% postal | physical | both
     , text                   :: binary()
     , line                   :: [binary()]
     , city                   :: binary()
     , district               :: binary()
     , state                  :: binary()
     , postalCode             :: binary()
     , country                :: binary()
     , period                 :: 'Period'()
}).
-opaque 'Address'() :: #'Address'{}.

-record('Annotation', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , author :: special:'Reference'() | string() | undefined
    , time :: date() | undefined
    , text :: binary()
    }).
-type 'Annotation'() :: #'Annotation'{}.

-record('Attachment', { 
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , contentType :: code()
    , language :: code()
    , data :: base64Binary()
    , url :: binary()
    , size :: integer()
    , hash :: base64Binary()
    , title :: binary()
    , creation :: date()
    }).
-opaque 'Attachment'() :: #'Attachment'{}.

-record('Coding', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , system       :: uri()
    , version      :: binary()
    , code         :: binary()
    , display      :: binary()
    , userSelected :: boolean()
    }).
-opaque 'Coding'() :: #'Coding'{}.


-record('CodeableConcept', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , coding       :: ['Coding'()]      %% Coding Code defined by a terminology system
    , text         :: binary()        %% Plain text representation of the concept
}).
-opaque 'CodeableConcept'() :: #'CodeableConcept'{}.

-record('ContactPoint', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , use                    :: code()     %% home | work | temp | old | mobile
    , system                 :: code()     %% phone | fax | email | pager | other
    , value                  :: binary()
    , rank                   :: non_neg_integer()
    , period                 :: 'Period'()
}).
-opaque 'ContactPoint'() :: #'ContactPoint'{}.

-record('HumanName', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    ,  use       = <<"official">>   :: code()  %% usual | official | temp | nickname | anonymous | old | maiden
     , text                   :: binary()
     , family                 :: [binary()]
     , given                  :: [binary()]
     , prefix                 :: [binary()]
     , suffix                 :: [binary()]
     , period                 :: 'Period'()
}).
-opaque 'HumanName'() :: #'HumanName'{}.

-record('Money', {anyAttribs :: anyAttribs(),
    id :: string() | undefined,
    extension :: [extensions:'Extension'()] | undefined,
    value :: decimal() | undefined,
    currency :: code() | undefined}).

-type 'Money'() :: #'Money'{}.

-record('Identifier', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    ,  use = <<"official">> :: code()    %% usual | official | temp | secondary 
     , type           :: 'CodeableConcept'() %% Description of 'Identifier'
     , system         :: uri()             %% The namespace for the 'Identifier'
     , value          :: binary()          %% The value that is unique
     , period         :: 'Period'()          %% Time period when id is/was valid for use
     , assigner       :: special:'Reference'()  %% Organization that issued id (may be just text)
}).
-opaque 'Identifier'()   :: #'Identifier'{}.

-record('Period', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    ,  'start'      :: dateTime()
     , 'end'        :: dateTime()
}).
-opaque 'Period'() :: #'Period'{}.

-record('Quantity', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , value :: float()
    , comparator :: binary()
    , unit :: binary()
    , system :: binary()
    , code :: binary()
    }).
-opaque 'Quantity'() :: #'Quantity'{}.

-record('Duration', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , value :: float()
    , comparator :: binary()
    , unit :: binary()
    , system :: binary()
    , code :: binary()
    }).
-opaque 'Duration'() :: #'Duration'{}.

-record('Range', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , low :: 'Quantity'()
    , high :: 'Quantity'()
    }).
-opaque 'Range'() :: #'Range'{}.

-record('Ratio', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , numerator :: 'Quantity'()
    , denominator :: 'Quantity'()
    }).
-opaque 'Ratio'() :: #'Ratio'{}.

-record('Timing.Repeat', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , modifierExtension     :: [extensions:'Extension'()]
    , bounds :: 'Period'() | 'Range'() | 'Duration'() | undefined
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
    , 'when' :: [binary()]
    , offset :: integer()
    }).
-opaque 'Timing.Repeat'() :: #'Timing.Repeat'{}.

-record('Signature', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , type :: ['Coding']
    , 'when' :: binary()
    , who :: special:'Reference'() 
    , targetFormat :: code() | undefined
    , sigFormat :: code() | undefined
    , data :: base64Binary() | undefined
    }).
-opaque 'Signature'() :: #'Signature'{}.

-record('Timing', {
      anyAttribs  :: anyAttribs()
    , id          :: string()
    , extension     :: [extensions:'Extension'()]
    , modifierExtension     :: [extensions:'Extension'()]
    , event :: [binary()]
    , repeat :: 'Timing.Repeat'()
    , code :: 'CodeableConcept'()
    }).
-opaque 'Timing'() :: #'Timing'{}.

%%====================================================================
%% API functions
%%====================================================================
to_address({Props}) -> to_address(Props);
to_address(Props) -> 
    DT = decode:xsd_info(<<"Address">>),
    % io:format("~p~n~p~n",[Props,DT]),
    #'Address'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
    , use        = decode:value(<<"use">>, Props, DT)
    , type       = decode:value(<<"type">>, Props, DT) 
    , text       = decode:value(<<"text">>, Props, DT)
    , line       = decode:value(<<"line">>, Props, DT)
    , city       = decode:value(<<"city">>, Props, DT) 
    , district   = decode:value(<<"district">>, Props, DT) 
    , state      = decode:value(<<"state">>, Props, DT) 
    , postalCode = decode:value(<<"postalCode">>, Props, DT) 
    , country    = decode:value(<<"country">>, Props, DT) 
    , period     = decode:value(<<"period">>, Props, DT)
    }.

to_annotation({Props}) -> to_annotation(Props);
to_annotation(Props) ->
    DT = decode:xsd_info(<<"Annotation">>),
    % io:format("~p~n~p~n",[Props,DT]),
    #'Annotation'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
    , author = decode:value(<<"author">>, Props, DT)
    , time = decode:value(<<"time">>, Props, DT)
    , text = decode:value(<<"text">>, Props, DT)
    }.

to_attachment({Props}) -> to_attachment(Props);
to_attachment(Props) -> 
    DT = decode:xsd_info(<<"Attachment">>),
    % io:format("~p~n~p~n",[Props,DT]),
    #'Attachment'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
    , contentType = decode:value(<<"contentType">>, Props, DT)
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
    % io:format("~p~n~p~n",[Props,DT]),
    #'Coding'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
      , system  = decode:value(<<"system">>, Props, DT)
      , version = decode:value(<<"version">>, Props, DT)
      , code    = decode:value(<<"code">>, Props, DT)
      , display = decode:value(<<"display">>, Props, DT)
      , userSelected = decode:value(<<"userSelected">>, Props, DT)
      }.

to_codeableConcept({Props}) -> to_codeableConcept(Props);
to_codeableConcept(Props) ->
    DT = decode:xsd_info(<<"CodeableConcept">>),
    % io:format("~p~n~p~n",[Props,DT]),
    #'CodeableConcept'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
      , coding  = decode:value(<<"coding">>, Props, DT)
      , text = decode:value(<<"text">>, Props, DT)
      }.

to_contactPoint({Props}) -> to_contactPoint(Props);
to_contactPoint(Props) -> 
    DT = decode:xsd_info(<<"ContactPoint">>),
    % io:format("~p~n~p~n",[Props,DT]),
    #'ContactPoint'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
    , use    = decode:value(<<"use">>, Props, DT)
    , system = decode:value(<<"system">>, Props, DT)
    , value  = decode:value(<<"value">>, Props, DT)
    , rank   = decode:value(<<"rank">>, Props, DT)
    , period = decode:value(<<"period">>, Props, DT)
    }.

to_humanName({Props}) -> to_humanName(Props);
to_humanName(Props) ->
    DT = decode:xsd_info(<<"HumanName">>),
    #'HumanName'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
     , use     = decode:value(<<"use">>, Props, DT) 
     , text    = decode:value(<<"text">>, Props, DT) 
     , family  = decode:value(<<"family">>, Props, DT) 
     , given   = decode:value(<<"given">>, Props, DT) 
     , prefix  = decode:value(<<"prefix">>, Props, DT) 
     , suffix  = decode:value(<<"suffix">>, Props, DT) 
     , period  = decode:value(<<"period">>, Props, DT)
    }.

to_money({Props}) -> to_money(Props);
to_money(Props) ->
    DT = decode:xsd_info(<<"Money">>),
    #'Money'{
        anyAttribs = decode:attrs(Props, DT)
      , id         = decode:value(<<"id">>, Props, DT)
      , extension  = decode:value(<<"extension">>, Props, DT)
      , value      = decode:value(<<"value">>, Props, DT) 
      , currency   = decode:value(<<"currency">>, Props, DT) 
    }.

to_identifier({Props}) -> to_identifier(Props);
to_identifier(Props) ->
    DT = decode:xsd_info(<<"Identifier">>),
    #'Identifier'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
      , use  = decode:value(<<"use">>, Props, DT)
      , type = decode:value(<<"type">>, Props, DT)
      , system = decode:value(<<"system">>, Props, DT)
      , value  = decode:value(<<"value">>, Props, DT)
      , period   = decode:value(<<"period">>, Props, DT)
      , assigner = decode:value(<<"assigner">>, Props, DT)
      }.

to_period({Props}) -> to_period(Props);
to_period(Props) ->
    DT = decode:xsd_info(<<"Period">>),
    #'Period'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
      , 'start'  = decode:value(<<"start">>, Props, DT)
      , 'end'    = decode:value(<<"end">>, Props, DT)
      }.

to_quantity({Props}) -> to_quantity(Props);
to_quantity(Props) ->
    DT = decode:xsd_info(<<"Quantity">>),
    #'Quantity'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
      , value = decode:value(<<"value">>, Props, DT)
      , comparator = decode:value(<<"comparator">>, Props, DT)
      , unit = decode:value(<<"unit">>, Props, DT)
      , system = decode:value(<<"system">>, Props, DT)
      , code = decode:value(<<"code">>, Props, DT)
      }.

to_range({Props}) -> to_range(Props);
to_range(Props) ->
    DT = decode:xsd_info(<<"Range">>),
    #'Range'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
    , low = decode:value(<<"low">>, Props, DT)
    , high = decode:value(<<"high">>, Props, DT)
    }.

to_ratio({Props}) -> to_ratio(Props);
to_ratio(Props) ->
    DT = decode:xsd_info(<<"Ratio">>),
    #'Ratio'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
    , numerator = decode:value(<<"numerator">>, Props, DT)
    , denominator = decode:value(<<"denominator">>, Props, DT)
    }.

to_timingRepeat({Props}) -> to_timingRepeat(Props);
to_timingRepeat(Props) ->
    DT = decode:xsd_info(<<"Timing.Repeat">>),
    #'Timing.Repeat'{
        anyAttribs = decode:attrs(Props, DT)
    ,   id          = decode:value(<<"id">>, Props, DT)
    , extension    = decode:value(<<"extension">>, Props, DT)
    , modifierExtension    = decode:value(<<"modifierExtension">>, Props, DT)
    , bounds = decode:value(<<"bounds">>, Props, DT)
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
    , 'when' = decode:value(<<"when_">>, Props, DT)
    , offset = decode:value(<<"offset">>, Props, DT)
    }.

to_signature({Props}) -> to_signature(Props);
to_signature(Props) ->
    DT = decode:xsd_info(<<"Signature">>),
    #'Signature'{
        anyAttribs = decode:attrs(Props, DT)
      , id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , 'when' = decode:value(<<"when">>, Props, DT)
    , who = decode:value(<<"who">>, Props, DT)
    , targetFormat = decode:value(<<"targetFormat">>, Props, DT)
    , sigFormat = decode:value(<<"sigFormat">>, Props, DT)
    , data = decode:value(<<"data">>, Props, DT)
    }.

to_timing({Props}) -> to_timing(Props);
to_timing(Props) ->
    DT = decode:xsd_info(<<"Timing">>),
    #'Timing'{
        anyAttribs = decode:attrs(Props, DT)
    ,   id          = decode:value(<<"id">>, Props, DT)
      , extension    = decode:value(<<"extension">>, Props, DT)
    , modifierExtension    = decode:value(<<"modifierExtension">>, Props, DT)
    , event = decode:value(<<"event">>, Props, DT)
    , repeat = decode:value(<<"repeat">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    }.


%%%
%%% EUnit
%%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(asrtto(A, B), ?assertEqual(B, A)).
-define(asrtpr(A, B), ?assertEqual(B, fhir_utils:rec_to_prop(A))).

complex_annotation_test() ->
    ?asrtto(complex:to_annotation({[{<<"text">>, <<"test">>}]}),
            {'Annotation',[],undefined,[],undefined,undefined,<<"test">>}),
    ?asrtto(complex:to_annotation({[{<<"authorReference">>, {[{<<"reference">>, <<"u-admin">>}]}}, {<<"text">>, <<"test">>}]}),
            {'Annotation',[],undefined,[],
              {'Reference',[],undefined,[],<<"u-admin">>,undefined, undefined,undefined},
              undefined,<<"test">>}),
    ?asrtto(complex:to_annotation({[{<<"authorString">>, <<"u-admin">>}, {<<"text">>, <<"test">>}]}),
            {'Annotation',[],undefined,[], {<<"String">>,<<"u-admin">>}, undefined,<<"test">>}),
    ?asrtto(complex:to_annotation({[{<<"author">>, {[{<<"reference">>, <<"u-admin">>}]}}, {<<"text">>, <<"test">>}]}),
            {'Annotation',[],undefined,[],undefined,undefined,<<"test">>}).

complex_coding_test() ->
    ?asrtto(complex:to_coding({[{<<"code">>, <<"test">>}]}),
            {'Coding',[], undefined, [], undefined,undefined,<<"test">>,undefined,undefined}),
    ?asrtto(complex:to_coding({[{<<"userSelected">>, false}]}),
            {'Coding',[], undefined, [], undefined,undefined,undefined,undefined, false}),
    ?asrtto(complex:to_coding({[{<<"system">>,<<"http://eNahar.org/test">>}, {<<"code">>, <<"test">>},{<<"display">>,<<"test">>}]}),
            {'Coding',[],undefined,[],<<"http://eNahar.org/test">>,undefined,<<"test">>,<<"test">>,undefined}).

complex_humanName_test() ->
    ?asrtto(complex:to_humanName({[{<<"use">>, <<"official">>}]}),
            {'HumanName',[],undefined,[],<<"official">>,undefined,undefined,[],[],[],undefined}),
    ?asrtto(complex:to_humanName({[{<<"use">>, <<"official">>},{<<"family">>,<<"Sokolow">>},{<<"given">>,[<<"Nicolai">>]}]}),
            {'HumanName',[],undefined,[],<<"official">>,undefined,<<"Sokolow">>,[<<"Nicolai">>],[],[],undefined}).

complex_signature_test() ->
    ?asrtto(complex:to_signature({[{<<"type">>, [{[{<<"coding">>, {[{<<"code">>, <<"1.2.840.10065.1.12.1.1">>},
                                                                    {<<"display">>, <<"Author's signature">>}
                                                                   ]}
                                                   }]},
                                                 {[{<<"coding">>, {[{<<"code">>, <<"1.2.840.10065.1.12.1.2">>},
                                                                    {<<"display">>, <<"Coauthor's signature">>}
                                                                   ]}
                                                   }]} ] },
                                   {<<"when">>, <<"2019-01-01T12:00:00">>},
                                   {<<"who">>, {[{<<"reference">>, <<"p-21666">>},
                                                 {<<"type">>, <<"Practitioner">>}
                                                ]}}
                                  ]}),
            {'Signature',[],undefined,[],
                     [{'Coding',[],undefined,[],undefined,undefined, undefined,undefined,undefined},
                      {'Coding',[],undefined,[],undefined,undefined, undefined,undefined,undefined}],
                     <<"2019-01-01T12:00:00">>,
                     {'Reference',[],undefined,[],<<"p-21666">>, <<"Practitioner">>,undefined,undefined},
                     undefined,undefined,undefined}
           ).

complex_timing_test() ->
    ?asrtto(complex:to_timing({[{<<"event">>, [<<"2019-07-15T12:00:00">>]}]}),
            {'Timing',[],undefined,[],[], [<<"2019-07-15T12:00:00">>], undefined,undefined}).

-endif.

