-module(leave).

%%%
%%% Leave
%%% Non-FHIR resource
%%%

-include("ecal.hrl").
-include_lib("fhir/include/fhir.hrl").
-include_lib("fhir/include/primitives.hrl").

-export([to_leave/0]).


%%%    <cause value="U" label="Urlaub" allDay="true"/>
%%%    <cause value="SU" label="Sonderurlaub" allDay="true"/>
%%%    <cause value="DR" label="Dienstreise" allDay="false"/>
%%%    <cause value="SUoB" label="SU ohne Bezüge" allDay="true"/>
%%%    <cause value="AU" label="AU krank" allDay="true"/>
%%%    <cause value="AUwK" label="AU Kind krank" allDay="true"/>
%%%    <cause value="REHA" label="REHA (AU)" allDay="true"/>
%%%    <cause value="BV" label="Berufsverbot" allDay="true"/>
%%%    <cause value="MS" label="Mutterschutz" allDay="true"/>
%%%    <cause value="EZ" label="Elternzeit" allDay="true"/>
	
-defined(CAUSES, #{
    <<"FZA">>  => {<<"Freizeit">>, true},
	<<"ND">>   => {<<"Nachtdienst">>, true},
	<<"KS-1">> => {<<"Spädienst-1">>, false},
	<<"KS-2">> => {<<"Spädienst-2">>, false},
	<<"L">>    => {<<"Lehre">>, false},
	<<"AB">>   => {<<"Abwesenheit">>, false},
	<<"???">>  => {<<"???">>, false}
	}).
	
-record('Leave', {
          anyAttribs :: anyAttribs()
	, id :: id() | undefined
	, implicitRules :: uri() | undefined
	, language :: code() | undefined
	, meta :: complex:'Meta'() | undefined
	, extension :: [extensions:'Extension'()]
	, modifierExtension :: [extensions:'Extension'()]
	, text :: special:'Narrative'() | undefined
	, contained :: resource:'ResourceContainer'() | undefined
	, 'identifier' :: [complex:'Identifier'()]
        , status :: complex:'Coding'()
	, cause :: complex:'Coding'()
	, actor :: special:'Reference'()
	, allDay :: boolean() | undefined
	, summary :: string() | undefined
	, description :: string() | undefined
	, period :: complex:'Period'()
	}).
-type 'Leave'() :: #'Leave'().

%%%
%%% API
%%%
to_leave({Props}) -> to_leave(Props);
to_leave(Props) ->
    DT = decode:xsd_info(<<"Leave">>),
	#'Leave'{
	  anyAttribs = decode:attrs(Props, DT)
        , id = decode:value(<<"id">>, Props, DT),
        , meta = decode:value(<<"meta">>, Props, DT),
        , implicitRules = decode:value(<<"implicitRules">>, Props, DT),
        , language = decode:value(<<"language">>, Props, DT),
        , extension = decode:value(<<"extension">>, Props, DT),
        , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT),
        , text = decode:value(<<"text">>, Props, DT),
        , contained = decode:value(<<"contained">>, Props, DT),
	, 'identifier' = decode:value(<<"identifier">>, Props, DT),
        , status = decode:value(<<"status">>, Props, DT),
        , cause = decode:value(<<"cause">>, Props, DT),
        , actor = decode:value(<<"actor">>, Props, DT),
        , allDay = decode:value(<<"allDay">>, Props, DT),
        , summary = decode:value(<<"summary">>, Props, DT),
        , description = decode:value(<<"description">>, Props, DT),
        , period = decode:value(<<"period">>, Props, DT),
	}.

%%%
%%% Eunit tests
%%%
-ifdef(TEST).

-endif.
