-module(searchparameter).
%%%
%%% FHIR 4.0.0 SearchParameter
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('SearchParameter', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , url :: uri()
    , version :: string() | undefined
    , name :: string()
    , derivedFrom :: canonical() | undefined
    , status :: code()
    , experimental :: boolean() | undefined
    , date :: dateTime() | undefined
    , publisher :: string() | undefined
    , contact :: [complex:'ContactDetail'()]
    , description :: markdown()
    , useContext :: [complex:'UsageContext'()]
    , jurisdiction :: [complex:'CodeableConcept'()]
    , purpose :: markdown() | undefined
    , code :: code()
    , base :: [code()]
    , type :: code()
    , expression :: string() | undefined
    , xpath :: string() | undefined
    , xpathUsage :: code() | undefined
    , target :: [code()]
    , multipleOr :: boolean() | undefined
    , multipleAnd :: boolean() | undefined
    , comparator :: [code()]
    , modifier :: [code()]
    , chain :: [string()]
    , component :: ['SearchParameter.Component'()]
    }).
-type 'SearchParameter'() :: #'SearchParameter'{}.


-record('SearchParameter.Component', { anyAttribs :: anyAttribs(),
      definition :: canonical()
    , expression :: string()
    }).
-type 'SearchParameter.Component'() :: #'SearchParameter.Component'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_searchParameter({Props}) -> to_searchParameter(Props);
to_searchParameter(Props) -> 
    DT = decode:xsd_info(<<"SearchParameter">>),
    #'SearchParameter'{
      anyAttribs = decode:attrs(Props, DT)
    , id = decode:value(<<"id">>, Props, DT)
    , meta = decode:value(<<"meta">>, Props, DT)
    , implicitRules = decode:value(<<"implicitRules">>, Props, DT)
    , language = decode:value(<<"language">>, Props, DT)
    , text = decode:value(<<"text">>, Props, DT)
    , contained = decode:value(<<"contained">>, Props, DT)
    , extension = decode:value(<<"extension">>, Props, DT)
    , modifierExtension = decode:value(<<"modifierExtension">>, Props, DT)
    , url = decode:value(<<"url">>, Props, DT)
    , version = decode:value(<<"version">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , derivedFrom = decode:value(<<"derivedFrom">>, Props, DT)
    , status = decode:value(<<"status">>, Props, DT)
    , experimental = decode:value(<<"experimental">>, Props, DT)
    , date = decode:value(<<"date">>, Props, DT)
    , publisher = decode:value(<<"publisher">>, Props, DT)
    , contact = decode:value(<<"contact">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , useContext = decode:value(<<"useContext">>, Props, DT)
    , jurisdiction = decode:value(<<"jurisdiction">>, Props, DT)
    , purpose = decode:value(<<"purpose">>, Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , base = decode:value(<<"base">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    , expression = decode:value(<<"expression">>, Props, DT)
    , xpath = decode:value(<<"xpath">>, Props, DT)
    , xpathUsage = decode:value(<<"xpathUsage">>, Props, DT)
    , target = decode:value(<<"target">>, Props, DT)
    , multipleOr = decode:value(<<"multipleOr">>, Props, DT)
    , multipleAnd = decode:value(<<"multipleAnd">>, Props, DT)
    , comparator = decode:value(<<"comparator">>, Props, DT)
    , modifier = decode:value(<<"modifier">>, Props, DT)
    , chain = decode:value(<<"chain">>, Props, DT)
    , component = decode:value(<<"component">>, Props, DT)
    }.

to_searchParameter_component({Props}) -> to_searchParameter_component(Props);
to_searchParameter_component(Props) -> 
    DT = decode:xsd_info(<<"SearchParameter.Component">>),
    #'SearchParameter.Component'{
      anyAttribs = decode:attrs(Props, DT)
    , definition = decode:value(<<"definition">>, Props, DT)
    , expression = decode:value(<<"expression">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, searchparameter:to_searchParameter(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
