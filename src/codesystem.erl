-module(codesystem).
%%%
%%% FHIR 4.0.0 CodeSystem
%%%
-include("fhir.hrl").
-include("primitives.hrl").
-include("codes.hrl").

-record('CodeSystem', { anyAttribs :: anyAttribs(),
      id :: id() | undefined
    , meta :: special:'Meta'() | undefined
    , implicitRules :: uri() | undefined
    , language :: code() | undefined
    , text :: special:'Narrative'() | undefined
    , contained :: [resource:'ResourceContainer'()]
    , extension :: [extensions:'Extension'()]
    , modifierExtension :: [extensions:'Extension'()]
    , url :: uri() | undefined
    , 'identifier' :: [complex:'Identifier'()]
    , version :: string() | undefined
    , name :: string() | undefined
    , title :: string() | undefined
    , status :: code()
    , experimental :: boolean() | undefined
    , date :: dateTime() | undefined
    , publisher :: string() | undefined
    , contact :: [complex:'ContactDetail'()]
    , description :: markdown() | undefined
    , useContext :: [complex:'UsageContext'()]
    , jurisdiction :: [complex:'CodeableConcept'()]
    , purpose :: markdown() | undefined
    , copyright :: markdown() | undefined
    , caseSensitive :: boolean() | undefined
    , valueSet :: canonical() | undefined
    , hierarchyMeaning :: code() | undefined
    , compositional :: boolean() | undefined
    , versionNeeded :: boolean() | undefined
    , content :: code()
    , supplements :: canonical() | undefined
    , count :: unsignedInt() | undefined
    , filter :: ['CodeSystem.Filter'()]
    , property :: ['CodeSystem.Property'()]
    , concept :: ['CodeSystem.Concept'()]
    }).
-type 'CodeSystem'() :: #'CodeSystem'{}.


-record('CodeSystem.Property', { anyAttribs :: anyAttribs(),
      code :: code()
    , uri :: uri() | undefined
    , description :: string() | undefined
    , type :: code()
    }).
-type 'CodeSystem.Property'() :: #'CodeSystem.Property'{}.


-record('CodeSystem.Filter', { anyAttribs :: anyAttribs(),
      code :: code()
    , description :: string() | undefined
    , operator :: [code()]
    , value :: string()
    }).
-type 'CodeSystem.Filter'() :: #'CodeSystem.Filter'{}.


-record('CodeSystem.Property1', { anyAttribs :: anyAttribs(),
      code :: code()
    , valueCode :: code()
    , valueCoding :: complex:'Coding'()
    , valueString :: string()
    , valueInteger :: integer()
    , valueBoolean :: boolean()
    , valueDateTime :: dateTime()
    , valueDecimal :: decimal()
    }).
-type 'CodeSystem.Property1'() :: #'CodeSystem.Property1'{}.


-record('CodeSystem.Designation', { anyAttribs :: anyAttribs(),
      language :: code() | undefined
    , use :: complex:'Coding'() | undefined
    , value :: string()
    }).
-type 'CodeSystem.Designation'() :: #'CodeSystem.Designation'{}.


-record('CodeSystem.Concept', { anyAttribs :: anyAttribs(),
      code :: code()
    , display :: string() | undefined
    , definition :: string() | undefined
    , designation :: ['CodeSystem.Designation'()]
    , property :: ['CodeSystem.Property1'()]
    , concept :: ['CodeSystem.Concept'()]
    }).
-type 'CodeSystem.Concept'() :: #'CodeSystem.Concept'{}.

%%
%% API exports
%% -exports([]).
%%

%%=====================================================================
%% API exports
%%=====================================================================
%%
to_codeSystem({Props}) -> to_codeSystem(Props);
to_codeSystem(Props) -> 
    DT = decode:xsd_info(<<"CodeSystem">>),
    #'CodeSystem'{
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
    , identifier = decode:value(<<"identifier">>, Props, DT)
    , version = decode:value(<<"version">>, Props, DT)
    , name = decode:value(<<"name">>, Props, DT)
    , title = decode:value(<<"title">>, Props, DT)
    , status = decode:value(<<"status">>, Props, DT)
    , experimental = decode:value(<<"experimental">>, Props, DT)
    , date = decode:value(<<"date">>, Props, DT)
    , publisher = decode:value(<<"publisher">>, Props, DT)
    , contact = decode:value(<<"contact">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , useContext = decode:value(<<"useContext">>, Props, DT)
    , jurisdiction = decode:value(<<"jurisdiction">>, Props, DT)
    , purpose = decode:value(<<"purpose">>, Props, DT)
    , copyright = decode:value(<<"copyright">>, Props, DT)
    , caseSensitive = decode:value(<<"caseSensitive">>, Props, DT)
    , valueSet = decode:value(<<"valueSet">>, Props, DT)
    , hierarchyMeaning = decode:value(<<"hierarchyMeaning">>, Props, DT)
    , compositional = decode:value(<<"compositional">>, Props, DT)
    , versionNeeded = decode:value(<<"versionNeeded">>, Props, DT)
    , content = decode:value(<<"content">>, Props, DT)
    , supplements = decode:value(<<"supplements">>, Props, DT)
    , count = decode:value(<<"count">>, Props, DT)
    , filter = decode:value(<<"filter">>, Props, DT)
    , property = decode:value(<<"property">>, Props, DT)
    , concept = decode:value(<<"concept">>, Props, DT)
    }.

to_codeSystem_property({Props}) -> to_codeSystem_property(Props);
to_codeSystem_property(Props) -> 
    DT = decode:xsd_info(<<"CodeSystem.Property">>),
    #'CodeSystem.Property'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , uri = decode:value(<<"uri">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , type = decode:value(<<"type">>, Props, DT)
    }.

to_codeSystem_filter({Props}) -> to_codeSystem_filter(Props);
to_codeSystem_filter(Props) -> 
    DT = decode:xsd_info(<<"CodeSystem.Filter">>),
    #'CodeSystem.Filter'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , description = decode:value(<<"description">>, Props, DT)
    , operator = decode:value(<<"operator">>, Props, DT)
    , value = decode:value(<<"value">>, Props, DT)
    }.

to_codeSystem_property1({Props}) -> to_codeSystem_property1(Props);
to_codeSystem_property1(Props) -> 
    DT = decode:xsd_info(<<"CodeSystem.Property1">>),
    #'CodeSystem.Property1'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , valueCode = decode:value(<<"valueCode">>, Props, DT)
    , valueCoding = decode:value(<<"valueCoding">>, Props, DT)
    , valueString = decode:value(<<"valueString">>, Props, DT)
    , valueInteger = decode:value(<<"valueInteger">>, Props, DT)
    , valueBoolean = decode:value(<<"valueBoolean">>, Props, DT)
    , valueDateTime = decode:value(<<"valueDateTime">>, Props, DT)
    , valueDecimal = decode:value(<<"valueDecimal">>, Props, DT)
    }.

to_codeSystem_designation({Props}) -> to_codeSystem_designation(Props);
to_codeSystem_designation(Props) -> 
    DT = decode:xsd_info(<<"CodeSystem.Designation">>),
    #'CodeSystem.Designation'{
      anyAttribs = decode:attrs(Props, DT)
    , language = decode:value(<<"language">>, Props, DT)
    , use = decode:value(<<"use">>, Props, DT)
    , value = decode:value(<<"value">>, Props, DT)
    }.

to_codeSystem_concept({Props}) -> to_codeSystem_concept(Props);
to_codeSystem_concept(Props) -> 
    DT = decode:xsd_info(<<"CodeSystem.Concept">>),
    #'CodeSystem.Concept'{
      anyAttribs = decode:attrs(Props, DT)
    , code = decode:value(<<"code">>, Props, DT)
    , display = decode:value(<<"display">>, Props, DT)
    , definition = decode:value(<<"definition">>, Props, DT)
    , designation = decode:value(<<"designation">>, Props, DT)
    , property = decode:value(<<"property">>, Props, DT)
    , concept = decode:value(<<"concept">>, Props, DT)
    }.

%%
%%=====================================================================
%% Eunit tests
%%=====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(asrtto(A, B), ?assertEqual(B, codesystem:to_codeSystem(A))).
-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).
-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).


-endif.
