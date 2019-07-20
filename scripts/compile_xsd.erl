-module(compile_xsd).
-compile(export_all).
%%%
%%% FHIR 4.0.0
%%% Datatypes
%%%
%%% transform parsed XSD DomainResources into modules with types and functions for decoding and encoding
%%% from and to proplists
%%%
-include("../include/fhir.hrl").
-include("../include/fhir_400.hrl").


-spec compile(Dir :: binary()) -> (ok | {error, binary()}).
compile(Dir) ->
    Types = maps:keys(?fhir_xsd),
    lists:foreach(fun (T) -> compile_type(T,Types,Dir) end, Types),
    ok.

compile_type(T,Types,Dir) ->
    case maps:get(T, ?fhir_xsd) of
        {<<"DomainResource">>, Info} -> BBEs = backbones(T,Types),
		                                module({T,Info},BBEs,Dir);
	_                                -> ok
    end.

backbones(M, Types) ->
    lists:filtermap(fun (T) -> backbone(T,M) end, Types).

backbone(T,DR) ->
    case binary:split(T,[DR,<<".">>]) of
        [<<>>,_] -> {<<"BackboneElement">>, Info} = maps:get(T, ?fhir_xsd),
                    {true, {T, Info}};
        _         -> false
    end.

    
module({Type,Info}=DR, Backbones, Dir) ->
    io:format("~p~n",[DR]),
    io:format("~p~n",[Backbones]),
    Module = string:lowercase(Type),
    File = list_to_binary([Dir,Module,<<".erl">>]),
    {ok, S} = file:open(File, [write]),
    io:format(S,"-module(~s).~n",[Module]),
    io:format(S,"%%%~n",[]),
    io:format(S,"%%% FHIR 4.0.0 ~s~n",[Type]),
    io:format(S,"%%%~n",[]),
    io:format(S,"-include(\"fhir.hrl\").~n",[]),
    io:format(S,"-include(\"primitives.hrl\").~n",[]),
    write_types([DR] ++ Backbones, S),
    io:format(S,"%%~n",[]),
    io:format(S,"%% API exports~n",[]),
    io:format(S,"%% -exports([]).~n",[]),
    io:format(S,"%%~n~n",[]),
    io:format(S,"%%=====================================================================~n",[]),
    io:format(S,"%% API exports~n",[]),
    io:format(S,"%%=====================================================================~n",[]),
    io:format(S,"%%~n",[]),
    write_funs([DR] ++ Backbones, S),
    file:close(S).

write_types(List, S) -> 
	lists:foreach(fun (T) -> write_type(T,S) end, List).

write_type({Type,Props}, S) ->
    io:format(S,"~n",[]),
    RecType = fhir_to_rec(Type),
    io:format(S,"-record(~s, {~n",[RecType]),
    RProps = resolve_base(Type,Props),       % prefix props from base type
    write_props(RProps,0,S),
    io:format(S,"    }).~n",[]),
    io:format(S,"-type ~s() :: #~s{}.~n~n",[RecType,RecType]).

write_props([],_,_) -> ok;
write_props([{Name,Info}|T],I,S) -> 
    io:format("~p~n",[Info]),
    Type = type(Info),
    case I of
        0 -> io:format(S,"      ~s :: ~s~n",[Name,Type]);
        _ -> io:format(S,"    , ~s :: ~s~n",[Name,Type])
    end,
    write_props(T,I+1,S).

type({TypeName, TypeClass, Rep}) ->
    Name = decapitalize(TypeName), 
    RepClass = case Rep of
        optional -> simple;
        required -> simple;
        list     -> list;
        non_empty_list -> list
        end,
    Type = case TypeClass of
        primitive -> [Name,<<"()">>];                             % fhir names are compatible
        complex   -> [<<"complex:">>,fhir_to_erl(Name),<<"()">>];
        extension -> [<<"extension:">>,fhir_to_erl(Name),<<"()">>];
        container -> [<<"container:">>,fhir_to_erl(Name),<<"()">>];
        code      -> [fhir_to_erl(Name),<<"()">>];
        bbelement -> [fhir_to_erl(Name),<<"()">>]
        end,
    case RepClass of
        simple -> list_to_binary(Type);
        list   -> list_to_binary([<<"[">>,Type,<<"]">>])
    end.

-spec resolve_base(Base :: binary()) -> list().
resolve_base(Base) ->
    resolve_base(Base,[]).
resolve_base(undefined, L) -> L;
resolve_base(<<"BackboneElement">>, L) -> L;
resolve_base(Base, L) ->
    {NewBase, BI} = maps:get(Base,?fhir_xsd),
    resolve_base(NewBase, BI++L).

write_funs(List, S) -> 
	lists:foreach(fun (T) -> write_fun(T,S) end, List).

write_fun({Type,Props}, S) ->
    RecType = fhir_to_rec(Type),
    Fun = list_to_binary([<<"to_">>,RecType]),
    io:format(S,"~s({Props}) -> ~s(Props);~n",[Fun,Fun]),
    io:format(S,"~s(Props) -> ~n",[Fun]),
    io:format(S,"    DT = complex:xsd_info(~p),~n",[Type]),
    io:format(S,"    #~s{~n",[RecType]),
    RProps = resolve_base(Type,Props),       % prefix props from base type
    write_values(RProps,0,S),
    io:format(S,"    }.~n~n",[]).

write_values([],_,_) -> ok;
write_values([{Name,_}|T],I,S) -> 
    case I of
        0 -> io:format(S,"      ~s = complex:get_value(~p, Props, DT)~n",[Name,Name]);
        _ -> io:format(S,"    , ~s = complex:get_value(~p, Props, DT)~n",[Name,Name])
    end,
    write_values(T,I+1,S).

fhir_to_erl(Type) -> string:replace(decapitalize(Type),".","_").
fhir_to_rec(Type) -> string:replace(string:lowercase(Type),".","_").

decapitalize(<<H,T/binary>>) ->
    First = string:to_lower(H),
    <<First,T/binary>>.

capitalize(S) ->
    F = fun([H|T]) -> [string:to_upper(H) | string:to_lower(T)] end,
    string:join(lists:map(F, string:tokens(S, " ")), " ").
