-module(resource).
-include("primitives.hrl").

-export([to_resource/1, text/1]).

-type 'ResourceContainer'() ::
      patient:'Patient'().
%%      careplan()
%%    | careteam()
%%    | composition()
%%    | condition()
%%    | consent()
%%    | encounter()
%%    | episodeocare()
%%    | goal()
%%    | patient()
%%    | requestgroup()
%%    | task().

to_resource(Props) ->
    Type = resourceType(Props),
    io:format("resource: ~p~n",[Type]),
     R = string:lowercase(Type),
     Fun = list_to_binary([<<"to_">>,R]),
     io:format("validate: apply: ~s:~s~n",[R,Fun]),
     apply(binary_to_atom(R,utf8),binary_to_atom(Fun,utf8),[Props]).
%    case Type of
%        <<"Patient">> -> patient:to_patient(Props)
%    end.


resourceType({EJson}) -> resourceType(EJson);
resourceType(EJson) ->
    proplists:get_value(<<"resourceType">>, EJson).

text(R) ->
    Type = element(1,R),
    apply(Type,text,[R]).

