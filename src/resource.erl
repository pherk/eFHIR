-module(resource).
-include("primitives.hrl").

-export([to_resource/1, text/1]).

-type resourceContainer() ::
      patient:patient().
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
    case Type of
        <<"Patient">> -> patient:to_patient(Props)
    end.

resourceType({EJson}) -> resourceType(EJson);
resourceType(EJson) ->
    proplists:get_value(<<"resourceType">>, EJson).

text(R) ->
    Type = element(1,R),
    apply(Type,text,[R]).

