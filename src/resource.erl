-module(resource).
-include("primitives.hrl").

-export([to_resource/1, text/1]).

-type 'ResourceContainer'() ::
      activitydefinition:'ActivityDefinition'()
    | allergyintolerance:'AllergyIntolerance'()
    | careplan:'CarePlan'()
    | careteam:'CareTeam'()
    | claim:'Claim'()
    | composition:'Composition'()
    | communication:'Communication'()
    | condition:'Condition'()
    | consent:'Consent'()
    | coverage:'Coverage'()
    | device:'Device'()
    | diagnosticreport:'DiagnosticReport'()
    | encounter:'Encounter'()
    | episodeofcare:'EpisodeOfCare'()
    | explanationofbenefit:'ExplanationOfBenefit'()
    | goal:'Goal'()
    | group:'Group'()
    | imagingstudy:'ImagingStudy'()
    | immunization:'Immunization'()
    | library:'Library'()
    | location:'Location'()
    | medicationrequest:'MedicationRequest'()
    | observation:'Observation'()
    | organization:'Organization'()
    | operationoutcome:'OperationOutcome'()
    | patient:'Patient'()
    | plandefinition:'PlanDefinition'()
    | practitioner:'Practitioner'()
    | practitionerrole:'PractitionerRole'()
    | procedure:'Procedure'()
    | provenance:'Provenance'()
    | questionnaire:'Questionnaire'()
    | questionnaireresponse:'QuestionnaireResponse'()
    | requestgroup:'RequestGroup'()
    | servicerequest:'ServiceRequest'()
    | task:'Task'().

to_resource(Props) ->
    Type = resourceType(Props),
    io:format("resource: ~p~n",[Type]),
    {Mod, Fun} = fhir_utils:type_to_fun(Type),
    % io:format("validate: apply: ~s:~s~n",[Mod,Fun]),
    apply(Mod, Fun,[Props]).


resourceType({EJson}) -> resourceType(EJson);
resourceType(EJson) ->
    proplists:get_value(<<"resourceType">>, EJson).

text(R) ->
    {Mod, _} = fhir_utils:type_to_fun(atom_to_binary(element(1,R),latin1)),
    apply(Mod,text,[R]).

