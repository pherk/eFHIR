-module(resource).
-include("primitives.hrl").

-export([fields/1, to_resource/1, text/1]).

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

fields('Address') -> complex:fields('Address');
fields('Annotation') -> complex:fields('Annotation');
fields('Attachment') -> complex:fields('Attachment');
fields('CodeableConcept') -> complex:fields('CodeableConcept');
fields('Coding') -> complex:fields('Coding');
fields('ContactPoint') -> complex:fields('ContactPoint');
fields('HumanName') -> complex:fields('HumanName');
fields('Identifier') -> complex:fields('Identifier');
fields('Money') -> complex:fields('Money');
fields('Period') -> complex:fields('Period');
fields('Quantity') -> complex:fields('Quantity');
fields('Range') -> complex:fields('Range');
fields('Ratio') -> complex:fields('Ration');
fields('Signature') -> complex:fields('Signature');
fields('Timing') -> complex:fields('Timing');
fields('Timing.Repeat') -> complex:fields('Timing.Repeat');
fields('Extension') -> extensions:fields('Extension');
fields('Patient') -> patient:fields('Patient');
fields('Patient.Communication') -> patient:fields('Patient.Communication');
fields('Patient.Contact') -> patient:fields('Patient.Contact');
fields('Patient.Link') -> patient:fields('Patient.Link').
