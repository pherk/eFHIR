-ifndef(fhir_400).

-define(fhir_xsd, 
    #{
       <<"Element">>         => {undefined, [{<<"Extension">>, {<<"Extension">>, extension, optional}}]}
     , <<"DomainResource">>   => {<<"Resource">>,  [{<<"text">>, {<<"Narrative">>, complex, optional}},
                                 {<<"contained">>, {<<"ResourceContainer">>, container, list}},
                                 {<<"extension">>, {<<"Extension">>, extension, list}},
                                 {<<"modifierExtension">>, {<<"extension">>, extension, list}}]}
     , <<"Resource">>        => {undefined, [{<<"id">>, {<<"binary">>, primitive, optional}},
                                 {<<"meta">>, {<<"meta">>, complex, optional}},
                                 {<<"implicitRules">>, {<<"uri">>, primitive, optional}},
                                 {<<"language">>, {<<"binary">>, primitive, optional}}]}
     , <<"Address">>         => {<<"Element">>,  [{<<"use">>, {binary, optional}},
                                 {<<"type">>, {binary, optional}},
                                 {<<"text">>, {binary, optional}},
                                 {<<"line">>, {binary, list}},
                                 {<<"city">>, {binary, optional}},
                                 {<<"district">>, {binary, optional}},
                                 {<<"state">>, {binary, optional}},
                                 {<<"postalCode">>, {binary, optional}},
                                 {<<"country">>, {binary, optional}},
                                 {<<"period">>, {period, optional}}]}
     , <<"Annotation">>      => {<<"Element">>,  [{<<"authorReference">>, {reference, optional}},
                                 {<<"time">>, {dateTime, optional}},
                                 {<<"text">>, {markdown, required}}]}
     , <<"Attachment">>      => {<<"Element">>,  [{<<"contentType">>, {binary, optional}},
                                 {<<"langugae">>, {binary, optional}},
                                 {<<"data">>, {base64Binary, optional}},
                                 {<<"url">>, {binary, optional}},
                                 {<<"size">>, {integer, optional}},
                                 {<<"hash">>, {base64Binary, optional}},
                                 {<<"title">>, {binary, optional}},
                                 {<<"creation">>, {date, optional}}]}
     , <<"CodeableConcept">> => {<<"Element">>,  [{<<"coding">>, {coding, list}}, 
                                 {<<"text">>, {binary, optional}}]}
%%         , <<"Coding">> => {<<"Element">>, [
%%            {<<"system">>, {<<"uri">>, optional}}
%%            {<<"version">>, {<<"string">>, optional}}
%%            {<<"code">>, {<<"code">>, optional}}
%%            {<<"display">>, {<<"string">>, optional}}
%%            {<<"userSelected">>, {<<"boolean">>, optional}}
%%            ]}
     , <<"Coding">>          => {<<"Element">>,  [{<<"system">>, {uri, optional}},
                                 {<<"version">>, {binary, optional}},
                                 {<<"code">>, {binary, optional}},
                                 {<<"display">>, {binary, optional}},
                                 {<<"userSelected">>, {boolean, optional}}]}
     , <<"ContactPoint">>    => {<<"Element">>,  [{<<"value">>, {binary, optional}},
                                 {<<"rank">>, {positiveInt, optional}},
                                 {<<"period">>, {period, optional}}]}
     , <<"HumanName">>       => {<<"Element">>,  [{<<"use">>, {binary, optional}},
                                 {<<"text">>, {binary, optional}},
                                 {<<"family">>, {binary, optional}},
                                 {<<"given">>, {binary, list}},
                                 {<<"prefix">>, {binary, list}},
                                 {<<"suffix">>, {binary, list}},
                                 {<<"period">>, {period, optional}}]}
     , <<"Identifier">>      => {<<"Element">>,  [{<<"use">>, {binary, optional}},
                                 {<<"type">>, {codeableConcept, optional}},
                                 {<<"system">>, {uri, optional}},
                                 {<<"value">>, {binary, optional}},
                                 {<<"period">>, {period, optional}},
                                 {<<"assigner">>, {reference, optional}}]}
     , <<"Period">>          => {<<"Element">>,  [{<<"start">>, {dateTime, optional}},
                                 {<<"end">>, {dateTime, optional}}]}
     , <<"Quantity">>        => {<<"Element">>,  [{<<"value">>, {float, optional}},
                                 {<<"comparator">>, {binary, optional}},
                                 {<<"unit">>, {binary, optional}},
                                 {<<"system">>, {binary, optional}},
                                 {<<"code">>, {binary, optional}}]}
     , <<"Range">>           => {<<"Element">>,  [{<<"low">>, {quantity, optional}},
                                 {<<"high">>, {quantity, optional}}]}
     , <<"Ratio">>           => {<<"Element">>,  [{<<"numerator">>, {quantity, optional}},
                                 {<<"denominator">>, {quantity, optional}}]}
     , <<"Repeat">>          => {<<"Element">>,  [{<<"boundsPeriod">>, {period, optional}},
                                 {<<"count">>, {positiveInt, optional}},
                                 {<<"countMax">>, {positiveInt, optional}},
                                 {<<"duration">>, {decimal, optional}},
                                 {<<"durationMax">>, {decimal, optional}},
                                 {<<"durationUnit">>, {ucum, optional}},
                                 {<<"frequency">>, {positiveInt, optional}},
                                 {<<"frequencyMax">>, {positiveInt, optional}},
                                 {<<"period">>, {decimal, optional}},
                                 {<<"periodMax">>, {decimal, optional}},
                                 {<<"periodUnit">>, {code, optional}},
                                 {<<"dayOfWeek">>, {code, list}},
                                 {<<"timeOfDay">>, {time, list}},
                                 {<<"when">>, {code, list}},
                                 {<<"offset">>, {unsignedInt, optional}}]}
     , <<"Signature">>       => {<<"Element">>,  [{<<"type">>, {coding, non_empty_list}},
                                 {<<"when">>, {instant, required}},
                                 {<<"whoReference">>, {reference, required}},
                                 {<<"onBehalfOfReference">>, {reference, optional}},
                                 {<<"targetFormat">>, {code, optional}},
                                 {<<"sigFormat">>, {code, optional}},
                                 {<<"data">>, {base64Binary, optional}}]}
     , <<"Timing">>          => {<<"Element">>,  [{<<"event">>, {dateTime, list}},
                                 {<<"repeat">>, {repeat, optional}},
                                 {<<"code">>, {codeableConcept, optional}}]}
%% Special
     , <<"Reference">>       => {<<"Element">>,  [{<<"reference">>, {binary, optional}},
%                                 {<<"type">>, {uri, optional}},
%                                 {<<"identifier">>, {identifier, optional}},
                                 {<<"display">>, {binary, optional}}]}
     , <<"Narrative">>       => {<<"Element">>,  [{<<"status">>, {code, required}},
                                 {<<"div">>, {binary, required}}]}
     , <<"Meta">>            => {<<"Element">>,  [{<<"versionId">>, {binary, optional}},
                                 {<<"lastUpdated">>, {dateTime, optional}},
                                 {<<"source">>, {binary, optional}},
                                 {<<"profile">>, {uri, list}},
                                 {<<"security">>, {coding, list}},
                                 {<<"tag">>, {coding, list}},
                                 {<<"extension">>, {extension, list}}]}
     , <<"RelatedArtifact">> => {<<"Element">>,  [{<<"type">>, {binary, optional}},
                                 {<<"display">>, {binary, optional}},
                                 {<<"citation">>, {binary, optional}},
                                 {<<"url">>, {binary, optional}},
                                 {<<"document">>, {attachment, optional}},
                                 {<<"resource">>, {reference, optional}}]}
%%
%% Resources
%%
%%
%% Patient
%% Demographics and other administrative information about an individual or animal receiving care or other health-related services.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Patient">> => {<<"DomainResource">>, [
              {<<"identifier">>, {<<"Identifier">>, complex, list}}
            , {<<"active">>, {<<"boolean">>, primitive, optional}}
            , {<<"name">>, {<<"HumanName">>, complex, list}}
            , {<<"telecom">>, {<<"ContactPoint">>, complex, list}}
            , {<<"gender">>, {<<"AdministrativeGender">>, code, optional}}
            , {<<"birthDate">>, {<<"date">>, primitive, optional}}
            , {<<"deceasedBoolean">>, {<<"boolean">>, primitive, optional}}
            , {<<"deceasedDateTime">>, {<<"dateTime">>, primitive, optional}}
            , {<<"address">>, {<<"Address">>, complex, list}}
            , {<<"maritalStatus">>, {<<"CodeableConcept">>, complex, optional}}
            , {<<"multipleBirthBoolean">>, {<<"boolean">>, primitive, optional}}
            , {<<"multipleBirthInteger">>, {<<"integer">>, primitive, optional}}
            , {<<"photo">>, {<<"Attachment">>, complex, list}}
            , {<<"contact">>, {<<"Patient.Contact">>, bbelement, list}}
            , {<<"communication">>, {<<"Patient.Communication">>, bbelement,list}}
            , {<<"generalPractitioner">>, {<<"Reference">>, complex, list}}
            , {<<"managingOrganization">>, {<<"Reference">>, complex, optional}}
            , {<<"link">>, {<<"Patient.Link">>, bbelement, list}}
            ]} 
%%
%% Patient.Contact
%% Demographics and other administrative information about an individual or animal receiving care or other health-related services.
%%
    , <<"Patient.Contact">> => {<<"BackboneElement">>, [
              {<<"relationship">>, {<<"CodeableConcept">>, complex, list}}
            , {<<"name">>, {<<"HumanName">>, complex, optional}}
            , {<<"telecom">>, {<<"ContactPoint">>, complex, list}}
            , {<<"address">>, {<<"Address">>, complex, optional}}
            , {<<"gender">>, {<<"AdministrativeGender">>, code, optional}}
            , {<<"organization">>, {<<"Reference">>, complex, optional}}
            , {<<"period">>, {<<"Period">>, complex, optional}}
            ]} 
%%
%% Patient.Communication
%% Demographics and other administrative information about an individual or animal receiving care or other health-related services.
%%
    , <<"Patient.Communication">> => {<<"BackboneElement">>, [
              {<<"language">>, {<<"CodeableConcept">>, complex, required}}
            , {<<"preferred">>, {<<"boolean">>, complex, optional}}
            ]} 
%%
%% Patient.Link
%% Demographics and other administrative information about an individual or animal receiving care or other health-related services.
%%
    , <<"Patient.Link">> => {<<"BackboneElement">>, [
              {<<"other">>, {<<"Reference">>, complex, required}}
            , {<<"type">>, {<<"LinkType">>, code, required}}
            ]}
         }).

%%
%% reserved key clashes
%% when, start end, reference
%%
%%
%%

-type prop_info() :: {binary(), prop_type(), occurs()}.
-type prop_type() ::
       single()
     | cluster()
     | metadata()
     | special().

-type single() ::
       base64Binary
     | binary
     | boolean
     | canonical
     | code
     | date
     | dateTime
     | decimal
     | dow
     | id
     | instant
     | markdown
     | oid
     | positiveInt
     | time
     | ucum
     | unsignedInt
     | uuid
     | uri
     | url.

-type cluster() ::
       address
     | annotation
     | attachment
     | codeableConcept
     | coding
     | contactPoint
     | humanName
     | identifier
     | period
     | quantity
     | range
     | ratio
     | repeat.

-type metadata() ::
       contactDetail
     | relatedArtefact.

-type special() ::
       extension
     | meta
     | narrative
     | reference.

-type occurs() ::
       optional
     | required
     | non_empty_list
     | list.

-endif.
