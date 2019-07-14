-ifndef(complex).

-define(ct_info, 
    #{
       <<"coding">>          => [{<<"system">>, {uri, optional}},
                                 {<<"version">>, {binary, optional}},
                                 {<<"code">>, {binary, optional}},
                                 {<<"display">>, {binary, optional}},
                                 {<<"userSelected">>, {boolean, optional}}]
     , <<"codeableConcept">> => [{<<"coding">>, {coding, list}}, 
                                 {<<"text">>, {binary, optional}}]
     , <<"identifier">>      => [{<<"use">>, {binary, optional}},
                                 {<<"type">>, {codeableConcept, optional}},
                                 {<<"system">>, {uri, optional}},
                                 {<<"value">>, {binary, optional}},
                                 {<<"period">>, {period, optional}},
                                 {<<"assigner">>, {reference, optional}}]
     , <<"period">>          => [{<<"start">>, {dateTime, optional}},
                                 {<<"end">>, {dateTime, optional}}]
     , <<"humanName">>       => [{<<"use">>, {binary, optional}},
                                 {<<"text">>, {binary, optional}},
                                 {<<"family">>, {binary, optional}},
                                 {<<"given">>, {binary, list}},
                                 {<<"prefix">>, {binary, list}},
                                 {<<"suffix">>, {binary, list}},
                                 {<<"period">>, {period, optional}}]
     , <<"address">>         => [{<<"use">>, {binary, optional}},
                                 {<<"type">>, {binary, optional}},
                                 {<<"text">>, {binary, optional}},
                                 {<<"line">>, {binary, list}},
                                 {<<"city">>, {binary, optional}},
                                 {<<"district">>, {binary, optional}},
                                 {<<"state">>, {binary, optional}},
                                 {<<"postalCode">>, {binary, optional}},
                                 {<<"country">>, {binary, optional}},
                                 {<<"period">>, {period, optional}}]
     , <<"contactPoint">>    => [{<<"value">>, {binary, optional}},
                                 {<<"rank">>, {positiveInt, optional}},
                                 {<<"period">>, {period, optional}}]
     , <<"relatedArtifact">> => [{<<"type">>, {binary, optional}},
                                 {<<"display">>, {binary, optional}},
                                 {<<"citation">>, {binary, optional}},
                                 {<<"url">>, {binary, optional}},
                                 {<<"document">>, {attachment, optional}},
                                 {<<"resource">>, {reference, optional}}]
     , <<"attachment">>      => [{<<"contentType">>, {binary, optional}},
                                 {<<"langugae">>, {binary, optional}},
                                 {<<"data">>, {base64Binary, optional}},
                                 {<<"url">>, {binary, optional}},
                                 {<<"size">>, {integer, optional}},
                                 {<<"hash">>, {base64Binary, optional}},
                                 {<<"title">>, {binary, optional}},
                                 {<<"creation">>, {date, optional}}]
     , <<"quantity">>        => [{<<"value">>, {float, optional}},
                                 {<<"comparator">>, {binary, optional}},
                                 {<<"unit">>, {binary, optional}},
                                 {<<"system">>, {binary, optional}},
                                 {<<"code">>, {binary, optional}}]
     , <<"range">>           => [{<<"low">>, {quantity, optional}},
                                 {<<"high">>, {quantity, optional}}]
     , <<"ratio">>           => [{<<"numerator">>, {quantity, optional}},
                                 {<<"denominator">>, {quantity, optional}}]
     , <<"timing">>          => [{<<"event">>, {dateTime, list}},
                                 {<<"repeat">>, {repeat, optional}},
                                 {<<"code">>, {codeableConcept, optional}}]
     , <<"repeat">>          => [{<<"boundsPeriod">>, {period, optional}},
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
                                 {<<"offset">>, {unsignedInt, optional}}]
     , <<"annotation">>      => [{<<"authorReference">>, {reference, optional}},
                                 {<<"time">>, {dateTime, optional}},
                                 {<<"text">>, {markdown, required}}]
     , <<"signature">>       => [{<<"type">>, {coding, non_empty_list}},
                                 {<<"when">>, {instant, required}},
                                 {<<"whoReference">>, {reference, required}},
                                 {<<"onBehalfOfReference">>, {reference, optional}},
                                 {<<"targetFormat">>, {code, optional}},
                                 {<<"sigFormat">>, {code, optional}},
                                 {<<"data">>, {base64Binary, optional}}]
     , <<"reference">>       => [{<<"reference">>, {binary, optional}},
%                                 {<<"type">>, {uri, optional}},
%                                 {<<"identifier">>, {identifier, optional}},
                                 {<<"display">>, {binary, optional}}]
     , <<"narrative">>       => [{<<"status">>, {code, required}},
                                 {<<"div">>, {binary, required}}]
     , <<"meta">>            => [{<<"versionId">>, {binary, optional}},
                                 {<<"lastUpdated">>, {dateTime, optional}},
                                 {<<"source">>, {binary, optional}},
                                 {<<"profile">>, {uri, list}},
                                 {<<"security">>, {coding, list}},
                                 {<<"tag">>, {coding, list}},
                                 {<<"extension">>, {extension, list}}]
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
