-ifndef(codes).
%%%
%%%
%%% FHIR XSD schema converted to erlang map and types
%%% @version : 4.0.0
%%% do not edit or know what you are doing ;-)
%%% generated with convert-xsd-erlang.xql script
%%% comments, known deficits:
%%% - attributes exists only in Element (id) and Extension (url)
%%% - xs:choice are all converted as simple or (no restriction on parallel use)
%%% - Narrative has to be manually fixed (xhtml:div ref, no type)
%%% - simple elements are defined in XSD as a type which refers to a type with value attribute plus code-list simple type
%%%   these are converted to a union type, that means these type are not in the big map
%%%


-endif.

