module Json.LD.Context exposing (Context, decoder, update)

{-| JSON-LD Context

This module contains the context related algorithms described in "JSON-LD
1.0 Processing Algorithms and API" (<https://www.w3.org/TR/json-ld-api/>)

-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Value exposing (JsonValue(..))
import List.Extra
import RDF.IRI as IRI exposing (IRI)



-- Association List helpers


type alias AssocList a =
    List ( String, a )


{-| Get element with specified key from an association list
-}
get : String -> AssocList a -> Maybe a
get key assocList =
    List.Extra.find (\( k, v ) -> k == key) assocList
        |> Maybe.map Tuple.second


{-| Remove entries with given key
-}
remove : String -> AssocList a -> AssocList a
remove key assocList =
    List.filter (\( k, _ ) -> k /= key) assocList


{-| Return list of keys in association list
-}
keys : AssocList a -> List String
keys =
    List.map Tuple.first



--


{-| JSON-LD Context (see: <https://www.w3.org/TR/json-ld/#the-context>)
-}
type alias Context =
    { -- The active context contains the active term definitions which specify how properties and values have to be interpreted
      termDefinitions : Dict String TermDefinition

    -- as well as the current base IRI
    , baseIRI : Maybe IRI

    -- the vocabulary mapping
    , vocabularyMapping : Maybe IRI

    -- the default language.
    , defaultLanguage : Maybe String
    }


{-| An empty context
-}
empty : Context
empty =
    { termDefinitions = Dict.empty
    , baseIRI = Nothing
    , vocabularyMapping = Nothing
    , defaultLanguage = Nothing
    }


{-| TODO
-}
type alias TermDefinition =
    -- Each term definition consists of
    { -- an IRI mapping,
      mapping : String

    -- a boolean flag reverse property,
    , reverse : Bool

    -- an optional type mapping
    , typeMapping : Maybe String

    -- or language mapping,
    -- TODO
    -- and an optional container mapping.
    -- TODO
    }



-- Error handling


type Error
    = InvalidLocalContext
    | InvalidBaseIRI
    | InvalidVocabMapping
    | InvalidDefaultLanguage
    | CyclicIRIMapping
    | KeywordRedefinition
    | InvalidTermDefinition
    | InvalidTypeMapping


{-| Json.Decode requires errors to be printable as string
-}
errorToString : Error -> String
errorToString error =
    case error of
        InvalidLocalContext ->
            "An invalid local context was detected."

        InvalidBaseIRI ->
            "An invalid base IRI has been detected, i.e., it is neither an absolute IRI nor null."

        InvalidVocabMapping ->
            "An invalid vocabulary mapping has been detected, i.e., it is neither an absolute IRI nor null."

        InvalidDefaultLanguage ->
            "The value of the default language is not a string or null and thus invalid."

        CyclicIRIMapping ->
            "A cycle in IRI mapping has been detected."

        KeywordRedefinition ->
            "A keyword redefinition has been detected."

        InvalidTermDefinition ->
            "An invalid term definition has been detected."

        InvalidTypeMapping ->
            "An @type member in a term definition was encountered whose value could not be expanded to an absolute IRI."


{-| Helper to convert Result to Json.Decode.Decoder
-}
resultToDecoder : Result Error Context -> Decoder Context
resultToDecoder result =
    case result of
        Ok c ->
            JD.succeed c

        Err e ->
            e
                |> errorToString
                |> JD.fail



-- Context Processing Algorithms


{-| Update active context with a local context (see <https://www.w3.org/TR/json-ld-api/#context-processing-algorithms>)
-}
update : List IRI -> JsonValue -> Context -> Result Error Context
update remote localContextValue active =
    case localContextValue of
        NullValue ->
            -- 3.1) If context is null, set result to a newly-initialized active context and continue with the next context. The base IRI of the active context is set to the IRI of the currently being processed document (which might be different from the currently being processed context), if available; otherwise to null. If set, the base option of a JSON-LD API Implementation overrides the base IRI.
            Ok empty

        StringValue value ->
            -- 3.2) If context is a string,
            Debug.todo "resolve remote context"

        ObjectValue local ->
            active
                |> updateBaseIRI remote local
                |> Result.andThen (updateVocabMapping local)
                |> Result.andThen (updateDefaultLanguage local)
                |> Result.andThen (invokeCreateTermDefinition local)

        _ ->
            -- 3.3) If context is not a JSON object, an invalid local context error has been detected and processing is aborted.
            Err InvalidLocalContext


{-| Helper to invoke the Create Term Definition Algorithm
-}
invokeCreateTermDefinition : AssocList JsonValue -> Context -> Result Error Context
invokeCreateTermDefinition localContextWithStuffWeDontWant active =
    let
        -- Remove @base @vocab and @language keys
        local =
            localContextWithStuffWeDontWant
                |> remove "@base"
                |> remove "@vocab"
                |> remove "@language"
    in
    List.foldl
        (\term -> Result.andThen (createTermDefinition local term))
        (Ok ( active, Dict.empty ))
        (keys local)
        |> Result.map Tuple.first


{-| 6.2 Create Term Definition

This algorithm is called from the Context Processing algorithm to create a term definition in the active context for a term being processed in a local context.

-}
createTermDefinition : AssocList JsonValue -> String -> ( Context, Dict String Bool ) -> Result Error ( Context, Dict String Bool )
createTermDefinition local term ( active, defined ) =
    case Dict.get term defined of
        Just True ->
            -- 1) if defined contains the key term and the associated value is true (indicating that the term definition has already been created), return.
            Ok ( active, defined )

        Just False ->
            -- Otherwise, if the value is false, a cyclic IRI mapping error has been detected and processing is aborted.
            Err CyclicIRIMapping

        Nothing ->
            let
                inProgressDefined =
                    defined
                        |> Dict.insert term False
            in
            if List.member term keywords then
                -- 3) Since keywords cannot be overridden, term must not be a keyword. Otherwise, a keyword redefinition error has been detected and processing is aborted.
                Err KeywordRedefinition

            else
                let
                    -- Helpers to map fields from this blob of stuff that gets pipelined
                    mapActive f x =
                        { x | active = f x.active }

                    setDefined d x =
                        { x | defined = x.defined |> Dict.insert term d }

                    mapDefinition f x =
                        { x | definition = f x.definition }
                in
                Ok
                    { active = active

                    -- 2) Set the value associated with defined's term key to false. This indicates that the term definition is now being created but is not yet complete.
                    , defined = defined |> Dict.insert term False
                    }
                    -- 4) Remove any existing term definition for term in active context.
                    |> Result.map
                        (mapActive (\context -> { context | termDefinitions = context.termDefinitions |> Dict.remove term }))
                    -- 5) Initialize value to a copy of the value associated with the key term in local context.
                    |> Result.andThen
                        (\x ->
                            (case get term local of
                                Just NullValue ->
                                    -- initialize NullValue to an object with NullValue at @id.
                                    [ ( "@id", NullValue ) ]
                                        |> Ok

                                Just (StringValue id) ->
                                    -- 7) Otherwise, if value is a string, convert it to a JSON object consisting of a single member whose key is @id and whose value is value.
                                    [ ( "@id", StringValue id ) ]
                                        |> Ok

                                Just (ObjectValue v) ->
                                    -- 8) Otherwise, value must be a JSON object,
                                    v
                                        |> Ok

                                Just _ ->
                                    -- if not, an invalid term definition error has been detected and processing is aborted.
                                    Err InvalidTermDefinition

                                Nothing ->
                                    -- This never happens as we are iterating over keys in the local context.
                                    Err InvalidTermDefinition
                            )
                                |> Result.map
                                    (\value ->
                                        { active = x.active
                                        , value = value
                                        , defined = x.defined
                                        }
                                    )
                        )
                    |> Result.andThen
                        (\x ->
                            case get "@id" x.value of
                                Just NullValue ->
                                    -- 6) If value is null or value is a JSON object containing the key-value pair @id-null, set the term definition in active context to null, set the value associated with defined's key term to true, and return.
                                    x
                                        |> mapActive (\context -> { context | termDefinitions = context.termDefinitions |> Dict.remove term })
                                        |> setDefined True
                                        |> Ok

                                _ ->
                                    Ok x
                        )
                    -- 9) Create a new term definition, definition.
                    |> Result.map
                        (\x ->
                            { active = x.active
                            , value = x.value
                            , definition =
                                { mapping = ""

                                -- 12) Set the reverse property flag of definition to false.
                                , reverse = False
                                , typeMapping = Nothing
                                }
                            , defined = x.defined
                            }
                        )
                    -- 10) If value contains the key @type:
                    |> Result.andThen
                        (\x ->
                            -- 10.1) Initialize type to the value associated with the @type key, which must be a string.
                            case get "@type" x.value of
                                Just (StringValue typeMapping) ->
                                    x
                                        -- 10.2) Set type to the result of using the IRI Expansion algorithm, passing active context, type for value, true for vocab, false for document relative, local context, and defined. If the expanded type is neither @id, nor @vocab, nor an absolute IRI, an invalid type mapping error has been detected and processing is aborted.
                                        -- TODO: IRI Expansion
                                        |> mapDefinition (\definition -> { definition | typeMapping = Just typeMapping })
                                        |> Ok

                                -- TODO: here processing should return instead of continuing
                                Just _ ->
                                    -- Otherwise, an invalid type mapping error has been detected and processing is aborted.
                                    Err InvalidTypeMapping

                                Nothing ->
                                    Ok x
                        )
                    -- TODO 11, 13, 14, 15, 16, 17
                    -- 18) Set the term definition of term in active context to definition and set the value associated with defined's key term to true.
                    |> Result.map
                        (mapActive
                            (\context ->
                                { context
                                    | termDefinitions =
                                        context.termDefinitions
                                            |> Dict.insert term x.definition
                                }
                            )
                            >> setDefined True
                        )
                    -- Return active context and defined
                    |> Result.map (\x -> ( x.active, x.defined ))



-- update helpers


updateBaseIRI : List String -> AssocList JsonValue -> Context -> Result Error Context
updateBaseIRI remoteContexts contextValues result =
    -- 3.4) If context has an @base key and remote contexts is empty, i.e., the currently being processed context is not a remote context:
    case remoteContexts of
        [] ->
            case get "@base" contextValues of
                Just NullValue ->
                    -- 3.4.2) If value is null, remove the base IRI of result.
                    Ok { result | baseIRI = Nothing }

                Just (StringValue value) ->
                    let
                        iri =
                            IRI.fromString value
                    in
                    if IRI.isAbsolute iri then
                        -- 3.4.3) Otherwise, if value is an absolute IRI, the base IRI of result is set to value.
                        Ok { result | baseIRI = Just iri }

                    else if IRI.isRelative iri then
                        -- 3.4.4) Otherwise, if value is a relative IRI and the base IRI of result is not null, set the base IRI of result to the result of resolving value against the current base IRI of result.
                        -- TODO: "resolve against the current base IRI"
                        Ok { result | baseIRI = Just iri }

                    else
                        -- 3.4.5) Otherwise, an invalid base IRI error has been detected and processing is aborted.
                        Err InvalidBaseIRI

                Just _ ->
                    -- 3.4.5) Otherwise, an invalid base IRI error has been detected and processing is aborted.
                    Err InvalidBaseIRI

                Nothing ->
                    Ok result

        _ ->
            Ok result


updateVocabMapping : List ( String, JsonValue ) -> Context -> Result Error Context
updateVocabMapping contextValues result =
    -- 3.5) If context has an @vocab key:
    case get "@vocab" contextValues of
        Just NullValue ->
            -- 3.5.2) If value is null, remove any vocabulary mapping from result.
            Ok { result | vocabularyMapping = Nothing }

        Just (StringValue value) ->
            -- 3.5.3) Otherwise, if value is an absolute IRI or blank node identifier, the vocabulary mapping of result is set to value. If it is not an absolute IRI or blank node identifier, an invalid vocab mapping error has been detected and processing is aborted.
            if IRI.fromString value |> IRI.isAbsolute then
                Ok { result | vocabularyMapping = Just value }

            else if isBlankNodeIdentifier value then
                Ok { result | vocabularyMapping = Just value }

            else
                Err InvalidVocabMapping

        Just _ ->
            Err InvalidVocabMapping

        Nothing ->
            Ok result


updateDefaultLanguage : List ( String, JsonValue ) -> Context -> Result Error Context
updateDefaultLanguage contextValues result =
    case get "@language" contextValues of
        -- 3.6) If context has an @language key:
        Just NullValue ->
            -- 3.6.2) If value is null, remove any default language from result.
            Ok { result | defaultLanguage = Nothing }

        Just (StringValue value) ->
            -- 3.6.3) Otherwise, if value is string, the default language of result is set to lowercased value.
            Ok { result | defaultLanguage = value |> String.toLower |> Just }

        Just _ ->
            -- If it is not a string, an invalid default language error has been detected and processing is aborted.
            Err InvalidDefaultLanguage

        Nothing ->
            Ok result



-- more helpers


{-| Detect if string is a blank node identifier (<https://www.w3.org/TR/json-ld-api/#dfn-blank-node>)
-}
isBlankNodeIdentifier : String -> Bool
isBlankNodeIdentifier =
    String.startsWith "_"


{-| List of JSON-LD keywords
-}
keywords : List String
keywords =
    [ "@context "
    , "@id"
    , "@value"
    , "@language"
    , "@type"
    , "@container"
    , "@list"
    , "@set"
    , "@reverse"
    , "@index"
    , "@base"
    , "@vocab"
    , "@graph"
    ]



--


{-| Decode a JSON-LD context
-}
decoder : Decoder Context
decoder =
    Json.Value.decoder
        |> JD.andThen
            (\json ->
                case json of
                    ObjectValue values ->
                        case get "@context" values of
                            Nothing ->
                                JD.succeed empty

                            Just local ->
                                update [] local empty
                                    |> resultToDecoder

                    _ ->
                        JD.succeed empty
            )
