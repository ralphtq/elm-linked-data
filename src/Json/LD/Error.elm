module Json.LD.Error exposing (Error(..), toString)

{-| Error that may occur during JSON-LD processing
-}


{-| Error that may occur during JSON-LD processing
-}
type Error
    = -- Errors as defined by specification
      InvalidLocalContext
    | InvalidBaseIRI
    | InvalidVocabMapping
    | InvalidDefaultLanguage
    | CyclicIRIMapping
    | KeywordRedefinition
    | InvalidTermDefinition
    | InvalidTypeMapping
    | InvalidReverseProperty
    | InvalidIRIMapping
    | InvalidContainerMapping
    | InvalidLanguageMapping
    | InvalidKeywordAlias
    | ListOfLists
    | CollidingKeywords
    | InvalidIDValue
      -- Implementation errors
    | AlgorithmDidNotReturn
    | DeveloperGivingUp


{-| Json.Decode requires errors to be printable as string
-}
toString : Error -> String
toString error =
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

        InvalidReverseProperty ->
            "An invalid reverse property definition has been detected."

        InvalidIRIMapping ->
            "A local context contains a term that has an invalid or missing IRI mapping."

        InvalidContainerMapping ->
            "An @container member was encountered whose value was not one of the following strings: @list, @set, or @index."

        InvalidLanguageMapping ->
            "An @language member in a term definition was encountered whose value was neither a string nor null and thus invalid."

        InvalidKeywordAlias ->
            "An invalid keyword alias definition has been encountered."

        ListOfLists ->
            "A list of lists was detected. List of lists are not supported in this version of JSON-LD due to the algorithmic complexity."

        CollidingKeywords ->
            "Two properties which expand to the same keyword have been detected. This might occur if a keyword and an alias thereof are used at the same time."

        InvalidIDValue ->
            "An @id member was encountered whose value was not a string."

        AlgorithmDidNotReturn ->
            "Algorithm did not return"

        DeveloperGivingUp ->
            "This shit is just not nice"
