module TalkToQUDT exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser as B
import Debug
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD
import Json.Encode as JE
import RDF
import RDF.Decode
import RDF.JSON
import Return exposing (Return)
import Url

-- Types

type alias SPARQLquery =
    String


-- Config


qudtQueryEndpointURL : String
qudtQueryEndpointURL =
    "https://www.qudt.org/fuseki/qudt/query"


prefixForOWL =
    Url.percentEncode "prefix owl:  <http://www.w3.org/2002/07/owl#>"

prefixForRDFS =
    Url.percentEncode "prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>"

prefixForQUDT =
   Url.percentEncode "prefix qudt: <http://qudt.org/schema/qudt/>"

encodedPrefixes =
    prefixForOWL ++ prefixForQUDT ++ prefixForRDFS


queryQUDTclasses =
    """
 SELECT ?class ?label ?description
 WHERE {
     ?class a owl:Class .
     OPTIONAL {?class rdfs:label ?label}
     OPTIONAL {?class rdfs:comment ?description}
 }
 """

queryQUDTunits =
    """
 SELECT ?unit ?p ?o
 WHERE {
     ?unit a qudt:Unit .
     ?unit ?p ?o .
 }
 """

queryCountOfUnits =
    """
 SELECT 
    (COUNT(?unit) AS ?numberOfUnits) 
    (COUNT(?qk) AS ?numberOfQuantityKinds)
    (COUNT(?constant) AS ?numberOfConstants)
 WHERE {
    {?unit a qudt:Unit }
    UNION
    {?qk a qudt:QuantityKind}
    UNION
    {?constant a qudt:PhysicalConstant}
 }
 """


queryQUDTtriples =
    """
 SELECT ?subject ?predicate ?object
 WHERE {
   ?subject ?predicate ?object
 }
 """


sparqlQuery : String -> Maybe Int -> String
sparqlQuery query max =
    case max of
      Just n -> query ++ "LIMIT" ++ " " ++ String.fromInt n
      Nothing -> query


queryUnits =
    encodedPrefixes ++ String.replace "%20" "+" (Url.percentEncode (sparqlQuery queryQUDTunits <| Just 40000))

queryCounts =
    encodedPrefixes ++ String.replace "%20" "+" (Url.percentEncode (sparqlQuery queryCountOfUnits Nothing))

queryClasses =
    encodedPrefixes ++ String.replace "%20" "+" (Url.percentEncode (sparqlQuery queryQUDTclasses <| Just 2000))


queryTriples =
    encodedPrefixes ++ String.replace "%20" "+" (Url.percentEncode (sparqlQuery queryQUDTtriples <| Just 1000))


main : Program {} Model Msg
main =
    B.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL


type alias Model =
    { graph : RDF.Graph
    , content : String
    , query : String
    }


init : {} -> Return Msg Model
init flags =
    { graph = RDF.fromList []
    , content = ""
    , query = ""
    }
        |> Return.singleton


queryEndpoint : SPARQLquery -> Cmd Msg
queryEndpoint query =
    Http.request
        { method = "POST"
        , headers = [ 
              Http.header "Authorization" "Basic" 
            -- , Http.header "Content-Type" "application/sparql-results+json;charset=utf-8"
             ]
        , url = "https://www.qudt.org/fuseki/qudt/query"
        , body = Http.stringBody "application/x-www-form-urlencoded" ("query=" ++ query)
        -- , body = Http.stringBody "application/sparql-results+json;charset=utf-8" ("query=" ++ query)
        , expect = Http.expectString GotQueryResponse
        , timeout = Nothing
        , tracker = Nothing
        }



-- UPDATE


type Msg
    = Receive (Result Http.Error RDF.Graph)
    | UpdateContent String
    | QueryClasses
    | QueryTriples
    | QueryUnits
    | QueryCounts
    | Refresh
    | GotQueryResponse (Result Http.Error String)


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Receive (Ok graph) ->
            graph
                |> (\graph_ -> { model | graph = graph_ })
                |> Return.singleton

        Receive (Err e) ->
            model
                |> Return.singleton

        UpdateContent s ->
            { model | content = s }
                |> Return.singleton

        QueryClasses ->
            { model | query = queryClasses }
                |> Return.singleton
                |> Return.command (queryEndpoint queryClasses)
        
        QueryUnits ->
            { model | query = queryUnits }
                |> Return.singleton
                |> Return.command (queryEndpoint queryUnits)
        
        QueryCounts ->
            { model | query = queryCounts }
                |> Return.singleton
                |> Return.command (queryEndpoint queryCounts)

        QueryTriples ->
            { model | query = queryTriples }
                |> Return.singleton
                |> Return.command (queryEndpoint queryTriples)

        Refresh ->
            model
                |> Return.singleton
                |> Return.command (queryEndpoint queryClasses)

        GotQueryResponse result ->
            case result of
                Ok response ->
                    { model | content = response } |> Return.singleton

                Err _ ->
                    model |> Return.singleton



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


showResults : Model -> H.Html Msg
showResults model =
    H.div []
        [ H.p []
            [ model.query
                |> String.replace "+" " "
                |> Url.percentDecode
                |> Maybe.withDefault model.query
                |> H.text
            ]
        , H.code []
            [ H.pre []
                [ model.content
                    |> H.text
                ]
            ]
        ]

view : Model -> B.Document Msg
view model = 
    { title = "Talk to QUDT (v0.4)"
    , body =
        [ H.header []
            [ H.h1 [] [ H.text "Talk to QUDT (v0.4)" ]
            , H.p [] [ H.text "Query the QUDT SPARQL Endpoint" ]
            ]
        , H.main_
            []
            [ H.h2 []
                [ H.text "SPARQL Query:"
                ]
            , H.button [ HE.onClick QueryTriples ] [ H.text "Run Triples Query" ]
            , H.button [ HE.onClick QueryClasses ] [ H.text "Run Classes Query" ]
            , H.button [ HE.onClick QueryUnits ] [ H.text "Run Units Query" ]
            , H.button [ HE.onClick QueryCounts ] [ H.text "Run Counts Query" ]
            , showResults model
            ]
        ]
    }