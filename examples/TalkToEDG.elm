module TalkToEDG exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Debug
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


prefixForEDG : String
prefixForEDG =
   Url.percentEncode "prefix edg: <http://edg.topbraid.solutions/model/>"

prefixForERA : String
prefixForERA =
   Url.percentEncode "prefix era: <http://www.era.europa.eu/era#>"

encodedPrefixes = 
    prefixForOWL ++ prefixForQUDT ++ prefixForRDFS ++ prefixForEDG ++ prefixForERA


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


queryQUDTtriples =
    """
 SELECT ?subject ?predicate ?object
 WHERE {
   ?subject ?predicate ?object
 }
 """


queryEDGtasks =
    """
 SELECT ?task ?predicate ?object
 WHERE { GRAPH <urn:x-evn-master:era_roadmap> {
   ?task a era:RoadmapItem .
   ?task ?predicate ?object .
   }
 }
 """

executeADScall = "todo"

completionDaysForDependency = "todo"


sparqlQuery : String -> Maybe Int -> String
sparqlQuery query max =
    case max of
      Just n ->  query ++ " LIMIT " ++ String.fromInt n
      Nothing -> query

queryClasses =
    encodedPrefixes ++ String.replace "%20" "+" (Url.percentEncode (sparqlQuery queryQUDTclasses <| Just 1000))

queryTasks =
    encodedPrefixes ++ String.replace "%20" "+" (Url.percentEncode (sparqlQuery queryEDGtasks Nothing))

queryTriples =
    encodedPrefixes ++ String.replace "%20" "+" (Url.percentEncode (sparqlQuery queryQUDTtriples <| Just 10))


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

-- https://en.wikipedia.org/wiki/List_of_HTTP_header_fields
-- application/sparql-results-rich+json
--   , body = Http.stringBody "application/x-www-form-urlencoded" ("query=" ++ query)

queryEndpoint : SPARQLquery -> Cmd Msg
queryEndpoint query =
    Http.request
        { method = "POST"
        , headers = []
        --  [ Http.header "Authorization" "Basic"  --  WwoUNbaz3ZDW3baPn8fs8ut11EU="
        --   ,Http.header "X-Requested-With" "XMLHttpRequest"
        --   ,Http.header "Origin" "http://localhost:8083/tbl"
        --  ]
        , url = "http://localhost:8083/tbl/sparql" -- "http://localhost:8083/tbl" -- "https://edg.doa.topbraid.net/edg/tbl"
        , body = Http.stringBody "application/x-www-form-urlencoded" ("query=" ++ query ++ "&format=JSON")
        -- , body = Http.stringBody 
        --    "application/x-www-form-urlencoded, application/json, text/plain, */*" "" -- ("query=" ++ "") -- ("query=" ++ query)
        , expect = Http.expectString GotQueryResponse
        , timeout = Nothing
        , tracker = Nothing
        }


adsEndpoint : String -> Cmd Msg
adsEndpoint payload =
    Http.request
        { method = "POST"
        , headers = []
        --  [ Http.header "Authorization" "Basic"  --  WwoUNbaz3ZDW3baPn8fs8ut11EU="
        --   ,Http.header "X-Requested-With" "XMLHttpRequest"
        --   ,Http.header "Origin" "http://localhost:8083/tbl"
        --  ]
        , url = "http://localhost:8083/tbl/ads"
        , body = Http.stringBody "application/x-www-form-urlencoded" ("payload=" ++ payload ++ "&format=JSON")
        , expect = Http.expectString GotADSresponse
        , timeout = Nothing
        , tracker = Nothing
        }


-- UPDATE


type Msg
    = Receive (Result Http.Error RDF.Graph)
    | UpdateContent String
    | QueryClasses
    | QueryTasks
    | QueryTriples
    | QueryADS
    | Refresh
    | GotQueryResponse (Result Http.Error String)
    | GotADSresponse (Result Http.Error String)

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
        
        QueryADS ->
            { model | query = completionDaysForDependency }
                |> Return.singleton
                |> Return.command (adsEndpoint completionDaysForDependency)
        
        QueryTasks ->
            { model | query = queryTasks }
                |> Return.singleton
                |> Return.command (queryEndpoint queryTasks)

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
        
        GotADSresponse result ->
            case result of
                Ok response ->
                    { model | content = "" } |> Return.singleton

                Err _ ->
                     { model | content = "Error - not implemented yet" } |> Return.singleton



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
    { title = "EDG API Tests (v0.2)"
    , body =
        [ H.header []
            [ H.h1 [] [ H.text "EDG API Tests (v0.2)" ]
            , H.p [] [ H.text "Test EDG APIs" ]
            ]
        , H.main_
            []
            [ H.h2 []
                [ "Run Checks:"
                    |> H.text
                ]
            , H.button [ ] [ H.text "Asset Collection Graphs" ]
            , H.button [ ] [ H.text "Governance Services" ] 
            , H.button [ HE.onClick QueryTasks ] [ H.text "Query for Tasks" ]
            , H.button [ HE.onClick QueryADS ] [ H.text "Call ADS Service" ]
            , H.button [ ] [ H.text "Call GraphQL Service" ]
            , H.button [ ] [ H.text "Request Statistics" ]
            , showResults model
            ]
        ]
    }

