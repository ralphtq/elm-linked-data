module RDF exposing
    ( BlankNode
    , Description
    , Graph
    , IRI
    , Literal
    , Node
    , Object
    , Predicate
    , Subject
    , Triple
    , asBlankNode
    , asIRI
    , asLiteral
    , asNode
    , asObject
    , asPredicate
    , asSubject
    , blankNode
    , descriptionGet
    , filterForDescriptions
    , graphFilterMap
    , graphGetObjects
    , iri
    , literal
    , namespace
    , objectBlankNode
    , objectIRI
    , objectLiteral
    , owl
    , predicateIRI
    , rdf
    , rdfs
    , subjectBlankNode
    , subjectIRI
    , triple
    , type_
    , xsd
    )

{-| Resource Description Framework (RDF). See <https://www.w3.org/TR/rdf11-concepts/>
-}

-- RDF Terms


{-| An Internationalized Resource Identifier. See <https://www.ietf.org/rfc/rfc3987.txt>.
-}
type alias IRI =
    String


{-| Create an IRI
-}
iri : String -> IRI
iri =
    identity


{-| A blank node
-}
type BlankNode
    = BNode String


{-| Create a new blank node.
-}
blankNode : String -> BlankNode
blankNode id =
    BNode id


{-| An RDF Literal
-}
type alias Literal =
    { value : String
    , datatype : IRI
    , language : Maybe String
    }


{-| Create a new literal
-}
literal : String -> IRI -> Maybe String -> Literal
literal value datatype maybeLanguage =
    Literal value datatype maybeLanguage



-- RDF Components


{-| Empty type for Subject component
-}
type CSubject
    = CSubject


{-| Empty type for Predicate component
-}
type CPredicate
    = CPredicate


{-| Empty type for Object component
-}
type CObject
    = CObject


{-| Internal type for nodes. This is a phantom type.
-}
type InternalNode a
    = NodeIRI IRI
    | NodeBlankNode BlankNode
    | NodeLiteral Literal


{-| Cast node to an IRI
-}
asIRI : InternalNode a -> Maybe IRI
asIRI node =
    case node of
        NodeIRI iri_ ->
            Just iri_

        _ ->
            Nothing


{-| Cast node to a Literal
-}
asLiteral : InternalNode a -> Maybe Literal
asLiteral node =
    case node of
        NodeLiteral literal_ ->
            Just literal_

        _ ->
            Nothing


{-| Cast node to a BlankNode
-}
asBlankNode : InternalNode a -> Maybe BlankNode
asBlankNode node =
    case node of
        NodeBlankNode bnode_ ->
            Just bnode_

        _ ->
            Nothing


{-| Node of the RDF Graph
-}
type alias Node =
    InternalNode Never


{-| Cast specialized node to a general node
-}
asNode : InternalNode a -> Node
asNode node =
    case node of
        NodeIRI iri_ ->
            NodeIRI iri_

        NodeBlankNode bnode_ ->
            NodeBlankNode bnode_

        NodeLiteral literal_ ->
            NodeLiteral literal_



-- Subject


{-| Subject which is an IRI or a blank node
-}
type alias Subject =
    InternalNode CSubject


{-| Create a subject from an IRI
-}
subjectIRI : IRI -> Subject
subjectIRI i =
    NodeIRI i


{-| Create a subject from a blank node
-}
subjectBlankNode : BlankNode -> Subject
subjectBlankNode bnode =
    NodeBlankNode bnode


{-| Cast a node to a subject
-}
asSubject : InternalNode a -> Maybe Subject
asSubject node =
    case node of
        NodeIRI i ->
            Just (NodeIRI i)

        NodeBlankNode bnode ->
            Just (NodeBlankNode bnode)

        NodeLiteral _ ->
            Nothing



-- Predicate


{-| Predicate which is an IRI
-}
type alias Predicate =
    InternalNode CPredicate


{-| Create a predicate from an IRI
-}
predicateIRI : IRI -> Predicate
predicateIRI i =
    NodeIRI i


{-| Cast a node to a Predicate
-}
asPredicate : InternalNode a -> Maybe Predicate
asPredicate node =
    case node of
        NodeIRI i ->
            Just (NodeIRI i)

        _ ->
            Nothing



-- Object


{-| Objects which can be a IRI, a blank node or a literal
-}
type alias Object =
    InternalNode CObject


objectIRI : IRI -> Object
objectIRI i =
    NodeIRI i


objectBlankNode : BlankNode -> Object
objectBlankNode bnode =
    NodeBlankNode bnode


objectLiteral : Literal -> Object
objectLiteral l =
    NodeLiteral l


{-| Cast node to object
-}
asObject : InternalNode a -> Object
asObject node =
    case node of
        NodeIRI i ->
            NodeIRI i

        NodeBlankNode b ->
            NodeBlankNode b

        NodeLiteral l ->
            NodeLiteral l



-- Triple


{-| Triple are the basic building blocks of an RDF Graph. They consiste of two
nodes (subject and object) that are connected by an edge labeled with predicate.
-}
type alias Triple =
    { subject : Subject
    , predicate : Predicate
    , object : Object
    }


{-| Create a new triple
-}
triple : Subject -> Predicate -> Object -> Triple
triple s p o =
    { subject = s, predicate = p, object = o }



-- Namespaces


{-| Helper to create IRIs for a namespace
-}
namespace : String -> String -> IRI
namespace prefix name =
    prefix
        ++ name
        |> iri


{-| The RDF syntax namespace
-}
rdf : String -> IRI
rdf =
    namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#"


{-| The rdf:type predicate
-}
type_ : Predicate
type_ =
    rdf "type" |> predicateIRI


{-| RDF schema namespace
-}
rdfs : String -> IRI
rdfs =
    namespace "http://www.w3.org/2000/01/rdf-schema#"


{-| OWL namespace
-}
owl : String -> IRI
owl =
    namespace "http://www.w3.org/2002/07/owl#"


{-| XSD namespace
-}
xsd : String -> IRI
xsd =
    namespace "http://www.w3.org/2001/XMLSchema#"



-- Graph operations


{-| Graph type

TODO Currently this is implemented as a list of triples. This is not very efficient and there are bette ways of doing this by creating indices for efficient lookup and querying.

-}
type alias Graph =
    List Triple


{-| Description is a graph with a pointer to a subject. This is useful when dealing with certain things in the graph, by providing a starting point for queries.
-}
type alias Description =
    { subject : Subject
    , graph : Graph
    }


graphFilterMap : (Triple -> Maybe a) -> Graph -> List a
graphFilterMap filter graph =
    graph
        |> List.filterMap filter


graphGetObjects : Graph -> Subject -> Predicate -> List Object
graphGetObjects graph subject predicate =
    graphFilterMap
        (\triple_ ->
            if subject == triple_.subject && predicate == triple_.predicate then
                Just triple_.object

            else
                Nothing
        )
        graph


filterForDescriptions : (Triple -> Bool) -> Graph -> List Description
filterForDescriptions filter graph =
    graph
        |> List.filterMap
            (\triple_ ->
                if filter triple_ then
                    { subject = .subject triple_
                    , graph = graph
                    }
                        |> Just

                else
                    Nothing
            )


descriptionGet : Predicate -> Description -> List Object
descriptionGet p description =
    .graph description
        |> List.filterMap
            (\{ subject, predicate, object } ->
                if subject == .subject description && p == predicate then
                    Just object

                else
                    Nothing
            )