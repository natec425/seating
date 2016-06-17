module Main exposing (..)

import Html exposing (Html, button, div, text, em, span, node)
import Html.App as Html
import Html.Attributes exposing (style, rel, href, src, classList, class, attribute)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Set exposing (Set)
import Maybe
import Random


main =
    Html.program
        { init = initial_state
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


names =
    Set.fromList
        [ "Miltreonna"
        , "James S."
        , "Keegan"
        , "Addey"
        , "Nicole"
        , "Adam"
        , "Haleem"
        , "Kyle"
        , "Jacob"
        , "James H."
        , "Ricky"
        , "Martin"
        , "Dustin"
        , "Edrick"
        ]


initial_state =
    ( { unassigned = names
      , assignments = Array.fromList (List.repeat (Set.size names) Nothing)
      }
    , Cmd.none
    )


type alias Model =
    { unassigned : Set String
    , assignments : Array (Maybe String)
    }


type Action
    = Assign Int
    | Assigned Int Int
    | Reset


assign : Int -> Int -> Model -> Model
assign i seat m =
    let
        assignee =
            setGet i m.unassigned |> Maybe.withDefault "ERROR"
    in
        { unassigned = Set.remove assignee m.unassigned
        , assignments = Array.set seat (Just assignee) m.assignments
        }


setGet : Int -> Set comparable -> Maybe comparable
setGet i s =
    Array.get i (Array.fromList (Set.toList s))


randomAssigned : Int -> Int -> Cmd Action
randomAssigned seat max =
    Random.generate (\i -> Assigned i seat) (Random.int 0 max)


update : Action -> Model -> ( Model, Cmd Action )
update a m =
    case a of
        Assign i ->
            ( m, randomAssigned i (Set.size m.unassigned - 1) )

        Assigned i seat ->
            ( assign i seat m, Cmd.none )

        Reset ->
            initial_state


stylesheet h =
    node "link" [ rel "stylesheet", href h ] []


script s =
    node "script" [ src s ] []


bootstrap : Html a
bootstrap =
    div []
        [ stylesheet "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
        , stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
        , script "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        ]


view : Model -> Html Action
view m =
    div [ center ]
        [ bootstrap
        , viewUnassigned m.unassigned
        , viewAssignments m.assignments
        , resetButton
        ]


margin =
    style [ ( "margin", "10px" ) ]


center =
    style [ ( "text-align", "center" ) ]


viewUnassigned : Set String -> Html a
viewUnassigned names =
    Set.toList names
        |> List.map (\n -> span [ margin ] [ text n ])
        |> div []


viewAssignments : Array (Maybe String) -> Html Action
viewAssignments assignments =
    let
        viewed =
            Array.indexedMap viewAssignment assignments |> Array.toList

        backRow =
            buffer :: (List.append (List.take 4 viewed) [ buffer ])

        middleRow =
            buffer :: (List.append (List.drop 4 viewed |> List.take 4) [ buffer ])

        frontRow =
            List.drop 8 viewed
    in
        div [ class "container" ] [ div [ margin, row ] backRow, div [ margin, row ] middleRow, div [ margin, row ] frontRow ]


buffer =
    div [ class "col-md-2" ] []


row =
    class "row"


bootButton atts children =
    button
        ((classList [ ( "btn", True ), ( "btn-primary", True ) ])
            :: atts
        )
        children


viewAssignment : Int -> Maybe String -> Html Action
viewAssignment i a =
    case a of
        Just name ->
            span [ class "col-md-2" ] [ em [] [ text name ] ]

        Nothing ->
            span [ class "col-md-2" ] [ bootButton [ onClick (Assign i) ] [ text "Assign This Seat" ] ]


resetButton =
    bootButton [ onClick Reset, class "btn-danger" ] [ text "Reset Seating Assignments" ]


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.none
