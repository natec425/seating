import Html exposing (Html, button, div, text, em, span, node)
import Html.App as Html
import Html.Attributes exposing (style, rel, href)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Set exposing (Set)
import Maybe
import Random

main = Html.program { init = initial_state, view = view, update = update, subscriptions = subscriptions}

names = Set.fromList ["Miltreonna", "James S.", "Keegan", "Addey", "Nicole", "Adam", "Haleem", "Kyle", "Jacob", "James H.", "Ricky", "Martin", "Dustin", "Edrick"]

initial_state =
  ({ unassigned = names,
     assignments = Array.fromList (List.repeat (Set.size names) Nothing)
   },
   Cmd.none)

type alias Model =
  { unassigned : Set String,
    assignments : Array (Maybe String) }

type Action = Assign Int
            | Assigned Int Int
            | Reset

assign : Int -> Int -> Model -> Model
assign i seat m =
  let assignee = setGet i m.unassigned |> Maybe.withDefault "ERROR"
  in
  { unassigned = Set.remove assignee m.unassigned,
    assignments = Array.set seat (Just assignee) m.assignments }

setGet : Int -> Set comparable -> Maybe comparable
setGet i s =
  Array.get i (Array.fromList (Set.toList s))

randomAssigned : Int -> Int -> Cmd Action
randomAssigned seat max =
    Random.generate (\i -> Assigned i seat) (Random.int 0 max)

update : Action -> Model -> (Model, Cmd Action)
update a m =
  case a of
    Assign i -> (m, randomAssigned i (Set.size m.unassigned - 1))
    Assigned i seat -> (assign i seat m, Cmd.none)
    Reset -> initial_state

bootstrap : Html a
bootstrap =
  div [] [
    node "link" [ rel "stylesheet", href "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"] []
  ]

view : Model -> Html Action
view m =
  div [center] [
  bootstrap,
  viewUnassigned m.unassigned,
  viewAssignments m.assignments,
  resetButton]

margin = style [ ("margin", "10px") ]
center = style [ ("text-align", "center")]


viewUnassigned : Set String -> Html a
viewUnassigned names =
  Set.toList names
  |> List.map (\n -> span [margin] [text n])
  |> div []


viewAssignments : Array (Maybe String) -> Html Action
viewAssignments assignments =
  let
    viewed = Array.indexedMap viewAssignment assignments |> Array.toList
    backRow = List.take 4 viewed
    middleRow = List.drop 4 viewed |> List.take 4
    frontRow = List.drop 8 viewed
  in
    div [] [div [margin] backRow, div [margin] middleRow, div [margin] frontRow]

viewAssignment : Int -> Maybe String -> Html Action
viewAssignment i a =
  case a of
    Just name -> em [margin] [text name]
    Nothing -> button [onClick (Assign i)] [text "Assign This Seat"]

resetButton =
  button [onClick Reset] [text "Reset Seating Assignments"]
  
subscriptions : Model -> Sub Action
subscriptions model =
  Sub.none
