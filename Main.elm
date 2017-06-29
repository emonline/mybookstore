module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set


type alias Model =
    { isbn_form : ISBN
    , editing : Maybe ISBN
    , books : Set.Set ISBN
    }


type alias ISBN =
    String


type Msg
    = NewISBN ISBN
    | AddBook
    | EditBook ISBN
    | RemoveBook ISBN
    | Review ISBN


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewISBN isbn ->
            { model | isbn_form = isbn }

        AddBook ->
            { model | books = Set.insert model.isbn_form model.books, isbn_form = "" }

        EditBook isbn ->
            { model | isbn_form = isbn, books = Set.remove isbn model.books }

        RemoveBook isbn ->
            { model | books = Set.remove isbn model.books }

        Review isbn ->
            { model | editing = Just isbn }


view : Model -> Html Msg
view model =
    div [ id "content" ]
        [ Html.form [ onSubmit AddBook, action "#" ]
            [ text "ISBN: "
            , input [ onInput NewISBN, value model.isbn_form ] []
            , button [] [ text "Add" ]
            ]
        , Set.toList model.books
            |> List.map displayBook
            |> ul []
        , reviewForm model.editing
        ]


reviewForm : Maybe ISBN -> Html Msg
reviewForm isbn =
    case isbn of
        Nothing ->
            div [] []

        Just isbn ->
            div []
                [ textarea [ cols 30, rows 6, placeholder <| "Add review here for " ++ isbn ] []
                , button [] [ text "Place review" ]
                ]


displayBook : ISBN -> Html Msg
displayBook isbn =
    li []
        [ text isbn
        , text " "
        , a [ href "#", onClick (EditBook isbn) ] [ text "Edit" ]
        , text " - "
        , a [ href "#", onClick (RemoveBook isbn) ] [ text "Remove" ]
        , text " - "
        , a [ href "#", onClick (Review isbn) ] [ text "Review" ]
        ]


main : Program Never Model Msg
main =
    beginnerProgram
        { model = { isbn_form = "", books = Set.fromList [ "toto" ], editing = Just "toto" }
        , view = view
        , update = update
        }
