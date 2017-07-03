module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)


type alias Model =
    { isbn_form : ISBN
    , editing : Maybe ISBN
    , books : Dict ISBN Book
    }


type alias ISBN =
    String


type alias Review =
    String


type alias Book =
    { title : String
    , review : Review
    }


type Msg
    = NewISBN ISBN
    | AddBook
    | EditBook ISBN
    | RemoveBook ISBN
    | Review ISBN
    | SetReview ISBN Review
    | CloseReviewForm


emptyBook : Book
emptyBook =
    { title = "", review = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewISBN isbn ->
            { model | isbn_form = isbn }

        AddBook ->
            { model | books = Dict.insert model.isbn_form emptyBook model.books, isbn_form = "" }

        EditBook isbn ->
            { model | isbn_form = isbn, books = Dict.remove isbn model.books }

        RemoveBook isbn ->
            { model | books = Dict.remove isbn model.books }

        Review isbn ->
            case model.editing of
                Nothing ->
                    { model | editing = Just isbn }

                Just previous ->
                    if previous == isbn then
                        { model | editing = Nothing }
                    else
                        { model | editing = Just isbn }

        SetReview isbn review ->
            { model
                | books = Dict.update isbn (updateBook review) model.books
            }

        CloseReviewForm ->
            { model | editing = Nothing }


updateBook : Review -> Maybe Book -> Maybe Book
updateBook review book =
    case book of
        Nothing ->
            Nothing

        Just book ->
            Just { book | review = review }


view : Model -> Html Msg
view model =
    div [ id "content" ]
        [ Html.form [ onSubmit AddBook, action "#" ]
            [ text "ISBN: "
            , input [ onInput NewISBN, value model.isbn_form ] []
            , button [] [ text "Add" ]
            ]
        , Dict.toList model.books
            |> List.map displayBook
            |> ul []
        , reviewForm model.editing model.books
        ]


reviewForm : Maybe ISBN -> Dict ISBN Book -> Html Msg
reviewForm isbn books =
    case isbn of
        Nothing ->
            div [] []

        Just isbn ->
            let
                book =
                    case Dict.get isbn books of
                        Nothing ->
                            emptyBook

                        Just book ->
                            book
            in
                div []
                    [ textarea
                        [ cols 30
                        , rows 6
                        , placeholder <| "Add review here for " ++ isbn
                        , value book.review
                        , onInput (SetReview isbn)
                        ]
                        []
                    , button [ onClick CloseReviewForm ] [ text "Save" ]
                    ]


displayBook : ( ISBN, Book ) -> Html Msg
displayBook ( isbn, book ) =
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
        { model = { isbn_form = "", books = Dict.fromList [ ( "toto", emptyBook ) ], editing = Just "toto" }
        , view = view
        , update = update
        }
