module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Http
import HttpBuilder exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias Model =
    { isbn_form : ISBN
    , editing : Maybe ISBN
    , books : Dict ISBN Book
    }


type alias ISBN =
    String


type alias Review =
    String


type alias Author =
    { name : String
    }


type alias Book =
    { title : String
    , subtitle : String
    , authors : List Author
    , publish_date : String
    , url : String
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
    | BookResponse (Result Http.Error ())


emptyBook : Book
emptyBook =
    { title = ""
    , subtitle = ""
    , authors = []
    , publish_date = ""
    , url = ""
    , review = ""
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { isbn_form = ""
            , books = Dict.fromList [ ( "0385472579", emptyBook ) ]
            , editing = Just "toto"
            }
    in
        ( model, requestBooks (Dict.keys model.books) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewISBN isbn ->
            ( { model | isbn_form = isbn }, Cmd.none )

        AddBook ->
            ( { model | books = Dict.insert model.isbn_form emptyBook model.books, isbn_form = "" }, requestBooks [ model.isbn_form ] )

        EditBook isbn ->
            ( { model | isbn_form = isbn, books = Dict.remove isbn model.books }, Cmd.none )

        RemoveBook isbn ->
            ( { model | books = Dict.remove isbn model.books }, Cmd.none )

        Review isbn ->
            case model.editing of
                Nothing ->
                    ( { model | editing = Just isbn }, Cmd.none )

                Just previous ->
                    if previous == isbn then
                        ( { model | editing = Nothing }, Cmd.none )
                    else
                        ( { model | editing = Just isbn }, Cmd.none )

        SetReview isbn review ->
            ( { model
                | books = Dict.update isbn (Maybe.map (\book -> { book | review = review })) model.books
              }
            , Cmd.none
            )

        CloseReviewForm ->
            ( { model | editing = Nothing }, Cmd.none )

        BookResponse _ ->
            ( model, Cmd.none )


requestBooks : List ISBN -> Cmd Msg
requestBooks isbns =
    HttpBuilder.get "http://openlibrary.org/api/books"
        |> withQueryParams
            [ ( "bibkeys", String.join "," <| List.map (\x -> "ISBN:" ++ x) isbns )
            , ( "jscmd", "data" )
            , ( "format", "json" )
            ]
        |> send BookResponse


extractISBN : String -> ISBN
extractISBN value =
    String.dropLeft 5 value


decodeBooks : Decoder (Dict ISBN Book)
decodeBooks =
    Decode.keyValuePairs bookDecoder
        |> Decode.map (List.map (Tuple.mapFirst extractISBN) >> Dict.fromList)


bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "title" Decode.string
        |> required "subtitle" Decode.string
        |> required "authors" (Decode.list authorDecoder)
        |> required "publish_date" Decode.string
        |> required "url" Decode.string
        |> hardcoded ""


authorDecoder : Decoder Author
authorDecoder =
    decode Author
        |> required "name" Decode.string


view : Model -> Html Msg
view model =
    div [ HA.id "content" ]
        [ Html.form [ HE.onSubmit AddBook, HA.action "#" ]
            [ text "ISBN: "
            , input [ HE.onInput NewISBN, HA.value model.isbn_form ] []
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
                        [ HA.cols 30
                        , HA.rows 6
                        , HA.placeholder <| "Add review here for " ++ isbn
                        , HA.value book.review
                        , HE.onInput (SetReview isbn)
                        ]
                        []
                    , button [ HE.onClick CloseReviewForm ] [ text "Save" ]
                    ]


displayBook : ( ISBN, Book ) -> Html Msg
displayBook ( isbn, book ) =
    li []
        [ text isbn
        , text " "
        , a [ HA.href "#", HE.onClick (EditBook isbn) ] [ text "Edit" ]
        , text " - "
        , a [ HA.href "#", HE.onClick (RemoveBook isbn) ] [ text "Remove" ]
        , text " - "
        , a [ HA.href "#", HE.onClick (Review isbn) ] [ text "Review" ]
        ]


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
