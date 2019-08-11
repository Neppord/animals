module Main exposing (Model(..), Tree(..), initView, main, noAnimalsView, update, view)

import Browser
import Html exposing (button, div, form, input, p, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput, onSubmit)


main =
    Browser.sandbox { init = Init, update = update, view = view }


type alias Yes =
    Tree


type alias No =
    Tree


type Tree
    = Animal String
    | Statement { statement : String, true : Tree, false : Tree }


type alias Rebuilder =
    Tree -> Tree


type Model
    = Init
    | NoAnimals String
    | Title Tree
    | Game Tree Rebuilder
    | LearnAnimal String Rebuilder String String


type Msg
    = StartGame
    | UpdateAnimalName String
    | UpdateStatement String
    | Wrong
    | Correct


update msg model =
    case ( msg, model ) of
        ( StartGame, Init ) ->
            NoAnimals ""

        ( StartGame, NoAnimals name ) ->
            Title (Animal <| cap name)

        ( StartGame, Title tree ) ->
            Game tree identity

        ( StartGame, LearnAnimal guessed rebuild statementText name ) ->
            let
                statement =
                    { statement = statementText
                    , false = Animal guessed
                    , true = Animal name
                    }
            in
            Title <| rebuild <| Statement statement

        ( UpdateAnimalName name, NoAnimals _ ) ->
            NoAnimals name

        ( UpdateAnimalName name, LearnAnimal guessed rebuild question _ ) ->
            LearnAnimal guessed rebuild question <| cap name

        ( UpdateStatement question, LearnAnimal guessed rebuild _ name ) ->
            LearnAnimal guessed rebuild question name

        ( Correct, Game (Statement ({ true } as rec)) rebuild ) ->
            Game true (\new_true -> rebuild <| Statement { rec | true = new_true })

        ( Wrong, Game (Statement ({ false } as rec)) rebuild ) ->
            Game false (\new_false -> rebuild <| Statement { rec | false = new_false })

        ( Correct, Game (Animal name) rebuild ) ->
            Title <| rebuild <| Animal name

        ( Wrong, Game (Animal name) rebuild ) ->
            LearnAnimal name rebuild "" ""

        _ ->
            model


view model =
    case model of
        Init ->
            initView

        NoAnimals name ->
            noAnimalsView name

        Title _ ->
            titleView

        Game tree _ ->
            gameView tree

        LearnAnimal guessed _ question newAnimal ->
            learnAnimalView guessed question newAnimal


learnAnimalView guessed statement animalName =
    form [ onSubmit StartGame ]
        [ p [] [ text "Du vann!" ]
        , p []
            [ text "Vilket djur tänkte du på?"
            , input [ onInput UpdateAnimalName, value animalName ] []
            ]
        , p []
            [ text "Vad gör en "
            , text animalName
            , text " men inte en "
            , text guessed
            , text "?:"
            , input [ onInput UpdateStatement, value statement ] []
            ]
        , p []
            [ text "En "
            , text animalName
            , text " "
            , text statement
            , text ". Men ingen "
            , text guessed
            , text " "
            , text statement
            , text "."
            ]
        , button [] [ text "Helt rätt!" ]
        ]


titleView =
    div
        []
        [ p [] [ text "Jag är redo att gissa på ett djur. Säg till när du är klar och tänker på ett nytt djur" ]
        , button [ onClick StartGame ] [ text "Klar" ]
        ]


gameView tree =
    case tree of
        Animal name ->
            div []
                [ p [] [ text ("Jag gissar " ++ name) ]
                , button [ onClick Correct ] [ text "Rätt!" ]
                , button [ onClick Wrong ] [ text "Fel!" ]
                ]

        Statement { statement } ->
            div []
                [ p []
                    [ text "Djuret "
                    , text statement
                    ]
                , button [ onClick Correct ] [ text "Rätt!" ]
                , button [ onClick Wrong ] [ text "Fel!" ]
                ]


initView =
    div
        []
        [ p [] [ text "Välkomen till Djur, spelet som lär sig om djur." ]
        , form [ onSubmit StartGame ]
            [ button [] [ text "Börja Spela" ]
            ]
        ]


noAnimalsView name =
    div
        []
        [ p [] [ text "Jag känner inte till några djur, snälla berätta om ett djurs namn:" ]
        , form [ onSubmit StartGame ]
            [ input [ onInput UpdateAnimalName, value name ] []
            , button [] [ text "Klar" ]
            ]
        ]


cap : String -> String
cap name =
    (String.toUpper <| String.left 1 name) ++ (String.toLower <| String.dropLeft 1 name)
