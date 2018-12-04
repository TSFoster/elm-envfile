module Envfile exposing
    ( encode
    , parser
    )

{-| Convert `Dict String String` to envfile format and back.

**Note:** This parser follows the simple rules laid out in [Docker's guide to its use of envfiles][docker-envfile].

[docker-envfile]: https://docs.docker.com/compose/env-file/


## Encode

@docs encode


## Parse

@docs parser

-}

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..))
import Regex exposing (Regex)
import Set



-- ENCODE


{-| Take a `Dict String String`, ignore the keys that bash wouldn’t interpret as
a variable name and encode the rest as variable declarations, one on each line.

    import Dict

    [ ("hello", "world")
    , ("_john", "_WAYNE")
    , ("234notavar", "Won’t be encoded")
    , ("also not a var", "Also won’t be encoded")
    , ("thing1", "thing\n2")
    ]
        |> Dict.fromList
        |> encode
        |> String.lines
        |> List.sort
    --> [ "_john=_WAYNE"
    --> , "hello=world"
    --> , "thing1=thing\\n2"
    --> ]

-}
encode : Dict String String -> String
encode =
    Dict.filter validVariableName
        >> Dict.map escapeNewlines
        >> Dict.toList
        >> List.map toDefinition
        >> String.join "\n"


validVariableName : String -> a -> Bool
validVariableName key _ =
    Regex.contains validVariableNameRegex key


validVariableNameRegex : Regex
validVariableNameRegex =
    "^[_a-zA-Z][_a-zA-Z0-9]*$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


escapeNewlines : a -> String -> String
escapeNewlines _ =
    String.replace "\n" "\\n"


toDefinition : ( String, String ) -> String
toDefinition ( k, v ) =
    k ++ "=" ++ v



-- PARSER


{-| A `Parser` to read an envfile as a `Dict String String`.

    import Parser
    import Dict

    Parser.run parser <| String.join "\n"
        [ "hello=world"
        , "# this is a comment"
        , ""
        , ""
        , "_abc=def\\nghi"
        ]
    --> Ok (Dict.fromList [ ("_abc", "def\nghi") , ("hello", "world") ])

-}
parser : Parser (Dict String String)
parser =
    Parser.loop [] lines


lines : List ( String, String ) -> Parser (Step (List ( String, String )) (Dict String String))
lines definitions =
    Parser.oneOf
        [ Parser.map (\definition -> Loop (definition :: definitions)) declaration
        , Parser.map (\_ -> Loop definitions) ignoredLine
        , Parser.map (\_ -> Done (Dict.fromList definitions)) Parser.end
        ]


declaration : Parser ( String, String )
declaration =
    Parser.succeed Tuple.pair
        |= variable
        |. equals
        |= value


variable : Parser String
variable =
    Parser.variable
        { start = \char -> char == '_' || Char.isAlpha char
        , inner = \char -> char == '_' || Char.isAlphaNum char
        , reserved = Set.empty
        }


equals : Parser ()
equals =
    Parser.symbol "="


value : Parser String
value =
    Parser.chompUntilEndOr "\n"
        |> Parser.getChompedString
        |> Parser.map (String.replace "\\n" "\n")


ignoredLine : Parser ()
ignoredLine =
    Parser.oneOf
        [ commentedLine
        , blankLine
        ]


commentedLine : Parser ()
commentedLine =
    Parser.lineComment "#"


blankLine : Parser ()
blankLine =
    Parser.token "\n"
