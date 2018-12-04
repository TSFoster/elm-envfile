module Tests exposing (suite)

import Dict
import Envfile exposing (..)
import Expect exposing (Expectation)
import Parser exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "Envfile"
        [ describe "encode"
            [ test "Correctly encodes empty Dict" <|
                encodeTest [ "" ] []
            , test "Removes invalid variable names" <|
                encodeTest [ "hello=world" ] [ ( "", "thing" ), ( "hello", "world" ), ( "1s1nvalid", "hello" ) ]
            , test "Allows variables that start with underscore or alphabetical character" <|
                encodeTest [ "_THING=_THING", "thing=thing", "THING=THING" ] [ ( "_THING", "_THING" ), ( "thing", "thing" ), ( "THING", "THING" ) ]
            , test "Allows variable names that conain numbers" <|
                encodeTest [ "hell0=w0rld" ] [ ( "hell0", "w0rld" ) ]
            ]
        , describe "parser"
            [ test "Correctly parses empty string" <|
                parserTest [] ""
            , test "Subsequent definitions overwrite previous ones" <|
                parserTest [ ( "a", "b" ) ] "a=a\na=c\na=b"
            , test "Allows empty strings" <|
                parserTest [ ( "a", "" ), ( "b", "c" ) ] "a=\nb=c"
            , test "Allows empty strings at the end of the file" <|
                parserTest [ ( "a", "" ) ] "a="
            , test "Allows empty lines" <|
                parserTest [ ( "a", "b" ) ] "\n\n\na=b\n\n\n"
            , test "Allows comments" <|
                parserTest [ ( "a", "b" ) ] "# This is a commented line\na=b\n# Here is another commment"
            , test "Fails when variables start with number" <|
                parserErrTest "3=4"
            , test "Fails when line is not blank, comment or assignment" <|
                parserErrTest "hello"
            ]
        ]


encodeTest : List String -> List ( String, String ) -> a -> Expectation
encodeTest results =
    Dict.fromList
        >> encode
        >> String.lines
        >> List.sort
        >> Expect.equal (List.sort results)
        >> always


parserTest : List ( String, String ) -> String -> a -> Expectation
parserTest dictList =
    always << Expect.equal (Ok (Dict.fromList dictList)) << run parser


parserErrTest : String -> a -> Expectation
parserErrTest =
    always << Expect.err << run parser
