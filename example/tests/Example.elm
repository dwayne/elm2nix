module Example exposing (suite)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "An example"
        [ test "1+1=2" <|
            \_ ->
                1 + 1 |> Expect.equal 2
        ]
