module Example exposing (inverterTests)

import Expect exposing (Expectation)
import Fuzz exposing (..)
-- import RippleCarryAdder exposing (..)
import Test exposing (..)


-- inverterTests : a
-- inverterTests =
--     describe "Inverter"
--         [ test "output is 0 when the input is 1" <|
--             \_ ->
--                 inverter 0
--                     |> Expect.equal 1
--         , test "output is 1 when the input is 0" <|
--             \_ ->
--                 inverter 1
--                     |> Expect.equal 0
--         ]