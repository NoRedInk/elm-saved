module Spec.Saved exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Saved exposing (Saved)
import Test exposing (..)


saved : Fuzzer a -> Fuzzer (Saved a)
saved value =
    let
        initSaved : a -> a -> Saved a
        initSaved initial changed =
            Saved.new initial
                |> Saved.change (\_ -> changed)
    in
    Fuzz.map2 initSaved value value


newValueDuality : Test
newValueDuality =
    fuzz Fuzz.string "`new` and `value` are duals" <|
        \str ->
            Saved.new str
                |> Saved.value
                |> Expect.equal str


changeValueDuality : Test
changeValueDuality =
    fuzz2 Fuzz.string Fuzz.string "`change` and `value` are duals" <|
        \initial changed ->
            Saved.new initial
                |> Saved.change (\_ -> changed)
                |> Saved.value
                |> Expect.equal changed


updateValueDuality : Test
updateValueDuality =
    fuzz2 Fuzz.string Fuzz.string "`update` and `value` are duals" <|
        \initial changed ->
            Saved.new initial
                |> Saved.update (\_ -> ( changed, () ))
                |> Tuple.first
                |> Saved.value
                |> Expect.equal changed


savedChecksEquality : Test
savedChecksEquality =
    fuzz2 Fuzz.string Fuzz.string "`saved` reports whether the initial and saved values are different" <|
        \initial changed ->
            Saved.new initial
                |> Saved.change (\_ -> changed)
                |> Saved.saved
                |> Expect.equal (initial == changed)


afterSaveItsSaved : Test
afterSaveItsSaved =
    fuzz2 Fuzz.string Fuzz.string "After calling `save`, `saved` is always True`" <|
        \initial changed ->
            Saved.new initial
                |> Saved.change (\_ -> changed)
                |> Saved.save
                |> Saved.saved
                |> Expect.equal True


savedDoesNotAlterValue : Test
savedDoesNotAlterValue =
    fuzz2 Fuzz.string Fuzz.string "`save` does not change the current value" <|
        \initial changed ->
            Saved.new initial
                |> Saved.change (\_ -> changed)
                |> Saved.save
                |> Saved.value
                |> Expect.equal changed


discardSetsUsBack : Test
discardSetsUsBack =
    fuzz2 Fuzz.string Fuzz.string "`discard` sets us back to the initial value" <|
        \initial changed ->
            Saved.new initial
                |> Saved.change (\_ -> changed)
                |> Saved.discard
                |> Saved.value
                |> Expect.equal initial


setSavedChangesInitialValue : Test
setSavedChangesInitialValue =
    fuzz2 Fuzz.string Fuzz.string "`setSaved` changes the initial value" <|
        \initial saved ->
            Saved.new initial
                |> Saved.setSaved saved
                |> Saved.discard
                |> Saved.value
                |> Expect.equal saved


setSavedDoesNotAlterValue : Test
setSavedDoesNotAlterValue =
    fuzz3 Fuzz.string Fuzz.string Fuzz.string "`setSaved` does not change the current value" <|
        \initial changed saved ->
            Saved.new initial
                |> Saved.change (\_ -> changed)
                |> Saved.setSaved saved
                |> Saved.value
                |> Expect.equal changed


mapIdentity1 : Test
mapIdentity1 =
    fuzz (saved Fuzz.string) "`map identity` does not change the current value" <|
        \anySaved ->
            Saved.map identity anySaved
                |> Saved.value
                |> Expect.equal (Saved.value anySaved)


mapIdentity2 : Test
mapIdentity2 =
    fuzz (saved Fuzz.string) "`map identity` does not change the initial value" <|
        \anySaved ->
            Saved.map identity anySaved
                |> initialValue
                |> Expect.equal (initialValue anySaved)


mapCompose1 : Test
mapCompose1 =
    fuzz (saved Fuzz.string) "`map f >> map g` ~ `map (f >> g)` (current value obeys)" <|
        let
            f =
                String.toUpper

            g =
                String.left 10
        in
        \anySaved ->
            anySaved
                |> Saved.map f
                |> Saved.map g
                |> Saved.value
                |> Expect.equal
                    (anySaved
                        |> Saved.map (f >> g)
                        |> Saved.value
                    )


mapCompose2 : Test
mapCompose2 =
    fuzz (saved Fuzz.string) "`map f >> map g` ~ `map (f >> g)` (initial value obeys)" <|
        let
            f =
                String.toUpper

            g =
                String.left 10
        in
        \anySaved ->
            anySaved
                |> Saved.map f
                |> Saved.map g
                |> initialValue
                |> Expect.equal
                    (anySaved
                        |> Saved.map (f >> g)
                        |> initialValue
                    )


customNew : Test
customNew =
    fuzz2 Fuzz.string Fuzz.string "The equality check passed to `customNew` is used in later comparisons" <|
        \initial changed ->
            let
                eq x y =
                    String.length x == String.length y
            in
            Saved.customNew eq initial
                |> Saved.change (\_ -> changed)
                |> Saved.saved
                |> Expect.equal (eq initial changed)


initialValue : Saved a -> a
initialValue saved =
    Saved.discard saved
        |> Saved.value
