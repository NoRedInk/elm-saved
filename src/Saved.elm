module Saved exposing
    ( Saved
    , new
    , value
    , change
    , save
    , discard
    , saved
    , setSaved
    , update
    , map
    , Eq
    , customNew
    , customMap
    )

{-| A type to keep track of a value that can be saved. It keeps track of the
changes to a value since it was last saved.

Internally the `Saved a` keeps track of two values: the last saved value and
the current value.

@docs Saved
@docs new
@docs value
@docs change
@docs save
@docs discard
@docs saved


## Advanced

These functions can be very useful.
They are not the best way to get to know this library though, which is why they
are split out into this section.

@docs setSaved
@docs update
@docs map


## Using a custom equality check

To determine whether a `Saved a` has changes, the saved and changed values are
compared.
Sometimes you may want to use a custom equality check for this.
The functions below allow you to create `Saved a` instances with such a custom
check.

@docs Eq
@docs customNew
@docs customMap

-}


{-| A type representing a value that can be saved.
-}
type Saved a
    = Saved (Eq a) a a


{-| Take a value and wrap it into a `Saved a`.
This initial value is marked as having been saved.

    new "Bear"
        |> saved --> True

-}
new : a -> Saved a
new a =
    customNew (==) a


{-| Get the current value from a `Saved a`.

    new "Bear"
        |> value --> "Bear"

-}
value : Saved a -> a
value (Saved _ _ latest) =
    latest


{-| Modify the current value in your `Saved a` using a function.
This does not affect the last saved value.

    new "Bear"
        |> change (\animal -> animal ++ "s")
        |> saved
        --> False

-}
change : (a -> a) -> Saved a -> Saved a
change fn (Saved eq initial latest) =
    let
        newLatest : a
        newLatest =
            fn latest

        -- If nothing relevant to the save state has changed, update the
        -- initial value too. This way, when we discard, we roll back to the
        -- most recent time the initial and latest values match.
        newInitial : a
        newInitial =
            if eq initial newLatest then
                newLatest

            else
                initial
    in
    Saved eq newInitial newLatest


{-| This function is like `change`, but allows your changing function to
produce a command. This is typically useful in the `update` function of your
program.
-}
update : (a -> ( a, cmd )) -> Saved a -> ( Saved a, cmd )
update updateFn saved_ =
    updateFn (value saved_)
        |> Tuple.mapFirst (\x -> change (always x) saved_)


{-| Save the current value.

    new "Bear"
        |> change (\animal -> animal ++ "s")
        |> save
        |> saved
        --> True

-}
save : Saved a -> Saved a
save (Saved eq _ latest) =
    Saved eq latest latest


{-| Discard any changes and reset the current value to the last saved value.

    new "Bear"
        |> change (\animal -> animal ++ "s")
        |> discard
        |> value
        --> "Bear"

-}
discard : Saved a -> Saved a
discard (Saved eq initial _) =
    Saved eq initial initial


{-| Check if any changes have been made since the value was last changed.

    new "Bear"
        |> saved
        --> True

    new "Bear"
        |> change (\animal -> animal ++ "s")
        |> saved
        --> False

-}
saved : Saved a -> Bool
saved (Saved eq initial latest) =
    eq initial latest


{-| Set the last saved value, keeping the current value unchanged.

You might use this when your backend reports to you a previous value was saved,
but your user might have made new changes in the meanwhile.

If you want to immediately mark the current value as saved you can use `save`.

    new "Bear"
        |> change (\animal -> animal ++ "s")
        |> change (\animal -> animal ++ "kin")
        |> setSaved "bears"
        |> saved
        --> False

-}
setSaved : a -> Saved a -> Saved a
setSaved newInitial (Saved eq _ latest) =
    Saved eq newInitial latest


{-| Map over the values in a saved.

Beware that this applies the mapping function to both the last saved and the
current value, and so should not be used to make changes only to the current
value (for that, use 'change').

As a rule of thumb, if your map function does not go from a type to another type
and so is `a -> a`, you probably don't want to use this.

-}
map : (a -> b) -> Saved a -> Saved b
map fn saved_ =
    customMap (==) fn saved_


{-| Alias for an equality function used to tell if two values are equal.
For most purposes this can be `==`, but some types require a customized
equality check.
-}
type alias Eq a =
    a -> a -> Bool


{-| Like `new`, but allowing you to specify an equality function.
-}
customNew : Eq a -> a -> Saved a
customNew eq value_ =
    Saved eq value_ value_


{-| Like `map`, but allowing you to specify a new equality function for the new
type returned from the mapping function.
-}
customMap : Eq b -> (a -> b) -> Saved a -> Saved b
customMap newEq fn (Saved _ initial latest) =
    Saved newEq (fn initial) (fn latest)
