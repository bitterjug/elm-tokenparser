module Type.Parser exposing
    ( ParseError
    , Parser
    , andThen
    , drop
    , either
    , end
    , fail
    , keep
    , many
    , map
    , oneOf
    , parse
    , satisfies
    , succeed
    , token
    )

import Helper.Result as Result


type alias ParseError =
    String


type alias Parser token a =
    List token -> Result ParseError ( a, List token )


parse : Parser token a -> List token -> Result ParseError a
parse parser tokens =
    parser tokens
        |> Result.map Tuple.first


succeed : a -> Parser token a
succeed a =
    \tokens ->
        Ok ( a, tokens )


fail : String -> Parser token a
fail message =
    \tokens -> Err message


token : Parser token token
token =
    \tokens ->
        case tokens of
            head :: tail ->
                Ok ( head, tail )

            [] ->
                Err "Unexpected end of input"


end : Parser token ()
end =
    \tokens ->
        case tokens of
            [] ->
                Ok ( (), [] )

            _ ->
                Err "Expected end of input"


satisfies : (token -> Result ParseError a) -> Parser token a
satisfies predicate =
    token
        |> andThen (predicate >> Result.either succeed fail)


andThen : (a -> Parser token b) -> Parser token a -> Parser token b
andThen continuation parser =
    parser
        >> Result.andThen
            (\( a, tokens ) -> continuation a tokens)


map : (a -> b) -> Parser token a -> Parser token b
map f parser =
    parser
        |> andThen (\a -> succeed (f a))


either : Parser token a -> Parser token a -> Parser token a
either parser1 parser2 =
    \tokens ->
        parser1 tokens
            |> Result.map Ok
            |> Result.withDefault (parser2 tokens)


oneOf : List (Parser token a) -> Parser token a
oneOf parsers =
    case parsers of
        [] ->
            fail "No parsers in oneOf"

        p :: ps ->
            List.foldr either p ps


keep : Parser token a -> Parser token (a -> b) -> Parser token b
keep pa pf =
    pf |> andThen (\f -> map f pa)


drop : Parser token drop -> Parser token keep -> Parser token keep
drop dropper keeper =
    keeper
        |> andThen
            (\value ->
                dropper
                    |> andThen (\_ -> succeed value)
            )


lazy : (() -> a -> b) -> (a -> b)
lazy f =
    \a -> f () a


many : Parser token a -> Parser token (List a)
many parser =
    either
        (succeed (::)
            |> keep parser
            |> keep (lazy (\() -> many parser))
        )
        (succeed [])
