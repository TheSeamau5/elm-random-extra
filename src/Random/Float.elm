module Random.Float where
{-| List of Float Generators

# Generators
@docs anyFloat, positiveFloat, negativeFloat, floatGreaterThan, floatLessThan, probability, negativeProbability, unitRange

# Gaussian Generators
@docs normal, standardNormal, gaussian

-}

import Random       exposing (Generator, float, maxInt, minInt, customGenerator, generate)
import Random.Extra exposing (map)


{-| Generator that generates any float
-}
anyFloat : Generator Float
anyFloat = float (toFloat minInt) (toFloat maxInt)

{-| Generator that generates any positive float
-}
positiveFloat : Generator Float
positiveFloat = float 0 (toFloat maxInt)

{-| Generator that generates any negative float
-}
negativeFloat : Generator Float
negativeFloat = float (toFloat minInt) 0

{-| Generator that generates a float greater than a given float
-}
floatGreaterThan : Float -> Generator Float
floatGreaterThan value = float value (toFloat maxInt)

{-| Generator that generates a float less than a given float
-}
floatLessThan : Float -> Generator Float
floatLessThan value = float (toFloat minInt) value

{-| Generator that generates a float between 0 and 1
-}
probability : Generator Float
probability = float 0 1

{-| Generator that generates a float between -1 and 0
-}
negativeProbability : Generator Float
negativeProbability = float -1 0

{-| Generator that generates a float between - 1 and 1
-}
unitRange : Generator Float
unitRange = float -1 1


{-| Create a generator of floats that is normally distributed with
given the mean, and standard deviation.
-}
normal : Float -> Float -> Generator Float
normal mean standardDeviation = map (\x -> x*standardDeviation + mean) standardNormal

{-| Generator that follows a standard normal distribution (as opposed to
a uniform distribution)
-}
standardNormal : Generator Float
standardNormal =
    -- This is the polar Box-Muller algorithm as described here http://www.design.caltech.edu/erik/Misc/Gaussian.html
    -- There may be more efficient and more correct algorithms; see here http://stackoverflow.com/questions/75677/converting-a-uniform-distribution-to-a-normal-distribution
    let
        loop seed =
            let
                (r1,seed') = generate (float 0 1) seed
                (r2,seed'') = generate (float 0 1) seed'
                x1 = 2.0 * r1 - 1.0
                x2 = 2.0 * r2 - 1.0
                w = x1*x1 + x2*x2
            in
                if | w >= 1.0 -> loop seed''
                   | otherwise ->
                        let
                            w' = sqrt (-2.0 * (logBase 10 w) / w)
                            y1 = x1 * w
                        in
                            (y1, seed'')
    in
        customGenerator loop

{-| Alias for `normal`.
-}
gaussian : Float -> Float -> Generator Float
gaussian = normal
