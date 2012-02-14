

-- squareRoot :: Rational -> Rational
squareRoot x = head (dropBadGuesses  x (makeGuesses x 1))

betterGuess x guess = (guess + (x / guess)) / 2
makeGuesses x start = iterate (betterGuess x) start

isGoodEnough delta x guess = abs (guess * guess - x) <= delta
dropBadGuesses delta x guesses = dropWhile (not . isGoodEnough delta x) guesses
