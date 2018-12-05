module Common
    (
    eitherError,
    doExercise,
    both,
    hasLength
    ) where


--simple wrapper for running an exercise on a given file
doExercise input ex = fmap ex (readFile input)


--convert an Either (e.g. parse result) to its Right or crash badly
eitherError :: Show a => Either a b -> b
eitherError (Left a) = (error . show) a
eitherError (Right b) = b

both :: (a->b) -> (a,a) -> (b,b)
both f (a1,a2) = (f a1, f a2)

hasLength 0 [] = True
hasLength 0 (_:_) = False
hasLength n [] = False
hasLength n (_:xs) = hasLength (n-1) xs
