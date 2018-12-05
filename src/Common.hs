module Common
    (
    eitherError,
    doExercise
    ) where


--simple wrapper for running an exercise on a given file
doExercise input ex = fmap ex (readFile input)


--convert an Either (e.g. parse result) to its Right or crash badly
eitherError :: Show a => Either a b -> b
eitherError (Left a) = (error . show) a
eitherError (Right b) = b
