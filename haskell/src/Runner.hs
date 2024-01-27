module Runner (DaySolver, run, placeholder) where

type DaySolver = (String -> IO (String, String))

placeholder :: DaySolver
placeholder _ = return ("placeholder", "placeholder")

run :: [DaySolver] -> Int -> DaySolver
run daySolvers day = daySolvers !! (day - 1)
