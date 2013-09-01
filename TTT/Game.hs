module TTT.Game (
  askWhoGoesFirst
 ,Person
 ,Computer
 ,Human
)  where

data Computer
data Human
data Person = Computer | Human

askWhoGoesFirst = do
  putStrLn "Who do you want to go first?"
  putStrLn "  1. Computer"
  putStrLn "  2. Human"
  putStrLn "choice: "
  ans <- getLine
  case ans of
    "1" -> return Computer
    "2" -> return Human
    _ -> askWhoGoesFirst

