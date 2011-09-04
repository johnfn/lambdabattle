import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Player = PlayerOne | PlayerTwo deriving Eq
data Ship = Invalid | ShipData [(Int, Int)] Player

getPosition :: Ship -> [(Int, Int)]
getPosition (ShipData pos _) = pos

instance Show Ship where
  show (ShipData a owner) = "A ship starting at " ++ (show (head a)) ++ " going to " ++ (show (last a)) ++ "."

addShip :: Ship -> Map.Map (Int, Int) String -> Map.Map (Int, Int) String
addShip ship board = foldr (\loc acc -> Map.insert loc "1" acc) board (getPosition ship)

addShips :: Map.Map (Int, Int) String -> [Ship] -> Map.Map (Int, Int) String
addShips board ships = foldr addShip board ships

showBoard :: Map.Map (Int, Int) String -> [Ship] -> String
showBoard board ships = intercalate "\n" [concat [fromJust $ Map.lookup (i, j) boardShips | i <- [0..9]] | j <- [0..9]]
                        where
                          boardShips = addShips board ships

getShip :: IO Ship
getShip = do
  putStrLn "(x1, y1) (x2, y2)"
  input <- getLine
  let coords = map (\x -> (read x::Int)) (words input)
  let xcoords = if coords !! 0 == coords !! 2 then repeat (coords !! 0) else [coords !! 0..coords !! 2]
      ycoords = if coords !! 1 == coords !! 3 then repeat (coords !! 1) else [coords !! 1..coords !! 3]

  return (ShipData (zip xcoords ycoords) PlayerOne)

main = do
  let board = take 10 (repeat (take 10 (repeat True)))
  let pairs = Map.fromList $ zip [(i, j) | i <- [0..9], j <- [0..9]] (repeat "0")

  ships <- sequence (take 2 (repeat getShip))
  putStrLn $ (showBoard pairs ships)

  putStrLn "This. Is. BATTLELAMBDA. It's like Battleship, but better.\n"
