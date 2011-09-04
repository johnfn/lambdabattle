import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Player = PlayerOne | PlayerTwo deriving Eq
data Ship = Invalid | ShipData [(Int, Int)] Player

instance Show Ship where
  show (ShipData a owner) = "A ship starting at " ++ (show (head a)) ++ " going to " ++ (show (last a)) ++ "."

instance Show Player where
  show PlayerOne = "1"
  show PlayerTwo = "2"

getPosition :: Ship -> [(Int, Int)]
getPosition (ShipData pos _) = pos

playerName :: Player -> String
playerName PlayerOne = "Player One"
playerName PlayerTwo = "Player Two"

numShips :: Int
numShips = 1

addShip :: Ship -> Map.Map (Int, Int) String -> Map.Map (Int, Int) String
addShip (ShipData positions player) board = foldr (\loc acc -> Map.insert loc playerChar acc) board positions
                                            where playerChar = show player

addShips :: Map.Map (Int, Int) String -> [Ship] -> Map.Map (Int, Int) String
addShips board ships = foldr addShip board ships

showBoard :: Map.Map (Int, Int) String -> [Ship] -> String
showBoard board ships = intercalate "\n" [concat [fromJust $ Map.lookup (i, j) boardShips | i <- [0..9]] | j <- [0..9]]
                        where boardShips = addShips board ships

getShip :: Player -> IO Ship
getShip player = do
  putStrLn $ (playerName player) ++ ", place your ship! (x1 y1 x2 y2)"
  input <- getLine
  let coords = map (\x -> (read x::Int)) (words input)
  let xcoords = if coords !! 0 == coords !! 2 then repeat (coords !! 0) else [coords !! 0..coords !! 2]
      ycoords = if coords !! 1 == coords !! 3 then repeat (coords !! 1) else [coords !! 1..coords !! 3]

  return (ShipData (zip xcoords ycoords) player)

emptyBoard :: Int -> [(Int, Int)]
emptyBoard size = [(i, j) | i <- [0..adjSize], j <- [0..adjSize]]
                where adjSize = size - 1

main = do
  let board = Map.fromList (zip (emptyBoard 10) (repeat "0"))

  ships <- sequence ((++) (take numShips (repeat $ getShip PlayerOne)) (take numShips (repeat $ getShip PlayerTwo)))
  putStrLn $ (showBoard board ships)

  putStrLn "This. Is. BATTLELAMBDA. It's like Battleship, but better.\n"
