import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- Allow shots to be taken

data Player = PlayerOne | PlayerTwo deriving (Eq, Ord, Show)
data Ship = Invalid | ShipData [(Int, Int)] Player deriving Show

repr PlayerOne = "1"
repr PlayerTwo = "2"

getPosition :: Ship -> [(Int, Int)]
getPosition (ShipData pos _) = pos

playerName :: Player -> String
playerName PlayerOne = "Player One"
playerName PlayerTwo = "Player Two"

players :: [Player]
players = [PlayerOne, PlayerTwo] --TODO: Some better way?

otherPlayer :: Player -> Player
otherPlayer PlayerOne = PlayerTwo
otherPlayer PlayerTwo = PlayerOne

numShips :: Int
numShips = 1

addShip :: Ship -> Map.Map (Int, Int) String -> Map.Map (Int, Int) String
addShip (ShipData positions player) board = foldr (\loc acc -> Map.insert loc playerChar acc) board positions
                                            where playerChar = repr player

addShips :: Map.Map (Int, Int) String -> [Ship] -> Map.Map (Int, Int) String
addShips board ships = foldr addShip board ships

showBoard :: Map.Map (Int, Int) String -> [Ship] -> String
showBoard board ships = intercalate "\n" [concat [fromJust $ Map.lookup (i, j) boardShips | i <- [0..9]] | j <- [0..9]]
                        where boardShips = addShips board ships

intArray :: String -> [Int]
intArray input = map (\chunk -> read chunk::Int) (words input)

getShip :: Player -> IO Ship
getShip player = do
  putStrLn $ (playerName player) ++ ", place your ship! (x1 y1 x2 y2)"
  input <- getLine
  let coords = intArray input
  let xcoords = if coords !! 0 == coords !! 2 then repeat (coords !! 0) else [coords !! 0..coords !! 2]
      ycoords = if coords !! 1 == coords !! 3 then repeat (coords !! 1) else [coords !! 1..coords !! 3]

  return (ShipData (zip xcoords ycoords) player)

emptyBoard :: Int -> [(Int, Int)]
emptyBoard size = [(i, j) | i <- [0..adjSize], j <- [0..adjSize]]
                where adjSize = size - 1

gameLoop boards ships shots currentPlayer = do
  let myMap = fromJust $ Map.lookup (otherPlayer currentPlayer) boards
  let otherShips = fromJust $ Map.lookup (otherPlayer currentPlayer) ships

  putStrLn (show otherShips)

  putStrLn ((show currentPlayer) ++ ", here is your board:")
  putStrLn (showBoard (fromJust (Map.lookup currentPlayer boards)) [(fromJust (Map.lookup currentPlayer ships))])
  putStrLn ((show currentPlayer) ++ ", take a shot. (x y)")
  input <- getLine
  let coords = intArray input
  -- if Map.lookup myMap coords /= "0"

  let updatedShots = Map.insertWith (++) currentPlayer [coords] shots
  putStrLn (show updatedShots)

  gameLoop boards ships updatedShots (otherPlayer currentPlayer)

main = do
  let boards = Map.fromList $ zip players $ take 2 $ repeat $ Map.fromList $ zip (emptyBoard 10) (repeat "0")
  shipList <- mapM getShip players
  let ships = Map.fromList (zip players shipList)
  let shots = Map.fromList (zip players (repeat []))

  putStrLn "This. Is. BATTLELAMBDA. It's like Battleship, but better.\n"

  gameLoop boards ships shots PlayerOne
