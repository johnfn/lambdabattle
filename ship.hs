import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Player = PlayerOne | PlayerTwo deriving (Eq, Ord, Show)
data Ship = Invalid | ShipData [(Int, Int)] Player deriving Show

repr PlayerOne = "1"
repr PlayerTwo = "2"

getPosition :: Ship -> [(Int, Int)]
getPosition (ShipData pos _) = pos

getPlayer :: Ship -> Player
getPlayer (ShipData _ player) = player

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

removeFromShip :: (Int, Int) -> Ship -> Ship
removeFromShip position ship@(ShipData coords player) =
  if position `elem` coords then (ShipData (delete position coords) player) else ship

removePosition position ships = 
  map (\shipCoords -> removeFromShip position shipCoords) ships

gameLoop boards ships shots currentPlayer = do
  let myMap = fromJust $ Map.lookup (otherPlayer currentPlayer) boards
  let otherShips = fromJust $ Map.lookup (otherPlayer currentPlayer) ships

  putStrLn ((show currentPlayer) ++ ", here is your board:")
  putStrLn (showBoard (fromJust (Map.lookup currentPlayer boards)) (fromJust (Map.lookup currentPlayer ships)))
  putStrLn ((show currentPlayer) ++ ", take a shot. (x y)")
  input <- getLine

  let coords = (arr !! 0, arr !! 1) where arr = intArray input

  --Nasty because you have to go into a data structure and change it.
  updatedOtherShips <- if coords `elem` (concat (map getPosition otherShips)) 
                            then do
                              putStrLn "HIT!"
                              return (removePosition coords otherShips)
                            else do
                              putStrLn "Miss. :("
                              return (otherShips)

  let updatedShots = Map.insertWith (++) currentPlayer [coords] shots
  let updatedShips = Map.insert other updatedOtherShips ships

  if (length (concat (map getPosition updatedOtherShips))) == 0
    then do
      putStrLn "You win."
    else do
      gameLoop boards updatedShips updatedShots other

  where other = (otherPlayer currentPlayer)

main = do
  let boards = Map.fromList $ zip players $ take 2 $ repeat $ Map.fromList $ zip (emptyBoard 10) (repeat "0")
  shipList <- mapM getShip players
  --TODO: This is pretty silly since right now shipList is not an array (bad name!) Should fix in future... maybe
  let ships = Map.fromList (zip players (map (\x -> [x]) shipList))
  let shots = Map.fromList (zip players (repeat []))

  putStrLn "This. Is. BATTLELAMBDA. It's like Battleship, but better.\n"

  gameLoop boards ships shots PlayerOne
