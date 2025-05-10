import Data.Map qualified as Map

data Location = DungeonEntrance | MainHall | DarkRoom1 | PitBottom | AlchemyLab | Fork1 | Chapel | Library | Kitchen | Fork2 | GuardQuarters | Jail | TortureChamber | Crypt | InnerSanctum
  deriving (Show, Eq, Read)

data Object = Torch | DungeonEntranceNote | Statue | Pit | EscapeAttempt | OpiumPowder | BlueVial | GreenVial | PurpleVial | RedVial | KitchenKnife | RottenMeat | Fish | Bread | WineBottle | Priest | DeadPriest | PyramidArtifact | SphereArtifact | CubeArtifact | AlchemyTechniques | Merthveer | SilentFigure | HumanRemains | HorrifiedMan | DyingPrisoner | CorrodedDagger | SleepingGuard | DeadGuard | Whisky | Vodka
  deriving (Show, Eq, Read)

data Direction = N | S | E | W | Beneath
  deriving (Show, Eq, Read)

data GameState = GameState
  { currentLocation :: Location,
    itemsInLocations :: Map.Map Location [Object],
    playerInventory :: Map.Map Object Int,
    playerSanity :: Int,
    playerHunger :: Int,
    isTorchLit :: Bool,
    torchTurnsRemaining :: Int,
    decayNoPickup :: Bool,
    isHallucinating :: Bool,
    hasDecayingHands :: Bool,
    decayTurnsLeft :: Int,
    artifactsPlacedCount :: Int,
    priestStatus :: PriestState
  }
  deriving (Show)

data PriestState = PriestAlive | PriestDead deriving (Show, Eq)

introductionText =
  [ "The year is 1433. A bitter wind carries the scent of decay as darkness falls over the land.",
    "Sir Galahad, once revered as the most valiant of the king''s knights, was dispatched to these",
    "accursed catacombs with a desperate mission - to retrieve the Chalice of Immaculate Tears,",
    "the only hope for our monarch who writhes in agony as black veins spread beneath his skin.",
    "Three moons have waxed and waned since Sir Galahad departed into these forsaken lands.",
    "As his loyal squire, you now stand where he once stood, the weight of duty like a millstone.",
    "The locals speak only in whispers of what dwells below - abominations born of forbidden alchemy,",
    "tortured souls that wander the eternal darkness, and horrors that feast upon sanity itself.",
    "Hunger and fear will be your constant companions in this labyrinth of the damned."
  ]

instructionsText =
  [ "Enter commands using standard Prolog syntax.",
    "Available commands are:",
    "go n | s | e | w | beneath   -- to go in that direction.",
    "take object                  -- to pick up an object.",
    "examine object               -- to display info about an item.",
    "use object                   -- to use an object.",
    "look.                        -- to look around you again.",
    "status.                      -- to display info about your inventory and status.",
    "help.                        -- to see this message again.",
    "halt.                        -- to end the game and quit."
  ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printIntroduction = printLines introductionText

printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
  putStr "> "
  xs <- getLine
  return xs

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: IO ()
gameLoop = do
  cmd <- readCommand
  case cmd of
    "help" -> do
      printInstructions
      gameLoop
    "quit" -> return ()
    _ -> do
      printLines ["Unknown command.", ""]
      gameLoop

main = do
  printIntroduction
  printInstructions
  gameLoop
