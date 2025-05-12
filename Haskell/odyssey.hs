{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (when, unless, forM_)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import System.Random (randomRIO)
import System.IO (hFlush, stdout)
import Data.Char (toLower)



data Location = DungeonEntrance | MainHall | DarkRoom1 | PitBottom | AlchemyLab
              | Fork1 | Chapel | Library | Kitchen | Fork2 | GuardQuarters
              | Jail | TortureChamber | Crypt | InnerSanctum
              deriving (Show, Eq, Read, Ord, Enum, Bounded)

data Object = Torch | DungeonEntranceNote | Statue | Pit | EscapeAttempt
            | OpiumPowder | BlueVial | GreenVial | PurpleVial | RedVial
            | KitchenKnife | RottenMeat | Fish | Bread | WineBottle
            | Priest | DeadPriest
            | PyramidArtifact | SphereArtifact | CubeArtifact
            | AlchemyTechniques | Merthveer
            | SilentFigure | HumanRemains | HorrifiedMan | DyingPrisoner
            | CorrodedDagger
            | SleepingGuard | DeadGuard
            | Whisky | Vodka
          
            deriving (Show, Eq, Ord, Read, Enum, Bounded)

data Direction = N | S | E | W | Beneath
               deriving (Show, Eq, Read, Ord) 

data PriestState = PriestAlive | PriestDefeated deriving (Show, Eq, Ord)
data GuardState = GuardAsleep | GuardDefeated deriving (Show, Eq, Ord)

data GameState = GameState
  { currentLocation :: Location
  , paths :: Map Location (Map Direction Location)
  , locationDescriptions :: Map Location (GameState -> String)
  , objectData :: Map Object ObjectData
  , itemsInLocations :: Map Location (Set Object)
  , playerInventory :: Map Object Int
  , playerSanity :: Int
  , playerHunger :: Int
  , isTorchLit :: Bool
  , torchTurnsRemaining :: Int
  , hasDecayingHands :: Bool
  , decayTurnsLeft :: Int
  , decayNoPickup :: Bool
  , isHallucinating :: Bool
  , placedArtifacts :: Set Object 
  , statueMoved :: Bool
  , priestState :: PriestState
  , guardState :: GuardState
  , gameShouldEnd :: Bool
  , messagesToPlayer :: [String]
  } 

data ObjectData = ObjectData
  { objectName :: String 
  , examineText :: GameState -> String 
  , isTakeable :: Bool
  , isUsable :: Bool
  }

type GameAction = StateT GameState IO


introductionText :: [String]
introductionText =
  [ "The year is 1433. A bitter wind carries the scent of decay as darkness falls over the land."
  , "Sir Galahad, once revered as the most valiant of the king's knights, was dispatched to these"
  , "accursed catacombs with a desperate mission - to retrieve the Chalice of Immaculate Tears,"
  , "the only hope for our monarch who writhes in agony as black veins spread beneath his skin."
  , "Three moons have waxed and waned since Sir Galahad departed into these forsaken lands."
  , "As his loyal squire, you now stand where he once stood, the weight of duty like a millstone."
  , "The locals speak only in whispers of what dwells below - abominations born of forbidden alchemy,"
  , "tortured souls that wander the eternal darkness, and horrors that feast upon sanity itself."
  , "Hunger and fear will be your constant companions in this labyrinth of the damned."
  ]

instructionsText :: [String]
instructionsText =
  [ "Enter commands using standard Prolog syntax. (Haskell adaptation uses simplified commands)"
  , "Available commands are:"
  , "  go [n|s|e|w|beneath]     - to go in that direction." 
  , "  take [object_name]      - to pick up an object."
  , "  examine [object_name]"   
  , "  use [object_name]         - to use an object."
  , "  look                    - to look around you again."
  , "  inventory               - to display your inventory."
  , "  status                  - to display info about your status."
  , "  help                    - to see this message again."
  , "  quit                    - to end the game and quit."
  ]


printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printIntroduction :: IO ()
printIntroduction = printLines introductionText

printInstructions :: IO ()
printInstructions = printLines instructionsText

addMessage :: String -> GameAction ()
addMessage msg = modify $ \gs -> gs { messagesToPlayer = messagesToPlayer gs ++ [msg] }

flushMessages :: GameAction ()
flushMessages = do
  msgs <- gets messagesToPlayer
  liftIO $ printLines msgs
  modify $ \gs -> gs { messagesToPlayer = [] }



initialGameState :: GameState
initialGameState = GameState
  { currentLocation = DungeonEntrance
  , paths = definePaths
  , locationDescriptions = defineLocationDescriptions 
  , objectData = defineObjectData                   
  , itemsInLocations = initialItemsInLocations
  , playerInventory = Map.empty
  , playerSanity = 100
  , playerHunger = 0
  , isTorchLit = False
  , torchTurnsRemaining = 0
  , hasDecayingHands = False
  , decayTurnsLeft = 20
  , decayNoPickup = False
  , isHallucinating = False
  , placedArtifacts = Set.empty
  , statueMoved = False
  , priestState = PriestAlive
  , guardState = GuardAsleep
  , gameShouldEnd = False
  , messagesToPlayer = []
  }

definePaths :: Map Location (Map Direction Location)
definePaths = Map.fromList
  [ (DungeonEntrance, Map.fromList [(N, MainHall)])
  , (MainHall, Map.fromList [(S, DungeonEntrance), (E, DarkRoom1), (W, AlchemyLab), (N, Fork1)])
  , (DarkRoom1, Map.fromList [(W, MainHall), (Beneath, PitBottom)])
  , (PitBottom, Map.empty)
  , (AlchemyLab, Map.fromList [(E, MainHall)])
  , (Fork1, Map.fromList [(S, MainHall), (W, Kitchen), (E, Chapel), (N, Fork2)])
  , (Kitchen, Map.fromList [(E, Fork1)])
  , (Chapel, Map.fromList [(W, Fork1), (S, Library)])
  , (Library, Map.fromList [(N, Chapel)])
  , (Fork2, Map.fromList [(S, Fork1), (W, GuardQuarters), (E, Jail), (N, Crypt)])
  , (GuardQuarters, Map.fromList [(E, Fork2)])
  , (Jail, Map.fromList [(W, Fork2), (N, TortureChamber)])
  , (TortureChamber, Map.fromList [(S, Jail)])
  , (Crypt, Map.fromList [(S, Fork2)])
  , (InnerSanctum, Map.empty)
  ]

initialItemsInLocations :: Map Location (Set Object)
initialItemsInLocations = Map.fromList
  [ (DungeonEntrance, Set.fromList [Torch, DungeonEntranceNote])
  , (MainHall, Set.fromList [Statue])
  , (DarkRoom1, Set.fromList [Pit])
  , (AlchemyLab, Set.fromList [OpiumPowder, BlueVial, GreenVial, PurpleVial, RedVial])
  , (Fork1, Set.fromList [Torch]) 
  , (Kitchen, Set.fromList [KitchenKnife, RottenMeat, Fish, Bread, WineBottle])
  , (Chapel, Set.fromList [Priest, PyramidArtifact])
  , (Library, Set.fromList [AlchemyTechniques, Merthveer])
  , (Jail, Set.fromList [SilentFigure, HumanRemains, HorrifiedMan])
  , (TortureChamber, Set.fromList [DyingPrisoner, CorrodedDagger])
  , (GuardQuarters, Set.fromList [SleepingGuard, Whisky, Vodka])
  , (Crypt, Set.fromList [CubeArtifact])
  ]



defineLocationDescriptions :: Map Location (GameState -> String)
defineLocationDescriptions = Map.fromList
  [ (DungeonEntrance, \_ -> unlines
      [ "You are at the entrance to the ancient catacombs. The stone steps lead down into darkness."
      , "A cold draft whispers from below, carrying with it the scent of old stone and something else..."
      ])
  , (MainHall, \gs -> unlines
      [ "You stand in the vast main hall of the catacombs."
      , "The vaulted ceiling vanishes into darkness, as if swallowed by the void itself."
      , "Stone pillars rise like calcified trees, each carved with symbols that seem to writhe"
      , "when viewed from the corner of your eye. Blood-red lichen clings to the damp walls."
      , "At the center looms a weathered statue of some ancient deity - part human, part beast -"
      , "its hollow eyes seeming to track your movement. A viscous substance drips from its mouth."
      , if statueMoved gs
        then "With a grinding sound of stone against stone, the floor beneath the statue has split open, revealing an impossibly narrow, spiral staircase carved into the bedrock, descending into absolute darkness (go beneath)."
        else "The base of the statue contains three distinct recesses, shaped to hold specific objects."
      , "Three archways lead into darkness: north (n), east (e), and west (w). To the south (s) is the way out of these catacombs."
      , "From somewhere deep below, you hear the faint sound of something that might be weeping... or laughter."
      ])
  , (DarkRoom1, \_ -> unlines
      [ "You enter a small, rectangular chamber with crumbling stone walls. In the center of"
      , "the floor gapes a dark, circular pit (pit). A putrid stench"
      , "rises from the abyss, making your eyes water and stomach churn."
      , "The darkness below seems absolute, swallowing the feeble light completely."
      , "Something about the way sound echoes when small debris falls in suggests"
      , "a terrifying depth. The rim of the hole is worn smooth."
      , "Dark stains radiate outward from the pit. Scratches cover the nearby walls,"
      , "some appearing to form words now too faded to read."
      , "You can attempt to descend into the pit (go beneath)."
      ])
  , (PitBottom, \gs -> unlines
      [ "You are submerged waist-deep in foul liquid at the bottom of the hole."
      , "The walls rise impossibly high above, the entrance now just a distant pinprick of light."
      , if isTorchLit gs
        then "Your torch struggles against the oppressive darkness of this place."
        else "The darkness here is absolute, suffocating."
      , "You notice what seems to be evidence of a previous escape attempt (escapeattempt) on the walls."
      ])
  , (AlchemyLab, \_ -> unlines
      [ "You enter an ancient alchemical workshop. Heavy stone tables are stained with centuries of spilled reagents."
      , "Cracked retorts, dusty alembics, and rusted instruments lie scattered across the workspace."
      , "Shelves line the walls, bearing clay jars with faded labels and the desiccated remains of strange herbs."
      , "In the center of the room, a curious apparatus of brass tubes and glass bulbs stands cold and silent."
      , "Among the debris on the main table, you notice four small vials of different colors: blue (bluevial), green (greenvial), purple (purplevial), and red (redvial)."
      , "There is also some opium powder (opiumpowder) here."
      ])
  , (Fork1, \_ -> unlines
      [ "You stand at a grim intersection where multiple pathways meet."
      , "The air is stagnant here, heavy with the scent of mold and decay."
      , "Carved into the weathered stone walls are strange symbols - warnings, perhaps,"
      , "or invocations to forgotten deities that once held sway in these depths."
      , "To the west (w), an unfamiliar odor wafts from the darkness."
      , "Eastward (e), you hear distant, rhythmic chanting that raises the hair on your neck."
      , "Northward (n), the passage descends deeper, and a trail of dark, congealed blood disappears into the shadows."
      , "The floor is marked with countless scratches, as if something was dragged through here repeatedly."
      , "There is another torch (torch) here."
      ])
  , (Kitchen, \_ -> unlines
      [ "You enter what once served as a kitchen for the inhabitants of these catacombs."
      , "A massive stone hearth dominates one wall, its interior blackened with ancient soot."
      , "Long wooden tables, warped and split with age, line the center of the room."
      , "Rusted implements hang from hooks on the walls, and cracked ceramic vessels"
      , "lie scattered across shelves and in corners. The air carries a sickly-sweet smell"
      , "of spoiled food mingled with something less identifiable. Dark stains cover"
      , "the large butchering block in the corner. Something scurries away as you enter."
      , "You see a kitchen knife (kitchenknife), some rotten meat (rottenmeat), a fish (fish), a loaf of bread (bread), and a wine bottle (winebottle)."
      ])
  , (Chapel, \gs -> unlines $
      if priestState gs == PriestAlive then
        [ "You enter a small chapel carved into the living rock. The air is thick with incense"
        , "and the sickly-sweet smell of decay. Rows of broken pews face a blackened stone altar,"
        , "upon which rest strange artifacts including a small stone pyramid (pyramidartifact). Dozens of candles cast"
        , "dancing shadows across wall carvings depicting scenes of sacrifice and transformation."
        , "At the altar stands a figure (priest) in tattered robes, facing away from you, muttering"
        , "in a language you do not understand. At the sound of your entrance, the figure"
        , "turns slowly, revealing a gaunt face with sunken eyes that gleam with madness."
        , "The priest's lips curl into a terrible smile."
        ]
      else
        [ "You enter the defiled chapel, now eerily silent except for the dripping of black ichor"
        , "from the altar. The stone pyramid (pyramidartifact) still rests among the ritual items. Candles burn with"
        , "strange blue flames, casting grotesque shadows of the dead priest's (deadpriest) body across the walls."
        , "The air feels heavier, as if the ritual space has been further corrupted by the violent death."
        , "A low, almost subsonic humming emanates from somewhere beneath the altar."
        ])
  , (Library, \_ -> unlines
      [ "You enter a library filled with dust-covered tomes and scrolls."
      , "Towering bookshelves stretch toward the shadowed ceiling, some tilting precariously under the weight of knowledge."
      , "Parchment scrolls and leather-bound volumes lie scattered across tables and the floor."
      , "Many books are bound in materials that don't appear to be ordinary leather."
      , "Strange symbols have been scratched into the stone floor, forming a pattern that seems"
      , "to converge at the center of the room where a reading podium stands."
      , "Some sections of the bookshelves appear burned, and you notice dark stains trailing"
      , "from several tomes as if they had bled."
      , "The air feels heavy with forgotten wisdom and forbidden knowledge."
      , "You see a book of alchemy techniques (alchemytechniques) and another strange tome titled 'Merthveer' (merthveer)."
      ])
  , (Fork2, \_ -> unlines
      [ "You reach another grim intersection, deeper into the catacombs."
      , "The air here is thick with the scent of damp stone and something faintly metallic."
      , "From the western passage (w), you hear heavy, rhythmic breathing - something large slumbers there."
      , "To the east (e), faint whimpers and the distant clink of chains echo through the darkness."
      , "The northern corridor (n) slopes gently downward, carrying the scent of ancient death."
      , "The walls here bear marks of violence - deep gouges in the stone and what appear to be"
      , "handprints made in dried blood. The trail continues here growing fresher"
      , "and more abundant as it disappears down the eastern passage."
      ])
  , (Jail, \_ -> unlines
      [ "You enter a dank prison chamber carved from the living stone."
      , "Three cells line the far wall, their rusted iron bars casting skeletal shadows in your light."
      , "In the first cell, a gaunt figure (silentfigure) sits motionless in the corner, eyes reflecting your light"
      , "like those of a nocturnal beast. He makes no sound, but never breaks his stare."
      , "The middle cell contains the withered corpse of what might have once been a man (humanremains),"
      , "now little more than leathery skin stretched over a crooked frame."
      , "From the third cell, a trembling voice whispers: \"Please... help me... He... This thing..."
      , "what he did to the others... oh gods... please...\" (horrifiedman)."
      , "The trail of blood leads to a dark passage on the northern wall (n),"
      , "disappearing into shadows so thick they seem to devour your light."
      , "Countless handprints in dried blood mark the stone around the entrance."
      ])
  , (TortureChamber, \_ -> unlines
      [ "You enter a chamber of horrors. The air reeks of blood, excrement, and fear."
      , "In the center of the room, suspended from the ceiling by rusted chains,"
      , "hangs what was once a man (dyingprisoner). His body is a canvas of methodical mutilation,"
      , "skin flayed in precise patterns. Blood still drips slowly from his wounds,"
      , "forming a dark pool beneath him. His eyes are open but unseeing - death approaches,"
      , "but has not yet granted mercy."
      , "Against the far wall stands a weapons rack containing a corroded dagger (corroddagger)." -- Corrected object name
      ])
  , (GuardQuarters, \gs -> unlines $
      if guardState gs == GuardAsleep then
        [ "You enter what appears to be living quarters for whatever guards this forsaken place."
        , "Several bunks line the walls, most collapsed with age and decay."
        , "A wooden cabinet against one wall holds numerous bottles of spirits, some still sealed (whisky, vodka)."
        , "In the center of the room, sprawled across a massive iron-framed bed, lies what first appears"
        , "to be a guard in enormous plate armor (sleepingguard)."
        ]
      else -- GuardDefeated
        [ "The guard quarters are now dominated by the fallen form of the mutated guard (deadguard)."
        , "Its body continues to dissolve where the acid made contact, revealing a disturbing"
        , "amalgamation of metal, flesh, and something else entirely beneath the armor."
        , "Black ichor pools around the corpse, bubbling occasionally as if still alive."
        , "Several bunks line the walls, most collapsed with age and decay."
        , "The air feels lighter somehow, as if a malevolent presence has been removed."
        , "You can still see the bottles of spirits (whisky, vodka)."
        ])
  , (Crypt, \_ -> unlines
      [ "You enter an ancient crypt, the air thick with the stench of death and decay."
      , "Stone sarcophagi line the walls, their carved lids depicting the tortured faces of those within."
      , "Most have been desecrated, broken open by force, with desiccated remains spilling onto the floor."
      , "In the center of the chamber stands a raised altar of black stone, its surface stained with ancient blood."
      , "Upon it rests a perfect cube of obsidian-black stone (cubeartifact)."
      , "From somewhere within the sealed sarcophagi, you hear faint scratching sounds, as if something is trying to get out."
      , "Whispers fill your mind when you focus on the obsidian cube, promising terrible knowledge and power."
      , "The floor beneath your feet feels unnaturally warm, and the shadows in the corners seem to move of their own accord."
      ])
  , (InnerSanctum, \_ -> unlines
      [ "You carefully navigate the treacherous spiral staircase, your hands pressed against"
      , "the damp stone walls for balance. The steps are unnaturally steep and narrow,"
      , "forcing you to descend sideways while hunched over."
      , "After what seems like an eternity of careful descent, the staircase opens into a vast chamber."
      , "Your torch illuminates a horrific scene before you. The chamber is vast, its ceiling lost to darkness."
      , "Massive columns rise like petrified trees, carved with scenes of torture and transformation."
      , "The floor is slick with black ichor that pulses with subtle movement."
      , "And there, sprawled before an obsidian altar, lies the broken body of Sir Galahad."
      , "His once-proud armor is shattered, the metal twisted and fused with his flesh in places."
      , "His face, frozen in an expression of absolute horror, bears signs of transformation - "
      , "skin stretched too tight over elongated features, eyes sunken yet somehow too large."
      , "Clutched in his deformed hand is the Chalice of Immaculate Tears, its crystalline surface"
      , "glimmering with unholy light. The liquid within moves against gravity, occasionally forming"
      , "shapes that resemble faces screaming in agony."
      , "As you approach, Sir Galahad's corpse twitches. His head turns with a sickening crack,"
      , "dead eyes fixing on yours. His jaw drops, dislocating with the movement, and"
      , "a voice that is not his whispers: \"You are too late, squire. The king is already ours.\""
      , "Darkness claims your vision as clawed hands grasp you from behind."
      , "Some things are not meant to be found. Some quests are not meant to succeed."
      , "Your story ends here, in the darkness beneath the earth, joining the countless others"
      , "who came seeking power, salvation, or knowledge."
      , "\nTHE END"
      ])
  ]

defineObjectData :: Map Object ObjectData
defineObjectData = Map.fromList
  [ (Torch, ObjectData "torch" (\_ -> unlines
      [ "A wooden torch soaked in pitch. It can provide light in the darkness, but will eventually burn out."
      , "Traveling with a lit torch helps ward off the oppressive darkness, keeping your mind more peaceful."
      ]) True True)
  , (DungeonEntranceNote, ObjectData "dungeonentrancenote" (\_ -> unlines
      [ "A bloodstained parchment with hurried, desperate writing. The message reads:"
      , "\"I have located the Chalice in the deepest chamber of these accursed catacombs. The whispers... they"
      , "speak truths too terrible to bear. Abominations stalk these halls - things that were once men."
      , "Three seals guard the inner sanctum. I have located one near the library archives, but the others..."
      , "The walls bleed. The shadows move when unwatched. If you read this, flee while your mind remains yours...\""
      ]) True False)
  , (Statue, ObjectData "statue" (\gs -> unlines
      [ "A grotesque effigy carved from obsidian-black stone. The deity it depicts defies comprehension -"
      , "part human, yet wrong in ways your mind struggles to process. Multiple limbs emerge at impossible"
      , "angles, and its face bears too many eyes, arranged in a pattern that makes your vision swim."
      , "You've never encountered anything resembling this entity in any religious text or traveler's tale."
      , "The base of the statue contains three distinct recesses, shaped to hold three specific objects:"
      , "- a cube"
      , "- a pyramid"
      , "- a sphere"
      , "Currently placed artifacts: " ++ show (Set.toList (placedArtifacts gs)) ++ "."
      , "Looking closer, you notice scrape marks on the stone floor around the statue's base,"
      , "suggesting it has been moved recently. The pattern of the marks hints that the statue"
      , "might conceal something hidden beneath it."
      , if statueMoved gs then "The statue has indeed moved, revealing a passage downwards." else ""
      ]) False False)
  , (Pit, ObjectData "pit" (\_ -> unlines
      [ "The pit descends into impenetrable darkness. You cannot see its bottom, even when"
      , "you carefully peer over the edge. The fetid smell makes you recoil involuntarily."
      , "When you drop a small stone in, you hear it bouncing against the walls for several"
      , "seconds before a distant splash echoes up."
      ]) False False)
  , (EscapeAttempt, ObjectData "escapeattempt" (\_ -> unlines
      [ "You desperately claw at the walls, trying to climb out of this foul pit."
      , "The stones are coated in centuries of filth and slime, offering no purchase."
      , "Each attempt leaves you sliding back down into the fetid muck below."
      , "After several exhausting tries, the terrible realization sinks in - there is no way out."
      , "This place will be your tomb, just as it was for others before you."
      -- This examine causes sanity 0 in Prolog.
      ]) False False) 
  , (OpiumPowder, ObjectData "opiumpowder" (\_ -> unlines
      [ "A small jar containing a crude brownish powder derived from poppy seeds."
      , "Such substances were used by medieval healers to dull pain, though none called it by this name."
      , "The jar bears symbols suggesting it will calm the mind and ease suffering."
      ]) True True)
  , (BlueVial, ObjectData "bluevial" (\_ -> unlines
      [ "A vial of luminous blue liquid that seems to swirl of its own accord."
      , "It smells faintly of sea salt and something unidentifiable."
      ]) True True)
  , (GreenVial, ObjectData "greenvial" (\_ -> unlines
      [ "A viscous green substance that occasionally bubbles on its own."
      , "It emits fumes that make your eyes water. The glass of the vial appears to be slowly"
      , "corroding from the inside."
      ]) True True)
  , (PurpleVial, ObjectData "purplevial" (\_ -> unlines
      [ "A thick purple liquid with tiny specks of light floating within it."
      , "The vial is pleasantly warm to the touch."
      ]) True True)
  , (RedVial, ObjectData "redvial" (\_ -> unlines
      [ "A crimson fluid that looks disturbingly like blood, yet moves more slowly."
      , "The vial pulses gently, almost like a heartbeat."
      ]) True True)
  , (KitchenKnife, ObjectData "kitchenknife" (\_ -> unlines
      [ "A large knife with a rusty blade and rotting wooden handle. Despite its condition,"
      , "it still holds a threatening edge. Flakes of what might be dried blood cling to the blade."
      ]) True True)
  , (RottenMeat, ObjectData "rottenmeat" (\_ -> unlines
      [ "A chunk of unidentifiable meat, green with mold and crawling with maggots."
      , "The stench emanating from it makes your stomach turn. What kind of creature"
      , "this once was is impossible to determine."
      ]) True True)
  , (Fish, ObjectData "fish" (\_ -> unlines
      [ "A dried fish, preserved with salt. While certainly not fresh,"
      , "it appears to be edible and free from obvious spoilage."
      , "The flesh is firm and the smell, while strong, is not putrid."
      , "It could provide sustenance if you're desperate enough."
      ]) True True)
  , (Bread, ObjectData "bread" (\_ -> unlines
      [ "A surprisingly intact loaf of dark bread. Though stale and hard as stone,"
      , "it appears free of mold and might still be edible. It would at least"
      , "quiet your growing hunger."
      ]) True True)
  , (WineBottle, ObjectData "winebottle" (\_ -> unlines
      [ "An ancient bottle of wine, its label long rotted away. The dark liquid inside"
      , "sloshes thickly when you move it. The cork is partially decomposed but still"
      , "seals the contents. It might provide a temporary respite from the horrors around you."
      ]) True True)
  , (Priest, ObjectData "priest" (\_ -> unlines
      [ "The priest's skeletal form is draped in rotting ceremonial robes adorned with"
      , "strange symbols and dried bloodstains. His skin is pale and stretched too tightly"
      , "across his skull. His eyes never blink as they watch your every movement."
      , "There is something unnatural about his movements - jerky, yet fluid, as if his body"
      , "is not entirely under his own control. His lips constantly move in silent prayer or curse."
      ]) False False) 
  , (DeadPriest, ObjectData "deadpriest" (\_ -> unlines
      [ "The priest's corpse lies in a pool of black fluid that seems to move slightly of its own accord."
      , "His face is frozen in that terrible smile, eyes still open and staring at nothing."
      , "The wound in his chest pulses faintly, as if something inside is still alive."
      , "Despite being clearly dead, his fingers occasionally twitch, and you swear"
      , "you can still hear faint whispering coming from his motionless lips."
      ]) False False) 
  , (PyramidArtifact, ObjectData "pyramidartifact" (\_ -> unlines
      [ "A small pyramid carved from obsidian-black stone. Its surface is covered with minute"
      , "carvings that seem to shift when not viewed directly. It feels unnaturally cold to the touch,"
      , "and somehow heavier than its size would suggest. This appears to match one of the recesses"
      , "in the statue base you saw in the main hall."
      ]) True True)
  , (SphereArtifact, ObjectData "sphereartifact" (\_ -> unlines
      [ "A perfect sphere of obsidian-black stone. No tool marks are visible on its flawless surface."
      , "It feels unnaturally cold to the touch, and somehow heavier than its size would suggest."
      , "When you look closely, you see pinpricks of light deep within, like distant stars."
      , "This appears to match one of the recesses in the statue base you saw in the main hall."
      ]) True True)
  , (CubeArtifact, ObjectData "cubeartifact" (\_ -> unlines
      [ "A perfect cube of obsidian-black stone. When you touch it, visions of unspeakable"
      , "horror flood your mind - people transformed into twisted amalgamations of flesh and metal,"
      , "rituals performed on living subjects whose screams echo through eternity."
      , "It feels unnaturally cold to the touch, and somehow heavier than its size would suggest."
      , "This appears to match one of the recesses in the statue base you saw in the main hall."
      
      ]) True True)
  , (AlchemyTechniques, ObjectData "alchemytechniques" (\_ -> unlines
      [ "A weathered tome with a cracked leather spine. The pages detail ancient alchemical processes."
      , "You find several relevant sections:"
      , "- Opium powder: \"A salve for the troubled mind. Calms the nerves and dispels phantasms.\""
      , "- Blue vial: \"Essence of clarity. Restores the mind to perfect equilibrium.\""
      , "- Green vial: \"The universal solvent. Dissolves flesh, bone, and even the hardest armor with ease.\""
      , "- Purple vial: \"Opens the third eye to visions beyond mortal comprehension.\""
      , "- Red vial: \"The curse of living decay. Flesh rots while consciousness remains.\""
      ]) True False) 
  , (Merthveer, ObjectData "merthveer" (\_ -> unlines
      [ "A book bound in what appears to be human skin. The title \"Merthveer\" is branded onto the cover."
      , "The pages contain disturbing illustrations of rituals performed before a many-limbed deity"
      , "with multiple eyes arranged in an unnatural pattern."
      , "The text describes an ancient being that exists beyond human comprehension, whose mere"
      , "presence corrupts the mind and warps reality."
      , "Several pages are stained with substances you prefer not to identify."
      , "Reading further makes your vision swim and your sanity waver."
     
      ]) True False)
  , (SilentFigure, ObjectData "silentfigure" (\_ -> unlines
      [ "You approach the bars to get a better look at the silent prisoner."
      , "His skin is unnaturally pale, stretched too tightly over sharp bones."
      , "His eyes never blink as they follow your every movement."
      , "His lips are sewn shut with crude black thread, yet you swear"
      , "you can hear faint whispers emanating from him."
      , "As you turn to leave, you notice his shadow doesn't match his body's position."
      -- Prolog: decrease_sanity(5) on examine.
      ]) False False) -- NPC
  , (HumanRemains, ObjectData "humanremains" (\_ -> unlines
      [ "The corpse has been here for some time, though the dry air has preserved it somewhat."
      , "The body is contorted in an unnatural position, suggesting a violent end."
      , "The skull has been opened and appears hollow, as if its contents were removed."
      , "There are strange symbols carved into the leathery skin across the chest."
      , "Several fingers and toes appear to have been systematically removed, the wounds cauterized."
      , "A small object glints faintly in the ribcage, partially hidden by the withered organs."
      -- Prolog mentions glinting object, could be sphere artifact hint or another item.
      ]) False False)
  , (HorrifiedMan, ObjectData "horrifiedman" (\_ -> unlines
      [ "The man cowers in the far corner of his cell as you approach."
      , "His once-fine clothes hang in tatters, and his body bears countless small, precise cuts."
      , "His eyes are wide with terror as he whispers urgently: \"They take us one by one..."
      , "down that passage... we hear the screams... then nothing. The guard...\""
      , "He clutches something small and metallic in his trembling hands, hiding it"
      , "whenever he hears a sound from the passage. \"I found a beautiful sphere in this place, black as night."
      , "That thing... the guard... took it from me. Said it belonged to his master...\""
      -- Hint for Sphere Artifact.
      ]) False False) -- NPC
  , (DyingPrisoner, ObjectData "dyingprisoner" (\_ -> unlines
      [ "You approach the hanging man, whose breaths come in shallow, wet gasps."
      , "His body has been systematically mutilated - skin peeled away in precise patterns"
      , "that form symbols similar to those you've seen elsewhere in the catacombs."
      , "His jaw has been removed entirely, and his tongue cut out, ensuring his silence."
      , "One eye has been surgically extracted."
      , "A small key on a chain hangs from a nail driven through his ankle."
      -- Prolog: Key not implemented as takeable here, would require adding Key item.
      ]) False False) -- NPC
  , (CorrodedDagger, ObjectData "corrodeddagger" (\_ -> unlines
      [ "A dagger with a blade corroded by time and strange fluids."
      , "Despite its deteriorated condition, it maintains a wicked edge."
      , "Strange symbols are etched into the blade, visible despite the corrosion."
      , "It feels unnaturally cold to the touch, and somehow heavier than it should be."
      ]) True True)
  , (SleepingGuard, ObjectData "sleepingguard" (\_ -> unlines
      [ "As you approach cautiously, you realize with horror that this is not armor at all."
      , "The metal plates are fused directly to the creature's flesh - grown from it, part of it."
      , "Its proportions are wrong - limbs too long, joints bending in impossible directions."
      , "Each breath it takes produces a sound like metal grinding against wet stone."
      , "Its helm-like head has no visor, only a solid plate with crude eyeholes that leak a yellow fluid."
      , "One massive gauntleted hand clutches an iron mace even in sleep."
      , "You pray it doesn't wake."
      ]) False False) -- NPC
  , (DeadGuard, ObjectData "deadguard" (\_ -> unlines
      [ "The guard quarters are now dominated by the fallen form of the mutated guard."
      , "Its body continues to dissolve where the acid made contact, revealing a disturbing"
      , "amalgamation of metal, flesh, and something else entirely beneath the armor."
      , "Black ichor pools around the corpse, bubbling occasionally as if still alive."
      , "The air feels lighter somehow, as if a malevolent presence has been removed."
      ]) False False) -- NPC State
  , (Whisky, ObjectData "whisky" (\_ -> unlines
      [ "A medium size bottle of amber liquid. The label has long since rotted away."
      , "The liquid inside still sloshes invitingly, preserved by the high alcohol content."
      , "Such spirits might numb your mind to the horrors of this place, at least temporarily."
      ]) True True)
  , (Vodka, ObjectData "vodka" (\_ -> unlines
      [ "A clear liquid in a dusty bottle. Despite its age, it appears unspoiled."
      , "The strong smell of alcohol burns your nostrils when you remove the cork."
      , "Drinking this might provide temporary relief for your fraying nerves."
      ]) True True)
  ]



findObjectByName :: String -> GameState -> Maybe Object
findObjectByName name gs =
  let lowerName = map toLower name
      matches = Map.filter (\objData -> map toLower (objectName objData) == lowerName) (objectData gs)
  in listToMaybe (Map.keys matches)

doLook :: GameAction ()
doLook = do
  gs <- get
  let loc = currentLocation gs
  let descFunc = (locationDescriptions gs) Map.! loc
  addMessage "----------------------------------------------------------------------"
  addMessage $ descFunc gs
  let itemsHere = fromMaybe Set.empty (Map.lookup loc (itemsInLocations gs))
  unless (Set.null itemsHere) $ do
    addMessage "You see here:"
    forM_ (Set.toList itemsHere) $ \objId -> do
      let objDataMap = objectData gs
      case Map.lookup objId objDataMap of
        Just od -> addMessage $ "- " ++ objectName od
        Nothing -> return ()
  addMessage "----------------------------------------------------------------------"


doGo :: Direction -> GameAction ()
doGo dir = do
  gs <- get
  let currentLoc = currentLocation gs
  case Map.lookup currentLoc (paths gs) >>= Map.lookup dir of
    Just newLoc -> do
      if dir == Beneath && currentLoc == MainHall && not (statueMoved gs) then
        addMessage "You can't go that way. The statue blocks the passage downwards."
      else do
        modify $ \s -> s { currentLocation = newLoc }
        if newLoc == InnerSanctum then do
            let finalDescFunc = (locationDescriptions gs) Map.! InnerSanctum
            addMessage (finalDescFunc gs)
            modify $ \s -> s { gameShouldEnd = True }
        else
            doLook 
    Nothing -> addMessage "You can't go that way."

consumeItemFromInventory :: Object -> GameAction ()
consumeItemFromInventory objId = modify $ \gs ->
  let inv = playerInventory gs
      newCount = (inv Map.! objId) - 1 
  in gs { playerInventory = if newCount <= 0 then Map.delete objId inv else Map.insert objId newCount inv }

doTake :: String -> GameAction ()
doTake objectNameInput = do
  gs <- get
  if decayNoPickup gs then
    addMessage "Your hands, ravaged by the spreading rot, are too weak to grasp anything.\nThe flesh crumbles and splits at the slightest pressure."
  else
    case findObjectByName objectNameInput gs of
      Nothing -> addMessage $ "There is no " ++ objectNameInput ++ " here to take."
      Just objId -> do
        let currentLoc = currentLocation gs
        let itemsHere = fromMaybe Set.empty (Map.lookup currentLoc (itemsInLocations gs))
        let objDetails = (objectData gs) Map.! objId

        
        if objId == PyramidArtifact && currentLoc == Chapel then
          if priestState gs == PriestAlive then do
            addMessage "As you reach for the pyramid, the priest shrieks with rage and lunges at you."
            inventory <- gets playerInventory
            if Map.member KitchenKnife inventory then do
              addMessage "In a moment of desperate reflexes, you draw your knife and defend yourself."
              let newInventory = Map.adjust (\n -> n-1) KitchenKnife inventory
              let finalInventory = if (newInventory Map.! KitchenKnife) <= 0 then Map.delete KitchenKnife newInventory else newInventory
              modify $ \s -> s { playerInventory = Map.insertWith (+) PyramidArtifact 1 finalInventory
                               , priestState = PriestDefeated
                               , itemsInLocations = Map.adjust (Set.delete PyramidArtifact . Set.insert DeadPriest . Set.delete Priest) Chapel (itemsInLocations s)
                               }
              addMessage "With shaking hands, you grab the stone pyramid from the altar."
            else do 
              addMessage "Defenseless, you can only shield yourself as the priest grasps your head."
              addMessage "He speaks words that burn into your mind. Your vision fractures,\nand reality warps around you. You are now hallucinating."
              modify $ \s -> s { playerSanity = max 0 (playerSanity s - 20), isHallucinating = True }
          else if priestState gs == PriestDefeated && Set.member objId itemsHere then 
            if isTakeable objDetails then do
                let newItemsInLoc = Map.adjust (Set.delete objId) currentLoc (itemsInLocations gs)
                let newInventory = Map.insertWith (+) objId 1 (playerInventory gs)
                modify $ \s -> s { itemsInLocations = newItemsInLoc, playerInventory = newInventory }
                addMessage $ "You pick up the " ++ objectName objDetails ++ "."
            else
                addMessage "You cannot take that with you." 
          else 
            addMessage $ "There is no " ++ objectNameInput ++ " here to take."

      
        else if Set.member objId itemsHere then
          if isTakeable objDetails then
            case objId of 
                Statue | currentLoc == MainHall -> addMessage "You cannot take the statue with you."
                Pit | currentLoc == DarkRoom1 -> addMessage "You cannot take the pit with you."
                Priest | currentLoc == Chapel -> addMessage "You cannot take the priest with you."
                DeadPriest | currentLoc == Chapel -> addMessage "You cannot take the dead priest with you."
                SleepingGuard | currentLoc == GuardQuarters -> addMessage "You cannot take the sleeping guard with you."
                DeadGuard | currentLoc == GuardQuarters -> addMessage "You cannot take the dead guard with you."
                SilentFigure | currentLoc == Jail -> addMessage "You cannot take the silent figure with you."
                HumanRemains | currentLoc == Jail -> addMessage "You cannot take the human remains with you."
                HorrifiedMan | currentLoc == Jail -> addMessage "You cannot take the horrified man with you."
                DyingPrisoner | currentLoc == TortureChamber -> addMessage "You cannot take the dying prisoner with you."
                EscapeAttempt | currentLoc == PitBottom -> addMessage "You cannot take your escape attempt with you."
                _ -> do 
                    let newItemsInLoc = Map.adjust (Set.delete objId) currentLoc (itemsInLocations gs)
                    let newInventory = Map.insertWith (+) objId 1 (playerInventory gs)
                    modify $ \s -> s { itemsInLocations = newItemsInLoc, playerInventory = newInventory }
                    addMessage $ "You pick up the " ++ objectName objDetails ++ "."
          else
            addMessage "You cannot take that with you."
        else
          addMessage $ "There is no " ++ objectNameInput ++ " here to take."


doExamine :: String -> GameAction ()
doExamine targetNameInput = do
  gs <- get
  case findObjectByName targetNameInput gs of
    Nothing -> addMessage $ "You don't see any " ++ targetNameInput ++ " here to examine."
    Just objId -> do
      let objDetails = (objectData gs) Map.! objId
      let currentLoc = currentLocation gs
      let itemsInLocSet = fromMaybe Set.empty (Map.lookup currentLoc (itemsInLocations gs))
      let inInventory = Map.member objId (playerInventory gs)

      
      let isPresent = case objId of
           
            Statue          -> currentLoc == MainHall
            Pit             -> currentLoc == DarkRoom1
            EscapeAttempt   -> currentLoc == PitBottom
            Priest          -> currentLoc == Chapel && priestState gs == PriestAlive
            DeadPriest      -> currentLoc == Chapel && priestState gs == PriestDefeated
            SilentFigure    -> currentLoc == Jail
            HumanRemains    -> currentLoc == Jail
            HorrifiedMan    -> currentLoc == Jail
            DyingPrisoner   -> currentLoc == TortureChamber
            SleepingGuard   -> currentLoc == GuardQuarters && guardState gs == GuardAsleep
            DeadGuard       -> currentLoc == GuardQuarters && guardState gs == GuardDefeated
            
            _               -> Set.member objId itemsInLocSet || inInventory

      if isPresent then do
        addMessage (examineText objDetails gs)
       
        case objId of
          CubeArtifact    -> modify $ \s -> s { playerSanity = max 0 (playerSanity s - 5) }
          Merthveer       -> modify $ \s -> s { playerSanity = max 0 (playerSanity s - 10) }
          SilentFigure    -> modify $ \s -> s { playerSanity = max 0 (playerSanity s - 5) }
          EscapeAttempt   -> modify $ \s -> s { playerSanity = 0 } 
          _               -> return ()
      else
        addMessage $ "You don't see any " ++ targetNameInput ++ " here to examine."


doUse :: String -> GameAction ()
doUse objectNameInput = do
  gs <- get
  case findObjectByName objectNameInput gs of
    Nothing -> addMessage $ "You don't have any " ++ objectNameInput ++ " to use."
    Just objId -> do
      let inv = playerInventory gs
      let objDetails = (objectData gs) Map.! objId
      if Map.member objId inv then
        if isUsable objDetails then
          useSpecificItem objId 
        else
          addMessage "This item can only be examined, not used." 
      else
        addMessage $ "You don't have any " ++ objectNameInput ++ " to use."


useSpecificItem :: Object -> GameAction ()
useSpecificItem Torch = do
  gs <- get
  if isTorchLit gs then
    addMessage "You already have a lit torch."
  else do
    consumeItemFromInventory Torch 
    modify $ \s -> s { isTorchLit = True, torchTurnsRemaining = 20 }
    addMessage "You light the torch. The darkness recedes as flickering flames cast long shadows on the ancient walls."

useSpecificItem KitchenKnife = do
  gs <- get
  if currentLocation gs == Chapel && priestState gs == PriestAlive then do
    addMessage "You grip the kitchen knife tightly and approach the priest. His eyes widen in realization\nbut the smile never leaves his face. You plunge the blade into his chest. Black ichor,\nnot blood, spills from the wound as he collapses to the floor."
    addMessage "His body crumples, but the laughter continues for several seconds after."
    consumeItemFromInventory KitchenKnife 
    modify $ \s -> s { priestState = PriestDefeated
                     , itemsInLocations = Map.adjust (Set.delete Priest . Set.insert DeadPriest) Chapel (itemsInLocations s)
                     }
  else
    addMessage "You test the edge of the knife against your thumb. Still sharp."

useSpecificItem RottenMeat = do
  consumeItemFromInventory RottenMeat
  modify $ \s -> s { playerHunger = max 0 (playerHunger s - 15)
                   , playerSanity = max 0 (playerSanity s - 10) }
  addMessage "You force yourself to eat the putrid flesh. The taste is foul beyond description,\nand the texture makes you gag, but it does quiet the pangs of hunger somewhat.\nYour mind recoils at what you've just consumed."

useSpecificItem Fish = do
  consumeItemFromInventory Fish
  modify $ \s -> s { playerHunger = max 0 (playerHunger s - 20) }
  addMessage "You tear into the dried fish. The taste is intensely salty and\nthe texture leathery, but it satisfies your hunger somewhat."

useSpecificItem Bread = do
  consumeItemFromInventory Bread
  modify $ \s -> s { playerHunger = max 0 (playerHunger s - 25) }
  addMessage "You force down the stale bread. It's nearly impossible to chew,\nbut your gnawing hunger subsides somewhat."

useSpecificItem WineBottle = do
  consumeItemFromInventory WineBottle
  modify $ \s -> s { playerSanity = min 100 (playerSanity s + 15)
                   , playerHunger = min 100 (playerHunger s + 5) } 
  addMessage "You pull the crumbling cork and drink deeply from the bottle.\nThe wine has turned to vinegar, but the alcohol still burns pleasantly,\ntemporarily dulling your awareness of the horrors surrounding you."

useSpecificItem OpiumPowder = do
  consumeItemFromInventory OpiumPowder
  modify $ \s -> s { playerSanity = min 100 (playerSanity s + 20)
                   , isHallucinating = False } 
  addMessage "You consume some of the opium powder. A numbing calm washes over you,\ndulling the terrors of the catacombs. Your mind feels more stable."

useSpecificItem BlueVial = do
  consumeItemFromInventory BlueVial
  modify $ \s -> s { playerSanity = 100 }
  addMessage "You drink the blue liquid. A wave of clarity washes over you as the catacombs\nseem less threatening. Your mind feels restored and focused."

useSpecificItem GreenVial = do
  gs <- get
  if currentLocation gs == GuardQuarters && guardState gs == GuardAsleep then do
    consumeItemFromInventory GreenVial
    addMessage "You carefully unstop the vial and throw the viscous green liquid at the sleeping guard."
    addMessage "The acid splashes across its metal-fused flesh, instantly hissing and bubbling."
    addMessage "The creature awakens with a horrific shriek as the corrosive substance eats through\nits armored plates, revealing pulsing, inhuman flesh beneath."
    
    inv <- gets playerInventory
    if Map.member CorrodedDagger inv then do
        addMessage "Acting on instinct, you lunge forward with your dagger, driving it deep into\nthe exposed flesh of its throat. Black ichor sprays from the wound as the creature\nthrashes wildly before collapsing in a heap. Its body continues to twitch\nas the acid dissolves more of its form."
        addMessage "As its body deteriorates, a perfect black sphere (sphereartifact) rolls from its remains."
        modify $ \s -> s { guardState = GuardDefeated
                         , itemsInLocations = Map.adjust (Set.insert SphereArtifact . Set.delete SleepingGuard . Set.insert DeadGuard) GuardQuarters (itemsInLocations s)
                         }
    else do
        addMessage "The creature rises in agony and rage, its faceplate splitting open to reveal\nrows of needle-like teeth. Though wounded and burning, it hefts its massive\nhalberd with terrifying speed. The last thing you see is the blade arcing\ntoward your neck before darkness claims you."
        modify $ \s -> s { gameShouldEnd = True, playerSanity = 0 } 
  else do 
    consumeItemFromInventory GreenVial
    addMessage "You drink the viscous green liquid, which immediately burns your throat."
    addMessage "To your horror, you feel it eating through your insides like acid."
    addMessage "Your body convulses as the corrosive substance dissolves you from within."
    addMessage "Your last sensation is that of your organs liquefying as darkness claims you."
    modify $ \s -> s { gameShouldEnd = True, playerSanity = 0 } 

useSpecificItem PurpleVial = do
  consumeItemFromInventory PurpleVial
  modify $ \s -> s { isHallucinating = True }
  addMessage "You drink the purple liquid. The world around you fractures into impossible geometries.\nShadows dance at the edge of your vision, and whispers fill your ears.\nReality itself seems unstable. You fear this effect may be long-lasting."

useSpecificItem RedVial = do
  consumeItemFromInventory RedVial
  modify $ \s -> s { hasDecayingHands = True, decayTurnsLeft = 20, decayNoPickup = False }
  addMessage "You drink the crimson liquid. At first, nothing happens.\nThen your hands begin to tingle unpleasantly. Looking down, you watch in horror\nas your flesh begins to rot before your eyes. The decay is spreading slowly.\nYou must find a way to stop it before it consumes you entirely."

useSpecificItem Whisky = do
  consumeItemFromInventory Whisky
  modify $ \s -> s { playerSanity = min 100 (playerSanity s + 25)
                   , playerHunger = min 100 (playerHunger s + 10) }
  addMessage "You take a long swig of the aged whisky. It burns going down,\nbut spreads a comforting warmth through your body.\nThe horrors around you seem slightly more distant now."

useSpecificItem Vodka = do
  consumeItemFromInventory Vodka
  modify $ \s -> s { playerSanity = min 100 (playerSanity s + 25)
                   , playerHunger = min 100 (playerHunger s + 10) }
  addMessage "You drink the strong, clear spirit. It sears your throat\nbut brings a numbing calm to your mind.\nYour fear recedes temporarily as the alcohol dulls your senses."

useSpecificItem CorrodedDagger = do
  gs <- get
  if currentLocation gs == GuardQuarters && guardState gs == GuardAsleep then do
    addMessage "You approach the sleeping armored figure, dagger raised to strike a killing blow."
    addMessage "The blade connects with the metallic plating but merely scrapes across it with a hideous screech."
    addMessage "The creature's eyes snap open - glowing yellow orbs behind the metal faceplate."
    addMessage "It rises with impossible speed, a massive gauntlet seizing your throat."
    addMessage "Your dagger clatters uselessly to the floor as your feet leave the ground."
    addMessage "The last thing you see is its faceplate splitting open vertically, revealing rows\nof needle-like teeth before darkness claims you..."
    addMessage "You awaken briefly in the torture chamber, strapped to a table as the creature\nmethodically begins its work. The pain is beyond comprehension."
    addMessage "Death, when it finally comes, is a mercy."
    modify $ \s -> s { gameShouldEnd = True, playerSanity = 0 } 
  else
    addMessage "You test the edge of the dagger. Despite its corrosion, it remains surprisingly sharp."

useSpecificItem PyramidArtifact = tryPlaceArtifact PyramidArtifact "pyramid"
useSpecificItem SphereArtifact  = tryPlaceArtifact SphereArtifact  "sphere"
useSpecificItem CubeArtifact    = tryPlaceArtifact CubeArtifact    "cube"

useSpecificItem item = do 
    objDataMap <- gets objectData
    case Map.lookup item objDataMap of
        Just od -> addMessage $ "You use the " ++ objectName od ++ ", but nothing happens."
        Nothing -> addMessage "Error: Unknown item in useSpecificItem."


tryPlaceArtifact :: Object -> String -> GameAction ()
tryPlaceArtifact artifactId shapeName = do
  gs <- get
  if currentLocation gs /= MainHall then
    addMessage $ "This artifact seems to match one of the recesses in the statue in the main hall."
  else do
    consumeItemFromInventory artifactId
    let newPlacedArtifacts = Set.insert artifactId (placedArtifacts gs)
    modify $ \s -> s { placedArtifacts = newPlacedArtifacts }
    addMessage $ "You carefully place the " ++ shapeName ++ " artifact into the matching recess in the statue base."

    
    case artifactId of
        PyramidArtifact -> addMessage "It fits perfectly, and you hear a faint click as it locks into place.\nThe air grows colder, and whispers fill your mind momentarily. The statue's eyes briefly glow red."
        SphereArtifact  -> addMessage "The sphere settles with an ominous hum, and you hear a faint click as it locks into place.\nThe ground beneath you trembles slightly. You notice drops of a black, viscous liquid\nbeginning to seep from the statue's mouth."
        CubeArtifact    -> do
            addMessage "As the cube slots into place, the entire chamber resonates with a deep, subsonic tone.\nShadows in the room appear to elongate, stretching toward the statue. You feel an\nuncomfortable pressure in your head, as if something is probing your thoughts."
            modify $ \s -> s { playerSanity = max 0 (playerSanity s - 10) }
        _ -> return ()

    let requiredArtifacts = Set.fromList [PyramidArtifact, CubeArtifact, SphereArtifact]
    when (Set.isSubsetOf requiredArtifacts newPlacedArtifacts && not (statueMoved gs)) $ do
      addMessage "As the final artifact is placed, the statue begins to tremble violently."
      addMessage "The eyes of the deity blaze with unholy light, and the mouth opens wider, disgorging\na torrent of black ichor that pools around the base."
      addMessage "With a grinding sound of stone against stone, the floor beneath the statue\nsplits open, revealing an impossibly narrow, spiral staircase carved into the bedrock."
      addMessage "The steps are worn and treacherously steep, descending at an unnatural angle into absolute darkness."
      addMessage "A cold draft rises from below, carrying whispers in a language you cannot comprehend."
      addMessage "The descent appears hazardous, but now accessible (go beneath)."
      modify $ \s -> s { statueMoved = True
                       , paths = Map.adjust (Map.insert Beneath InnerSanctum) MainHall (paths s)
                       }

doInventory :: GameAction ()
doInventory = do
  gs <- get
  let inv = playerInventory gs
  if Map.null inv then
    addMessage "Your inventory is empty."
  else do
    addMessage "You are carrying:"
    forM_ (Map.toList inv) $ \(objId, count) ->
      let objDetails = (objectData gs) Map.! objId
      in addMessage $ "- " ++ objectName objDetails ++ " (" ++ show count ++ ")"

doStatus :: GameAction ()
doStatus = do
  gs <- get
  addMessage "--- STATUS ---"
  addMessage $ "Sanity: " ++ show (playerSanity gs) ++ "%"
  addMessage $ "Hunger: " ++ show (playerHunger gs) ++ "%"
  when (isTorchLit gs) $
    addMessage $ "Torch: Lit (approx. " ++ show (torchTurnsRemaining gs) ++ " turns remaining)."
  when (isHallucinating gs) $
    addMessage "You are hallucinating."
  when (hasDecayingHands gs) $
    addMessage $ "Hands: Decaying (approx. " ++ show (decayTurnsLeft gs) ++ " turns remaining). " ++
                 if decayNoPickup gs then "You cannot pick things up!" else ""
  addMessage "--------------"

updateGameStatus :: GameAction ()
updateGameStatus = do
  gs <- get

 
  let sanityLossBase = if isTorchLit gs then 1 else 2
  
  let sanityLossHallu = if isHallucinating gs then 4 else 0 -- This might be too much if base is also applied. Prolog logic was: if hallucinating, then use 4, else use 1 (or 2 if no torch).
  
  let actualSanityLoss = if isHallucinating gs then sanityLossHallu else sanityLossBase
  let newSanity = max 0 (playerSanity gs - actualSanityLoss)
  modify $ \s -> s { playerSanity = newSanity }

 
  let newHunger = min 100 (playerHunger gs + 3)
  modify $ \s -> s { playerHunger = newHunger }
  when (newHunger >= 80 && playerHunger gs < 80) $ addMessage "Your stomach twists painfully with hunger."
  when (newHunger >= 50 && newHunger < 80 && playerHunger gs < 50) $ addMessage "You feel hungry."

  
  when (isTorchLit gs) $ do
    let turnsLeft = torchTurnsRemaining gs - 1
    if turnsLeft <= 0 then do
      modify $ \s -> s { isTorchLit = False, torchTurnsRemaining = 0 }
      addMessage "Your torch flickers and dies, plunging you into darkness."
    else do
      modify $ \s -> s { torchTurnsRemaining = turnsLeft }
      when (turnsLeft `elem` [1, 5, 10, 15]) $ 
        addMessage $ "Your torch burns dimmer. " ++ show turnsLeft ++ " turns remaining."


  when (hasDecayingHands gs) $ do
    let decayTurns = decayTurnsLeft gs - 1
    modify $ \s -> s { decayTurnsLeft = decayTurns }
    if decayTurns <= 0 then do
      addMessage "The decay has consumed your arms entirely. Flesh sloughs from bone as\nyour limbs are reduced to twisted, useless appendages. The rot continues\nto spread up throughout your body. Death comes slowly."
      modify $ \s -> s { playerSanity = 0 } 
    else do
      when (decayTurns < 10 && not (decayNoPickup gs)) $ do
        modify $ \s -> s { decayNoPickup = True }
        addMessage "Your hands are severely decayed. The rot is spreading up your arms."
      when (decayTurns < 5 && decayTurns > 0) $
        addMessage "The decay is nearly complete. Your limbs are barely recognizable."

      addMessage $ "The decay continues to spread. You estimate approximately " ++ show decayTurns ++ " turns remain before it consumes you entirely."


  
  let currentSanityVal = newSanity 
  let alreadyHallucinating = isHallucinating gs
  
  let triggerHallucination = (currentSanityVal <= 15 && not alreadyHallucinating && True) || 
                             (currentSanityVal <= 30 && currentSanityVal > 15 && not alreadyHallucinating) 

  shouldStartHallucinating <- if triggerHallucination then
                                if currentSanityVal <= 15 then return True
                                else do 
                                    rollForHallu :: Int <- liftIO $ randomRIO (1, if currentSanityVal <=30 then 3 else 5) 
                                    return (rollForHallu == 1)
                              else return False

  when shouldStartHallucinating $ do
      modify $ \s -> s { isHallucinating = True }
     
  gsAfterUpdate <- get
  when (isHallucinating gsAfterUpdate && playerSanity gsAfterUpdate > 0) $ do
      randEffect :: Int <- liftIO $ randomRIO (1, 5)
      case randEffect of
          1 -> addMessage "You hear whispering voices coming from the walls."
          2 -> addMessage "For a moment, you see blood dripping from the ceiling, but it vanishes when you blink."
          3 -> addMessage "Something skitters across your peripheral vision, but nothing is there when you turn."
          4 -> addMessage "You feel cold, invisible hands brushing against your face."
          _ -> addMessage "The ground beneath you seems to pulse, as if breathing."

  -- Check game over conditions
  gsFinal <- get -- Get state again after all updates
  when (playerSanity gsFinal <= 0 && not (gameShouldEnd gsFinal)) $ do -- Only if not already ended by other means
    addMessage "The darkness starts to consume your mind entirely. You collapse to the floor,\nclawing at your own skin, desperate to escape the horrors in your mind.\nYour journey ends here, lost in madness beneath the earth."
    modify $ \s -> s { gameShouldEnd = True }
  when (playerHunger gsFinal >= 100 && not (gameShouldEnd gsFinal)) $ do
    addMessage "Overwhelming hunger causes you to collapse. Your body is too weak to continue."
    modify $ \s -> s { gameShouldEnd = True }


parseCommand :: String -> GameAction ()
parseCommand cmdStr =
  let parts = words (map toLower cmdStr) -- Normalize to lowercase
  in case parts of
    ["go", dirStr] -> case dirStr of
                         "n" -> doGo N
                         "s" -> doGo S
                         "e" -> doGo E
                         "w" -> doGo W
                         "beneath" -> doGo Beneath
                         _ -> addMessage "Unknown direction. Try n, s, e, w, or beneath."
    ["look"] -> doLook
    ["l"] -> doLook 
    ["take", objName] -> doTake objName
    ("take":objNameParts) -> doTake (unwords objNameParts)
    ["examine", targetName] -> doExamine targetName
    ("examine":targetNameParts) -> doExamine (unwords targetNameParts)
    ["x", targetName] -> doExamine targetName 
    ("x":targetNameParts) -> doExamine (unwords targetNameParts)
    ["use", objName] -> doUse objName
    ("use":objNameParts) -> doUse (unwords objNameParts)
    ["inventory"] -> doInventory
    ["i"] -> doInventory 
    ["status"] -> doStatus
    ["help"] -> liftIO printInstructions
    ["quit"] -> modify $ \s -> s { gameShouldEnd = True }
    [] -> return ()
    _ -> addMessage $ "I don't understand the command: '" ++ cmdStr ++ "'."

-- ### MAIN GAME LOOP 

gameLoop :: GameAction ()
gameLoop = do
  shouldEnd <- gets gameShouldEnd
  unless shouldEnd $ do
    liftIO $ putStr "> "
    liftIO $ hFlush stdout
    cmd <- liftIO getLine
    parseCommand cmd
    -- Update status after most commands, except purely informational ones or quit
    let passiveCommands = ["help", "status", "inventory", "i", "quit", "look", "l"] 
                        
    let firstWord = if null (words cmd) then "" else head (words (map toLower cmd))
    unless (firstWord `elem` passiveCommands || null cmd) $
        updateGameStatus
    flushMessages
    gameLoop

main :: IO ()
main = do
  printIntroduction
  putStrLn "" 
  printInstructions
  putStrLn ""
  evalStateT (addMessage "You are at the dungeon entrance. What do you do?" >> doLook >> flushMessages >> gameLoop) initialGameState
  putStrLn "Thank you for playing!"