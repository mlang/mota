module Sounds where

import Affjax (URL)
import Affjax (get) as AJAX
import Affjax.ResponseFormat ( ResponseFormatError, arrayBuffer )
import Audio.WebAudio.BaseAudioContext ( createBufferSource, createPanner
                                       , currentTime
                                       , destination, listener, newAudioContext
                                       , decodeAudioDataAsync )
import Audio.WebAudio.Types (AudioBuffer, AudioContext)
import Control.Monad.Except (throwError)
import Control.Monad.Except.Trans (ExceptT, except, withExceptT)
import Control.Parallel (parTraverse)
import Data.Either (Either)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Prelude (class Eq, class Show, map, show, ($), (<<<), (<>), (>>=))
import Types

moveSound :: GroundType -> Speed -> Int -> String
moveSound Water speed n = "Swim_" <> show speed  <> show n
moveSound ground Slow n = "Walk_" <> show ground <> show n
moveSound ground Fast n = "Run_"  <> show ground <> show n

weaponSound :: Weapon -> String
weaponSound weapon = "Fire_" <> show weapon

landSound :: GroundType -> String
landSound ground = "Land_" <> show ground

loadSound :: URL -> AudioContext -> Aff (Either ResponseFormatError AudioBuffer)
loadSound url ctx =
  AJAX.get arrayBuffer url >>= traverse (decodeAudioDataAsync ctx) <<< (_.body)

loadSounds :: AudioContext -> Array String
           -> Aff (Either ResponseFormatError (Map String AudioBuffer))
loadSounds ctx = map (map Map.fromFoldable <<< sequence) <<< parTraverse f where
  f s = (map <<< map) (Tuple s) $ loadSound ("sounds/" <> s <> ".wav") ctx

sounds :: Array String
sounds = [
--"Ambience_Tomb", "Ambience_Underwater",
  "Angela_Ammo", "Angela_Better", "Angela_Blocked", "Angela_Close",
  "Angela_Company", "Angela_Dark", "Angela_Door", "Angela_Freezing",
  "Angela_Grip", "Angela_Light", "Angela_Statue", "Angela_Stronger",
  "Angela_Success", "Angela_Trapped", "Angela_Trouble",
  "Angela_Unavailable",
  "Angela_Warning1", "Angela_Warning2", "Angela_Warning3", "Angela_Warning4",
  "Bow_Empty",
  "Centaur_Attack1", "Centaur_Attack2",
  "Centaur_Death1", "Centaur_Death2",
  "Centaur_Move1", "Centaur_Move2",
  "Centaur_Pain1", "Centaur_Pain2",
  "Climb_Rope1", "Climb_Rope2", "Climb_Rope3", "Climb_Rope4",
  "Cyclops_Attack", "Cyclops_Death", "Cyclops_Pain",
  "Dagger_Draw", "Dagger_Sheath",
  "Dart_Attack", "Death_Blades", "Death_Crushed", "Death_Drown", "Death_Fall",
  "Death_Fire", "Death_Lava", "Death_Spikes", "Death_Standard", "Death_Trap",
  "Door_Open",
  "Fire_Bow", "Fire_Dagger", "Fire_Magnum", "Fire_Pistol", "Fire_Shotgun",
  "Fire_Sword", "Fire_Uzi", "Fire_Whip",
  "Game_Intro",
  "Get_Ammo", "Get_Arrows", "Get_Gem", "Get_Gold", "Get_Potion", "Get_Scroll",
  "Get_Torch", "Get_Weapon1", "Get_Weapon2", "Get_Weapon3", "Get_Weapon4",
  "Get_Weapon5", "Get_Weapon6", "Get_Weapon7", "Get_Weapon8",
  "Gun_Draw", "Gun_Empty", "Gun_Holster",
  "Harpy_Attack1", "Harpy_Attack2", "Harpy_Attack3", "Harpy_Attack4",
  "Harpy_Death1", "Harpy_Death2", "Harpy_Death3", "Harpy_Death4",
  "Harpy_Move1", "Harpy_Move2", "Harpy_Move3", "Harpy_Move4",
  "Harpy_Pain1", "Harpy_Pain2", "Harpy_Pain3", "Harpy_Pain4",
  "Hit_Ceiling", "Hit_Wall",
  "Kerberos_Attack", "Kerberos_Death", "Kerberos_Move", "Kerberos_Pain",
  "Land_Bridge", "Land_Dirt", "Land_Ledge", "Land_Metal", "Land_Mud",
  "Land_Rope", "Land_Sand", "Land_Staircase", "Land_Stone", "Land_Water",
  "Land_Wood",
  "Loop_Blades",
  "Loop_Chasm1", "Loop_Chasm2", "Loop_Chasm3",
  "Loop_Dagger",
  "Loop_Fire1", "Loop_Fire2",
  "Loop_Gem", "Loop_Gold",
  "Loop_Lava1", "Loop_Lava2",
  "Loop_Ledge", "Loop_Lever", "Loop_Orb", "Loop_Potion", "Loop_Rope",
  "Loop_Scroll",
  "Loop_Spikes1", "Loop_Spikes2",
  "Loop_Statue", "Loop_Sword", "Loop_Torch", "Loop_Water", "Loop_Weapon",
  "Loop_Whip",
  "Lower_Bridge",
  "Minotaur_Attack", "Minotaur_Death", "Minotaur_Move", "Minotaur_Pain",
--"Music_Level1", "Music_Level2", "Music_Title",
  "Player_Duck", "Player_Injured", "Player_Jump",
  "Player_Kick1", "Player_Kick2",
  "Player_Miss",
  "Player_Pain1", "Player_Pain2", "Player_Pain3", "Player_Pain4",
  "Player_Punch1", "Player_Punch2",
  "Player_Stand", "Player_Swing",
  "Run_Bridge1", "Run_Bridge2", "Run_Bridge3", "Run_Bridge4",
  "Run_Dirt1", "Run_Dirt2", "Run_Dirt3", "Run_Dirt4",
  "Run_Ledge1", "Run_Ledge2", "Run_Ledge3", "Run_Ledge4",
  "Run_Metal1", "Run_Metal2", "Run_Metal3", "Run_Metal4",
  "Run_Mud1", "Run_Mud2", "Run_Mud3", "Run_Mud4",
  "Run_Sand1", "Run_Sand2", "Run_Sand3", "Run_Sand4",
  "Run_Staircase1", "Run_Staircase2", "Run_Staircase3", "Run_Staircase4",
  "Run_Stone1", "Run_Stone2", "Run_Stone3", "Run_Stone4",
  "Run_Wood1", "Run_Wood2", "Run_Wood3", "Run_Wood4",
  "Skeleton_Attack1", "Skeleton_Attack2", "Skeleton_Attack3", "Skeleton_Attack4",
  "Skeleton_Death1", "Skeleton_Death2", "Skeleton_Death3", "Skeleton_Death4",
  "Skeleton_Move1", "Skeleton_Move2", "Skeleton_Move3", "Skeleton_Move4",
  "Skeleton_Pain1", "Skeleton_Pain2", "Skeleton_Pain3", "Skeleton_Pain4",
--"Start_Level1", "Start_Level2",
  "Statue_Move",
  "Swim_Fast1", "Swim_Fast2", "Swim_Fast3", "Swim_Fast4",
  "Swim_Slow1", "Swim_Slow2", "Swim_Slow3", "Swim_Slow4",
  "Sword_Draw", "Sword_Sheath",
  "Torch_Light", "Torch_Out",
  "Use_Lever", "Use_Potion", "Use_Torch",
  "Walk_Bridge1", "Walk_Bridge2", "Walk_Bridge3", "Walk_Bridge4",
  "Walk_Dirt1", "Walk_Dirt2", "Walk_Dirt3", "Walk_Dirt4",
  "Walk_Ledge1", "Walk_Ledge2", "Walk_Ledge3", "Walk_Ledge4",
  "Walk_Metal1", "Walk_Metal2", "Walk_Metal3", "Walk_Metal4",
  "Walk_Mud1", "Walk_Mud2", "Walk_Mud3", "Walk_Mud4",
  "Walk_Sand1", "Walk_Sand2", "Walk_Sand3", "Walk_Sand4",
  "Walk_Staircase1", "Walk_Staircase2", "Walk_Staircase3", "Walk_Staircase4",
  "Walk_Stone1", "Walk_Stone2", "Walk_Stone3", "Walk_Stone4",
  "Walk_Wood1", "Walk_Wood2", "Walk_Wood3", "Walk_Wood4",
  "Whip_Draw", "Whip_Holster",
  "Zombie_Attack1", "Zombie_Attack2",
  "Zombie_Death1", "Zombie_Death2",
  "Zombie_Move1", "Zombie_Move2",
  "Zombie_Pain1", "Zombie_Pain2"
]

