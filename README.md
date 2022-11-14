
![Hello_world](https://user-images.githubusercontent.com/87304213/169082414-0dfeb9d8-ef69-40e1-a240-494fe8a030e2.png)
<div  align="center"><img src="https://user-images.githubusercontent.com/87304213/169082813-f0f65e26-75e0-494a-9326-24707739e68a.gif" /></div>

```Haskell
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports     #-}
{-# OPTIONS_GHC -Wno-unused-top-binds   #-}

module Pokemon() where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import System.Random
import Data.Typeable
import Data.Type.Equality

type            IsPkmType :: Type -> Constraint
class Show t => IsPkmType t

data Bug      = Bug      deriving stock Show deriving anyclass IsPkmType
data Ice      = Ice      deriving stock Show deriving anyclass IsPkmType
data Dark     = Dark     deriving stock Show deriving anyclass IsPkmType
data Rock     = Rock     deriving stock Show deriving anyclass IsPkmType
data Fire     = Fire     deriving stock Show deriving anyclass IsPkmType
data Water    = Water    deriving stock Show deriving anyclass IsPkmType
data Grass    = Grass    deriving stock Show deriving anyclass IsPkmType
data Fairy    = Fairy    deriving stock Show deriving anyclass IsPkmType
data Fight    = Fight    deriving stock Show deriving anyclass IsPkmType
data Ghost    = Ghost    deriving stock Show deriving anyclass IsPkmType
data Steel    = Steel    deriving stock Show deriving anyclass IsPkmType
data Dragon   = Dragon   deriving stock Show deriving anyclass IsPkmType
data Ground   = Ground   deriving stock Show deriving anyclass IsPkmType
data Normal   = Normal   deriving stock Show deriving anyclass IsPkmType
data Poison   = Poison   deriving stock Show deriving anyclass IsPkmType
data Flying   = Flying   deriving stock Show deriving anyclass IsPkmType
data Psychic  = Psychic  deriving stock Show deriving anyclass IsPkmType
data Electric = Electric deriving stock Show deriving anyclass IsPkmType

type            IsNumOfPkmType :: Type -> Constraint
class Show n => IsNumOfPkmType n

data MonoType = forall t . IsPkmType t => JustType t
  deriving anyclass IsNumOfPkmType

instance Show MonoType where
  show (JustType t) = "\n[Mono type]: " <> show t

data DualType = forall t1 t2 . (IsPkmType t1, IsPkmType t2, (t1 == t2) ~ 'False) => BothType t1 t2
  deriving anyclass IsNumOfPkmType

instance Show DualType where
  show (BothType t1 t2) = "\n[Dual type]: " <> show t1 <> ", " <> show t2

data PkmType = forall n . IsNumOfPkmType n => Is n

instance Show PkmType where
  show (Is n) = show n

data StatType
  =  HP
  |  Atk
  |  Def
  |  SpAtk
  |  SpDef
  |  Spd
  deriving stock Show

newtype PkmStat (pkmName :: Symbol) (statType :: StatType)
      = PkmStat Int deriving newtype Show

data Pokemon (pkmName :: Symbol)
  =  Pokemon
  { pkmType  :: PkmType
  , pkmHP    :: PkmStat pkmName 'HP
  , pkmAtk   :: PkmStat pkmName 'Atk
  , pkmDef   :: PkmStat pkmName 'Def
  , pkmSpAtk :: PkmStat pkmName 'SpAtk
  , pkmSpDef :: PkmStat pkmName 'SpDef
  , pkmSpd   :: PkmStat pkmName 'Spd
  }

instance KnownSymbol pkmName => Show (Pokemon pkmName) where
  show Pokemon {..} = "[Name]: " <> symbolVal (Proxy @pkmName)
    <> show pkmType
    <> "\n[HP]:    " <> show pkmHP
    <> "\n[Atk]:   " <> show pkmAtk
    <> "\n[Def]:   " <> show pkmDef
    <> "\n[SpAtk]: " <> show pkmSpAtk
    <> "\n[SpDef]: " <> show pkmSpDef
    <> "\n[Spd]:   " <> show pkmSpd

type  BoundedStatType :: Symbol -> Constraint
class BoundedStatType pkmName where
  maxMinStat :: forall (statType :: StatType) . Typeable statType => Proxy statType
    -> (PkmStat pkmName statType, PkmStat pkmName statType)

  randomStat :: forall (statType :: StatType) . Typeable statType => IO (PkmStat pkmName statType)
  randomStat = do
    let (PkmStat minStat, PkmStat maxStat) = maxMinStat @pkmName (Proxy @statType)
    seed <- newStdGen
    let (newStat, _) = randomR (minStat, maxStat) seed
    pure (PkmStat newStat)

is :: forall (statType' :: StatType) (statType :: StatType) . 
      (Typeable statType', Typeable statType) => Proxy statType -> Bool
is = (typeRep (Proxy @statType') ==) . typeRep

instance BoundedStatType "Pikachu" where
  maxMinStat     proxy
    | is @'HP    proxy = (PkmStat 180, PkmStat 274)
    | is @'Atk   proxy = (PkmStat 103, PkmStat 229)
    | is @'Def   proxy = (PkmStat  76, PkmStat 196)
    | is @'SpAtk proxy = (PkmStat  94, PkmStat 218)
    | is @'SpDef proxy = (PkmStat  94, PkmStat 218)
    | is @'Spd   proxy = (PkmStat 166, PkmStat 306)
    | otherwise        = error "StatType not found"

wildPikachu :: IO (Pokemon "Pikachu")
wildPikachu = do
  pkmHP    <- randomStat @_ @'HP
  pkmAtk   <- randomStat @_ @'Atk
  pkmDef   <- randomStat @_ @'Def
  pkmSpAtk <- randomStat @_ @'SpAtk
  pkmSpDef <- randomStat @_ @'SpDef
  pkmSpd   <- randomStat @_ @'Spd
  pure Pokemon { pkmType  = Is (BothType Electric Psychic), .. }

main :: IO ()
main = do
  myFstPkm <- wildPikachu
  print myFstPkm  
```

<div  align="center"><img src="https://raw.githubusercontent.com/DickyDicky7/DickyDicky7/main/synthwave3.png" /></div>
<div  align="center"><img src="https://raw.githubusercontent.com/DickyDicky7/DickyDicky7/main/asuka-synthwave.png" /></div>
