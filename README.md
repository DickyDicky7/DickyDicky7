
![Hello_world](https://user-images.githubusercontent.com/87304213/169082414-0dfeb9d8-ef69-40e1-a240-494fe8a030e2.png)
<div  align="center"><img src="https://user-images.githubusercontent.com/87304213/169082813-f0f65e26-75e0-494a-9326-24707739e68a.gif" /></div>

```Haskell
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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

data PkmType
  = forall t     .  IsPkmType t                                      => Mono t
  | forall t1 t2 . (IsPkmType t1, IsPkmType t2, (t1 == t2) ~ 'False) => Dual t1 t2

instance Show PkmType where
  show (Mono t)     = "\n[Mono type]: " <> show t
  show (Dual t1 t2) = "\n[Dual type]: " <> show t1 <> ", " <> show t2

data StatType
  =  HP
  |  Atk
  |  Def
  |  SpAtk
  |  SpDef
  |  Spd
  deriving (Typeable, Show)

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

data Fire     = Fire     deriving stock Show deriving anyclass IsPkmType
data Water    = Water    deriving stock Show deriving anyclass IsPkmType
data Electric = Electric deriving stock Show deriving anyclass IsPkmType

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

type CheckStatType = forall (statType :: StatType) . Typeable statType => Proxy statType -> Bool

isHP :: CheckStatType
isHP = (typeRep (Proxy @'HP) ==) . typeRep

isAtk :: CheckStatType
isAtk = (typeRep (Proxy @'Atk) ==) . typeRep

isDef :: CheckStatType
isDef = (typeRep (Proxy @'Def) ==) . typeRep

isSpAtk :: CheckStatType
isSpAtk = (typeRep (Proxy @'SpAtk) ==) . typeRep

isSpDef :: CheckStatType
isSpDef = (typeRep (Proxy @'SpDef) ==) . typeRep

isSpd :: CheckStatType
isSpd = (typeRep (Proxy @'Spd) ==) . typeRep

instance BoundedStatType "Pikachu" where
  maxMinStat  proxy
    | isHP    proxy = (PkmStat 180, PkmStat 274)
    | isAtk   proxy = (PkmStat 103, PkmStat 229)
    | isDef   proxy = (PkmStat  76, PkmStat 196)
    | isSpAtk proxy = (PkmStat  94, PkmStat 218)
    | isSpDef proxy = (PkmStat  94, PkmStat 218)
    | isSpd   proxy = (PkmStat 166, PkmStat 306)
    | otherwise     = error "StatType not found"

wildPikachu :: IO (Pokemon "Pikachu")
wildPikachu = do
  pkmHP    <- randomStat @_ @'HP
  pkmAtk   <- randomStat @_ @'Atk
  pkmDef   <- randomStat @_ @'Def
  pkmSpAtk <- randomStat @_ @'SpAtk
  pkmSpDef <- randomStat @_ @'SpDef
  pkmSpd   <- randomStat @_ @'Spd
  pure Pokemon { pkmType  = Mono Electric, .. }

main :: IO ()
main = do
  myFstPkm <- wildPikachu
  print myFstPkm 
```

<div  align="center"><img src="https://raw.githubusercontent.com/DickyDicky7/DickyDicky7/main/synthwave3.png" /></div>
<div  align="center"><img src="https://raw.githubusercontent.com/DickyDicky7/DickyDicky7/main/asuka-synthwave.png" /></div>
