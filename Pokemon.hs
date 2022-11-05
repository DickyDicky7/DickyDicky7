{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports     #-}
{-# OPTIONS_GHC -Wno-unused-top-binds   #-}

{-# LANGUAGE RecordWildCards #-}

module Pokemon() where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
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

newtype PkmStat (pkmName :: Symbol) (statType :: StatType)
      = PkmStat Int deriving newtype Show

instance Bounded (PkmStat "Pikachu" 'HP) where
  minBound = PkmStat 180
  maxBound = PkmStat 274

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

fstPkm :: Pokemon "Pikachu"
fstPkm = Pokemon
  { pkmType  = Mono Electric
  , pkmHP    = PkmStat 200
  , pkmAtk   = PkmStat 0
  , pkmDef   = PkmStat 0
  , pkmSpAtk = PkmStat 0
  , pkmSpDef = PkmStat 0
  , pkmSpd   = PkmStat 0
  } 

