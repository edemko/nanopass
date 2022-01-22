{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lang1.TH where

import Language.Nanopass.LangDef (runModify, modifyLang)
import Lang1

import qualified Lang.TH as L0


$(runModify ext)

deriving stock instance (Show a) => Show (Expr a)
