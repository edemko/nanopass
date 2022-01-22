{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lang.TH where

import Language.Nanopass.LangDef (runDefine, defineLang)
import Lang

$(runDefine $ defineLang lang)

deriving stock instance (Show a) => Show (Expr a)
deriving stock instance (Show a) => Show (Stmt a)
