{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Lambdabot.Main
import Lambdabot.Plugin.Haskell

modulesInfo :: Modules
modulesInfo = $(modules $ corePlugins ++ haskellPlugins)
