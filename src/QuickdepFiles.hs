-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# LANGUAGE TemplateHaskell #-}

module QuickdepFiles where

import           Data.FileEmbed (embedStringFile)

template :: String
template = $(embedStringFile "data/control.in")
