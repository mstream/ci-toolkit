module CiToolkit.Common.AsciiDoc
  ( TitleLevel(..)
  , bullet
  , link
  , sourceBlock
  , title
  ) where

import Prelude

import Data.Maybe (Maybe, maybe)

data TitleLevel
  = L0
  | L1
  | L2
  | L3

instance Show TitleLevel where
  show = case _ of
    L0 → "="
    L1 → "=="
    L2 → "==="
    L3 → "===="

bullet ∷ String → String
bullet s = "- " <> s

link ∷ { alias ∷ String, href ∷ String } → String
link { alias, href } = "link:" <> href <> "[" <> alias <> "]"

sourceBlock
  ∷ { code ∷ String, language ∷ String, title ∷ Maybe String } → String
sourceBlock { code, language, title } =
  (maybe "" (\s → "." <> s <> "\n") title)
    <> "[source,"
    <> language
    <> "]\n"
    <> "----\n"
    <> code
    <> "\n----"

title ∷ TitleLevel → String → String
title level s = show level <> " " <> s
