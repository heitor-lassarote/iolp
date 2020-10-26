module Language.LanguageConverter
    ( LanguageConverter (..)
    , convertDef
    ) where

import Universum

import Data.Default.Class

class LanguageConverter source destination where
    type ConverterState source destination
    convert :: source -> State (ConverterState source destination) destination

convertDef
    :: (Default (ConverterState source destination), LanguageConverter source destination)
    => source
    -> destination
convertDef source = evalState (convert source) def
