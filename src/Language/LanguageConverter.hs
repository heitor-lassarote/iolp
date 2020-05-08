module Language.LanguageConverter where

class LanguageConverter source destination where
    convert :: source -> destination
