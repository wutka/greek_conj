module Main where

import Parsing
import Conjugation
import Data.Maybe (mapMaybe)

showConjugation :: (ConjugationType, String) -> String
showConjugation (conj, finalForm) =
  (show conj) ++ "  final: " ++ finalForm
  
main :: IO ()
main = do
  verbs <- loadVerbFile "beta_conj.txt"
  putStrLn $ unlines (map showConjugation (mapMaybe runConjugation verbs))
  
