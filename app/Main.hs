module Main where

import Parsing
import Conjugation

showConjugation :: Maybe (ConjugationType, String) -> String
showConjugation Nothing = ""
showConjugation (Just (conj, finalForm)) =
  (show conj) ++ "  final: " ++ finalForm
  
main :: IO ()
main = do
  verbs <- loadVerbFile "beta_conj.txt"
  putStrLn $ unlines (map showConjugation (map runConjugation verbs))
  
