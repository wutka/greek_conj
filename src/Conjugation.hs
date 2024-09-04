module Conjugation where

import Parsing

data PartsType = Root String | ConnectingVowel Char | PersonalEnding String
data ModificationType = LengthenForNoPersonalEnding
data ModifyType = Modify ModificationType String
data ConjugationType = Conjugation [PartsType] String [ModifyType]
data VerbLemmaType = WVerb VerbType | MIVerb VerbType | DeponentVerb VerbType | UnknownVerb VerbType

endsWith :: String -> String -> Bool
endsWith suffix str =
  (length str >= length suffix) && (drop (length str - length suffix) str) == suffix
  
classifyVerb :: VerbType -> VerbLemmaType
classifyVerb verb@(Verb _ lemma _) =
  if endsWith "W" lemma then
    WVerb verb
  else if endsWith "MI" lemma then
    MIVerb verb
  else if endsWith "MAI" lemma then
    DeponentVerb verb
  else
    UnknownVerb verb

checkConjugation :: VerbType -> String -> String
checkConjugation (Verb parsing lemma originalConjugation) conjugation =
  if conjugation == originalConjugation then
    conjugation
  else
    error ("Unable to conjugate " ++ lemma ++ " into " ++ originalConjugation ++ " (" ++
          (unparseParsing parsing) ++ "), got as far as " ++ conjugation)

doModifications :: VerbType -> ConjugationType -> String -> (ConjugationType, String)
doModifications _ conjugation currentForm =
  (conjugation, currentForm)

--conjugateW_PAI :: VerbType -> (ConjugationType, String)
--conjugateW_PAI (Verb optPerson _ _ _ _ optNumber _) (Some  lemma conjugation) =
  
  


  
