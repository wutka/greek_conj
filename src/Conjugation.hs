module Conjugation where

import Parsing
import Utils
import Data.Maybe (fromJust)

data PartsType = Root String | ConnectingVowel Char | PersonalEnding String
  deriving (Show, Eq)
data ModificationType = LengthenForNoPersonalEnding | MovableNu | DropNInONS
  deriving (Show, Eq)
data ModifyType = Modify ModificationType String
  deriving (Show, Eq)
data ConjugationType = Conjugation [PartsType] String [ModifyType]
  deriving (Show, Eq)
data VerbLemmaType = WVerb VerbType | MIVerb VerbType | DeponentVerb VerbType | UnknownVerb VerbType
  deriving (Show, Eq)

type ModificationFunc = VerbType -> ConjugationType -> String -> Maybe (ConjugationType, String)

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

modLengthenOnNoEnding :: VerbType -> ConjugationType -> String -> Maybe (ConjugationType, String)
modLengthenOnNoEnding _ (Conjugation parts initial mods) currentForm =
  if endsWith "O-" currentForm then
    Just (Conjugation parts initial (mods ++ [newMod]), newForm)
  else
    Nothing
  where
    newForm = (trunc 2 currentForm) ++ "W"
    newMod = Modify LengthenForNoPersonalEnding newForm

modMovableNuEnding :: VerbType -> ConjugationType -> String -> Maybe (ConjugationType, String)
modMovableNuEnding (Verb _ _ conjugated) (Conjugation parts initial mods) currentForm =
  if endsWith "(N)" currentForm then
    if endsWith "N" conjugated then
      Just (Conjugation parts initial (mods ++ [modAddN]), newFormAddN)
    else
      Just (Conjugation parts initial (mods ++ [modNoAddN]), newFormNoAddN)
  else
    Nothing
  where
    newFormAddN = newFormNoAddN ++ "N"
    modAddN = Modify MovableNu newFormAddN
    newFormNoAddN = trunc 3 currentForm
    modNoAddN = Modify MovableNu newFormNoAddN

modONS :: VerbType -> ConjugationType -> String -> Maybe (ConjugationType, String)
modONS _ (Conjugation parts initial mods) currentForm =
  case findSubstr "ONS" currentForm of
    Nothing -> Nothing
    Just (before, after) -> Just (Conjugation parts initial
                                  (mods ++ [Modify DropNInONS (newMod before after)]),
                                 (newMod before after))
  where
    newMod before after = before ++ "OUS" ++ after

modificationChain :: [ModificationFunc]
modificationChain = [modLengthenOnNoEnding, modONS, modMovableNuEnding]

runModificationChain :: [ModificationFunc] -> [ModificationFunc] -> VerbType ->
  ConjugationType -> String -> (ConjugationType, String)
runModificationChain _ [] _ conjugation currentForm = (conjugation, currentForm)
runModificationChain fullChain (nextMod : restMods) verb conjugation currentForm =
  case nextMod verb conjugation currentForm of
    Nothing -> runModificationChain fullChain restMods verb conjugation currentForm
    Just (newConjugation, newCurrentForm) ->
      runModificationChain fullChain fullChain verb newConjugation newCurrentForm
    
doModifications :: VerbType -> ConjugationType -> (ConjugationType, String)
doModifications verb conj@(Conjugation _ initialForm _) =
  runModificationChain modificationChain modificationChain verb conj initialForm


connectingVowel :: Person -> Number -> Char
connectingVowel First Singular = 'O'
connectingVowel Second Singular = 'E'
connectingVowel Third Singular = 'E'
connectingVowel First Plural = 'O'
connectingVowel Second Plural = 'E'
connectingVowel Third Plural = 'O'

primaryActiveEnding :: Person -> Number -> String
primaryActiveEnding First Singular = "-"
primaryActiveEnding Second Singular = "IS"
primaryActiveEnding Third Singular = "I"
primaryActiveEnding First Plural = "MEN"
primaryActiveEnding Second Plural = "TE"
primaryActiveEnding Third Plural = "NSI(N)"

wStem :: String -> String
wStem lemma = trunc 1 lemma

conjugateW_PAI :: VerbType -> ConjugationType
conjugateW_PAI (Verb (Parsing optPerson _ _ _ _ optNumber _) lemma _) =
  Conjugation [Root stem, ConnectingVowel cv, PersonalEnding pe] initialForm []
  where
    person = fromJust optPerson
    number = fromJust optNumber
    stem = wStem lemma
    cv = connectingVowel person number
    pe = primaryActiveEnding person number
    initialForm = stem ++ [cv] ++ pe

conjugateW :: VerbType -> Maybe ConjugationType
conjugateW verb@(Verb (Parsing _ Present Active Indicative _ _ _) _ _) =
  Just $ conjugateW_PAI verb
conjugateW _ = Nothing

conjugate :: VerbType -> Maybe ConjugationType
conjugate verb =
  case classifyVerb verb of
    WVerb classified -> conjugateW classified
    MIVerb _ -> Nothing
    DeponentVerb _ -> Nothing
    UnknownVerb _ -> Nothing
  
runConjugation :: VerbType -> Maybe (ConjugationType, String)
runConjugation verb =
  case conjugate verb of
    Nothing -> Nothing
    Just conjugation ->
      case doModifications verb conjugation of
        (finalConj, finalForm) ->
          Just (finalConj, checkConjugation verb finalForm)
    
  


  
