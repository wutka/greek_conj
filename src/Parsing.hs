module Parsing where

import qualified Data.List as List

data Person = First | Second | Third
  deriving (Show, Eq)
data Tense = Present | Imperfect | Future | Aorist | Perfect | Pluperfect
  deriving (Show, Eq)
data Voice = Active | Middle | Passive
  deriving (Show, Eq)
data Mood = Indicative | Imperative | Subjunctive | Optative | Infinitive | Participle
  deriving (Show, Eq)
data Case = Nominative | Accusative | Genitive | Dative | Vocative
  deriving (Show, Eq)
data Number = Singular | Plural
  deriving (Show, Eq)
data Gender = Masculine | Feminine | Neuter
  deriving (Show, Eq)

data ParsingType = Parsing (Maybe Person) Tense Voice Mood (Maybe Case) (Maybe Number) (Maybe Gender)
  deriving (Show, Eq)

data VerbType = Verb ParsingType String String
  deriving (Show, Eq)

data ParsingMapType a = ParsingMap String Int [(Char, a)]
  deriving (Show, Eq)

personMap :: ParsingMapType Person
personMap = ParsingMap "person" 0 [('1', First), ('2', Second), ('3', Third)]

tenseMap :: ParsingMapType Tense
tenseMap = ParsingMap "tense" 1 [('P', Present), ('I', Imperfect), ('F', Future),
                                 ('A', Aorist), ('X', Perfect), ('Y', Pluperfect)]

voiceMap :: ParsingMapType Voice
voiceMap = ParsingMap "voice" 2 [('A', Active), ('M', Middle), ('P', Passive)]

moodMap :: ParsingMapType Mood
moodMap = ParsingMap "mood" 3 [('I', Indicative), ('D', Imperative), ('S', Subjunctive),
                               ('O', Optative), ('N', Infinitive), ('P', Participle)]

caseMap :: ParsingMapType Case
caseMap = ParsingMap "case" 4 [('N', Nominative), ('A', Accusative), ('G', Genitive),
                               ('D', Dative), ('V', Vocative)]

numberMap :: ParsingMapType Number
numberMap = ParsingMap "number" 5 [('S', Singular), ('P', Plural)]

genderMap :: ParsingMapType Gender
genderMap = ParsingMap "gender" 6 [('M', Masculine), ('F', Feminine), ('N', Neuter)]

plainBeta :: String -> String
plainBeta s = filter isPlain s
  where
    isPlain ch = ch >= 'A' && ch <= 'Z'

parseCode :: String -> ParsingMapType a -> a
parseCode parsing (ParsingMap name index items) =
  case List.find ((code ==) . fst) items of
    Just (_, parse) -> parse
    Nothing -> error ("Invalid " ++ name ++ " code " ++ [code] ++ " in " ++ parsing)
    
    where
      code = parsing !! index
        
unparseCode :: (Eq a) => a -> ParsingMapType a -> Char
unparseCode parse (ParsingMap name _ items) =
  case List.find ((parse ==) . snd) items of
    Just (ch, _) -> ch
    Nothing -> error ("Missing type in parse map for " ++ name)

unparseOptCode :: (Eq a) => Maybe a -> ParsingMapType a -> Char
unparseOptCode Nothing _ = '-'
unparseOptCode (Just p) parsingMap = unparseCode p parsingMap

parseParsing :: String -> ParsingType
parseParsing parsing =
  case mood of
    Infinitive -> Parsing Nothing tense voice mood Nothing Nothing Nothing
    Participle -> Parsing Nothing tense voice mood verbCase number gender
    _ -> Parsing person tense voice mood Nothing number Nothing
  where
    person = Just $ parseCode parsing personMap
    tense = parseCode parsing tenseMap
    voice = parseCode parsing voiceMap
    mood = parseCode parsing moodMap
    verbCase = Just $ parseCode parsing caseMap
    number = Just $ parseCode parsing numberMap
    gender = Just $ parseCode parsing genderMap

unparseParsing :: ParsingType -> String
unparseParsing (Parsing person tense voice mood verbCase number gender) =
  [unparseOptCode person personMap,
   unparseCode tense tenseMap,
   unparseCode voice voiceMap,
   unparseCode mood moodMap,
   unparseOptCode verbCase caseMap,
   unparseOptCode number numberMap,
   unparseOptCode gender genderMap]
  
parseConjugation :: String -> VerbType
parseConjugation line =
  case words line of
    [parsing, lemma, conjugated] -> Verb (parseParsing parsing) (plainBeta lemma) (plainBeta conjugated)
    _ -> error ("Unexpected number of items in line (" ++ (show $ length line) ++ ") expected 3: " ++ line)

loadVerbFile :: String -> IO [VerbType]
loadVerbFile filename = do
  content <- readFile filename
  return $ map parseConjugation $ lines content
