module Utils where

endsWith :: String -> String -> Bool
endsWith suffix str =
  (length str >= length suffix) && (drop (length str - length suffix) str) == suffix

trunc :: Int -> String -> String
trunc n str =
  if length str < n then
    ""
  else
    take (length str - n) str

findSubstr' :: String -> String -> String -> Maybe (String, String)
findSubstr' _ [] _ = Nothing
findSubstr' substr str acc =
  if substr == take (length substr) str then
    Just (reverse acc, drop (length substr) str)
  else
    findSubstr' substr (tail str) ((head str) : acc)
    
findSubstr :: String -> String -> Maybe (String, String)
findSubstr substr str = findSubstr' substr str []
