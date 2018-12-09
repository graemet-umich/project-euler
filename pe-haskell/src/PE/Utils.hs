-- Utils.hs
-- Utility functions

module PE.Utils
  (
    split
  ) where

-- Split string s on c 0 or more times. For example,
--      split ',' ",yo,,yo,,"
--      ["","yo","","yo","",""]
split :: Char -> String -> [String]
split c s = split' s []
  where split' s acc
          | s  == "" = acc
          | s  == [c] = acc ++ [""] ++ [""]  -- from more than 1 trailing c
          | s2 == "" = acc ++ [s1]  -- end
          | otherwise = split' (tail s2) (acc ++ [s1])
          where s1 = takeWhile (/= c) s
                s2 = dropWhile (/= c) s
