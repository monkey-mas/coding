{-Time complexity is 'practically' improved compared to TwoWaysSorting2.hs
  as this function takes exactly |N| (i.e. depending on the size of stringList)
  Also another improvement is found like:
  once the function finds there is no possiblity of eighther sorting method,
  which is indicated by isLexic==False and isLen==False,
  then it stops computation and returns "none"
-}
sortingMethod:: [[Char]] -> [Char]
sortingMethod stringList = sortingMethod' stringList True True
    where sortingMethod' _ False False = "none"
          sortingMethod' [_] isLexic isLen
	        | isLexic && isLen  = "both"
	        | isLexic           = "lexicographically"
	        | isLen             = "lengths"
	        | otherwise         = "none"
          sortingMethod' (s1:s2:rest) isLexic isLen
            = stringMethod' (s2:rest) isLexic' isLen'
	        where isLexic' = (s1 <= s2) && isLexic
	              isLen'   = (length s1 <= length s2) && isLen	          
