{-
sample imputs and corresponding answers
1) ["1", "22", "333"] => "both"
2) ["111", "22", "3"] => "lexicographically"
3) ["3", "22", "111"] => "lengths"
4) ["333", "22", "1"] => "none"
-}

sortingMethod:: [[Char]] -> [Char]
sortingMethod stringList
	| lexic && len  = "both"
	| lexic         = "lexicographically"
	| len           = "lengths"
	| otherwise     = "none"
	where lexic = sortedLexic stringList
	      len   = sortedLen   stringList

--Check whether stringList is sorted in lexicographically
sortedLexic:: [[Char]] -> Bool
sortedLexic [_] = True
sortedLexic (s1:(s2:rest))
	| s1 <= s2  = sortedLexic (s2:rest)
	| otherwise = False

--Check whether stringList is sorted in lengths(accending) of each string
sortedLen:: [[Char]] -> Bool
sortedLen [_] = True
sortedLen (s1:(s2:rest))
	| l1 <= l2    = sortedLen (s2:rest)
	| otherwise   = False
	where l1 = length s1
	      l2 = length s2