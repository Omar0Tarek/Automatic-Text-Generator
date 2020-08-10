import DataFile

splitWords:: String -> (String, [String])
splitWords [] = ("", [])
splitWords (x:xs) = if(elem x punct)
					then(
						if(length s == 0)
						then((s, ([x]:l)))
						else(([], [x]:s:l))
					)
					else(
						if(x == ' ')
						then(
							if(length s == 0)
							then((s, l))
							else(([], s:l))
						)
						else((x:s, l))
					)
					where (s, l) = splitWords(xs)

getSplittedWords:: (String, [String]) -> [String]
getSplittedWords (x, y) = if(length x /= 0) then(x:y) else(y)

wordToken:: String -> [String]
wordToken s = getSplittedWords (splitWords s)

wordTokenList:: [String] -> [String]
wordTokenList l = foldr (++) [] (map wordToken l)

genBigrams:: [String] -> [(String, String)]
genBigrams [] = []
genBigrams [x] = []
genBigrams (x:y:xs) = if(elem (x, y) rest) then(rest) else((x, y):rest)
	where rest = genBigrams(y:xs)

genTrigrams:: [String] -> [(String, String, String)]
genTrigrams [] = []
genTrigrams [x] = []
genTrigrams [x, y] = []
genTrigrams (x:y:z:xs) = if(elem (x, y, z) rest) then(rest) else((x, y, z):rest)
	where rest = genTrigrams(y:z:xs)

makeUnique:: Ord a => [a] -> [a]
makeUnique [] = []
makeUnique (x:xs) = if(elem x rest) then(rest) else(x:rest)
	where rest = makeUnique(xs)

uniqueBigrams:: [String] -> [(String, String)]
uniqueBigrams l = makeUnique (genBigrams l)

uniqueTrigrams:: [String] -> [(String, String, String)]
uniqueTrigrams l = makeUnique (genTrigrams l)

countElemInList:: (Ord a, Num b) => a -> [a] -> b
countElemInList y [] = 0
countElemInList y (x:xs) = if(x == y) then(1 + countElemInList y xs) else(countElemInList y xs)

countOccur:: (Ord a, Num b) => [a] -> [a] -> [(a, b)]
countOccur [] y = []
countOccur (x:xs) y = (x, countElemInList x y):(countOccur xs y)

bigramsFreq:: Num a => [String] -> [((String, String), a)]
bigramsFreq l = countOccur (uniqueBigrams l) (genBigrams l)

trigramsFreq:: Num a => [String] -> [((String, String, String), a)]
trigramsFreq l = countOccur (uniqueTrigrams l) (genTrigrams l)

getFreq:: (Ord a, Num b) => a -> [(a, b)] -> b
getFreq x [] = 0
getFreq x ((y, z):xs) = if(x == y) then(z) else(getFreq x xs)

generateOneProb:: Fractional a => ((String, String, String), a) -> [((String, String), a)] -> a
generateOneProb ((x, y, z), c1) l = c1 / (getFreq (x, y) l)

generateOneProbAsPair:: Fractional a => [((String, String), a)] -> ((String, String, String), a) -> ((String, String, String), a)
generateOneProbAsPair bl (s, f) = (s, generateOneProb (s, f) bl)

genProbPairs:: Fractional a => [((String, String, String), a)] -> [((String, String), a)] -> [((String, String, String), a)]
genProbPairs tl bl = map (generateOneProbAsPair bl) tl

getSuitableList:: (Ord a, Fractional a) => (String, String) -> [((String, String, String), a)] -> [String]
getSuitableList (a, b) [] = []
getSuitableList (a, b) (((x, y, z), p):xs) = if(a == x && b == y && p > 0.03) then(z:(getSuitableList(a, b) xs)) else(getSuitableList(a, b) xs)

generateNextWord:: (Ord a, Fractional a) => [String] -> [((String, String, String), a)] -> String
generateNextWord [x, y] l = if(length list == 0) then("Sorry, it is not possible to infer from current database") else(list !! (randomZeroToX ((length list) - 1)))
	where list = getSuitableList (x, y) l

generate:: (Ord a, Fractional a) => [String] -> [((String, String, String), a)] -> Int -> String
generate [x, y] tp 0 = (x ++ " ") ++ y
generate [x, y] tp n = (x ++ " ") ++ (generate [y, z] tp (n - 1))
	where z = generateNextWord [x, y] tp

generateText:: String -> Int -> String
generateText s n = generate (wordToken s) (genProbPairs (trigramsFreq processedDocs) (bigramsFreq processedDocs)) n
	where processedDocs = wordTokenList docs




