-----------------------------------------------------------------------------
--
-- Module      :  Utils
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Utils where
import Data.Time
import Data.Maybe
import Data.List
import Data.String.Utils
import Numeric
import Text.Regex.Posix


--dopasowuje date za pomoca wyrazenia regularnego
matchDate :: String  -> [String]
matchDate str = getAllTextMatches $ str =~ "[0-9]+" :: [String]

--parsuje date za pomoca listy dopasowania z wyrazenia regularnego
parseDate :: [String] -> Maybe Day
parseDate (y:m:d:_) = Just (fromGregorian (toInteger (read y ::Int)) (read m ::Int) (read d ::Int))
parseDate _ = Nothing

--sprawdza czy sparsowana data jest zgodna z ta wpisana w stringu - eleminuje daty typu 30 lutego
validateMatchedDate :: [String] -> Day -> Bool
validateMatchedDate (y:m:d:_) date = (length y == 4) && (length m == 2) && (length d == 2) && (y1 == y2) && (m1 == m2) && (d1 == d2)
                                    where (y1,m1,d1) = (toInteger (read y ::Int), read m ::Int, read d ::Int)
                                          (y2,m2,d2) = toGregorian date
validateMatchedDate _ _ = False

--zwraca date sparsowana ze string lub nothing w razie niepowodzenia
getDateWithValidation:: String -> Maybe Day
getDateWithValidation str = if ( not( validateMatchedDate matchedDate ( fromJust maybeDate )) )
                then
                    Nothing
                else
                    maybeDate
                        where
                            matchedDate = matchDate str
                            maybeDate = parseDate (matchedDate);

--dopasowuje wyrazeniem regularnym adres mailowy
matchEmail :: String  -> [String]
matchEmail str = getAllTextMatches $ str =~ "^[_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4})$" :: [String]

--parsuje mail na podstawie wyniku dopasowania z wyrazenia regularnego
parseMatchedEmail :: [String] -> Maybe String
parseMatchedEmail (mail:_) = Just mail
parseMatchedEmail _ = Nothing

--parsuje mail ze stringa
parseEmail:: String -> Maybe String
parseEmail mail = parseMatchedEmail (matchEmail mail)


--Stale
justification :: Int
justification = 30

fill :: Char
fill = ' '


-- zmiana elementu w liscie na n-tej pozycji
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

-- Wczytanie obiekt Maybe z ciagu znakow
readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
        case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe


-- Aliasy uzywanych funkcji wbudowanych
printNewLine = putStrLn ""
printSeparator = print "---"
printList list = mapM_ putStrLn list


-- Ladowanie z pliku modelu danych
loadFromFile :: FilePath -> (String -> b) -> IO b
loadFromFile filePath readFunction = do
        raw <- readFile filePath
        return (readFunction (strip raw))

-- Zapisywanie do pliku modelu danych
saveToFile :: (Show a) => a -> FilePath -> IO ()
saveToFile object filePath = writeFile filePath (show object)

-- Ponumerowanie elementow listy
enumerate :: [a] -> Int -> [(Int, a)]
enumerate [] _ = []
enumerate (x:xs) start = [(start, x)] ++ (enumerate xs (start + 1))

-- Usniecie elementu z listy
removeItem :: (Eq t) => t -> [t] -> [t]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = ys
                    | otherwise = y : removeItem x ys


-- Wysrodkowanie ciagu znakow
center :: Int -> Char -> String -> String
center width fill string =
        if width <= stringLength
        then
                string
        else
                centeredString
        where
                stringLength = length string
                fillLength = width - stringLength
                centeredString = beginning ++ string ++ ending
                partition = divide fillLength
                beginning = takeRepeat (fst partition) fill
                ending = takeRepeat (snd partition) fill
                divide number = if mod number two == one
                                        then
                                                (part, complement)
                                        else
                                                (half, half)
                                        where
                                                part = half + one
                                                complement = number - part
                                                half = div number two
                                                two = 2
                                                one = 1

-- Powtorzenie podanego znaku n razy
takeRepeat :: Int -> a -> [a]
takeRepeat n item = take n (repeat item)

-- Otoczenie listy danym elementem
surround :: [a] -> a -> [a]
surround list item = [item] ++ list ++ [item]

-- Pobranie srodkowego elementu listy
middle :: [a] -> [a]
middle [] = []
middle [x] = []
middle (x:xs) = init xs

-- Aliasy funkcji formatowania
centerEach justification fill list = map (center justification fill) list
format justification fill fields = concat (centerEach justification fill fields)
formatField = format justification fill


-- parsuje inta ze stringa
parseInt:: String -> Int
parseInt str = parseIntWithDefValue (readDec str) 0
parseIntWithDefValue [] def = def
parseIntWithDefValue [(id, _)] def = id

-- zwraca obecna date
getCurrentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay

