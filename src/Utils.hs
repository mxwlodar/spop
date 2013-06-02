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

import Data.String.Utils
import Numeric



--do wywalenia jak wszystko bedzie zrobione
todo addressBook = do
            putStrLn "!!!!!!!!!!!!!!!!!!!!"
            putStrLn "!!                !!"
            putStrLn "!!  Do zrobienia  !!"
            putStrLn "!!                !!"
            putStrLn "!!!!!!!!!!!!!!!!!!!!"
            return addressBook

--do wywalenia jak wszystko bedzie zrobione
showData addressBook = do
            putStrLn "!!!!!!!!!!!!!!!!!!!!"
            putStrLn "!!                !!"
            putStrLn "!! Pogladowe dane !!"
            putStrLn "!!                !!"
            putStrLn "!!!!!!!!!!!!!!!!!!!!"
            print addressBook
            return addressBook


--Stale
justification :: Int
justification = 30

fill :: Char
fill = ' '

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



