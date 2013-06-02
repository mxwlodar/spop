-----------------------------------------------------------------------------
--
-- Module      :  Ui
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

module Ui where


import Data.Maybe
import Data.String.Utils
import Strings
import Utils



-- Stworzenie obramowanego komunikatu
messageBox "" = ""
messageBox message =
        unlines (surround centeredMessageLinesWithBars verticalBar)
        where
                messageLines = map (surroundWith ' ') (lines message)
                messageLengths = map length messageLines
                maximumMessageLength = maximum messageLengths
                verticalBar = surround (take maximumMessageLength (repeat '-')) ' '
                centeredMessageLines = map (center maximumMessageLength ' ') messageLines
                centeredMessageLinesWithBars = map (surroundWith '|') centeredMessageLines
                surroundWith item = flip (surround) item


-- Wyswietlenie menu
showMenuBox menuItems = do
        putStr (menuBox menuItems)
        function <- showMenuOptionInputBox menuItems
        return function

-- Wyswietlenie polecenia zachety dla wyboru pozycji z menu
showMenuOptionInputBox menuItems = do
        input <- showInputBox "Podaj numer opcji"
        let optionNumber = readMaybe input
        if (isNothing optionNumber) || (notElem (fromJust optionNumber) [1..(length menuItems)])
            then
                do
                        putStrLn "Nieprawidłowy numer opcji"
                        showMenuOptionInputBox menuItems
            else
                return (snd (menuItems !! ((fromJust optionNumber) - 1)))


-- Wyświetlenie polecenia zachęty
inputBox message = message ++ ": "
showInputBox message = do
        putStrLn (inputBox message)
        input <- getLine
        let strippedInput = strip input
        if null strippedInput
            then
               showInputBox message
            else
                return strippedInput


-- Stworzenie menu
menuBox menuItems =
        unlines numberedMenuItemTexts
        where
                menuItemTexts = map fst menuItems
                numberedMenuItemTexts = map (\(x, y) -> (show x) ++ ". " ++ y) (enumerate menuItemTexts 1)

-- Wyswietlenie komunikatu
showMessageBox message = putStr (messageBox message)

-- Pobranie nazwy obiektu z funkcją weryfikującą
getObjectName objectName message = do
    objectName <- showInputBox message
    return (Just objectName)

-- Wyświetlenie polecenia zachęty dla wczytania pliku
showFileInputBox = showInputBox "Podaj ścieżkę do pliku"


