{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where


import System.Exit
import Ui
import Strings
import Utils
import Types
import Dao

main = do
      showMessageBox welcomeInfo
      mainLoop null

--Główna pętla
mainLoop model = do
        printNewLine
        function <- showMainMenu
        model <- function model
        mainLoop model

showMainMenu = do
         showMenuBox [("Zarządzanie kontaktami", showSubmenuContactsLoop),
            ("Zarządzanie grupami", showSubmenuGroupsLoop),
            ("Wyszukiwanie kontaktów",todo),
            ("Wyświetlenie osób obchodzących dzisiaj urodziny",todo),
            ("Zapis danych do pliku",  todo),
            ("Odczyt danych z pliku", todo),
            ("Wyczyszczenie danych", todo),
            ("Zakończ", exit)]


--------------------------------------------------------------------------------

--- Zarzadanie kontaktami
showSubmenuContactsLoop model = do
      printNewLine
      model <- submenuContacts model
      showSubmenuContactsLoop model

--Wyświetla podmenu zarzadzania kontaktami
submenuContacts model = do
      function <- showMenuBox[("Dodaj nowy kontakt", todo),
            ("Modyfikuj kontakt", todo),
            ("Usuń kontakt", todo),
            ("Przypisz kontakt do grupy", todo),
            ("Usuń z grupy", todo),
            ("Powrót do menu głównego", mainLoop)]
      model <- function model
      return model


--------------------------------------------------------------------------------

--- Zarzadanie grupami
showSubmenuGroupsLoop model = do
      printNewLine
      model <- submenuGroups model
      showSubmenuGroupsLoop model

--Wyświetla podmenu zarzadzania kontaktami
submenuGroups model = do
      function <- showMenuBox[("Dodaj nową grupę", todo),
            ("Modyfikuj grupę", todo),
            ("Usuń grupę", todo),
            ("Scal dwie grupy w nową grupę", todo),
            ("Powrót do menu głównego", mainLoop)]
      model <- function model
      return model

--------------------------------------------------------------------------------


-- Wyjscie z programu
exit _ = exitWith ExitSuccess
