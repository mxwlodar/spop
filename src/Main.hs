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
      mainLoop emptyAddressBook

--Główna pętla
mainLoop addressBook = do
        printNewLine
        function <- showMainMenu
        addressBook <- function addressBook
        mainLoop addressBook

showMainMenu = do
         showMenuBox [("Zarządzanie kontaktami", showSubmenuContactsLoop),
            ("Zarządzanie grupami", showSubmenuGroupsLoop),
            ("Wyszukiwanie kontaktów",todo),
            ("Wyświetlenie osób obchodzących dzisiaj urodziny",todo),
            ("Pokaz wszystkie dane",  showContactsReverse),
            ("Zapis danych do pliku",  saveData),
            ("Odczyt danych z pliku", loadData),
            ("Wyczyszczenie danych", createEmptyAddressBook),
            ("Zakończ", exit)]


--------------------------------------------------------------------------------

--- Zarzadanie kontaktami
showSubmenuContactsLoop addressBook = do
      printNewLine
      addressBook <- submenuContacts addressBook
      showSubmenuContactsLoop addressBook

--Wyświetla podmenu zarzadzania kontaktami
submenuContacts addressBook = do
      function <- showMenuBox[("Dodaj nowy kontakt", addPerson),
            ("Modyfikuj kontakt", todo),
            ("Usuń kontakt", deletePersonAction),
            ("Przypisz kontakt do grupy", todo),
            ("Usuń z grupy", todo),
            ("Powrót do menu głównego", mainLoop)]
      addressBook <- function addressBook
      return addressBook


--------------------------------------------------------------------------------

--- Zarzadanie grupami
showSubmenuGroupsLoop addressBook = do
      printNewLine
      model <- submenuGroups addressBook
      showSubmenuGroupsLoop addressBook

--Wyświetla podmenu zarzadzania grupami
submenuGroups addressBook = do
      function <- showMenuBox[("Dodaj nową grupę", todo),
            ("Modyfikuj grupę", todo),
            ("Usuń grupę", todo),
            ("Scal dwie grupy w nową grupę", todo),
            ("Powrót do menu głównego", mainLoop)]
      addressBook <- function addressBook
      return addressBook

--------------------------------------------------------------------------------


-- Wyjscie z programu
exit _ = exitWith ExitSuccess
