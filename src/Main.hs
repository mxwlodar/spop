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
            ("Wyświetlenie osób obchodzących dzisiaj urodziny",showPersonsBirthdayAction),
            ("Pokaż wszystkie dane",  showAddressBookAction),
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
            ("Modyfikuj kontakt", modifyPersonAction),
            ("Usuń kontakt", deletePersonAction),
            ("Wyświetl wszystkie kontakty",  showContactsAction),
            ("Przypisz kontakt do grupy", addPersonToGroup),
            ("Usuń z grupy", deletePersonFromGroup),
            ("Powrót do menu głównego", mainLoop)]
      addressBook <- function addressBook
      return addressBook


--------------------------------------------------------------------------------

--- Zarzadanie grupami
showSubmenuGroupsLoop addressBook = do
      printNewLine
      addressBook <- submenuGroups addressBook
      showSubmenuGroupsLoop addressBook

--Wyświetla podmenu zarzadzania grupami
submenuGroups addressBook = do
      function <- showMenuBox[("Dodaj nową grupę", addGroup),
            ("Modyfikuj grupę", todo),
            ("Usuń grupę", deleteGroupAction),
            ("Wyświetl wszystkie grupy", showGroupsAction),
            ("Scal dwie grupy w nową grupę", todo),
            ("Powrót do menu głównego", mainLoop)]
      addressBook <- function addressBook
      return addressBook

--------------------------------------------------------------------------------


-- Wyjscie z programu
exit _ = exitWith ExitSuccess
