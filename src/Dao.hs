-----------------------------------------------------------------------------
--
-- Module      :  Dao
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

module Dao where
import Data.Maybe
import Data.Time
import Types
import Strings
import Ui
import Utils





-- tutaj beda funkcje dostepu i manipulacji na danych



-- stworzenie nowej pustej ksiazki adresowej
createEmptyAddressBook _ = return emptyAddressBook


--
getNextPersonId:: [Person] -> Int
getNextPersonId [] = 1;
getNextPersonId (p:ps) = (max (getPersonId p) (getNextPersonId ps))+1 ;

getPersonById:: Int -> [Person] -> Maybe Person
getPersonById id [] = Nothing
getPersonById id (p:ps) = if (id == getPersonId p )
                                then
                                    Just p
                                else
                                    getPersonById id ps

getNextGroupId:: [Group] -> Int
getNextGroupId [] = 1;
getNextGroupId (g:gs) = (max (getGroupId g) (getNextGroupId gs))+1 ;

getGroupById:: Int -> [Group] -> Maybe Group
getGroupById id [] = Nothing
getGroupById id (g:gs) = if (id == getGroupId g )
                                then
                                    Just g
                                else
                                    getGroupById id gs

---Dodanie osoby
addPerson (AddressBook persons  groups)  = do
  maybeFirstName <- getObjectName firstName "Podaj imie"
  maybeLastName <- getObjectName lastName "Podaj nazwisko"
  maybeCompanyName <- getObjectName companyName "Podaj nazwe firmy"
  maybePhone <- getObjectName phoneNumber "Podaj numer telefonu"
  maybeMail <- getObjectName eMail "Podaj adres email"
  maybeBirthdayString <- getObjectName birthDay "Podaj date urodzenia (wymagany format to YYYY-MM-DD, np. 1980-04-20)"

  let maybeBirthday = getDateWithValidation (  fromJust maybeBirthdayString )


  -- walidacja daty
  if  ( isNothing maybeBirthday )
        then do
            showError "Podano nieprawidłową datę"
        else
            putStr ""


  if isNothing maybeFirstName ||
     isNothing maybeLastName ||
     isNothing maybeCompanyName ||
     isNothing maybePhone ||
     isNothing maybeMail ||
     isNothing maybeBirthday
    then do
       showMessageBox operationFailedStr
       return (AddressBook persons  groups)
    else do



    let newPersons = doAddPerson (getNextPersonId persons)
                                ( fromJust maybeFirstName)
                                ( fromJust maybeLastName)
                                ( fromJust maybeCompanyName)
                                ( fromJust maybePhone)
                                ( fromJust maybeMail)
                                ( fromJust maybeBirthday )
                                persons

    showMessageBox operationSuccessStr
    return (AddressBook newPersons  groups)

doAddPerson  id firstName lastName companyName phone email birthDay persons = do
        [(Person id firstName lastName companyName phone email birthDay [])] ++ persons




-- Zapis danych do pliku
saveData addressBook = do
  filePath <- showFileInputBox
  saveToFile addressBook filePath
  showMessageBox operationSuccessStr
  return addressBook


-- Wczytanie danych z pliku
loadData addressBook = doLoadData addressBook
  where
    doLoadData addressBook = do
      filePath <- showFileInputBox
      maybeAddressBook <- loadFromFile filePath readmaybeAddressBook
      if isNothing maybeAddressBook
          then do
            showMessageBox invalidFormatErrorStr
            return addressBook
          else do
            showMessageBox operationSuccessStr
            let newAddressBook = fromJust maybeAddressBook
            return newAddressBook
                 where
                     readmaybeAddressBook :: String -> Maybe AddressBook
                     readmaybeAddressBook = readMaybe

