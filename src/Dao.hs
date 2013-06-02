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



---Dodanie osoby
addPerson (AddressBook persons  groups)  = do
  maybeFirstName <- getObjectName firstName "Podaj imie"
  maybeLastName <- getObjectName lastName "Podaj nazwisko"
  maybeCompanyName <- getObjectName companyName "Podaj nazwe firmy"
  maybePhone <- getObjectName phoneNumber "Podaj numer telefonu"
  maybeMail <- getObjectName eMail "Podaj adres email"
  maybeBirthdayString <- getObjectName birthDay "Podaj date urodzenia (wymagany format to YYYY-MM-DD, np. 1980-04-20)"
  let matchedDate = matchDate (fromJust maybeBirthdayString)
  let maybeBirthday = parseDate (matchedDate);


  -- walidacja daty
  if  ( isNothing maybeBirthday )
    then
        showError "Niewłaściwy format daty"
    else if ( validateMatchedDate matchedDate ( fromJust maybeBirthday ) )
        then do
            let maybeBirthday = Nothing
            showError "Podano nieprawidłową datę"
        else
            putStr ""

  -- walidacja maila  - musi byc unikalny



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


--    let birthDay = (20, 02, 1638);
    --let birthDay = fromGregorian 1988 20 02
    let newPersons = doAddPerson ( fromJust maybeFirstName)
                                ( fromJust maybeLastName)
                                ( fromJust maybeCompanyName)
                                ( fromJust maybePhone)
                                ( fromJust maybeMail)
                                ( fromJust maybeBirthday )
                                persons

    showMessageBox operationSuccessStr
    return (AddressBook newPersons  groups)

doAddPerson  firstName lastName companyName phone email birthDay persons = do
        [(Person firstName lastName companyName phone email birthDay [])] ++ persons



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

