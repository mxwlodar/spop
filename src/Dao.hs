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
import Types
import Strings
import Ui


-- tutaj beda funkcje dostepu i manipulacji na danych

---Dodanie osoby
addPerson (AddressBook persons  groups)  = do
  maybeFirstName <- getObjectName firstName "Podaj imie"
  maybeLastName <- getObjectName lastName "Podaj nazwisko"
  maybeCompanyName <- getObjectName companyName "Podaj nazwe firmy"
  maybePhone <- getObjectName phoneNumber "Podaj numer telefonu"
  maybeMail <- getObjectName eMail "Podaj adres email"
  maybeBirthday <- getObjectName birthDay "Podaj date urodzenia"
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
    let birthday = parseDate ( fromJust maybeBirthday);
    let newPerson = doAddPerson ( fromJust maybeFirstName)
                                ( fromJust maybeLastName)
                                ( fromJust maybeCompanyName)
                                ( fromJust maybePhone)
                                ( fromJust maybeMail)
                                birthday persons
    showMessageBox operationSuccessStr
    return (AddressBook persons  groups)

doAddPerson  firstName lastName companyName phone email birthDay persons = do
        [(Person firstName lastName companyName phone email birthDay [])] ++ persons


