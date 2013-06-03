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
import Data.List
import Types
import Strings
import Numeric
import Ui
import Utils
import Data.Typeable




-- stworzenie nowej pustej ksiazki adresowej
createEmptyAddressBook _ = return emptyAddressBook


-- wygeneruj nastepny id osoby
getNextPersonId:: [Person] -> Int
getNextPersonId [] = 1;
getNextPersonId (p:ps) = (max (getPersonId p) (getNextPersonId ps))+1 ;

-- zwraca osobe na podstawie identyfikatora
getPersonById:: Int -> [Person] -> Maybe Person
getPersonById id [] = Nothing
getPersonById id (p:ps) = if (id == getPersonId p )
                                then
                                    Just p
                                else
                                    getPersonById id ps

-- wygeneruj nastepny id grupy
getNextGroupId:: [Group] -> Int
getNextGroupId [] = 1;
getNextGroupId (g:gs) = (max (getGroupId g) (getNextGroupId gs))+1 ;

-- zwraca osobe na podstawie identyfikatora
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


--modyfikacja kontaktu
modifyPersonAction (AddressBook persons  groups)  = do
    showContactsReverse (AddressBook persons groups)
    objectName <- showInputBox "Podaj id kontkatu"
    let id = parseInt ( fromJust (Just objectName) )

    if (id == 0)
        then do
            showError "Podano nieprawidłowy identyfikator"
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybePerson = getPersonById id persons

            if ( isNothing maybePerson )
                then do
                    showError "Nie znaleziono kontaktu o wskazanym identyfikatorze"
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do --tutaj modyfikacja
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
                    else do -- stworzenie nowej osoby i zastapienie nia starej
                       let person = fromJust maybePerson
                       let index = fromJust( elemIndex person persons)
                       let modifiedPerson = (Person (getPersonId person)
                                ( fromJust maybeFirstName)
                                ( fromJust maybeLastName)
                                ( fromJust maybeCompanyName)
                                ( fromJust maybePhone)
                                ( fromJust maybeMail)
                                ( fromJust maybeBirthday )
                                ( getPersonGroups person)
                                )
                       let modifiedPersons = replaceNth index modifiedPerson persons
                       showMessageBox operationSuccessStr
                       return (AddressBook modifiedPersons  groups)




--usuwa kontakt
deletePersonAction (AddressBook persons groups) = do
    showContactsReverse (AddressBook persons groups)
    objectName <- showInputBox "Podaj id kontkatu"
    let id = parseInt ( fromJust (Just objectName) )

    if (id == 0)
        then do
            showError "Podano nieprawidłowy identyfikator"
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybePerson = getPersonById id persons

            if ( isNothing maybePerson )
                then do
                    showError "Nie znaleziono kontaktu o wskazanym identyfikatorze"
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do
                    let restPersons = removeItem (fromJust maybePerson) persons
                    showMessageBox operationSuccessStr
                    return (AddressBook restPersons groups)


-- wyswietla osoby obchodzace urodziny
showPersonsBirthdayAction (AddressBook persons groups) = do
    showMessageBox "Obchodzący dzisiaj urodziny:"
    (y,m,d) <- getCurrentDate
    let birthDayPersons = filter (\p -> hasBirthDayAtDate p (fromGregorian y m d)) persons
    showContacts (reverse birthDayPersons)
    return (AddressBook persons groups)

-- sprawdza czy osoba ma dzisiaj urodziny
hasBirthDayAtDate :: Person -> Day -> Bool
hasBirthDayAtDate p date = (yp == y) && (mp == m) && (dp == d)
                where
                    (y, m, d) = toGregorian date
                    (yp, mp, dp) = toGregorian (getPersonBirthday p)


-- wyswietlenie listy kontaktow
showContactsReverse (AddressBook persons groups) = do
            showMessageBox "Lista kontaktow:"
            showContacts (reverse persons)
            return (AddressBook persons groups)

-- wywietla kontakty
showContacts [] = putStr "";
showContacts (p:ps) = do
            showContact(p)
            showContacts (ps)

-- wyswietla kontakt
showContact (Person id firstName lastName companyName phone email birthDay []) = do
            putStrLn ("id: " ++ (show id))
            putStrLn ("Imię i nazwisko: " ++firstName ++ " " ++ lastName)
            putStrLn ("Firma: " ++ companyName)
            putStrLn ("Telefon: " ++ phone)
            putStrLn ("Email: " ++ email)
            putStrLn ("Data urodzenia: " ++ (show birthDay) ++ "\n\n" )


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

