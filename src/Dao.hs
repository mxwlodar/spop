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

-- zwraca grupe na podstawie identyfikatora
getGroupById:: Int -> [Group] -> Maybe Group
getGroupById id [] = Nothing
getGroupById id (g:gs) = if (id == getGroupId g )
                                then
                                    Just g
                                else
                                    getGroupById id gs

-- zwraca grupe na podstawie nazwy
getGroupByName :: String -> [Group] -> Maybe Group
getGroupByName name [] = Nothing
getGroupByName name (g:gs) = if (name == getGroupName g )
                                then
                                    Just g
                                else
                                    getGroupByName name gs

---Dodanie osoby
addPerson (AddressBook persons  groups)  = do
  maybeFirstName <- getObjectName firstName typeFirstName
  maybeLastName <- getObjectName lastName typeLastName
  maybeCompanyName <- getObjectName companyName typeCompanyName
  maybePhone <- getObjectName phoneNumber typePhone
  maybeEmailString <- getObjectName eMail typeEmail
  maybeBirthdayString <- getObjectName birthDay typeBirthday

  let maybeEmail = parseEmail (fromJust maybeEmailString)
  let maybeBirthday = getDateWithValidation (  fromJust maybeBirthdayString )

  --sprawdzanie maila
  if  ( isNothing maybeEmail )
        then do
            showError wrongEmail
        else
            putStr ""

  -- walidacja daty
  if  ( isNothing maybeBirthday )
        then do
            showError wrongDate
        else
            putStr ""


  if isNothing maybeFirstName ||
     isNothing maybeLastName ||
     isNothing maybeCompanyName ||
     isNothing maybePhone ||
     isNothing maybeEmail ||
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
                                ( fromJust maybeEmail)
                                ( fromJust maybeBirthday )
                                persons
    showMessageBox operationSuccessStr
    return (AddressBook newPersons  groups)

doAddPerson  id firstName lastName companyName phone email birthDay persons = do
        [(Person id firstName lastName companyName phone email birthDay [])] ++ persons


modifyPersonAction (AddressBook persons  groups)  = do
    showContactsAction (AddressBook persons groups)
    objectName <- showInputBox typeContactId
    let id = parseInt ( fromJust (Just objectName) )

    if (id == 0)
        then do
            showError wrongIdStr
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybePerson = getPersonById id persons

            if ( isNothing maybePerson )
                then do
                    showError contactIdNotFoundStr
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do --tutaj modyfikacja
                  maybeFirstName <- getObjectName firstName typeFirstName
                  maybeLastName <- getObjectName lastName typeLastName
                  maybeCompanyName <- getObjectName companyName typeCompanyName
                  maybePhone <- getObjectName phoneNumber typePhone
                  maybeEmailString <- getObjectName eMail typeEmail
                  maybeBirthdayString <- getObjectName birthDay typeBirthday

                  let maybeEmail = parseEmail (fromJust maybeEmailString)
                  let maybeBirthday = getDateWithValidation (  fromJust maybeBirthdayString )

                  --sprawdzanie maila
                  if  ( isNothing maybeEmail )
                        then do
                            showError wrongEmail
                        else
                            putStr ""

                  -- walidacja daty
                  if  ( isNothing maybeBirthday )
                        then do
                            showError wrongDate
                        else
                            putStr ""


                  if isNothing maybeFirstName ||
                     isNothing maybeLastName ||
                     isNothing maybeCompanyName ||
                     isNothing maybePhone ||
                     isNothing maybeEmail ||
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
                                ( fromJust maybeEmail)
                                ( fromJust maybeBirthday )
                                ( getPersonGroups person)
                                )
                       let modifiedPersons = replaceNth index modifiedPerson persons
                       showMessageBox operationSuccessStr
                       return (AddressBook modifiedPersons  groups)




--usuwa kontakt
deletePersonAction (AddressBook persons groups) = do
    showContactsAction (AddressBook persons groups)
    objectName <- showInputBox typeContactId
    let id = parseInt ( fromJust (Just objectName) )

    if (id == 0)
        then do
            showError wrongIdStr
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybePerson = getPersonById id persons

            if ( isNothing maybePerson )
                then do
                    showError contactIdNotFoundStr
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
    showContacts birthDayPersons groups
    return (AddressBook persons groups)

-- sprawdza czy osoba ma dzisiaj urodziny
hasBirthDayAtDate :: Person -> Day -> Bool
hasBirthDayAtDate p date = (mp == m) && (dp == d)
                where
                    (y, m, d) = toGregorian date
                    (yp, mp, dp) = toGregorian (getPersonBirthday p)


-- wyswietlenie listy kontaktow
showContactsAction (AddressBook persons groups) = do
            showContacts persons groups
            return (AddressBook persons groups)

-- wywietla kontakty
showContacts [] _ = showMessageBox "Lista kontaktów:"
showContacts (p:ps) allGroups = do
            showContacts ps allGroups
            showContact p allGroups


-- wyswietla kontakt
showContact (Person id firstName lastName companyName phone email birthDay groups) allGroups = do
            putStrLn ("id: " ++ (show id))
            putStrLn ("Imię i nazwisko: " ++firstName ++ " " ++ lastName)
            putStrLn ("Firma: " ++ companyName)
            putStrLn ("Telefon: " ++ phone)
            putStrLn ("Email: " ++ email)
            putStrLn ("Data urodzenia: " ++ (show birthDay) )
            putStr ("Grupy: " )
            showGroupsWithDescription groups allGroups
            putStrLn ("\n\n" )

showGroupsWithDescription [] _ = putStr ""
showGroupsWithDescription (id:ids) allGroups = do
            let g = fromJust( getGroupById id allGroups)
            showGroupsWithDescription ids allGroups
            putStr ( (show (getGroupId g)) ++  "-" ++ (getGroupName g) ++ "     " )



--GRUPY
----------------------------------------------------------------------------------------------------
-- wyswietlenie listy grup
showGroupsAction (AddressBook persons groups) = do
            showGroups (groups)
            return (AddressBook persons groups)

-- wywietla grupy
showGroups [] = showMessageBox "Lista grup:"
showGroups (g:gs) = do
            showGroups(gs)
            showGroup(g)


-- wyswietla grupe
showGroup (Group groupId groupName) = do
            putStrLn ("id: " ++ (show groupId))
            putStrLn ("Nazwa: " ++groupName ++ "\n")

---Dodanie grupy
addGroup (AddressBook persons  groups)  = do
  maybeGroupName <- getObjectName groupName typeGroupName

  if isNothing maybeGroupName
    then do
       showMessageBox operationFailedStr
       return (AddressBook persons  groups)
    else do

    let newGroups = doAddGroup (getNextGroupId groups)
                               ( fromJust maybeGroupName)
                                groups

    showMessageBox operationSuccessStr
    showGroupsAction (AddressBook persons newGroups)
    return (AddressBook persons  newGroups)

doAddGroup groupId groupName groups = do
        [(Group groupId groupName)] ++ groups

--usuwa grupe
deleteGroupAction (AddressBook persons groups) = do
    showGroupsAction (AddressBook persons groups)
    objectName <- showInputBox typeGroupId
    let groupId = parseInt ( fromJust (Just objectName) )

    if (groupId == 0)
        then do
            showError wrongIdStr
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybeGroup = getGroupById groupId groups

            if ( isNothing maybeGroup )
                then do
                    showError groupIdNotFound
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do
                    let restGroups = removeItem (fromJust maybeGroup) groups
                    let newPersons = deleteGroup groupId persons
                    showMessageBox operationSuccessStr
                    return (AddressBook newPersons restGroups)

deleteGroup _ [] = []
deleteGroup groupId (p:ps) = do
        let newGroupList = delete groupId (getPersonGroups p)
        let modifiedPerson = (Person (getPersonId p)
                            ( getPersonFirstName p)
                            ( getPersonLastName p)
                            ( getPersonCompanyName p)
                            ( getPersonPhoneNumber p)
                            ( getPersonEmail p)
                            ( getPersonBirthday p)
                            ( newGroupList)
                            )
        modifiedPerson : (deleteGroup groupId ps)


--dodaje osobe do grupy
addPersonToGroup (AddressBook persons groups) = do
    showGroupsAction (AddressBook persons groups)
    objectName <- showInputBox typeContactId
    let id = parseInt ( fromJust (Just objectName) )

    if (id == 0)
        then do
            showError wrongPersonIdStr
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybePerson = getPersonById id persons

            if ( isNothing maybePerson )
                then do
                    showError contactIdNotFoundStr
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do
                    objectName2 <- showInputBox typeGroupId
                    let groupId = parseInt ( fromJust (Just objectName2) )

                    if (groupId == 0)
                        then do
                            showError wrongGroupIdStr
                            showMessageBox operationFailedStr
                            return (AddressBook persons groups)
                        else do
                            let maybeGroup = getGroupById groupId groups

                            if ( isNothing maybeGroup )
                                then do
                                    showError groupIdNotFound
                                    showMessageBox operationFailedStr
                                    return (AddressBook persons groups)
                                else do
                                    if (elem groupId (getPersonGroups (fromJust maybePerson)))
                                        then do
                                            showError "Kontakt jest już przypisany do tej grupy"
                                            showMessageBox operationFailedStr
                                            return (AddressBook persons groups)
                                        else do
                                            let person = fromJust maybePerson
                                            let newGroupList = groupId : (getPersonGroups person)
                                            let index = fromJust( elemIndex person persons)
                                            let modifiedPerson = (Person (getPersonId person)
                                                                ( getPersonFirstName person)
                                                                ( getPersonLastName person)
                                                                ( getPersonCompanyName person)
                                                                ( getPersonPhoneNumber person)
                                                                ( getPersonEmail person)
                                                                ( getPersonBirthday person)
                                                                ( newGroupList)
                                                                )
                                            let modifiedPersons = replaceNth index modifiedPerson persons
                                            showMessageBox operationSuccessStr
                                            return (AddressBook modifiedPersons  groups)

--usuwa osobe z grupy
deletePersonFromGroup (AddressBook persons groups) = do
    objectName <- showInputBox typeContactId
    let id = parseInt ( fromJust (Just objectName) )

    if (id == 0)
        then do
            showError wrongPersonIdStr
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybePerson = getPersonById id persons

            if ( isNothing maybePerson )
                then do
                    showError contactIdNotFoundStr
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do
                    showContact (fromJust maybePerson) groups
                    objectName2 <- showInputBox typeGroupId
                    let groupId = parseInt ( fromJust (Just objectName2) )

                    if (groupId == 0)
                        then do
                            showError wrongGroupIdStr
                            showMessageBox operationFailedStr
                            return (AddressBook persons groups)
                        else do
                            if (notElem groupId (getPersonGroups (fromJust maybePerson)))
                                then do
                                    showError "Kontakt nie jest przypisany do tej grupy"
                                    showMessageBox operationFailedStr
                                    return (AddressBook persons groups)
                                else do
                                let person = fromJust maybePerson
                                let newGroupList = delete groupId (getPersonGroups person)
                                let index = fromJust( elemIndex person persons)
                                let modifiedPerson = (Person (getPersonId person)
                                                    ( getPersonFirstName person)
                                                    ( getPersonLastName person)
                                                    ( getPersonCompanyName person)
                                                    ( getPersonPhoneNumber person)
                                                    ( getPersonEmail person)
                                                    ( getPersonBirthday person)
                                                    ( newGroupList)
                                                    )
                                let modifiedPersons = replaceNth index modifiedPerson persons
                                showMessageBox operationSuccessStr
                                return (AddressBook modifiedPersons  groups)

--zmiana nazwy grupy
modifyGroupAction (AddressBook persons groups) = do
    showGroupsAction (AddressBook persons groups)
    objectName <- showInputBox typeGroupId
    let groupId = parseInt ( fromJust (Just objectName) )

    if (groupId == 0)
        then do
            showError wrongIdStr
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybeGroup = getGroupById groupId groups

            if ( isNothing maybeGroup )
                then do
                    showError groupIdNotFound
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do
                  maybeGroupName <- getObjectName groupName "Podaj nową nazwę grupy"
                  if isNothing maybeGroupName
                    then do
                       showMessageBox operationFailedStr
                       return (AddressBook persons  groups)
                    else do
                    let group = fromJust maybeGroup
                    let index = fromJust( elemIndex group groups)
                    let modifiedGroup = (Group (getGroupId group)
                                        (fromJust maybeGroupName)
                                        )
                    let modifiedGroups = replaceNth index modifiedGroup groups
                    showMessageBox operationSuccessStr
                    return (AddressBook persons modifiedGroups)

--scalanie grup
joinGroups (AddressBook persons groups) = do
    showGroupsAction (AddressBook persons groups)
    objectName1 <- showInputBox "Podaj id pierwszej grupy"
    let groupId1 = parseInt ( fromJust (Just objectName1) )

    if (groupId1 == 0)
        then do
            showError wrongIdStr
            showMessageBox operationFailedStr
            return (AddressBook persons groups)
        else do
            let maybeGroup1 = getGroupById groupId1 groups
            if ( isNothing maybeGroup1 )
                then do
                    showError groupIdNotFound
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do
                    objectName2 <- showInputBox "Podaj id drugiej grupy"
                    let groupId2 = parseInt ( fromJust (Just objectName2) )

                    if (groupId2 == 0)
                        then do
                            showError wrongIdStr
                            showMessageBox operationFailedStr
                            return (AddressBook persons groups)
                        else do
                            let maybeGroup2 = getGroupById groupId2 groups
                            if ( isNothing maybeGroup2 )
                                then do
                                    showError groupIdNotFound
                                    showMessageBox operationFailedStr
                                    return (AddressBook persons groups)
                                else do
                                    maybeGroupName <- getObjectName groupName "Podaj nazwę nowej grupy"
                                    if isNothing maybeGroupName
                                        then do
                                           showMessageBox operationFailedStr
                                           return (AddressBook persons  groups)
                                        else do
                                            let restGroups = removeItem (fromJust maybeGroup1) groups
                                            let restGroups1 = removeItem (fromJust maybeGroup2) restGroups
                                            let newGroups = doAddGroup (getNextGroupId groups)
                                                                       (fromJust maybeGroupName)
                                                                       restGroups1
                                            let newPersons = doJoinGroups groupId1
                                                                          groupId2
                                                                          (getNextGroupId groups)
                                                                          persons
                                            showMessageBox operationSuccessStr
                                            return (AddressBook newPersons newGroups)
doJoinGroups _ _ _ [] = []
doJoinGroups groupId1 groupId2 newGroupId (p:ps) = do
    if elem groupId1 (getPersonGroups p) ||
       elem groupId2 (getPersonGroups p)
        then do
            let newGroupList = delete groupId1 (getPersonGroups p)
            let newGroupList2 = delete groupId2 newGroupList
            let modifiedPerson = (Person (getPersonId p)
                            ( getPersonFirstName p)
                            ( getPersonLastName p)
                            ( getPersonCompanyName p)
                            ( getPersonPhoneNumber p)
                            ( getPersonEmail p)
                            ( getPersonBirthday p)
                            ( newGroupId : newGroupList2)
                            )
            modifiedPerson : (doJoinGroups groupId1 groupId2 newGroupId ps)
        else do
            p : (doJoinGroups groupId1 groupId2 newGroupId ps)

--wyswietla kontakty w danej grupie
showGroupAction (AddressBook persons groups) = do
    showGroupsAction (AddressBook persons groups)
    maybeGroupName <- getObjectName groupName typeGroupName
    if isNothing maybeGroupName
        then do
           showMessageBox operationFailedStr
           return (AddressBook persons  groups)
        else do

            let maybeGroup = getGroupByName (fromJust maybeGroupName) groups
            if ( isNothing maybeGroup )
                then do
                    showError "Nie ma takiej grupy"
                    showMessageBox operationFailedStr
                    return (AddressBook persons groups)
                else do
                    let personsToShow = doShowGroup (getGroupId (fromJust maybeGroup)) persons
                    showContacts personsToShow groups
                    return (AddressBook persons  groups)


doShowGroup _ [] = []
doShowGroup groupId (p:ps) = do
    if elem groupId (getPersonGroups p)
        then do
            p : doShowGroup groupId ps
        else do
            doShowGroup groupId ps

--Wyszukiwanie kontaktow
----------------------------------------------------------------------------------------------------
searchByFirstName (AddressBook persons groups) = do
    maybeFirstName <- getObjectName groupName typeFirstName
    if isNothing maybeFirstName
        then do
            showMessageBox operationFailedStr
            return (AddressBook persons  groups)
        else do
            let searchPersons = filter (\p -> (getPersonFirstName p) == (fromJust maybeFirstName)) persons
            showContacts searchPersons groups
            return (AddressBook persons  groups)

searchByLastName (AddressBook persons groups) = do
    maybeLastName <- getObjectName groupName typeLastName
    if isNothing maybeLastName
        then do
            showMessageBox operationFailedStr
            return (AddressBook persons  groups)
        else do
            let searchPersons = filter (\p -> (getPersonLastName p) == (fromJust maybeLastName)) persons
            showContacts searchPersons groups
            return (AddressBook persons  groups)

searchByCompanyName (AddressBook persons groups) = do
    maybeCompanyName <- getObjectName groupName typeCompanyName
    if isNothing maybeCompanyName
        then do
            showMessageBox operationFailedStr
            return (AddressBook persons  groups)
        else do
            let searchPersons = filter (\p -> (getPersonCompanyName p) == (fromJust maybeCompanyName)) persons
            showContacts searchPersons groups
            return (AddressBook persons  groups)

searchByPhoneNumber (AddressBook persons groups) = do
    maybePhoneNumber <- getObjectName groupName typePhone
    if isNothing maybePhoneNumber
        then do
            showMessageBox operationFailedStr
            return (AddressBook persons  groups)
        else do
            let searchPersons = filter (\p -> (getPersonPhoneNumber p) == (fromJust maybePhoneNumber)) persons
            showContacts searchPersons groups
            return (AddressBook persons  groups)

searchByEmail (AddressBook persons groups) = do
    maybeEmail <- getObjectName groupName typeEmail
    if isNothing maybeEmail
        then do
            showMessageBox operationFailedStr
            return (AddressBook persons  groups)
        else do
            let searchPersons = filter (\p -> (getPersonEmail p) == (fromJust maybeEmail)) persons
            showContacts searchPersons groups
            return (AddressBook persons  groups)

searchByBirthday (AddressBook persons groups) = do
    maybeBirthdayString <- getObjectName birthDay typeBirthday
    let maybeBirthday = getDateWithValidation (  fromJust maybeBirthdayString )
    if isNothing maybeBirthday
        then do
            showError wrongDate
            showMessageBox operationFailedStr
            return (AddressBook persons  groups)
        else do
            let searchPersons = filter (\p -> (getPersonBirthday p) == (fromJust maybeBirthday)) persons
            showContacts searchPersons groups
            return (AddressBook persons  groups)
----------------------------------------------------------------------------------------------------

-- wyswietlenie calej ksiazki adresowej
showAddressBookAction (AddressBook persons groups) = do
            showContacts (persons) groups
            showGroups (groups)
            return (AddressBook persons groups)



-- Zapis i danych z/do pliku
----------------------------------------------------------------------------------------------------

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
