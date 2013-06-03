-----------------------------------------------------------------------------
--
-- Module      :  Strings
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

module Strings where

-- Do jednolitego wyswietlania komunikatow bledow
errorStr msg = "Błąd: " ++ msg
showError msg = putStrLn (errorStr msg)



welcomeInfo = "SPOP2013L - Marcin Włodarczyk && Piotr Cebulski"


operationSuccessStr = "Pomyślnie wykonano operację"
operationFailedStr = "Wykonanie operacji nie powiodło się..."


typeFirstName = "Podaj imię"
typeLastName = "Podaj nazwisko"
typeGroupId = "Podaj id grupy"
typeContactId = "Podaj id kontaktu"
typeCompanyName = "Podaj nazwę firmy"
typePhone = "Podaj numer telefonu"
typeEmail = "Podaj adres e-mail"
typeBirthday = "Podaj datę urodzenia (wymagany format to YYYY-MM-DD, np. 1980-04-20)"
typeGroupName = "Podaj nazwę grupy"


wrongEmail = "Podano nieprawidłowy adres e-mail"
wrongDate = "Podano nieprawidłową datę"
wrongIdStr = "Podano nieprawidłowy identyfikator"
wrongPersonIdStr = "Podano nieprawidłowy identyfikator osoby"
wrongGroupIdStr = "Podano nieprawidłowy identyfikator grupy"

contactIdNotFoundStr = "Nie znaleziono kontaktu o wskazanym identyfikatorze"
groupIdNotFound = "Nie znaleziono grupy o wskazanym identyfikatorze"



--Błędy IO
invalidFormatErrorStr =  errorStr "Nieprawidłowy format danych"
cannotOpenFileErrorStr = errorStr "Nie można odczytać pliku"



