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


--Błędy IO
invalidFormatErrorStr =  errorStr "Nieprawidłowy format danych"
cannotOpenFileErrorStr = errorStr "Nie można odczytać pliku"
