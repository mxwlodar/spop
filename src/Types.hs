-----------------------------------------------------------------------------
--
-- Module      :  Types
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

module Types where
import Data.Time
import Data.Maybe
import Data.Char
import Numeric
import Text.Regex.Posix


-- tutaj beda zdefiniowane nasze typy danych



--dopasowuje date za pomoca wyrazenia regularnego
matchDate :: String  -> [String]
matchDate str = getAllTextMatches $ str =~ "[0-9]+" :: [String]

--parsuje date za pomoca listy dopasowania z wyrazenia regularnego
parseDate :: [String] -> Maybe Day
parseDate (y:m:d:_) = Just (fromGregorian (toInteger (read y ::Int)) (read m ::Int) (read d ::Int))
parseDate _ = Nothing

--sprawdza czy sparsowana data jest zgodna z ta wpisana w stringu - eleminuje daty typu 30 lutego
validateMatchedDate :: [String] -> Day -> Bool
validateMatchedDate (y:m:d:_) date = (length y == 4) && (length m == 2) && (length d == 2) && (y1 == y2) && (m1 == m2) && (d1 == d2)
                                    where (y1,m1,d1) = (toInteger (read y ::Int), read m ::Int, read d ::Int)
                                          (y2,m2,d2) = toGregorian date
validateMatchedDate _ _ = False

--zwraca date sparsowana ze string lub nothing w razie niepowodzenia
getDateWithValidation:: String -> Maybe Day
getDateWithValidation str = if ( not( validateMatchedDate matchedDate ( fromJust maybeDate )) )
                then
                    Nothing
                else
                    maybeDate
                        where
                            matchedDate = matchDate str
                            maybeDate = parseDate (matchedDate);

--dopasowuje wyrazeniem regularny adres mailowy
matchEmail :: String  -> [String]
matchEmail str = getAllTextMatches $ str =~ "^[_a-z0-9-]+(\\.[_a-z0-9-]+)@[a-z0-9-]+(\\.[a-z0-9-]+)(\\.[a-z]{2,4})$" :: [String]

--parsuje mail na podstawie wyniku dopasowania z wyrazenia regularnego
parseMatchedEmail :: [String] -> Maybe String
parseMatchedEmail (mail:_) = Just mail
parseMatchedEmail _ = Nothing

--parsuje mail ze stringa
parseEmail:: String -> Maybe String
parseEmail mail = parseMatchedEmail (matchEmail mail)

data Person = Person { id :: Int,
                       firstName :: String,
                       lastName :: String,
                       companyName :: String,
                       phoneNumber :: String,
                       eMail :: String,
                       birthDay :: Day,
                       groups :: [Int]
                     } deriving (Show, Read, Eq)

getPersonId(Person id _ _ _ _ _ _ _) = id
getPersonFirstName (Person _ firstName _ _ _ _ _ _) = firstName
getPersonLastName (Person _ _ lastName _ _ _ _ _) = lastName
getPersonCompanyName (Person _ _ _ companyName _ _ _ _) = companyName
getPersonPhoneNumber (Person _ _ _ _ phoneNumber _ _ _) = phoneNumber
getPersonEmail (Person _ _ _ _ _ eMail _ _) = eMail
getPersonBirthday(Person _ _ _ _ _ _ birthDay _) = birthDay
getPersonGroups (Person _ _ _ _ _ _ _ groups) = groups

data Group = Group { groupId :: Int,
                     groupName :: String
                   } deriving (Show, Read, Eq)
getGroupId (Group groupId _) = groupId
getGroupName (Group _ groupName) = groupName

data AddressBook = AddressBook [Person] [Group] deriving (Show, Read)
getPersons(AddressBook p _) = p
getGroups(AddressBook _ g) = g


emptyAddressBook = AddressBook [] []
