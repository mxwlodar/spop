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
import Data.Char
import Numeric
import Text.Regex.Posix


-- tutaj beda zdefiniowane nasze typy danych

type Date = (Int, Int, Int)



matchDate :: String  -> [String]
matchDate str = getAllTextMatches $ str =~ "[0-9]+" :: [String]

parseDate :: [String] -> Maybe Day
parseDate (y:m:d:_) = Just (fromGregorian (toInteger (read y ::Int)) (read m ::Int) (read d ::Int))
parseDate _ = Nothing

--sprawdza czy sparsowana data jest zgodna z ta wpisana w stringu - eleminuje daty typu 30 lutego
validateMatchedDate :: [String] -> Day -> Bool
validateMatchedDate (y:m:d:_) date = (length y == 4) && (length m == 2) && (length d == 2) && (y1 == y2) && (m1 == m2) && (d1 == d2)
                                    where (y1,m1,d1) = (toInteger (read y ::Int), read m ::Int, read d ::Int)
                                          (y2,m2,d2) = toGregorian date
validateMatchedDate _ _ = False





type ID = Int
data Person = Person { firstName :: String,
                       lastName :: String,
                       companyName :: String,
                       phoneNumber :: String,
                       eMail :: String,
                       birthDay :: Day,
                       groups :: [Group]
                     } deriving (Show, Read)

data Group = Group String [Person] deriving (Show, Read)
getGroupName (Group s _) = s
getPersonsInGroup (Group _ p) = p

type Counter = Int
data AddressBook = AddressBook [Person] [Group] deriving (Show, Read)
getPersons(AddressBook p _) = p
getGroups(AddressBook _ g) = g


emptyAddressBook = AddressBook [] []
