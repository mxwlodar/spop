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

import Data.Char
import Text.Printf
import Numeric
-- tutaj beda zdefiniowane nasze typy danych

type Date = (Int, Int, Int)

parseDate :: String -> Date
parseDate s = (y,m,d)
    where [(m,rest) ] = readDec (filter (not . isSpace) s)
          [(d,rest1)] = readDec (tail rest)
          [(y, _)   ] = readDec rest1

showDate::(Int, Int, Int) -> String
showDate (y,m,d)= yy ++ '-':mm ++ '-':dd
    where dd = show d
          mm = show m
          yy = show y

type ID = Int
data Person = Person { firstName :: String,
                       lastName :: String,
                       companyName :: String,
                       phoneNumber :: String,
                       eMail :: String,
                       birthDay :: Date,
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
