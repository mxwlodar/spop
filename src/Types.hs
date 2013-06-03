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
