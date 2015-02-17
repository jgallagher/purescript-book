module Data.PhoneBook where

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }

type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ entry.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons
 
findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findByPhone :: String -> PhoneBook -> Maybe Entry
findByPhone phone = head <<< filter filterEntry
  where
      filterEntry :: Entry -> Boolean
      filterEntry entry = entry.phone == phone

contains :: String -> String -> PhoneBook -> Boolean
contains firstName lastName = isJust <<< findEntry firstName lastName

removeDuplicates :: PhoneBook -> PhoneBook
removeDuplicates book = nubBy sameName book
  where
      sameName :: Entry -> Entry -> Boolean
      sameName e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
