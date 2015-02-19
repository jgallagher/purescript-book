module Data.AddressBook.Validation where

import Data.Array
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable

import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply

type Errors = [String]

arrayNonEmpty :: forall a. String -> [a] -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" ++ field ++ "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

phoneNumberRegex :: R.Regex
phoneNumberRegex = 
  R.regex 
    "^\\d{3}-\\d{3}-\\d{4}$" 
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false 
    }

twoLetterRegex :: R.Regex
twoLetterRegex =
    R.regex
    "^[a-zA-Z]{2}$"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false 
    }

whitespaceRegex :: R.Regex
whitespaceRegex =
    R.regex
    "^\\s*$"
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false 
    }

matches :: String -> R.Regex -> String -> V Errors String
matches _     regex value | R.test regex value = pure value
matches field _     _     = invalid ["Field '" ++ field ++ "' did not match the required format"]

nonWhitespace :: String -> String -> V Errors String
nonWhitespace field value | R.test whitespaceRegex value =
    invalid ["Field '" ++ field ++ "' cannot be empty"]
nonWhitespace _ value = pure value

validateAddress :: Address -> V Errors Address 
validateAddress (Address o) = 
  address <$> (nonWhitespace "Street" o.street)
          <*> (nonWhitespace "City"   o.city)
          <*> (matches "State" twoLetterRegex o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) = 
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonWhitespace "First Name" o.firstName)
         <*> (nonWhitespace "Last Name"  o.lastName)
         <*> validateAddress o.address
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = runV Left Right $ validatePerson p
