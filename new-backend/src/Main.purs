module Main where

import Prelude
import Control.Plus (empty)
import Data.Maybe (Maybe)
import Data.List (List(..), filter, head)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)



type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type AddressBook = List Entry

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ showEntry entry


showAddress :: Address -> String
showAddress addr =
  addr.street <> ", " <>
  addr.city <> ", " <>
  addr.state

showEntry :: Entry -> String
showEntry entry =
  entry.lastName <> ", " <>
  entry.firstName <> ": " <>
  showAddress entry.address


address = { street: "123 Fake St.", city: "Faketown", state: "CA" }
entry = { firstName: "John", lastName: "Smith", address: address }

addressBook :: AddressBook
addressBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findByStreet :: String -> AddressBook -> Maybe Entry
findByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street
