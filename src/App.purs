module App where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a = InputName String a

type State = { name :: String }

app :: forall m. H.Component HH.HTML Query Unit Void m
app =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { name: "" }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "What's your name?" ]
      , HH.input
          [ HE.onValueInput (HE.input InputName) ]
      , HH.p_
          [ HH.text $ "Hello, " <> state.name ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    InputName name next -> do
      H.modify (\state -> { name: name })
      pure next
