module App where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Child as Child

data Query a = Empty a

type State = Unit

type Slot = Unit

app :: forall m. H.Component HH.HTML Query Unit Void (Child.AppEff m)
app = H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Query Child.Query Unit (Child.AppEff m)
    render state =
      HH.div_
        [ HH.div [ HP.class_ $ HH.ClassName "row" ]
            [ HH.slot unit Child.component unit absurd
            , HH.slot unit Child.component unit absurd
            ]
        , HH.text "hello"
        ]

    eval :: Query ~> H.ParentDSL State Query Child.Query Slot Void (Child.AppEff m)
    eval (Empty next) = pure next

