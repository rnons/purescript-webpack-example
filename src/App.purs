module App where

import Prelude

import Control.Monad (liftM1)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Node (isEqualNode, contains)
import DOM.Event.EventTarget (eventListener, addEventListener) as DOM
import DOM.HTML (window) as DOM
import DOM.Event.Types (Event)
import DOM.HTML.Event.EventTypes as ET
import DOM.HTML.Event.HashChangeEvent as HCE
import DOM.HTML.Event.Types (HashChangeEvent, readHashChangeEvent) as DOM
import DOM.HTML.Types (windowToEventTarget, htmlElementToNode) as DOM
import DOM.Classy.Event (target)
import DOM.Classy.Node (nodeName)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES

data Query a
  = InputName String a
  | Init a
  | ClickWindow Event a

type State = { name :: String }

type AppEff m = Aff (dom :: DOM, avar :: AVAR, console :: CONSOLE | m)

rootRef :: H.RefLabel
rootRef = H.RefLabel "root"

app :: forall m. H.Component HH.HTML Query Unit Void (AppEff m)
app = H.lifecycleComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where

  initialState :: State
  initialState = { name: "" }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.ref rootRef ]
      [ HH.h1_
          [ HH.text "What's your name?" ]
      , HH.input
          [ HE.onValueInput (HE.input InputName) ]
      , HH.p_
          [ HH.text $ "Hello, " <> state.name ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (AppEff m)
  eval = case _ of
    Init next -> do
      win <- H.liftEff $ DOM.window
      let winTarget = DOM.windowToEventTarget win
          bindClick f = DOM.addEventListener ET.click (DOM.eventListener f) false winTarget
          handleClick e = pure $ (ClickWindow e) ES.Listening
      H.subscribe $ H.eventSource bindClick handleClick
      pure next
    ClickWindow event next -> do
      let targetNode = target event
      mRootNode <- liftM1 DOM.htmlElementToNode <$> H.getHTMLElementRef rootRef
      case mRootNode of
        Just rootNode -> do
          isEqual <- H.liftEff $ isEqualNode rootNode targetNode
          isChild <- H.liftEff $ contains rootNode targetNode
          if (isEqual || isChild)
             then H.liftEff $ log "inside"
             else H.liftEff $ log "outside"
          pure next
        Nothing -> pure next
    InputName name next -> do
      H.modify (\state -> { name: name })
      pure next
