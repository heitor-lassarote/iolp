module LowCode.UI.Canvas
    ( Slot
    , Query
    , Message
    , component
    ) where

import Prelude

import Control.Comonad.Cofree as CF
import Data.Array as Array
import Data.Lens as Lens
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Data.Tree as Tree
import Data.Tuple (Tuple (..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

import LowCode.Draggable as Draggable
import LowCode.Draggable (Child, Family, Identifier, Item)
import LowCode.MouseEventType (clientXY, MouseEventType (..))
import LowCode.Point (Point)
import LowCode.UI.Element (Tag (..))

type Slot = H.Slot Query Message

type DragStatus =
    { delta :: Point
    , child :: Family
    }

_delta :: Lens.Lens' DragStatus Point
_delta = Lens.lens _.delta $ _ { delta = _ }

_child :: Lens.Lens' DragStatus Family
_child = Lens.lens _.child $ _ { child = _ }

type State =
    { dragStatus :: Maybe DragStatus
    , children :: Tree.Forest Item
    , generator :: Identifier
    , mouseOver :: Child
    }

_dragStatus :: Lens.Lens' State (Maybe DragStatus)
_dragStatus = Lens.lens _.dragStatus $ _ { dragStatus = _ }

_children :: Lens.Lens' State (Tree.Forest Item)
_children = Lens.lens _.children $ _ { children = _ }

_generator :: Lens.Lens' State Identifier
_generator = Lens.lens _.generator $ _ { generator = _ }

data Action
    = AddDraggable Tag
    | HandleInner Draggable.Message
    | HandleMouseEvent MouseEventType ME.MouseEvent

data Query i

data Message

_inner :: SProxy "inner"
_inner = SProxy

type ChildSlots =
    ( inner :: Draggable.Slot Int
    )

initialState :: forall i. i -> State
initialState _ =
    { dragStatus: Nothing
    , children: L.Nil
    , generator: 0
    , mouseOver: L.Nil
    }

component
    :: forall i
     . H.Component HH.HTML Query i Message Aff
component =
    H.mkComponent
        { initialState: initialState
        , render: render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            }
        }

render
    :: forall m
     . State
    -> H.ComponentHTML Action ChildSlots m
render state =
    HH.div
        [ HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
        , HE.onMouseLeave (Just <<< HandleMouseEvent MouseLeave)
        , HE.onMouseMove  (Just <<< HandleMouseEvent MouseMove)
        , HE.onMouseUp    (Just <<< HandleMouseEvent MouseUp)
        , HP.class_ $ HH.ClassName "canvas"
        ]
        [ HH.h1_
            [ HH.text $ "Mouse over: " <> show state.mouseOver
            ]
        , HH.h1_
            [ HH.text $ "Tree: " <> show (map Tree.showTree state.children)
            ]
        , HH.div
            [ HP.class_ $ HH.ClassName "canvas-editor"
            ]
            draggableSlots
        , HH.div
            [ HP.classes
                [ HH.ClassName "sidebar"
                , HH.ClassName "canvas-items"
                ]
            ]
            [ HH.p
                [ HE.onClick \ev -> createDraggable ev Button ]
                [ HH.text "Button" ]
            , HH.p
                [ HE.onClick \ev -> createDraggable ev Div ]
                [ HH.text "Div" ]
            , HH.p
                [ HE.onClick \ev -> createDraggable ev H1 ]
                [ HH.text "Header" ]
            , HH.p
                [ HE.onClick \ev -> createDraggable ev P ]
                [ HH.text "Paragraph" ]
            ]
        , HH.div
            [ HP.classes
                [ HH.ClassName "sidebar"
                , HH.ClassName "canvas-properties"
                ]
            ]
            [ HH.h2_ [ HH.text "Properties" ]
            ]
        ]
  where
    mkSlot tree =
        let id = (CF.head tree).identifier
         in HH.slot _inner id Draggable.component tree (Just <<< HandleInner)
    draggableSlots = Array.fromFoldable $ map mkSlot state.children
    createDraggable ev = Just <<< AddDraggable

handleAction
    :: forall o
     . Action
    -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction = case _ of
    AddDraggable tag ->
        H.modify_ \st ->
            let item = { tag: tag, point: zero, properties: [], identifier: st.generator }
            in
            st { children = CF.mkCofree item L.Nil L.: st.children
               , generator = st.generator + 1
               }

    HandleInner msg -> handleInner msg

    HandleMouseEvent evTy ev -> handleMouseEvent evTy ev

handleInner
    :: forall f i o m
     . Draggable.Message
    -> H.HalogenM State i f o m Unit
handleInner = case _ of
    Draggable.Clicked point child mousePos ->
        H.modify_ _
            { dragStatus = Just
                { child: child
                , delta: point - mousePos
                }
            }

    Draggable.Entered child -> H.modify_ _ { mouseOver = NEL.toList child }

handleMouseEvent
    :: forall i o
     . MouseEventType
    -> ME.MouseEvent
    -> H.HalogenM State i ChildSlots o Aff Unit
handleMouseEvent evTy ev = case evTy of
    MouseMove -> do
        let ev' = ME.toEvent ev
        H.liftEffect $ WE.preventDefault ev'
        st <- H.get
        case st.dragStatus of
            Nothing -> pure unit
            Just dragStatus -> do
                let point = clientXY ev + dragStatus.delta
                    child = NEL.toList dragStatus.child
                case updateChild st.children (_ { point = point }) child of
                    Nothing -> pure unit
                    Just tree -> H.put $ st { children = tree }

    MouseUp -> do
        st <- H.get
        case Tuple st.mouseOver st.dragStatus of
            Tuple over@(c L.: cs) (Just dragStatus) ->
                -- Parent: over
                -- Child: dragStatus.child
                case recreateTree st.children over (NEL.toList dragStatus.child) of
                    Nothing ->
                        H.liftEffect $ log $
                            "Could not add children. Relevant paths:\n"
                            <> "Parent:\n\t"
                            <> show over
                            <> "\nChild:\n\t"
                            <> show (NEL.toList dragStatus.child)
                            <> "\nOver the tree:\n"
                            <> show (map Tree.showTree $ (map <<< map) _.identifier st.children)
                    Just newTree -> do
                        H.liftEffect $ log $
                            "Successfully added children, with tree:\n"
                            <> show (map Tree.showTree $ (map <<< map) _.identifier newTree)
                        H.put $ st { children = newTree }
            _ -> pure unit

        H.modify_ _ { dragStatus = Nothing }

    _ -> pure unit
  where
    updateChild :: Tree.Forest Item -> (Item -> Item) -> Child -> Maybe (Tree.Forest Item)
    updateChild tree f L.Nil = Nothing
    updateChild tree f (i L.: is) = case is of
        L.Nil -> do
            Just $ map (\cf -> if (CF.head cf).identifier == i then CF.mkCofree (f $ CF.head cf) (CF.tail cf) else cf) tree
        _ -> do
            subTree <- L.find (\cf -> (CF.head cf).identifier == i) tree
            newTree <- updateChild (CF.tail subTree) f is
            Just $ L.singleton $ CF.mkCofree (CF.head subTree) newTree

    getChildren :: Tree.Forest Item -> Child -> Maybe (Tree.Forest Item)
    getChildren tree L.Nil = Nothing
    getChildren tree (i L.: is) = do
        subTree <- L.find (\cf -> (CF.head cf).identifier == i) tree
        case is of
            L.Nil -> Just $ L.singleton subTree
            _ -> getChildren (CF.tail subTree) is

    removeChildren :: Tree.Forest Item -> Child -> Maybe (Tree.Forest Item)
    removeChildren tree L.Nil = Nothing
    removeChildren tree (i L.: is) = case is of
        L.Nil -> Just $ L.filter ((_ /= i) <<< _.identifier <<< CF.head) tree
        _ -> do
            subTree <- L.find (\cf -> (CF.head cf).identifier == i) tree
            newTree <- removeChildren (CF.tail subTree) is
            Just $ L.singleton $ CF.mkCofree (CF.head subTree) newTree

    addChildren :: Tree.Forest Item -> Child -> Tree.Forest Item -> Maybe (Tree.Forest Item)
    addChildren tree L.Nil children = Nothing
    addChildren tree (i L.: is) children = do
        subTree <- L.find (\cf -> (CF.head cf).identifier == i) tree
        case is of
            L.Nil -> Just $ L.singleton $ CF.mkCofree (CF.head subTree) (CF.tail subTree <> children)
            _ -> addChildren (CF.tail subTree) is children

    recreateTree :: Tree.Forest Item -> Child -> Child -> Maybe (Tree.Forest Item)
    recreateTree tree parentPath childrenPath = do
        newTree <- removeChildren tree childrenPath
        childrenSubTree <- getChildren tree childrenPath
        addChildren newTree parentPath childrenSubTree
