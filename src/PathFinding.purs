module PathFinding (findPath, class PFNode, pfNodeKey) where

import Prelude
import Global (infinity)
import Data.StrMap (StrMap, lookup, empty, member)
import Data.StrMap (insert) as SM
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array (reverse, snoc, head, findIndex, deleteAt, index, filter, insertBy, deleteBy)
import Data.Foldable (foldl)

fNot :: forall t8 t9. HeytingAlgebra t9 => (t8 -> t9) -> t8 -> t9
fNot f = (\ a -> not $ f a)

class PFNode a where
  pfNodeKey :: a -> String

byFieldF :: forall e g. {f::Number | e} -> {f::Number | g} -> Ordering
byFieldF a b = let a' = a.f 
                   b' = b.f
               in
                if (a'<b')
                then LT
                else if (a'>b')
                     then GT
                     else EQ


removeGet :: forall a. (a -> Boolean) -> a -> (Array a) -> (Tuple a (Array a))
removeGet test dflt array =
  let r = do i <- findIndex test array
             element <- index array i
             newArray <- deleteAt i array
             pure (Tuple element newArray)
  in
   case r of
     (Just result) -> result
     Nothing -> (Tuple dflt array)

openMatches :: forall a. PFNode a => a -> {f::Number,g::Number,node::a} -> Boolean
openMatches node nodeData = (pfNodeKey node) == (pfNodeKey nodeData.node)

nodeDataEq :: forall a. PFNode a => {f::Number,g::Number,node::a} -> {f::Number,g::Number,node::a} -> Boolean
nodeDataEq a b = (pfNodeKey a.node) == (pfNodeKey b.node)

getNode :: forall a. PFNode a => {f::Number,g::Number,node::a} -> a
getNode bndl = bndl.node

pathFindingFoldStep :: forall a. PFNode a =>
                       (a -> a -> Number) -> 
                       {f::Number, g::Number, node::a} ->
                       a ->
                       {o::(Array {f::Number,g::Number,node::a}), c::(StrMap a)} ->
                       a ->
                       {o::(Array {f::Number,g::Number,node::a}), c::(StrMap a)}
pathFindingFoldStep heuristic currentData goalNode {o:o, c:c} nbr =
  let (Tuple nbrData o') = removeGet (openMatches nbr) {f:infinity, g:infinity,node:nbr} o
      currentG = nbrData.g
      tenativeG = currentData.g + 1.0
  in
   if tenativeG >= currentG
   then {o:insertBy byFieldF nbrData o', c:c}
   else {
     o:insertBy byFieldF {f:(tenativeG + (heuristic nbr goalNode)), g:tenativeG, node:nbr} o',
     c:SM.insert (pfNodeKey nbr) (getNode currentData) c
     }


findPath :: forall a. PFNode a =>
            (a -> Array a) ->
            (a -> a -> Number) ->
            a ->
            a ->
            (Maybe (Array a))
findPath neighbors heuristic start end =
  do returnMap <- reallyFindPath {end:end, open:[{f:heuristic start end,g:0.0,node:start}], closed:empty, cameFrom:empty}
     let path = readPath end returnMap [end] 
     case head path of
       Nothing -> Nothing
       (Just hd) -> if not ((pfNodeKey hd) == (pfNodeKey end))
                    then Nothing
                    else Just $ reverse path
  where
    readPath :: a -> (StrMap a) -> (Array a) -> (Array a)
    readPath current mp path = case lookup (pfNodeKey current) mp of
      (Just next) -> readPath next mp (snoc path next)
      Nothing -> path
    reallyFindPath :: {end::a, open::(Array {f::Number,g::Number,node::a}), closed::(StrMap Boolean), cameFrom::(StrMap a)} -> (Maybe (StrMap a))    
    reallyFindPath {end:end', open:open, closed:closed, cameFrom:cameFrom} =
      case head open of
        (Just current) ->
          if (pfNodeKey end) == (pfNodeKey (getNode current))
          then (Just cameFrom)
          else
            let open' = deleteBy nodeDataEq current open
                closed' = (SM.insert (pfNodeKey (getNode current)) true closed)
                notInClosed n = not $ member (pfNodeKey n) closed'
                justNbrs = neighbors (getNode current)
                nbrs = filter notInClosed justNbrs
                folder = pathFindingFoldStep heuristic current end'                  
                {o:o,c:c} = foldl folder {o:open', c:cameFrom} nbrs
            in reallyFindPath {end:end', open:o, closed:closed', cameFrom:c}
        Nothing -> Nothing
