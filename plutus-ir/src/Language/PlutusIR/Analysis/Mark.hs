module Language.PlutusIR.Analysis.Mark where

import Language.PlutusIR.Analysis.FreeGraph2

import Algebra.Graph.AdjacencyMap as AM
import           Data.Text (Text)
import qualified Data.Set as S

import qualified Language.PlutusCore                as PLC
import qualified Language.PlutusCore.Name           as PLC


import Language.PlutusIR.Parser
import           Language.PlutusIR.Transform.Rename          ()
import           Language.PlutusCore.Quote
import Language.PlutusCore.Pretty

import Control.Exception (assert)

type U = (PLC.Unique, Text)

data Mark = LetMark { name :: U
                    , distance :: Int -- distance away from the lamdep
                    , lamdep :: Maybe (Int, U) -- the column of the lambda and its name. the name is needed because we do a holistic/clean code gen.
                    }
          | LamMark { name :: U
                    , depth :: Int
                    } deriving (Eq, Show)

instance Ord Mark where
  LetMark n1 d1 l1  `compare` LetMark n2 d2 l2 = compare l1 l2 <> compare d1 d2 <> compare n1 n2 -- first compare by lamdep then by distance, then by name (name is required for Set)
  LamMark n1 d1 `compare` LamMark n2 d2 = compare d1 d2 <> compare n1 n2 -- first compare by column, then by name (name is required for Set)
  LetMark {lamdep = l1} `compare` LamMark {name=n2, depth=d2} = -- because of this, we don't need transitive reduction
    case compare l1 (Just (d2,n2)) of
      EQ -> GT                  -- let comes after lam
      x -> x
  LamMark {name=n1, depth=d1} `compare`  LetMark {lamdep = l2} =  -- because of this, we don't need transitive reduction
    case compare (Just (d1,n1)) l2 of
      EQ -> LT
      x -> x

-- the minimum default mark, it represents that there are no dependencies
topMark :: Mark
topMark = LetMark { distance = -1
                  , lamdep = Nothing
                  , name = undefined -- not used
                  }


mark :: AM.AdjacencyMap Node -> S.Set Mark
mark g =
  -- start from root
  sconcat $ S.map mark' $ S.filter (\ v -> S.null (AM.preSet v g)) $ vertexSet g -- start from all the roots

  where

    mark':: Node -> S.Set Mark
    mark' thisNode =
      let children = AM.postSet thisNode g
          childMarks = sconcat $ S.map mark' children
      in case thisNode of
           LamNode v ->
             assert (length childMarks <= 1) $
                case S.lookupMax childMarks of
                  Just LamMark {depth=otherD} -> LamMark { name = v, depth=otherD+1} `S.insert` childMarks -- just mark its column
                  _ -> S.singleton $ LamMark {name = v, depth = 0}
           LetNode v ->
             let maxChildMark = maximum $ S.insert topMark childMarks
             in (case maxChildMark of
                  LetMark _ letDis dep ->  LetMark { name=v, distance = letDis+1, lamdep = dep }
                  LamMark lamName lamCol -> LetMark { name=v, distance = 0, lamdep = Just (lamCol, lamName)  }
                ) `S.insert` childMarks


-- HELPERS
sconcat :: (Foldable t, Monoid (t a)) => t (t a) -> t a
sconcat = foldr (<>) mempty     -- more general mconcat to use in Sets
