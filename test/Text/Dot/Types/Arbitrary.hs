{-# LANGUAGE CPP #-}
module Text.Dot.Types.Arbitrary where

import           Text.Dot.Types.Internal

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
import           Data.Monoid
#endif

import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Arbitrary

import           Test.QuickCheck
import           Test.QuickCheck.Checkers

instance EqProp Dot where
    (=-=) = eq

instance Arbitrary Dot where
    arbitrary = oneof
        [
          Node <$> arbitrary <*> arbitrary
        , Edge <$> arbitrary <*> arbitrary <*> arbitrary
        , Declaration <$> arbitrary <*> arbitrary
        , Subgraph <$> arbitrary <*> arbitrary
        , RawDot <$> arbitrary
        , Label <$> arbitrary
        , Rankdir <$> arbitrary
        , DotSeq <$> arbitrary <*> arbitrary
        , pure DotEmpty
        ]

    shrink (DotSeq d1 d2) = shrink d1 ++ shrink d2
    shrink m = [m]

instance Arbitrary NodeId where
    arbitrary = oneof
        [
          UserId <$> arbitrary
        , Nameless <$> arbitrary
        ]

instance Arbitrary DecType where
    arbitrary = elements [DecGraph, DecNode, DecEdge]

instance Arbitrary RankdirType where
    arbitrary = elements [LR, TB]

