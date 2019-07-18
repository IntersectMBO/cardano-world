
module Ouroboros.Network.Subscription.Subscriber
    ( SubscriptionTarget (..)
    , constantSubscriptionTarget
    , listSubscriptionTarget
    ) where

-- | Generate subscription targets in some monad.
-- Examples include obtaining targets from a fixed list, or from a DNS lookup.
newtype SubscriptionTarget m target = SubscriptionTarget
    { getSubscriptionTarget :: m (Maybe (target, SubscriptionTarget m target))
      -- ^ This should be used with the exception that implementations can block on
      -- the order of seconds.
    }

constantSubscriptionTarget :: Applicative m => target -> SubscriptionTarget m target
constantSubscriptionTarget target =
    SubscriptionTarget (pure (Just (target, constantSubscriptionTarget target)))

listSubscriptionTarget
    :: Applicative m
    => [target]
    -> SubscriptionTarget m target
listSubscriptionTarget []     = SubscriptionTarget $ pure Nothing
listSubscriptionTarget (t:ts) = SubscriptionTarget $ pure (Just (t, listSubscriptionTarget ts))

