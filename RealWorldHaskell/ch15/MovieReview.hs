import Control.Monad

{-

name=Attila+%42The+Hun%42&occupation=Khan

[("name", Just "Attila \"The Hun\""), ("occupation", Just "Khan")]

-}

data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
    }

maybeReview alist = do
    title <- lookup1 "title" alist
    user <- lookup1 "user" alist
    review <- lookup1 "review" alist
    return (MovieReview title user review)

lookup1 key alist = case lookup key alist of
    Just (Just s@(_:_)) -> Just s
    _ -> Nothing

liftedReview alist =
    liftM3 MovieReview
        (lookup1 "title" alist)
        (lookup1 "user" alist)
        (lookup1 "review" alist)

{-
ap :: m (a -> b) -> m a -> m b

MovieReview :: String -> String -> String -> MovieReview

liftM (MovieReview (lookup "title" alist)) :: Maybe (String -> String -> MovieReview)

-}
apReview alist =
    MovieReview
        `liftM` lookup1 "title" alist
        `ap` lookup1 "title" alist
        `ap` lookup1 "review" alist
