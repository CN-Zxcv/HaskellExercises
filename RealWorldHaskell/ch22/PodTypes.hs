
module PodTypes where

data Podcast = Podcast
    { castId :: Integer -- 播客ID
    , castURL :: String -- URL
    } deriving (Eq, Show, Read)

data Episode = Episode
    { epId :: Integer   -- 分集ID
    , epCast :: Podcast  -- 所属播客
    , epURL :: String   -- URL
    , epDone :: Bool    -- 是否看过
    } deriving (Eq, Show, Read)
