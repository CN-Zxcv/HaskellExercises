
module PodParser where

import PodTypes
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Html.Generate(showattr)
import Data.Char
import Data.List

data PodItem = PodItem {
      itemtitle :: String
    , enclosureurl :: String
    } deriving (Eq, Show, Read)

data Feed = Feed {
      channeltitle :: String
    , items :: [PodItem]
    } deriving (Eq, Show, Read)

{- | 根据给定的广播和 PodItem ，产生一个分集。 -}
item2ep :: Podcast -> PodItem -> Episode
item2ep pc item = Episode {
      epId = 0
    , epCast = pc
    , epURL = enclosureurl item
    , epDone = False
    }

{- | 从给定的字符串里面分析出数据，给定的名字在有需要的时候会被用在错误消息里面。 -}
parse :: String -> String -> Feed
parse content name = Feed {
      channeltitle = getTitle doc
    , items = getEnclosures doc
    }

    where
        parseResult = xmlParse name (stripUnicodeBOM content)
        doc = getContent parseResult

        getContent :: Document Posn -> Content Posn
        getContent (Document _ _ e _) = CElem e noPos

        {- | Some Unicode documents begin with a binary sequence;
        strip it off before processing. -}
        stripUnicodeBOM :: String -> String
        stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
        stripUnicodeBOM x = x

{- | 从文档里面提取出频道部分（channel part）

注意 HaXml 会将 CFilter 定义为：

> type CFilter = Content -> [Content]
-}
channel :: CFilter Posn
channel = tag "rss" /> tag "channel"

getTitle :: Content Posn -> String
getTitle doc = contentToStringDefault "Untitled Podcast"
    (channel /> tag "title" /> txt $ doc)

getEnclosures :: Content Posn -> [PodItem]
getEnclosures doc = concatMap procPodItem $ getPodItems doc
    where
        procPodItem :: Content Posn -> [PodItem]
        procPodItem item = concatMap (procEnclosure title) enclosure
            where
                title = contentToStringDefault "Untitled Episode"
                    (keep /> tag "title" /> txt $ item)
                enclosure = (keep /> tag "enclosure") item

        getPodItems :: CFilter Posn
        getPodItems = channel /> tag "item"

        procEnclosure :: String -> Content Posn -> [PodItem]
        procEnclosure title enclosure =
            map makePodItem (showattr "url" enclosure)
            where
                makePodItem :: Content Posn -> PodItem
                makePodItem x = PodItem {
                      itemtitle = title
                    , enclosureurl = contentToString [x]
                    }

{- | 将 [Content] 转换为可打印的字符串，
如果传入的 [Content] 为 [] ，那么向用户说明此次匹配未成功。 -}
contentToStringDefault :: String -> [Content Posn] -> String
contentToStringDefault msg [] = msg
contentToStringDefault _ x = contentToString x

{- | 将 [Content] 转换为可打印的字符串，并且小心地对它进行反解码（unescape）。

一个没有反解码实现的实现可以简单地定义为：

> contentToString = concatMap (show . content)

因为 HaXml 的反解码操作只能对 Elements 使用，
我们必须保证每个 Content 都被包裹为 Element ，
然后使用 txt 函数去将 Element 内部的数据提取出来。 -}
contentToString :: [Content Posn] -> String
contentToString = concatMap procContent
    where
        procContent x = verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) noPos

        fakeElem :: Content Posn -> Element Posn
        fakeElem x = Elem (N "fake") [] [x]

        unesc :: Element Posn -> Element Posn
        unesc = xmlUnEscape stdXmlEscaper
