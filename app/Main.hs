{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Network.Wreq
import Control.Lens
import Text.HTML.Parser
import Data.String.Conversions
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  allEpURLs    <- genEpURLs
  episodePages <- getEpisodePages allEpURLs
  let books   = concatMap getBooks episodePages
      grouped = groupBy similarNames $ sortBy myCompare books
      result  = fmap fuse grouped
  writeOutput (reverse . sortOn thrd $ result)


thrd (_,_,x) = x

myCompare (a,_) (b,_) = compare (sand a) (sand b)

similarNames (t1,_) (t2,_) = (sand t1 == sand t2)
                              || (editDist (show t1) (show t2) < 5)
sand = T.strip . mapNumbers . T.filter isAlphaNum . T.takeWhile (\x -> x `notElem` [',',':']) . T.toLower

fuse xxs@((title,_):xs) =
  let author = foldr f "Unknown" xxs
      f (_,"") acc = acc
      f (_,a) acc
        | isUpper (T.head a) = a
        | otherwise = acc
  in (title, author, length xxs)

mapNumbers =  T.replace "one" "1"
            . T.replace "two" "2"
            . T.replace "three" "3"
            . T.replace "four" "4"
            . T.replace "five" "5"
            . T.replace "six" "6"
            . T.replace "seven" "7"
            . T.replace "eight" "8"
            . T.replace "nine" "9"
            . T.replace "ten" "10"


writeOutput :: [(StrictText, StrictText, Int)] -> IO ()
writeOutput books = writeFile "bookList.txt" output
  where
    output = foldr outputBuilder "" books
    outputBuilder (t, a, c) acc =
      show t ++
      " by " ++
      show a ++
      " -- Count: " ++
      show c ++
      "\n" ++
      acc


getEpisodePages :: [StrictText] -> IO [Response LazyByteString]
getEpisodePages = mapM (get . cs)

genEpURLs :: IO [StrictText]
genEpURLs =
  let pagesURLs = fmap ((++) "https://www.biggerpockets.com/podcast?page=" . show) [1..16]
  in concatMap getEpisodeURLs <$> mapConcurrently get pagesURLs

getBooks :: Response LazyByteString -> [(StrictText, StrictText)]
getBooks = filter (not . T.null . fst) . fmap getBook . intermediate

intermediate :: Response LazyByteString -> [[Token]]
intermediate response =
  let tokenized  = parseTokensLazy . cs $ response ^. responseBody
      --tokenized = parseTokensLazy response
      atHeader   = dropWhile (not . htmlContentText "Books Mentioned") tokenized
      (books, _) = takeTag $ dropWhile
        (\x -> not (htmlTagOpen "ul" x) && not (htmlTagOpen "p" x))
        atHeader
  in  buildBookList $ dropWhile (not . htmlTagOpen "a") books


getBook :: [Token] -> (StrictText, StrictText)
getBook [] = ("", "")
getBook bk =
  let bk' = dropWhile (not . htmlContent) bk
      title =
        case safeHead bk' of
          (Just (ContentText t)) -> T.strip . fst . T.breakOn "by " . clean $ t
          _ -> ""
      bk'' = dropWhile (not . htmlContent) (safeTail bk')
      author =
        case safeHead bk'' of
          (Just (ContentText a)) -> T.strip . snd . T.breakOnEnd "By " . snd . T.breakOnEnd "by " . clean $ a
          _ -> ""
  in (title, author)

clean :: StrictText -> StrictText
clean s = case T.stripPrefix "The " s'' of
  (Just remThe) -> remThe
  Nothing       -> s''
 where
  s' = case T.stripPrefix "BiggerPockets" s of
    (Just stripped) -> stripped
    Nothing         -> s
  s'' =
    T.dropWhile (not . isAlphaNum)
      . fst
      . T.breakOn " - "
      . cleanAppostrophy'
      . clearQuoteL
      . clearQuoteR
      . cleanAmpersand
      . cleanSpace
      . clearQuoteL
      . clearQuoteR
      . cleanAppostrophy
      . clearDash
      $ s'
  cleanSpace        = T.replace "\160" " "
  clearQuoteL       = T.replace "&#8220;" ""
  clearQuoteR       = T.replace "&#8221;" ""
  clearQuoteL'      = T.replace "\8220" ""
  clearQuoteR'      = T.replace "\8221" ""
  cleanAppostrophy  = T.replace "&#8217;" "\'"
  cleanAppostrophy' = T.replace "\8217" "\'"
  clearDash         = T.replace "&#8211;" "-"
  cleanAmpersand    = T.replace "&amp" "and"



editDist :: Eq a => [a] -> [a] -> Int
editDist a b = last
  ( if lab == 0
    then mainDiag
    else if lab > 0
      then lowers !! (lab - 1){- < 0 -}
      else uppers !! (-1 - lab)
  )
 where
  mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
  uppers   = eachDiag a b (mainDiag : uppers) -- upper diagonals
  lowers   = eachDiag b a (mainDiag : lowers) -- lower diagonals
  eachDiag a [] diags = []
  eachDiag a (bch:bs) (lastDiag:diags) =
    oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
    where nextDiag = head (tail diags)
  oneDiag a b diagAbove diagBelow = thisdiag
   where
    doDiag []       b        nw n w = []
    doDiag a        []       nw n w = []
    doDiag (ach:as) (bch:bs) nw n w = me : doDiag as bs me (tail n) (tail w)
      where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
    firstelt = 1 + head diagBelow
    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
  lab = length a - length b
  min3 x y z = if x < y then x else min y z


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

--htmlTagOpen :: _ -> Token -> Bool






htmlTagOpen tagName (TagOpen tag _)
  | tagName == tag = True
htmlTagOpen _ _ = False

htmlContent :: Token -> Bool
htmlContent (ContentText _) = True
htmlContent _ = False

getEpisodeURLs :: Response LazyByteString -> [StrictText]
getEpisodeURLs response =
  let tokenized = parseTokensLazy . cs $ response ^. responseBody
      tokenized' = filter isTag tokenized
      (episodeList, _) = takeTag $ dropWhile (not . htmlEpisodeList) tokenized'
      episodes = tail $ tagContents episodeList
      listEpisodes = buildEpisodeList episodes
  in mapMaybe getURL listEpisodes

getURL ep =
  case drop 5 ep of
    (TagOpen "a" [Attr "href" url]:_)
      | "https" `T.isPrefixOf` url -> Just url
    _ -> Nothing

buildBookList = split (dropInitBlank . keepDelimsL $ whenElt (htmlTagOpen "a"))

buildEpisodeList [] = []
buildEpisodeList eps =
  case takeTag eps of
    ([], _) -> []
    (ep, rest) -> ep:buildEpisodeList rest

isTag (TagOpen _ _) = True
isTag (TagClose _) = True
isTag _ = False

tagContents :: [Token] -> [Token]
tagContents [] = []
tagContents [_] = []
tagContents [_,_] = []
tagContents xs = init . tail $ xs

takeTag :: [Token] -> ([Token], [Token])
takeTag [] =  ([], [])
takeTag (t@(TagOpen tagType _):ts) = let (taken, notTaken) = takeTag' ts
                                     in (t:taken, notTaken)
  where
    takeTag' tts'@(t'@(TagOpen tagType' _):ts')
      | tagType' == tagType = let (innerTag, rest) = takeTag tts'
                                  (taken, notTaken) = takeTag' rest
                              in (innerTag ++ taken, notTaken)
    takeTag' (t'@(TagClose tagType'):ts')
      | tagType' == tagType = ([t'], ts')
    takeTag' (t':ts') = let (taken, notTaken) = takeTag' ts'
                        in (t':taken, notTaken)
    takeTag' [] = ([], [])
takeTag (_:ts) = takeTag ts

htmlEpisodeList :: Token -> Bool
htmlEpisodeList (TagOpen "div" attrs) = isJust $ find htmlEpisodeListClass attrs
  where
    htmlEpisodeListClass (Attr "class" "episode-list") = True
    htmlEpisodeListClass _ = False
htmlEpisodeList _ = False

htmlContentText :: StrictText -> Token -> Bool
htmlContentText str (ContentText t) = str `T.isPrefixOf` t
htmlContentText _ _ = False
