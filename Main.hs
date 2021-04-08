{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import System.Environment
import System.Exit
import Text.HTML.Scalpel

prefixes :: HM.HashMap String String
prefixes = HM.fromList [
    ("br", "br")
  , ("eune", "eune")
  , ("euw", "euw")
  , ("jp", "jp")
  , ("kr", "www")
  , ("lan", "lan")
  , ("las", "las")
  , ("na", "na")
  , ("oce", "oce")
  , ("ru", "ru")
  , ("tr", "tr")
  ]

data Rank = D2 | D1 | M | GM | C
  deriving (Eq, Show, Ord)

data Player = Player {
    playerURL :: BSL.ByteString
  , rank :: Rank
  , games :: Int
  , winrate :: Int
  } deriving (Eq, Show)

instance Ord Player where
  compare p1 p2
    | rank p1 > rank p2 = GT
    | rank p1 < rank p2 = LT
    | games p1 > games p2 = GT
    | games p1 < games p2 = LT
    | otherwise = EQ

readRank :: BSL.ByteString -> Rank
readRank "D2"  = D2
readRank "D1"  = D1
readRank "M1"  = M
readRank "GM1" = GM
readRank "C1"  = C

getTop3Url :: Scraper BSL.ByteString BSL.ByteString
getTop3Url = mappend "https:" <$> (attr "href" $ "div" @: [hasClass "ranking-summary-summoner__item"] // "a")

getTop3Rank :: Scraper BSL.ByteString Rank
getTop3Rank = readRank <$> (text $ "div" @: [hasClass "ranking-summary-summoner__tierlevel"] // "b")

getTop3Games :: Scraper BSL.ByteString Int
getTop3Games = (read . BSL.toString) <$> (text $ "div" @: [hasClass "ranking-summary-summoner__gamecount"] // "b")

dropLast :: String -> String
dropLast s = take (length s - 1) s

getTop3Winrate :: Scraper BSL.ByteString Int
getTop3Winrate = (read . dropLast . BSL.toString) <$> (text $ "div" @: [hasClass "ranking-summary-summoner__gamecount"] // "small")

getTop3Player :: Scraper BSL.ByteString Player
getTop3Player = do
  plink <- getTop3Url
  prank <- getTop3Rank
  pgames <- getTop3Games
  pwinrate <- getTop3Winrate
  return $ Player plink prank pgames pwinrate

getTop3Players :: Scraper BSL.ByteString [Player]
getTop3Players = chroots ("div" @: [hasClass "ranking-summary-summoner__item"]) getTop3Player

getUrl :: Scraper BSL.ByteString BSL.ByteString
getUrl = mappend "https:" <$> (attr "href" $ "td" @: [hasClass "champion-ranking-table__cell--summoner"] // "a")

getRank :: Scraper BSL.ByteString Rank
getRank = readRank <$> (text $ "span" @: [hasClass "champion-ranking-table__tier"])

getGames :: Scraper BSL.ByteString Int
getGames = (read . takeWhile isDigit . BSL.toString) <$> (text $ "td" @: [hasClass "champion-ranking-table__cell--gamecount"])

getWinrate :: Scraper BSL.ByteString Int
getWinrate = (read . dropLast . BSL.toString) <$> (text $ "span" @: [hasClass "winratio__text"])

getPlayer :: Scraper BSL.ByteString Player
getPlayer = do
  plink <- getUrl
  prank <- getRank
  pgames <- getGames
  pwinrate <- getWinrate
  return $ Player plink prank pgames pwinrate

getPlayers :: Scraper BSL.ByteString [Player]
getPlayers = chroots ("table" @: [hasClass "champion-ranking-table"] // "tr") getPlayer

scrapeTop10 :: BSL.ByteString -> [Player]
scrapeTop10 page = take 10 $ reverse $ sort (top3 ++ rest)
  where
    top3 = fromJust $ scrapeStringLike page getTop3Players
    rest = fromJust $ scrapeStringLike page getPlayers

maxUrlLength :: [Player] -> Int
maxUrlLength = maximum . map (length . BSL.toString . playerURL)

createTable :: [Player] -> String
createTable players = intercalate "\n" $ map (playerToString maxUrlLen) players
  where
    maxUrlLen = maxUrlLength players

playerToString :: Int -> Player -> String
playerToString maxUrlLen p =
     (BSL.toString $ playerURL p)
  ++ padding
  ++ (show $ rank p) ++ "\t"
  ++ (show $ games p) ++ "\t"
  ++ (show $ winrate p)
  where
    currentUrlLen = length $ BSL.toString $ playerURL p
    padding = replicate (maxUrlLen - currentUrlLen + 4) ' '

fetchDataFromURL :: String -> IO BSL.ByteString
fetchDataFromURL url = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request manager
  return $ HTTP.responseBody response

fetchVersionsJSON :: IO BSL.ByteString
fetchVersionsJSON = fetchDataFromURL "https://ddragon.leagueoflegends.com/api/versions.json"

parseLatestVersion :: BSL.ByteString -> String
parseLatestVersion = head . fromJust . decode

fetchLatestVersion :: IO String
fetchLatestVersion = do
  json <- fetchVersionsJSON
  return $ parseLatestVersion json

updateChampionJSON :: IO ()
updateChampionJSON = do
  version <- fetchLatestVersion
  championJSON <- fetchDataFromURL $ "http://ddragon.leagueoflegends.com/cdn/" ++ version ++ "/data/en_US/champion.json"
  -- Convert to lowercase for case insensitive search
  let lowerChampionJSON = T.encodeUtf8 $ T.toLower $ T.decodeUtf8 championJSON
  BSL.writeFile "champion.json" lowerChampionJSON

parseChampionMap :: Value -> Parser (HM.HashMap String String)
parseChampionMap = withObject "object" $ \obj -> do
  d <- obj .: "data"
  mapM (.: "key") d

loadChampionMap :: IO (HM.HashMap String String)
loadChampionMap = do
  file <- BSL.readFile "champion.json"
  let decoded = fromJust $ decode file :: Value
  return $ fromJust $ parseMaybe parseChampionMap decoded

findChampionKeyOrDie :: HM.HashMap String String -> String -> IO String
findChampionKeyOrDie champions champion =
  case maybeKey of
    Just key -> return key
    Nothing -> die $ "Unrecognized champion: " ++ champion
  where
    -- Convert to lowercase for case insensitive search
    championLower = map toLower champion
    maybeKey = champions HM.!? championLower

findPrefixOrDie :: String -> IO String
findPrefixOrDie region =
  case maybePrefix of
    Just prefix -> return prefix
    Nothing -> die $ "Unrecognized region: " ++ region
  where
    -- Convert to lowercase for case insensitive search
    regionLower = map toLower region
    maybePrefix = prefixes HM.!? regionLower

printTable :: String -> String -> IO ()
printTable region champion = do
  urlPrefix <- findPrefixOrDie region 
  championMap <- loadChampionMap
  championKey <- findChampionKeyOrDie championMap champion
  page <- fetchDataFromURL $ "https://" ++ urlPrefix ++ ".op.gg/ranking/ajax2/champions.list/championId=" ++ championKey
  let top10 = scrapeTop10 page
  putStrLn $ createTable top10

printHelp :: IO ()
printHelp = do
  putStrLn "To view top10: lctop REGION CHAMPION\n"
  putStrLn "Available regions:"
  putStrLn $ intercalate "\n" $ HM.elems prefixes
  putStrLn "\nTo update champion list: lctop update"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("update":[]) -> updateChampionJSON
    (region:champion:[]) -> printTable region champion
    otherwise -> printHelp
    

