
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Word
import Data.Int
import System.IO
import Text.Printf
import Data.List
import Data.List.Split
import qualified Data.List as L
import qualified Data.Time.Clock.POSIX as Posix
import qualified Data.Time.Format as Format
import qualified Data.Time.Clock as Clock
import System.Environment

import Data.Binary.Get


date_format :: String
date_format = "%F %H:%M:%S"


quote_label :: String
quote_label = "B6034"


msg_offset :: Int64
msg_offset = 42


sec_threshold :: Int
sec_threshold = 3


formatPTime :: Clock.UTCTime -> String
formatPTime utc_time = Format.formatTime Format.defaultTimeLocale date_format utc_time


data PHeader = PHeader {
  ts_sec :: Word32,
  ts_usec :: Word32,
  incl_len :: Word32,
  orig_len :: Word32,
  utc_time :: Clock.UTCTime
  } deriving Show


getPHeader :: BL.ByteString -> (PHeader, BL.ByteString)
getPHeader bytes = (runGet parse head, rest)
  where
    (head, rest) = BL.splitAt 16 bytes
    parse :: Get PHeader
    parse = do
      ts_sec <- getWord32le
      ts_usec <- getWord32le
      incl_len <- getWord32le
      orig_len <- getWord32le
      let
        seconds = fromIntegral ts_sec
        utc_time = Posix.posixSecondsToUTCTime seconds
      return PHeader {
        ts_sec = ts_sec,
        ts_usec = ts_usec,
        incl_len = incl_len,
        orig_len = orig_len,
        utc_time = utc_time
        }


data GHeader = GHeader {
  magic_number  :: Word32,
  version_major :: Word16,
  version_minor :: Word16,
  thiszone      :: Int32,
  sigfigs       :: Word32,
  snaplen       :: Word32,
  network       :: Word32
  } deriving Show


data Packet = Packet {
  header :: PHeader,
  message :: Message
  } deriving Show


data Message = Message {
  quote :: String,
  issue_code :: String,

  bprice1 :: String,
  bqty1 :: String,

  bprice2 :: String,
  bqty2 :: String,

  bprice3 :: String,
  bqty3 :: String,

  bprice4 :: String,
  bqty4 :: String,

  bprice5 :: String,
  bqty5 :: String,

  aprice1 :: String,
  aqty1 :: String,

  aprice2 :: String,
  aqty2 :: String,

  aprice3 :: String,
  aqty3 :: String,

  aprice4 :: String,
  aqty4 :: String,

  aprice5 :: String,
  aqty5 :: String,

  accept_time :: String,

  period :: Int

  } | NoMessage deriving Show


getMessage :: BL.ByteString -> Message
getMessage bytes
  | BL.length bytes < 5 = NoMessage
  | CL.unpack (BL.take 5 bytes) /= quote_label = NoMessage
  | otherwise = runGet parse bytes
  where
    parse :: Get Message
    parse = do
      quote <- getByteString 5
      issue_code <- getByteString 12

      skip 3 -- Issue seq.-no.
      skip 2 -- Market Status Type
      skip 7 -- Total bid quote volume

      bprice1 <- getByteString 5
      bqty1 <- getByteString 7

      bprice2 <- getByteString 5
      bqty2 <- getByteString 7

      bprice3 <- getByteString 5
      bqty3 <- getByteString 7

      bprice4 <- getByteString 5
      bqty4 <- getByteString 7

      bprice5 <- getByteString 5
      bqty5 <- getByteString 7

      skip 7 -- Total ask quote volume

      aprice1 <- getByteString 5
      aqty1 <- getByteString 7

      aprice2 <- getByteString 5
      aqty2 <- getByteString 7

      aprice3 <- getByteString 5
      aqty3 <- getByteString 7

      aprice4 <- getByteString 5
      aqty4 <- getByteString 7

      aprice5 <- getByteString 5
      aqty5 <- getByteString 7

      skip 50 -- No. fields in total

      accept_time <- getByteString 8

      let (hh, rest1) = B.splitAt 2 accept_time
          (mm, rest2) = B.splitAt 2 rest1
          (ss, _) = B.splitAt 2 rest2
          hh_int = read (C8.unpack hh) :: Int
          mm_int = read (C8.unpack mm) :: Int
          ss_int = read (C8.unpack ss) :: Int
          seconds = hh_int * 3600 + mm_int * 60 + ss_int
          period = mod seconds sec_threshold

      return Message {
        quote = C8.unpack quote,
        issue_code = C8.unpack issue_code,

        bprice1 = C8.unpack bprice1,
        bqty1 = C8.unpack bqty1,

        bprice2 = C8.unpack bprice2,
        bqty2 = C8.unpack bqty2,

        bprice3 = C8.unpack bprice3,
        bqty3 = C8.unpack bqty3,

        bprice4 = C8.unpack bprice4,
        bqty4 = C8.unpack bqty4,

        bprice5 = C8.unpack bprice5,
        bqty5 = C8.unpack bqty5,

        aprice1 = C8.unpack aprice1,
        aqty1 = C8.unpack aqty1,

        aprice2 = C8.unpack aprice2,
        aqty2 = C8.unpack aqty2,

        aprice3 = C8.unpack aprice3,
        aqty3 = C8.unpack aqty3,

        aprice4 = C8.unpack aprice4,
        aqty4 = C8.unpack aqty4,

        aprice5 = C8.unpack aprice5,
        aqty5 = C8.unpack aqty5,

        accept_time = C8.unpack accept_time,

        period = period
        }


getGHeader :: BL.ByteString -> (GHeader, BL.ByteString)
getGHeader bytes = (runGet parse head, rest)
  where
    parse :: Get GHeader
    parse = GHeader <$> getWord32le <*> getWord16le <*> getWord16le
            <*> getInt32le <*>  getWord32le <*> getWord32le <*> getWord32le
    (head, rest) = BL.splitAt 24 bytes


getPacket :: BL.ByteString -> (Packet, BL.ByteString)
getPacket bytes = (Packet header message, return_bytes)
  where
    (header, rest_bytes) = getPHeader bytes
    PHeader{incl_len = incl_len} = header
    offset = fromIntegral incl_len
    (body_bytes, return_bytes) = BL.splitAt offset rest_bytes
    (_, message_bytes) = BL.splitAt msg_offset body_bytes
    message = getMessage message_bytes


getPackets :: BL.ByteString -> [Packet]
getPackets bytes
  | BL.null bytes = []
  | otherwise = packet:packets
    where
      (packet, rest_bytes) = getPacket bytes
      packets = getPackets rest_bytes


formatPacket :: Packet -> String
formatPacket packet = printf "%s %s %s %s@%s %s@%s %s@%s %s@%s %s@%s %s@%s %s@%s %s@%s %s@%s %s@%s"
                      pkt_time accept_time issue_code bqty5 bprice5 bqty4 bprice4 bqty3 bprice3 bqty2
                      bprice2 bqty1 bprice1 aqty1 aprice1 aqty2 aprice2 aqty3 aprice3 aqty4 aprice4
                      aqty5 aprice5
  where
    Packet{message = message, header = header} = packet
    PHeader{utc_time = utc_time} = header
    pkt_time = formatPTime utc_time
    Message{
      issue_code = issue_code,

      bqty5 = bqty5,
      bprice5 = bprice5,

      bqty4 = bqty4,
      bprice4 = bprice4,

      bqty3 = bqty3,
      bprice3 = bprice3,

      bqty2 = bqty2,
      bprice2 = bprice2,

      bqty1 = bqty1,
      bprice1 = bprice1,

      aqty1 = aqty1,
      aprice1 = aprice1,

      aqty2 = aqty2,
      aprice2 = aprice2,

      aqty3 = aqty3,
      aprice3 = aprice3,

      aqty4 = aqty4,
      aprice4 = aprice4,

      aqty5 = aqty5,
      aprice5 = aprice5,

      accept_time = accept_time

      } = message


packetPredicate :: Packet -> Bool
packetPredicate Packet{message=NoMessage} = False
packetPredicate Packet{message=Message{}} = True


sorter :: Packet -> String
sorter packet = accept_time
  where
    Packet{message=message} = packet
    accept_time = case message of
      Message{accept_time=accept_time} -> accept_time
      NoMessage -> ""


flattern :: [[a]] -> [a]
flattern xss = foldr (++) [] xss


grouper :: Packet -> Packet -> Bool
grouper packet1 packet2 = period1 == period2
  where
    Packet{message=message1} = packet1
    Packet{message=message2} = packet2
    period1 = case message1 of
      Message{period=period1} -> period1
      NoMessage -> 0
    period2 = case message2 of
      Message{period=period2} -> period2
      NoMessage -> 0


revPackets :: [Packet] -> [Packet]
revPackets packets = flattern chunks_sorted
  where
    chunks = groupBy grouper packets
    chunks_sorted = [sortOn sorter chunk | chunk <- chunks]


printPackets = mapM_ (putStrLn . formatPacket)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "No file name provided!"
    (filePath:args_rest) -> withFile filePath ReadMode $ \h -> do
      bytes <- BL.hGetContents h
      let (global_header, bytes_rest) = getGHeader bytes
          packets_all = getPackets bytes_rest
          packets_clear = L.filter packetPredicate packets_all
      case args_rest of
        [] -> printPackets packets_clear
        ("-r":_) -> printPackets $ revPackets packets_clear
