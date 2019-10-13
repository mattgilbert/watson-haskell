{-# LANGUAGE OverloadedStrings #-}

module UUID where

import Prelude hiding (take, drop)
import qualified Data.UUID as UUID_
import qualified Data.UUID.V1 as UUIDv1 (nextUUID)
import qualified Data.List as List
import Data.Text as Text
import qualified Data.List as List (take)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8 (filter)
import Data.Aeson (withText, Value(..), eitherDecode', encode, decode, decode', ToJSON(..), FromJSON(..))
import qualified Data.Aeson.Encoding as E (unsafeToEncoding)

newtype UUID' = MkUUID UUID_.UUID

instance Show UUID' where
    show (MkUUID u) = show u

instance Eq UUID' where
    (MkUUID u1) == (MkUUID u2) = u1 == u2

instance FromJSON UUID.UUID' where
    parseJSON = withText "UUID" $
        maybe (fail "invalid UUID") pure . UUID.fromText

instance ToJSON UUID.UUID' where
    toJSON = toJSON . UUID.toText
    toEncoding =  E.unsafeToEncoding . quote . B.byteString . removeHyphens . UUID.toASCIIBytes
        where
            quote b = B.char8 '"' <> b <> B.char8 '"'

removeHyphens :: ByteString -> ByteString
removeHyphens = BS8.filter (\b -> b /= '-')

toString :: UUID' -> String
toString (MkUUID u) =
    List.take 7 $ UUID_.toString u

fromText :: Text -> Maybe UUID'
fromText uuidStr =
    MkUUID <$> (UUID_.fromText addHyphens)
    where
        addHyphens = 
            case (find (=='-') uuidStr) of
                Just _ -> uuidStr
                Nothing ->
                    intercalate "-"
                        [ chunk 0 8 uuidStr
                        , chunk 8 4 uuidStr
                        , chunk 12 4 uuidStr
                        , chunk 16 4 uuidStr
                        , chunk 20 12 uuidStr
                        ]

        chunk a b = take b . drop a

toText :: UUID' -> Text
toText (MkUUID u) = UUID_.toText u

toASCIIBytes :: UUID' -> ByteString
toASCIIBytes (MkUUID u) = UUID_.toASCIIBytes u

nil :: UUID'
nil = MkUUID UUID_.nil

nextUUID :: IO (Maybe UUID')
nextUUID = do
    newId <- UUIDv1.nextUUID
    return $ MkUUID <$> newId

partialStringMatch :: String -> UUID' -> Bool
partialStringMatch str (MkUUID uuid) = 
    (List.take (Prelude.length str) $ UUID_.toString uuid) == str

