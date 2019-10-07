{-# LANGUAGE OverloadedStrings #-}

module UUID where

import Prelude hiding (take, drop)
import qualified Data.UUID as GUID
import qualified Data.UUID.V1 as UUIDv1 (nextUUID)
import Data.Text as Text
import Data.ByteString (ByteString)

newtype UUID = MkUUID GUID.UUID

fromText :: Text -> Maybe UUID
fromText uuidStr =
    MkUUID <$> (GUID.fromText addHyphens)
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

toText :: UUID -> Text
toText (MkUUID u) = GUID.toText u

toASCIIBytes :: UUID -> ByteString
toASCIIBytes (MkUUID u) = GUID.toASCIIBytes u

nil :: UUID
nil = MkUUID GUID.nil

nextUUID :: IO (Maybe UUID)
nextUUID = do
    newId <- UUIDv1.nextUUID
    return $ MkUUID <$> newId
