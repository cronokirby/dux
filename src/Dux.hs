{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Dux
    ( Expr(..)
    , Label(..)
    , Hash
    , prettyHash
    , hashExpr
    )
where

import           Relude

import           Data.Binary                    ( encode )
import qualified Data.ByteString.Base16        as Base16
import qualified Data.Digest.Pure.SHA          as SHA

-- | Represents the label for a record
newtype Label = Label Text deriving (Eq, Show)

-- | Represents an expression.
data Expr
    -- | Represents a tuple `(1, 2)` containing multiple expressions
    = Tuple [Expr]
    -- | Represents a record, e.g. `(a: 1, b: 2)`
    | Record [(Label, Expr)]
    -- | Represents a numeric litteral, e.g. `-30`
    | Number Int64
    -- | Represents a string of data, e.g. `"foo"`
    | Str Text
    -- | Represents a sequence of bytes, e.g. `xDEADBEAF"
    | Bytes ByteString
    deriving (Eq, Show)

-- | Represents the result of hashing an expression
newtype Hash = Hash { getHash :: ByteString } deriving (Eq)

prettyHash :: Hash -> Text
prettyHash = decodeUtf8 . Base16.encode . getHash

hashBytes :: ByteString -> Hash
hashBytes = Hash . toStrict . SHA.bytestringDigest . SHA.sha512 . toLazy

-- | Combine a prefix and a sequence of hashes into a bigger hash
combine :: ByteString -> [Hash] -> Hash
combine prefix hashes =
    let bytes = mconcat (prefix : map getHash hashes) in hashBytes bytes

-- | Calculate the hash of an expression.
hashExpr :: Expr -> Hash
hashExpr expr = case expr of
    Tuple  exprs  -> combine "tuple" (map hashExpr exprs)
    Record record -> combine "record" (map hashRecordEl record)
    Number n      -> hashBytes ("number" <> toStrict (encode n))
    Str    txt    -> hashBytes ("string" <> encodeUtf8 txt)
    -- This means that bytes have a different hash than the equivalent
    -- binary file. This is necessary to avoid collisions between the following
    -- bytestring `b"stringA"` and the following string `"A"`. This would collide
    -- without hashing bytes differently.
    Bytes  bytes  -> hashBytes ("bytes" <> bytes)
  where
    hashRecordEl :: (Label, Expr) -> Hash
    hashRecordEl (label, expr) =
        combine "record-element" [hashLabel label, hashExpr expr]
    hashLabel :: Label -> Hash
    hashLabel (Label txt) = hashBytes ("label" <> encodeUtf8 txt)
