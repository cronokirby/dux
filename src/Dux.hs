{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Dux
    ( Expr(..)
    , Label(..)
    , Hash
    , prettyHash
    , hashExpr
    , parseExpr
    )
where

import           Relude

import           Data.Binary                    ( encode )
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Char8         as BS
import qualified Data.Digest.Pure.SHA          as SHA
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L


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

type Parser = P.Parsec Void Text

sc :: Parser ()
sc = L.space P.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

int :: Parser Integer
int = lexeme L.decimal

string :: Parser Text
string =
    fromString <$> lexeme (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

label :: Parser Text
label = fromString <$> lexeme go
  where
    go = do
        c    <- P.letterChar
        rest <- P.manyTill (P.letterChar <|> P.digitChar) (P.char ':')
        return (one c <> rest)

bytes :: Parser ByteString
bytes = lexeme $ do
    _      <- P.char 'x'
    digits <- P.some P.hexDigitChar
    return (BS.pack (convert digits))
  where
    hexToInt :: Char -> Int
    hexToInt c | c <= '9'  = ord c - ord '0'
               | c <= 'F'  = ord c - ord 'A'
               | otherwise = ord c - ord 'a'
    join :: Char -> Char -> Char
    join x1 x2 = chr (16 * hexToInt x1 + hexToInt x2)
    convert :: String -> String
    convert []             = []
    convert [x           ] = [join x '0']
    convert (x1 : x2 : xs) = join x1 x2 : convert xs

expr :: Parser Expr
expr = sc *> (number <|> str <|> bytestr <|> P.try tuple <|> P.try record)
  where
    tuple :: Parser Expr
    tuple = Tuple <$> parens (expr `P.sepBy` symbol ",")
    record :: Parser Expr
    record = Record <$> parens (recordEl `P.sepBy` symbol ",")
    recordEl :: Parser (Label, Expr)
    recordEl = (,) <$> (Label <$> label) <*> expr
    number :: Parser Expr
    number = Number . fromInteger <$> int
    str :: Parser Expr
    str = Str <$> string
    bytestr :: Parser Expr
    bytestr = Bytes <$> bytes

parseExpr :: Text -> Either (P.ParseErrorBundle Text Void) Expr
parseExpr = P.parse expr ""
