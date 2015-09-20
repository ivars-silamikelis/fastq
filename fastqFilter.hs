import System.Environment
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8
--import Data.Attoparsec.ByteString.Lazy
import qualified Data.Set as S
--import Test.QuickCheck
import Control.Applicative
data Settings = Settings { db :: String}

data Fastq = Fastq {
                     idField :: C.ByteString,
                     dna :: C.ByteString,
                     qual :: C.ByteString
                   } deriving (Show, Eq)
{-
instance Arbitrary C.ByteString where
    arbitrary = C.pack <$> arbitrary
    shrink xs = C.pack <$> shrink (C.unpack xs)

instance Arbitrary Fastq where
    arbitrary = Fastq <$> arbitrary <*> arbitrary <*> arbitrary
 -}
nl :: Parser Char
nl = satisfy (== '\n')

myAt :: Parser Char
myAt=satisfy (=='@')

isNl x = if x == '\n' then True else False
fastqParser :: Parser Fastq
fastqParser = do
    myAt
    idField <- takeTill isNl
    nl
    dna <- takeTill isNl
    nl
    pl <- takeTill isNl
    nl
    quality <- takeTill isNl
    nl
    return $ Fastq {idField=idField, dna=dna, qual=quality}

parseris contents = 
  case parse fastqParser contents of
    Fail _ _ y -> error y
    Done contents' x 
      |contents' /= C.pack "" -> x: parseris contents'
      |otherwise -> [x]


--filterById
--quickChecked
filterById :: [C.ByteString] -> [Fastq] -> [Fastq]
filterById _ [] = []
filterById [] _ = []
filterById (d:db) (el:els)
    | (idField el) == d = el : (filterById db els)
    | otherwise = (filterById (d:db) els)
{-
    | (idField el) `S.member` dataBase = el : (filterById db els)
    | otherwise = filterById db els
    where dataBase = S.fromList db
-}
fastqPrinter :: Fastq -> IO ()
fastqPrinter x = do
    putStr "@"
    C.putStrLn (idField x)
    C.putStrLn (dna x)
    putStrLn ("+")
    C.putStrLn (qual x)

main = do
    dbName <- getArgs
    db <- C.readFile (head dbName)
    cont <- C.getContents
    mapM_ fastqPrinter $ filterById (C.lines db) $ parseris cont 
