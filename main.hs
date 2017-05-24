{-# LANGUAGE OverloadedStrings
    , FlexibleInstances
    , UndecidableInstances
    , IncoherentInstances
    #-}

import Numeric (showIntAtBase)
import Data.Char (chr, ord)
import Data.Csv
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy.Encoding as E
import System.Process (system)

type Prog = [Statement]

data BitDev =
    X Int
  | Y Int
  | M Int
  | B Int
  | SM Int
  | Db Int Int
  | Wb Int Int
  | ZRb Int Int
  | SDb Int Int
  deriving (Show, Eq, Ord)

data WordDev =
    D Int
  | W Int
  | ZR Int
  | SD Int
  | Z Int
  | Xk Int Int
  | Yk Int Int
  | Mk Int Int
  | Bk Int Int
  | SMk Int Int
  deriving (Show, Eq, Ord)

data ConstDev =
    K Int
  | H Int
  deriving (Show, Eq, Ord)

data TimerDev = T Int deriving (Show, Eq, Ord)
data CounterDev = C Int deriving (Show, Eq, Ord)

class Dev a where
  dev :: a -> DevTx

instance Dev BitDev where
  dev (X i) = T.cons 'X' (numh3 i)
  dev (Y i) = T.cons 'Y' (numh3 i)
  dev (M i) = T.cons 'M' (numd3 i)
  dev (B i) = T.cons 'B' (numh3 i)
  dev (SM i) = T.append "SM" (numd3 i)
  dev (Db i n) = T.concat ["D", numd3 i, ".", T.pack $ numh 1 n]
  dev (Wb i n) = T.concat ["W", numh3 i, ".", T.pack $ numh 1 n]
  dev (ZRb i n) = T.concat ["ZR", numd3 i, ".", T.pack $ numh 1 n]
  dev (SDb i n) = T.concat ["SD", numd3 i, ".", T.pack $ numh 1 n]

instance Dev WordDev where
  dev (D i) = T.cons 'D' (numd3 i)
  dev (W i) = T.cons 'W' (numh3 i)
  dev (SD i) = T.append "SD" (numd3 i)
  dev (Z i) = T.cons 'Z' (numd3 i)
  dev (Xk i n) = T.concat ["K", T.pack $ show n, "X", numh3 i]
  dev (Yk i n) = T.concat ["K", T.pack $ show n, "Y", numh3 i]
  dev (Mk i n) = T.concat ["K", T.pack $ show n, "M", numd3 i]
  dev (Bk i n) = T.concat ["K", T.pack $ show n, "B", numh3 i]
  dev (SMk i n) = T.concat ["K", T.pack $ show n, "SM", numd3 i]

instance Dev ConstDev where
  dev (K i) = T.cons 'K' (numd3 i)
  dev (H i) = T.cons 'H' (numh3 i)

instance Dev TimerDev where
  dev (T i) = T.cons 'T' (numd3 i)

instance Dev CounterDev where
  dev (C i) = T.cons 'C' (numd3 i)

class Dev a => SWord a

instance SWord WordDev

instance SWord TimerDev

instance SWord CounterDev

instance SWord ConstDev

class Dev a => DWord a

instance DWord WordDev

instance DWord TimerDev

instance DWord CounterDev

class Dev a => SBit a
instance SBit BitDev
instance SBit TimerDev
instance SBit CounterDev

class Dev a => DBit a
instance DBit BitDev
instance DBit TimerDev
instance DBit CounterDev

data LdType = LdNorm
  | LdPls
  | LdPlf
  deriving (Show, Eq)

type DevTx = Text

data Expr = AND Expr Expr
             | OR Expr Expr
             | LD LdType Bool DevTx
             | COMP CompT CompO DevTx DevTx
             | MPS
             | MRD
             | MPP
             deriving (Show, Eq)

data CompT = CTw | CTd | CTe | CTs
             deriving (Show, Eq, Ord, Enum)

data CompO = LdDiff | LdLt | LdLtEq | LdEq | LdGtEq | LdGt
             deriving (Show, Eq, Ord, Enum)

class ToExpr a where
  toExpr :: Bool -> a -> Expr

instance Dev a => ToExpr a where
  toExpr b a = LD LdNorm b (dev a)

instance ToExpr Expr where
  toExpr False a = notE a
  toExpr True a = a

data Statement = Statement Expr [Statement]
                 | Command Text [DevTx]
                 deriving (Show)

data ILstate = ILAND
 | ILOR
 | ILLD
 deriving (Show, Eq)


data Row = Row {rowStep :: Text
                , rowStmt :: Text
                , rowCmd :: Text
                , rowDev :: Text
                , rowNone :: Text
                , rowPIstmg :: Text
                , rowNote :: Text}
           | Head
           deriving (Show)

instance ToRecord Row where
  toRecord (Row step stmt cmd dev none pis note) = record
    [toField step
    , toField stmt
    , toField cmd
    , toField dev
    , toField none
    , toField pis
    , toField note]
  toRecord Head = record []

defaultRow = Row "" "" "" "" "" "" ""

showDigit :: Int -> Int -> String
showDigit d n = showIntAtBase d numChr n ""

numChr :: Int -> Char
numChr n
  | n >= 10   = chr $ (n - 10) + (ord 'A')
  | otherwise = chr $ n + (ord '0')

rjust :: Int -> a -> [a] -> [a]
rjust n c str = replicate (n - length str) c ++ str

numh :: Int -> Int -> String
numh d n = rjust d '0' (showDigit 16 n)

numh3 :: Int -> Text
numh3 n = T.pack $ numh 3 n

numd :: Int -> Int -> String
numd d n = rjust d '0' (showDigit 10 n)

numd3 :: Int -> Text
numd3 n = T.pack $ numd 3 n

ld :: Dev a => a -> Expr
ld = LD LdNorm True . dev

ldi :: Dev a => a -> Expr
ldi = LD LdNorm False . dev

ldp :: Dev a => a -> Expr
ldp = LD LdPls True . dev

ldpi :: Dev a => a -> Expr
ldpi = LD LdPls False . dev

ldf :: Dev a => a -> Expr
ldf = LD LdPlf True . dev

ldfi :: Dev a => a -> Expr
ldfi = LD LdPlf False . dev

ldeq :: (SWord a, SWord b) => a -> b -> Expr
ldeq a b = COMP CTw LdEq (dev a) (dev b)

ldlt :: (SWord a, SWord b) => a -> b -> Expr
ldlt a b = COMP CTw LdLt (dev a) (dev b)

ldlteq :: (SWord a, SWord b) => a -> b -> Expr
ldlteq a b = COMP CTw LdLtEq (dev a) (dev b)

ldgteq :: (SWord a, SWord b) => a -> b -> Expr
ldgteq a b = COMP CTw LdGtEq (dev a) (dev b)

ldgt :: (SWord a, SWord b) => a -> b -> Expr
ldgt a b = COMP CTw LdGt (dev a) (dev b)

lddiff :: (SWord a, SWord b) => a -> b -> Expr
lddiff a b = COMP CTw LdDiff (dev a) (dev b)

infixl 7 &&&
infixl 7 ~&&
infixl 7 &&~
infixl 7 ~&~

(&&&) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a &&& b = AND (toExpr True a) (toExpr True b)

(~&&) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a ~&& b = AND (toExpr False a) (toExpr True b)

(&&~) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a &&~ b = AND (toExpr True a) (toExpr False b)

(~&~) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a ~&~ b = AND (toExpr False a) (toExpr False b)

infixl 6 |||
infixl 6 ~||
infixl 6 ||~
infixl 6 ~|~

(|||) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a ||| b = OR (toExpr True a) (toExpr True b)

(~||) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a ~|| b = OR (toExpr False a) (toExpr True b)

(||~) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a ||~ b = OR (toExpr True a) (toExpr False b)

(~|~) :: (ToExpr a, ToExpr b) => a -> b -> Expr
a ~|~ b = OR (toExpr False a) (toExpr False b)

notE :: Expr -> Expr
notE (LD tt b t) = LD tt (not b) t 
notE (AND e1 e2) = (OR (notE e1) (notE e2))
notE (OR e1 e2) = (AND (notE e1) (notE e2))

out :: DBit a => a -> Statement
out d = Command "OUT" [dev d]

set :: Dev a => a -> Statement
set d = Command "SET" [dev d]

rst :: Dev a => a -> Statement
rst d = Command "RST" [dev d]

pls :: DBit a => a -> Statement
pls d = Command "PLS" [dev d]

plf :: DBit a => a -> Statement
plf d = Command "PLF" [dev d]

tim :: SWord a => Int -> a -> Statement
tim t d = Command "OUT" [dev (T t), dev d]

cnt :: SWord a => Int -> a -> Statement
cnt t d = Command "OUT" [dev (C t), dev d]

mov :: (SWord a, DWord b) => a -> b -> Statement
mov a b = Command "MOV" [dev a, dev b]

dmov :: (SWord a, DWord b) => a -> b -> Statement
dmov a b = Command "DMOV" [dev a, dev b]

cmd = Command
stmt = Statement

andE ls = foldr1  AND $ map ld ls

parse :: Prog -> [Row]
parse ss = replicate 3 Head
  ++ parseS (ss ++ [Command "END" []])

parseS :: [Statement] -> [Row]
parseS s = parseS1 s


commandRecord c [] = [defaultRow {rowCmd = c}]
commandRecord c (d:ds) = [defaultRow {rowCmd = c, rowDev =d}] ++ map devRecord ds
  where
    devRecord d1 = defaultRow {rowDev = d1}

parseS1 ((Command c ds):[]) = commandRecord c ds
parseS1 ((Command c ds):sss) = commandRecord c ds ++ parseS1 sss
parseS1 ((Statement e ss):[]) = parseE e ++ parseS2 False ss
parseS1 ((Statement e ss):sss) = parseE e ++ parseS2 False ss ++ parseS1 sss

parseS2 False ((Command c ds):[]) = commandRecord c ds
parseS2 True ((Command c ds):[]) = parseE MPP ++ commandRecord c ds
parseS2 False ((Command c ds):sss) = commandRecord c ds ++ parseS2 False sss
parseS2 True ((Command c ds):sss) = parseE MPP ++ commandRecord c ds ++ parseS2 False sss
parseS2 False ((Statement e ss):[]) = parseE e ++ parseS2 False ss
parseS2 True ((Statement e ss):[]) = parseE (MPP &&& e) ++ parseS2 False ss
parseS2 False ((Statement e ss):sss) = parseE (MPS &&& e) ++ parseS2 False ss ++ parseS2 True sss
parseS2 True ((Statement e ss):sss) = parseE (MRD &&& e) ++ parseS2 False ss ++ parseS2 True sss

parseE e = pe ILLD e
  where
    pe :: ILstate -> Expr -> [Row]
    pe _ MPS = [defaultRow {rowCmd = "MPS"}]
    pe _ MRD = [defaultRow {rowCmd = "MRD"}]
    pe _ MPP = [defaultRow {rowCmd = "MPP"}]
    pe i (LD t b d) = [defaultRow {rowCmd = ldCmd i t b, rowDev = d}]
    pe i (COMP t o d1 d2) = [defaultRow {rowCmd = comp i t o, rowDev = d1}, defaultRow {rowDev = d2}]
    pe ILLD (AND e1 e2) = pe ILLD e1 ++ pe ILAND e2
    pe ILAND (AND e1 e2) = pe ILAND e1 ++ pe ILAND e2
    pe ILOR (AND e1 e2) = pe ILLD e1 ++ pe ILAND e2 ++ [defaultRow {rowCmd = "ORB"}]
    pe ILLD (OR e1 e2) = pe ILLD e1 ++ pe ILOR e2
    pe ILAND (OR e1 e2) = pe ILLD e1 ++ pe ILOR e2 ++ [defaultRow {rowCmd = "ANB"}]
    pe ILOR (OR e1 e2) = pe ILOR e1 ++ pe ILOR e2
    ldCmd ILLD LdNorm True = "LD"
    ldCmd ILLD LdNorm False = "LDI"
    ldCmd ILLD LdPls True = "LDP"
    ldCmd ILLD LdPls False = "LDPI"
    ldCmd ILLD LdPlf True = "LDF"
    ldCmd ILLD LdPlf False = "LDFI"
    ldCmd ILAND LdNorm True = "AND"
    ldCmd ILAND LdNorm False = "ANI"
    ldCmd ILAND LdPls True = "ANDP"
    ldCmd ILAND LdPls False = "ANDPI"
    ldCmd ILAND LdPlf True = "ANDF"
    ldCmd ILAND LdPlf False = "ANDFI"
    ldCmd ILOR LdNorm True = "OR"
    ldCmd ILOR LdNorm False = "ORI"
    ldCmd ILOR LdPls True = "ORP"
    ldCmd ILOR LdPls False = "ORPI"
    ldCmd ILOR LdPlf True = "ORF"
    ldCmd ILOR LdPlf False = "ORFI"
    comp ILLD CTw LdDiff = "LD<>"
    comp ILLD CTw LdLt = "LD<"
    comp ILLD CTw LdLtEq = "LD<="
    comp ILLD CTw LdEq = "LD="
    comp ILLD CTw LdGtEq = "LD>="
    comp ILLD CTw LdGt = "LD>"
    comp ILAND CTw LdDiff = "AND<>"
    comp ILAND CTw LdLt = "AND<"
    comp ILAND CTw LdLtEq = "AND<="
    comp ILAND CTw LdEq = "AND="
    comp ILAND CTw LdGtEq = "AND>="
    comp ILAND CTw LdGt = "AND>"
    comp ILOR CTw LdDiff = "OR<>"
    comp ILOR CTw LdLt = "OR<"
    comp ILOR CTw LdLtEq = "OR<="
    comp ILOR CTw LdEq = "OR="
    comp ILOR CTw LdGtEq = "OR>="
    comp ILOR CTw LdGt = "OR>"
    comp ILLD CTd LdDiff = "LDD<>"
    comp ILLD CTd LdLt = "LDD<"
    comp ILLD CTd LdLtEq = "LDD<="
    comp ILLD CTd LdEq = "LDD="
    comp ILLD CTd LdGtEq = "LDD>="
    comp ILLD CTd LdGt = "LDD>"
    comp ILAND CTd LdDiff = "ANDD<>"
    comp ILAND CTd LdLt = "ANDD<"
    comp ILAND CTd LdLtEq = "ANDD<="
    comp ILAND CTd LdEq = "ANDD="
    comp ILAND CTd LdGtEq = "ANDD>="
    comp ILAND CTd LdGt = "ANDD>"
    comp ILOR CTd LdDiff = "ORD<>"
    comp ILOR CTd LdLt = "ORD<"
    comp ILOR CTd LdLtEq = "ORD<="
    comp ILOR CTd LdEq = "ORD="
    comp ILOR CTd LdGtEq = "ORD>="
    comp ILOR CTd LdGt = "ORD>"

normOn = ld $ SM 400
normOff = ldi $ SM 400

for :: Int -> Prog -> Prog
for n p = [ stmt normOn [mov (K 0) (Z 0)]
  , cmd "FOR" [dev $ K n]]
  ++ p ++
  [stmt normOn [cmd "INC" [dev $ Z 0]]
  , cmd "NEXT" []]

x0 = X 10 &&~ X 1 ||~ M 0 ||| M 1 &&& M 10 &&& M 12
x1 = foldr1 (&&&) $ map (ld . X) [0..11]
x2 = X 50 &&~ (X 1 ||~ M 0 ||| M 1) &&& M 10 &&& M 14
x3 = M 0x3a &&~ (X 1 ||~ M 0 ||| M 1) ||| M 10 &&& M 15

fx n = stmt 
         ((X n) &&~ (M $ n + 10) ||| (M n) &&~ (M $ n + 20) &&& ldgteq (K 0) (D n))
         [out (M n)
         , stmt ((M $ 30 + n) ||| (M $ 40 + n)) [out (Y n)]
         , stmt (ld (M $ 50 + n)) [out (Y $ 10 + n)]
         , stmt (ldp (M $ 60 + n)) [out (Y $ 20 + n)]
         , stmt (ldfi (M $ 70 + n)) [out (Y $ 30 + n)]
         ]

pg1 = (map fx [0..1])

main = do
  B.writeFile "testmain.csv" $ encode $ parse (for 21 pg1)
  system "iconv -f UTF-8 -t UTF-16LE testmain.csv > testmain16.csv"


  --B.writeFile "testmain.csv" $ encodeWith (defaultEncodeOptions {encDelimiter = 9, encQuoting = QuoteAll}) $ parseE x3
  -- B.writeFile "testmain.csv" $ encodeWith (defaultEncodeOptions {encQuoting = QuoteAll}) $ parseE x3
