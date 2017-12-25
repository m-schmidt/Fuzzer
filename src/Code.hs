-- |Module to generate C Code
module Code where

import Control.Applicative ((<**>))
--import Control.Monad
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 as L (ByteString)
import Data.Foldable
import Data.Monoid
import Data.Sequence


-- C base elements
space :: Builder
space = char8 ' '

comma :: Builder
comma = char8 ','

semicolon :: Builder
semicolon = char8 ';'

newline :: Builder
newline = char8 '\n'

inset :: Builder -> Builder
inset = (<>) (string8 "  ")

cIf :: Builder
cIf = string8 "if"

cElse :: Builder
cElse = string8 "else"

cFor :: Builder
cFor = string8 "for"

cWhile :: Builder
cWhile = string8 "while"

cDo :: Builder
cDo = string8 "do"


-- |Intersperse a sequence with an element
intersperse :: a -> Seq a -> Seq a
intersperse y xs = case viewl xs of
  EmptyL -> empty
  p :< ps -> p <| (ps <**> (const y <| singleton id))


-- |Enclose a builder in parentheses
enParen :: Builder -> Builder
enParen b = string8 "(" <> b <> string8 ")"


-- |Create a function call from function name 'f' and parameters 'ps'
mkCall :: Builder -> Seq Builder -> Builder
mkCall f ps = f <> enParen params
    where params = fold $ intersperse (comma <> space) ps

mkCall' :: Builder -> [Builder] -> Builder
mkCall' f ps = mkCall f (fromList ps)


-- |Indent a sequence of builders 'sb' by one step and enclose the result in curly braces
mkBlock :: Seq Builder -> Seq Builder
mkBlock sb = string8 "{" <| (inset <$> sb |> string8 "}")


-- |Create an if-statement from a condition 'cond' and a sequence of code when the condition is met
mkIf :: Builder -> Seq Builder -> Seq Builder
mkIf cond code = cIf <> space <> enParen cond <| mkBlock code


-- |Create an if-else-statement from a condition and two code sequences the true/false case
mkIfElse :: Builder -> Seq Builder -> Seq Builder -> Seq Builder
mkIfElse cond ifTrue ifFalse =
  singleton (cIf <> space <> enParen cond)
  >< mkBlock ifTrue
  >< singleton cElse
  >< mkBlock ifFalse


-- |Create a for-loop
mkFor :: Builder -> Builder -> Builder -> Seq Builder -> Seq Builder
mkFor setup cond increment body =
  cFor <> space <> enParen params <| mkBlock body
  where
    params = setup <> sep <> cond <> sep <> increment
    sep = semicolon <> space


-- |Create a while-loop from a condition and a sequence of code when the condition is met
mkWhile :: Builder -> Seq Builder -> Seq Builder
mkWhile cond body =
  cWhile <> space <> enParen cond <| mkBlock body


-- |Create a do-while-loop from a condition and a sequence of code when the condition is met
mkDo :: Seq Builder -> Builder -> Seq Builder
mkDo body cond =
  singleton cDo >< mkBlock body >< singleton (cWhile <> space <> enParen cond)



-- |Fold a code sequence into a lazy bytestring
toLBS :: Seq Builder -> L.ByteString
toLBS sb = toLazyByteString $ fold $ intersperse newline sb
