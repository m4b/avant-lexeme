module Scanner1(scan) where 
import FiniteStateAutomata(DFA'(..))
import ParseLang(Relevance(..))
import Recognize(tokenize)
import Data.List(maximumBy,lookup)
import Data.Maybe(fromJust)
type Location = Int
data Class = Base | Suffix | Whitespace deriving Show
data Token = Token {kind :: Class, relevance :: Relevance,location :: Location, token :: String} deriving Show
base :: DFA' Char
base = read "DFA' {alpha = fromList \"abor\", ss = fromList [(0,fromList [('b',1)]),(1,fromList [('a',1),('o',1),('r',2)]),(2,fromList [])], accept = fromList [2], st = 0}"
suffix :: DFA' Char
suffix = read "DFA' {alpha = fromList \"gin\", ss = fromList [(0,fromList [('i',1)]),(1,fromList [('n',2)]),(2,fromList [('g',3)]),(3,fromList [])], accept = fromList [3], st = 0}"
whitespace :: DFA' Char
whitespace = read "DFA' {alpha = fromList \"\n \", ss = fromList [(1,fromList [('\n',1),(' ',1)])], accept = fromList [1], st = 1}"
dfas :: [DFA' Char]
dfas = [base,suffix,whitespace]
base' :: Location -> String -> Token
base' = Token Base Relevant
suffix' :: Location -> String -> Token
suffix' = Token Suffix Irrelevant
whitespace' :: Location -> String -> Token
whitespace' = Token Whitespace Discard
constructors :: [Location -> String -> Token]
constructors = [base',suffix',whitespace']
table :: [(DFA' Char, Location -> String -> Token)]
table = zip dfas constructors
getToken :: DFA' Char -> Location -> [Char] -> Token
getToken dfa loc token = construct loc token where
  construct = fromJust . lookup dfa $ table
scan :: String -> [Token]
scan = scan' [] where
  scan' :: [Token] -> String -> [Token]
  scan' acc s = if done then reverse (t:acc) else reCURSE where
    reCURSE = if err then error "Could not parse." else scan' (t:acc) rem
    done = rem == []
    err = consumed == 0
    list = zip dfas . map (tokenize s) $ dfas
    (dfa,(consumed, token, rem)) = maximumBy compareTrip list
    t = getToken dfa consumed token
    compareTrip (_,(i,_,_)) (_,(i2,_,_)) = compare i i2