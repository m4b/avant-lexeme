module Scanner2(scan) where 
import FiniteStateAutomata(DFA'(..))
import ParseLang(Relevance(..))
import Recognize(tokenize)
import Data.List(maximumBy,lookup)
import Data.Maybe(fromJust)
type Location = Int
data Class = Greek | Integer | FloatingPoint | IntegerArithmatic | Punctuation | Whitespace deriving Show
data Token = Token {kind :: Class, relevance :: Relevance,location :: Location, token :: String} deriving Show
greek :: DFA' Char
greek = read "DFA' {alpha = fromList \"ahilp\", ss = fromList [(0,fromList [('a',2),('p',1)]),(1,fromList [('h',7),('i',6)]),(2,fromList [('l',3)]),(3,fromList [('p',4)]),(4,fromList [('h',5)]),(5,fromList [('a',6)]),(6,fromList []),(7,fromList [('i',6)])], accept = fromList [6], st = 0}"
integer :: DFA' Char
integer = read "DFA' {alpha = fromList \"0123456789\", ss = fromList [(1,fromList [('0',1),('1',1),('2',1),('3',1),('4',1),('5',1),('6',1),('7',1),('8',1),('9',1)])], accept = fromList [1], st = 1}"
floatingPoint :: DFA' Char
floatingPoint = read "DFA' {alpha = fromList \".0123456789\", ss = fromList [(0,fromList [('.',1),('0',0),('1',0),('2',0),('3',0),('4',0),('5',0),('6',0),('7',0),('8',0),('9',0)]),(1,fromList [('0',1),('1',1),('2',1),('3',1),('4',1),('5',1),('6',1),('7',1),('8',1),('9',1)])], accept = fromList [1], st = 0}"
integerArithmatic :: DFA' Char
integerArithmatic = read "DFA' {alpha = fromList \"*+-/0123456789\", ss = fromList [(0,fromList [('*',1),('+',1),('-',1),('/',1),('0',0),('1',0),('2',0),('3',0),('4',0),('5',0),('6',0),('7',0),('8',0),('9',0)]),(1,fromList [('0',1),('1',1),('2',1),('3',1),('4',1),('5',1),('6',1),('7',1),('8',1),('9',1)])], accept = fromList [1], st = 0}"
punctuation :: DFA' Char
punctuation = read "DFA' {alpha = fromList \",.:;_\", ss = fromList [(1,fromList [(',',1),('.',1),(':',1),(';',1),('_',1)])], accept = fromList [1], st = 1}"
whitespace :: DFA' Char
whitespace = read "DFA' {alpha = fromList \"\n \", ss = fromList [(1,fromList [('\n',1),(' ',1)])], accept = fromList [1], st = 1}"
dfas :: [DFA' Char]
dfas = [greek,integer,floatingPoint,integerArithmatic,punctuation,whitespace]
greek' :: Location -> String -> Token
greek' = Token Greek Relevant
integer' :: Location -> String -> Token
integer' = Token Integer Relevant
floatingPoint' :: Location -> String -> Token
floatingPoint' = Token FloatingPoint Relevant
integerArithmatic' :: Location -> String -> Token
integerArithmatic' = Token IntegerArithmatic Relevant
punctuation' :: Location -> String -> Token
punctuation' = Token Punctuation Irrelevant
whitespace' :: Location -> String -> Token
whitespace' = Token Whitespace Discard
constructors :: [Location -> String -> Token]
constructors = [greek',integer',floatingPoint',integerArithmatic',punctuation',whitespace']
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