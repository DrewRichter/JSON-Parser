-- Andrew Richter and Pengyu Wang
module JsonParser where
import Data.Maybe
import MonadicParsing
import StateTransformer
import Control.Applicative
import Text.Read
--(string, number, object, array, boolean or null)
type List = [ObjectValue]
type Dict = [JsonPair] 

data ObjectValue = Val Float |Good |Bad | Str  String| Nested Json | Null | Integer Int 
  deriving (Show, Eq, Read)
type JsonPair = (String, ObjectValue) 

data Json = Curly Dict | Regular List | Plain ObjectValue   --Regular to represent regular brackets and Curly for Curly brackets
  deriving (Show, Eq, Read)

-- given a String, turn it into a Json
-- returns Nothing if it could not be parsed
-- DO NOT modify
decode :: String -> Maybe Json
decode s = let res = parse json s in
               -- result of parse is a list of parses, empty if no parse
               if res == [] then Nothing else Just (fst (res !! 0))

-- given a Json, turn it into a String that is valid JSON
-- Note: This is different from the derived Show for Json.
--       The derived Show will depend on your Json definition
--       while encode has identical output (up to whitespace) for everyone
--
--       You should be able to test this before your Parser even
--       works by constructing example Jsons your self
quote x = "\"" ++ x ++ "\""
replace (x:"") = (x:"")
replace (x:y:"") = (x:y:"")
replace (x:y:z:"") = (x:y:z:"")
replace (x:y:z:xs) | [x]=="," && [y]==" " && ([z]== "}" || [z]== "]") = (" "++[z])++ (replace xs)
 | otherwise = (x:(replace (y:z:xs)))
dropEnd x = reverse (drop 2 (reverse x))
encodeList ((a, (Nested b)):xs) = quote a ++ " : " ++ encodeHelp b ++ encodeList xs
encodeList ((a, Null):xs) = quote a ++ " : " ++ "null" ++ ", " ++ encodeList xs
encodeList [] = ""
encodeList ((a, (Str b)):xs) = "\"" ++ a ++ "\"" ++ " : \"" ++ b ++ "\", " ++ encodeList xs
encodeList ((a, (Val x)):xs) = quote a ++ " : " ++ show x ++ ", " ++ encodeList xs
encodeList ((a, (Integer x)):xs) = quote a ++ " : " ++ show x ++ ", " ++ encodeList xs
encodeList ((a, (Bad)):xs) = quote a ++ " : " ++ "false" ++ ", " ++ encodeList xs
encodeList ((a, (Good)):xs) = quote a ++ " : " ++ "true" ++ ", " ++ encodeList xs
encodeList' ( (Nested b):xs) =  encodeHelp b ++ encodeList' xs
encodeList' ( Null:xs) =  "null" ++ ", " ++ encodeList' xs
encodeList' [] = ""
encodeList' ((Str b):xs) = "\"" ++ b ++ "\", " ++ encodeList' xs
encodeList' ((Val x):xs) =  show x ++ ", " ++ encodeList' xs
encodeList' ( (Integer x):xs) =  show x ++ ", " ++ encodeList' xs
encodeList' ( (Bad):xs) =  "false" ++ ", " ++ encodeList' xs
encodeList' ( (Good):xs) =  "true" ++ ", " ++ encodeList' xs
encodeHelp (Curly x) = "{ " ++ encodeList x ++ "}, "
encodeHelp (Regular x) = "[ " ++ encodeList' x ++ "], "
encode (Plain (Val x)) = show x
encode (Plain (Good)) = "true"
encode (Plain (Bad)) = "false"
encode (Plain (Nested a)) = encode a
encode (Plain (Integer a)) = show a 
encode (Plain Null) = "null"
encode (Plain (Str a)) = "\"" ++ a ++ "\""

encode x =  dropEnd (replace ( encodeHelp x))
encode :: Json -> String

-- Parsing a String into a Json
json :: Parser Json
json = do
        symbol "{"
        symbol "\""
        t <- tupo
        return (Curly (t))
        <|> do
          symbol "["
          n <- array
          return (Regular (n))
          <|> do
            o <- object
            return (Plain o)

json' = do -- make sure Plain case will go back to json
        symbol "{"
        t <- tupo
        return (Curly (t))
        <|> do
          symbol "["
          n <- array
          return (Regular (n))
object :: Parser ObjectValue
object =
        do
          f <- integer
          symbol "."
          s <- nat
          return (Val (read ((show f) ++ "." ++ (show s)):: Float))
          <|> do
            i <- integer
            return (Integer i)
            <|> do
              symbol "null"
              return (Null)
              <|> do
                symbol "true"
                return (Good)
                <|> do
                  symbol "false"
                  return (Bad)
                  <|> do
                    j <- json'
                    return (Nested j)
                    <|> do
                      symbol "\""
                      s <- hahaha
                      symbol "\""
                      return (Str s)

array :: Parser [ObjectValue]
array = do
          o <- object
          symbol ","
          a <- array
          return ([o]++a) -- is an array
          <|> do
            o <- object
            symbol "]"
            return ([o]) -- last object of array, or only on object in array
tupo :: Parser [(String, ObjectValue)]
tupo = do
         symbol "\""
         s <- haha
         symbol "\""
         symbol ":"
         o <- object
         loop <- tupo
         return ([(s,o)]++loop)
         <|> do
          symbol ","
          loop <- tupo
          return (loop)  -- if there is symbol (,) that mean it has more than one tupo
          <|> do
            symbol "}"
            return ([])  -- if there is }, it only got one object
            <|> do
              symbol "{"
              loop <- tupo
              return (loop)

                
ident' :: Parser String
ident'  = do
            space 
            x <- many alphanum
            return (x)
str' :: Parser String
str' = token ident'

str'' :: Parser String
str'' = do
          s <- ident'
          spaceKeep
          loop <- str''
          return (s++" "++loop)
          <|> do
            s <- ident'
            symbol "\""
            return (s)
haha :: Parser String
haha = do 
      x <- many notQuote
      return (x)
hahaha :: Parser String
hahaha = token haha
         


-- hint: you will need to define lots of grammar components
--       e.g., for recognizing JSON objects, arrays, etc.
--
--       Since you control the implementation of Json data type
--       try starting with a subset of JSON and build up gradually,
--       testing your encode and decode

-- Querying a Json

-- given a Json (object), return the list of keys at the top level

listTopLevelKeys :: Json -> [String]
listTopLevelKeys (Curly x) =  [k | (k,v) <- x ]
listTopLevelKeys (Regular x) = []
listTopLevelKeys (Plain (Nested a)) = listTopLevelKeys a
listTopLevelKeys x =[]
-- given a Json, return the list of all keys in the data structure
listKeys :: Json -> [String]

listKeys (Curly [])= []
listKeys (Regular [])= []
listKeys (Curly ((a, (Nested b)):xs)) = [a] ++ listKeys b ++ listKeys (Curly xs)
listKeys (Curly ((a, b):xs)) = [a] ++ listKeys (Curly xs)
listKeys (Regular ((Nested b):xs)) = listKeys b ++ listKeys (Regular xs)
listKeys (Regular (x:xs)) = listKeys (Regular xs)
listKeys (Plain (Nested a)) = listKeys a

listKeys x = []

-- given a key and a Json, return the value
-- return value is Maybe so that Nothing can indicate no such key exists
-- (if Json contains duplicates of the key, then any of the corresponding
-- values may be returned suffices)

searchByKey :: String -> Json -> Maybe Json
searchByKey x (Curly [])  = Nothing
searchByKey x (Regular [])  = Nothing
searchByKey x (Curly ((a, (Nested b)):xs)) | a==x = Just b 
 | searchByKey x b == Nothing = searchByKey x (Curly xs)
 | otherwise = searchByKey x b 
searchByKey x (Curly ((a, b):xs)) | a==x = Just (Plain b) 
 | otherwise = searchByKey x (Curly xs)
searchByKey x (Regular ((Nested b):xs)) | searchByKey x b == Nothing = searchByKey x (Regular xs)
 | otherwise = searchByKey x b 
searchByKey  x (Regular (y:ys)) = searchByKey x (Regular ys)
searchByKey x (Plain (Nested a)) = searchByKey x a 
searchByKey x y = Nothing
-- given a list of keys and a Json, return the list of values.
-- for a given result, return Nothing if the key was missing
maySearchAll :: [String] -> Json -> [Maybe Json]
maySearchAll x j =  zipWith (searchByKey ) x (take (length(x)) (repeat j))

-- given a list of keys and a Json, return the list of values
-- return Nothing if any one of the keys is missing
strip (Just a ) = a
mustSearchAll :: [String] -> Json -> Maybe [Json]
mustSearchAll x j | elem Nothing (maySearchAll x j) = Nothing
 | otherwise =  Just (map strip  (maySearchAll x j))


-- data type to be used below (DO NOT modify)
data KeyOrIndex = Key String | Index Int
  deriving (Show,Eq)
  
-- given a list of object keys and array indexes denoting a path, return the value in a list of length 1 (indicates succcess)
-- or empty list to indicate failure (path not found)

searchPath :: [KeyOrIndex] -> Json -> [Json]
searchPath ((Key y) :[]) (Curly (x:[])) | y== (fst x) = [ Plain (snd x)]
 | otherwise =[]
searchPath ((Key y): ys)  (Curly ((a,(Nested b)): xs)) | y== a = searchPath ys b
 | otherwise = searchPath ((Key y): ys) (Curly xs) 
searchPath ((Index y) :ys) (Curly (x:xs)) = searchPath ((Index y) :ys) (Curly xs)
searchPath ((Index y) :ys) (Curly [])=[]
searchPath ((Key y) :ys) (Regular (x:xs)) = searchPath ((Key y) :ys) (Regular xs)
searchPath ((Index y) :[]) (Regular x) | y>=length x =[]
 | otherwise =[Plain(x !!y)]
searchPath ((Key y) :[]) (Curly (x:xs)) | y== (fst x) = [ Plain (snd x)]
 | otherwise =searchPath [(Key y )] (Curly xs)
-- searchPath ((Index y):[]) (Curly x)= [] -- this have warning
searchPath((Key y):[]) (Regular x) = []
searchPath ((Index y):ys) (Regular (x:xs))| y>=length (x:xs) =[]
 |otherwise = searchPath ys (Plain ((x:xs) !!y))
searchPath ((Key y):ys) (Curly (x:xs)) = searchPath ((Key y):ys) (Curly xs)
searchPath x (Plain (Nested a )) = searchPath x a
searchPath x (Curly []) = []
searchPath x (Regular []) = []
searchPath x (Plain _) = [] 




-- searchPath ((Index y) :ys) (Curly x) = []
-- searchPath ((Key y) :ys) (Regular x) = []
-- searchPath ((Index y) :[]) (Regular x) | y>=length x =[]
--  | otherwise =[Plain(x !!y)]

-- searchPath ((Key y) :[]) (Curly (x:xs)) | y== (fst x) = [ Plain (snd x)]
--  | otherwise =[]
-- searchPath ((Index y):ys) (Regular ((Nested x):xs))= searchPath ys x
-- searchPath ((Index y):ys) (Regular (x:xs))=[]

-- --searchByKey ((Index y):ys) (Regular x:xs) =
-- searchPath ((Key y): ys)  (Curly ((a,(Nested b)): xs)) | y== a  && searchPath ys b /= [] = searchPath ys b
--  | y==a = searchPath ys (Curly xs) 
--  | otherwise = []

-- searchPath ((Key y):ys) (Curly (x:xs)) = searchPath ys (Curly xs)

-- searchPath x (Plain (Nested a )) = searchPath x a
-- searchPath x y =[]


{- Given a Json that may have duplicate keys,
return a Json where the keys are de-duplicated by renaming.
If the Json has keys "a", "b", "a", "c", "c" you should rename
them "a0", "b1", "a2", "c3", "c4". It doesn't matter which order
you rename the keys, as long as you use this numbering system.

DO NOT modify: instead, see label below
-}
makeKeysUnique :: Json -> Json
makeKeysUnique j = fst (app (label j) 0)

{- helper function to be used in your implementation of
label -}
fresh :: ST Int
fresh = S (\n -> (n, n+1))
--labelHelpUnique j num   = fst (app (labelHelp j)num )
zip' _ [] = []
zip' (x:xs) ((a, b):ys) =([((a ++ x),b)] ++zip' xs ys )
--zip' (x:xs) ((a ,(Nested (Curly b))):ys)= ([((a++x),(zip'  [xs !!0.. (length (listTopLevelKeys b))-1 +(xs !!0)] b)] ++ zip' [(length (listTopLevelKeys b))+(xs !!0)..(tail xs)]
curlTest (Curly x) = (Curly  (zip' (map show [0..(length x) -1]) x))
--curlTry (Curly (x:y:z)) = 
{- relabels the keys in the Json to have a suffix integer.
(see makeKeysUnique). You should use the tree labeling
example in Ch 12.3 or list labeling from lecture 14 as your inspiration (alabel or mlabel).
-}
label :: Json -> ST Json
--labelHelp (a,b) = (\n -> (a++show n , b)) <$> fresh

label x= transform (makeKeysUnique' x)(length(listKeys x ))
a= Curly [("s", Str "d"), ("s", Str "s")]
transform a b = S( \n -> (a , b))
--makeKeysUniqueHelp n (x:[]) =[((fst x ++ show n), snd x)]		
makeKeysUniqueHelp n [] = []
makeKeysUniqueHelp n ((a, (Nested (Curly b))):xs) = [((a++(show n)),(Nested (Curly (makeKeysUniqueHelp (n+1) b))))] ++ makeKeysUniqueHelp (n+1+ (length (listKeys (Curly b)))) xs
makeKeysUniqueHelp n ((a, (Nested (Regular b))):xs) = [((a++(show n)),(Nested (Regular (arrayHelp n b))))] ++ makeKeysUniqueHelp (n+1+ (length (listKeys (Regular b)))) xs
makeKeysUniqueHelp n (x:xs) = [((fst x ++ show n), snd x)] ++ makeKeysUniqueHelp (n+1) xs
arrayHelp n []=[]
arrayHelp n ((Nested (Regular a)):xs) = arrayHelp (n+1) a ++ (arrayHelp (n+(length(listKeys (Regular a)))) xs)
arrayHelp n ((Nested (Curly a)):xs) = [(Nested (Curly (makeKeysUniqueHelp (n+1) a)))] ++ arrayHelp (n+length(listKeys (Curly a))) xs --problem here
arrayHelp n (x:xs) =[x] ++ arrayHelp n xs
makeKeysUnique' (Curly x) = Curly (makeKeysUniqueHelp 0 x )
makeKeysUnique' (Regular x)= Regular (arrayHelp (-1) x )
makeKeysUnique' (Plain (Nested a )) = makeKeysUnique' a
makeKeysUnique' x = x
c = [("d", Str "d"), ("d", Str "d")]
b = [("a", Str "n"), ("a", Str "n"),("a", Str "n"),("a", Str "n"), ("a", Nested (Curly c)),("a", Nested (Curly c)) ]
