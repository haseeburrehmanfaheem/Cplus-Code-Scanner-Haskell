-- import Control.Monad
-- import System.IO

import Control.Monad
-- import Data.List.Split

import Distribution.Simple.Program.HcPkg (list)
import GHC.Unicode (isDigit, isUpper)
import System.Directory.Internal.Prelude (getArgs)
import System.IO

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  -- let singlewords = words contents
  -- print contents
  -- print
  writeFile "output.txt" (runner (func2 contents [] []))
  -- print (((seperator (func2 contents [] [] !! 2) [] [])))
  -- print (caller contents 0)

  --   print list
  hClose handle

func2 [] list list2 = list2
func2 ('\n' : xs) list list2 = func2 xs [] (list2 ++ [list])
func2 (x : xs) list list2 = (func2 xs (list ++ [x]) list2)

heavy_comment [] = []
heavy_comment [x] = x
-- heavy_comment ([] : xs) = "\n" ++ heavy_comment xs
heavy_comment ((x : xs : xsz) : y : ysz) =
  if (x == '/' && xs == '*')
    then "<multiline-comment,/*" ++ heavy_comment ((xsz) : y : ysz)
    else
      if (x == '*' && xs == '/')
        then "*/>\n" ++ runner ((xsz) : y : ysz)
        else
          if (x == ' ')
            then " " ++ heavy_comment ((xs : xsz) : y : ysz)
            else [x] ++ heavy_comment ((xs : xsz) : y : ysz)
heavy_comment (x : xs) = x ++ "\n" ++ heavy_comment xs

runner [] = []
runner [x] =
  ( let p = x !! 0
     in if p == ':' || p == ';' || p == ','
          then "<delimiter," ++ [p] ++ ">\n"
          else
            if p == '+' || p == '*' || p == '-' || p == '/' || p == '%' || p == '<' || p == '>' || p == '=' || p == '!' || p == '|' || p == '&' || p == '[' || p == ']' || p == '(' || p == ')' || p == '{' || p == '}'
              then "<operator," ++ [p] ++ ">\n"
              else variable_check ([x])
  )
runner (x : xs : xsz) =
  if (x == [])
    then runner (xs : xsz)
    else
      if length x == 1
        then
          ( let p = x !! 0
             in if p == ':' || p == ';' || p == ','
                  then "<delimiter," ++ [p] ++ ">\n" ++ runner (xs : xsz)
                  else
                    if p == '+' || p == '*' || p == '-' || p == '/' || p == '%' || p == '<' || p == '>' || p == '=' || p == '!' || p == '|' || p == '&' || p == '[' || p == ']' || p == '(' || p == ')' || p == '{' || p == '}'
                      then "<operator," ++ [p] ++ ">\n" ++ runner (xs : xsz)
                      else variable_check (x : xs : xsz)
          )
        else -- "error" ++ runner (xs : xsz)

          if (x !! 0 == '/' && x !! 1 == '*')
            then heavy_comment (x : xs : xsz)
            else
              if (x !! 0 == ' ')
                then runner ((drop_helper 1 x) : xs : xsz)
                else delimiter (x : xs : xsz)

-- else "ez scene"


-- (x:xs)

delimiter ((x : xs : xsz) : y : ysz) =
  if x == ':' || x == ':' || x == ','
    then "<delimiter," ++ [x] ++ "> " ++ runner ((xs : xsz) : y : ysz)
    else operator ((x : xs : xsz) : y : ysz)

-- func3332 (x:xy:xsy:xsad:asdasdad) = if x == 'i' &&

operator ((x : xs : xsz) : y : ysz) =
  if (x == '*' || x == '%' || x == '[' || x == ']' || x == '{' || x == '}' || x == '(' || x == ')')
    then "<operator," ++ [x] ++ "> " ++ runner ((xs : xsz) : y : ysz)
    else
      if (x == '+' || x == '-' || x == '<' || x == '>' || x == '=' || x == '!' || x == '&' || x == '|' || x == '/')
        then
          if (x == '+' && xs == '+') || (x == '-' && xs == '-') || (x == '<' && xs == '<') || (x == '<' && xs == '=') || (x == '>' && xs == '>') || (x == '>' && xs == '=') || (x == '=' && xs == '=') || (x == '!' && xs == '=') || (x == '&' && xs == '&') || (x == '|' && xs == '|')
            then "<operator," ++ [x] ++ [xs] ++ "> " ++ runner ((xsz) : y : ysz)
            else
              if (x == '/' && xs == '/')
                then "<comment," ++ (x : xs : xsz) ++ ">\n" ++ runner (y : ysz)
                else "<operator," ++ [x] ++ "> " ++ runner ((xs : xsz) : y : ysz)
        else keyword ((x : xs : xsz) : y : ysz)

keyword [] = []
keyword (x : xs : xsz) =
  if (length x >= 6 && ((x !! 0 == 's' && x !! 1 == 't' && x !! 2 == 'r' && x !! 3 == 'i' && x !! 4 == 'n' && x !! 5 == 'g') || (x !! 0 == 'r' && x !! 1 == 'e' && x !! 2 == 't' && x !! 3 == 'u' && x !! 4 == 'r' && x !! 5 == 'n') || (x !! 0 == 'd' && x !! 1 == 'o' && x !! 2 == 'u' && x !! 3 == 'b' && x !! 4 == 'l' && x !! 5 == 'e') || (x !! 0 == 's' && x !! 1 == 't' && x !! 2 == 'r' && x !! 3 == 'u' && x !! 4 == 'c' && x !! 5 == 't')))
    then
      if (length x == 6)
        then "<keyword," ++ (take_helper 6 x) ++ "> " ++ keyword (xs : xsz)
        else "<keyword," ++ (take_helper 6 x) ++ "> " ++ keyword (drop_helper 6 x : xs : xsz)
    else
      if (length x >= 5 && ((x !! 0 == 'f' && x !! 1 == 'l' && x !! 2 == 'o' && x !! 3 == 'a' && x !! 4 == 't') || (x !! 0 == 'b' && x !! 1 == 'r' && x !! 2 == 'e' && x !! 3 == 'a' && x !! 4 == 'k') || (x !! 0 == 'w' && x !! 1 == 'h' && x !! 2 == 'i' && x !! 3 == 'l' && x !! 4 == 'e') || (x !! 0 == 'a' && x !! 1 == 'r' && x !! 2 == 'r' && x !! 3 == 'a' && x !! 4 == 'y') || (x !! 0 == 'f' && x !! 1 == 'a' && x !! 2 == 'l' && x !! 3 == 's' && x !! 4 == 'e') || (x !! 0 == 'c' && x !! 1 == 'l' && x !! 2 == 'a' && x !! 3 == 's' && x !! 4 == 's')))
        then
          if (length x == 5)
            then "<keyword," ++ (take_helper 5 x) ++ "> " ++ keyword (xs : xsz)
            else "<keyword," ++ (take_helper 5 x) ++ "> " ++ keyword (drop_helper 5 x : xs : xsz)
        else
          if (length x >= 4 && ((x !! 0 == 'c' && x !! 1 == 'h' && x !! 2 == 'a' && x !! 3 == 'r') || (x !! 0 == 'b' && x !! 1 == 'o' && x !! 2 == 'o' && x !! 3 == 'l') || (x !! 0 == 'e' && x !! 1 == 'l' && x !! 2 == 's' && x !! 3 == 'e') || (x !! 0 == 'v' && x !! 1 == 'o' && x !! 2 == 'i' && x !! 3 == 'd') || (x !! 0 == 'c' && x !! 1 == 'a' && x !! 2 == 's' && x !! 3 == 'e') || (x !! 0 == 'c' && x !! 1 == 'o' && x !! 2 == 'u' && x !! 3 == 't') || (x !! 0 == 't' && x !! 1 == 'r' && x !! 2 == 'u' && x !! 3 == 'e') || (x !! 0 == 'e' && x !! 1 == 'n' && x !! 2 == 'd' && x !! 3 == 'l')))
            then
              if (length x == 4)
                then "<keyword," ++ (take_helper 4 x) ++ "> " ++ keyword (xs : xsz)
                else "<keyword," ++ (take_helper 4 x) ++ "> " ++ keyword (drop_helper 4 x : xs : xsz)
            else
              if (length x >= 3 && ((x !! 0 == 'i' && x !! 1 == 'n' && x !! 2 == 't') || (x !! 0 == 'c' && x !! 1 == 'i' && x !! 2 == 'n') || (x !! 0 == 'f' && x !! 1 == 'o' && x !! 2 == 'r')))
                then
                  if (length x == 3)
                    then "<keyword," ++ (take_helper 3 x) ++ "> " ++ keyword (xs : xsz)
                    else "<keyword," ++ (take_helper 3 x) ++ "> " ++ keyword (drop_helper 3 x : xs : xsz)
                else
                  if (length x >= 2 && ((x !! 0 == 'i' && x !! 1 == 'f')))
                    then
                      if (length x == 2)
                        then "<keyword," ++ (take_helper 2 x) ++ "> " ++ keyword (xs : xsz)
                        else "<keyword," ++ (take_helper 2 x) ++ "> " ++ keyword (drop_helper 2 x : xs : xsz)
                    else string_check (x : xs : xsz)
keyword (x : xs) = runner (xs)

string_check (x : xs : xsz) =
  if x !! 0 == '"'
    then "<stringConstant,\"" ++ string_check2 ((drop_helper 1 x) : xs : xsz)
    else char_check (x : xs : xsz)

string_check2 (x : xs : xsz) =
  if (x !! 0 == '"')
    then "\"> " ++ runner ((drop_helper 1 x) : xs : xsz)
    else [(x !! 0)] ++ string_check2 ((drop_helper 1 x) : xs : xsz)

char_check (x : xs : xsz) =
  if x !! 0 == '\''
    then "<charConstant,'" ++ char_check2 ((drop_helper 1 x) : xs : xsz)
    else number_check (x : xs : xsz)

char_check2 (x : xs : xsz) =
  if (x !! 0 == '\'')
    then "'> " ++ runner ((drop_helper 1 x) : xs : xsz)
    else [(x !! 0)] ++ char_check2 ((drop_helper 1 x) : xs : xsz)

number_check (x : xs : xsz) =
  if (isDigit (x !! 0))
    then number_helper (x : xs : xsz) [] False False
    else variable_check (x : xs : xsz) ------------------

number_helper (x : xs : xsz) rlist err fll =
  if (isDigit (x !! 0))
    then
      if (x !! 0 > '6')
        then number_helper ((drop_helper 1 x) : xs : xsz) (rlist ++ [x !! 0]) True fll
        else number_helper ((drop_helper 1 x) : xs : xsz) (rlist ++ [x !! 0]) err fll
    else
      if (x !! 0 == '.')
        then number_helper ((drop_helper 1 x) : xs : xsz) (rlist ++ [x !! 0]) err True
        else -- else rlist ++ runner ((x) : xs : xsz)
        -- ++ runner ((x) : xs : xsz)

          if (err)
            then "<error," ++ rlist ++ "> " ++ runner ((x) : xs : xsz)
            else
              if (fll)
                then "<floatConstant," ++ rlist ++ "> " ++ runner ((x) : xs : xsz)
                else "<intConstant," ++ rlist ++ "> " ++ runner ((x) : xs : xsz)

variable_check (x : xs : xsz) =
  if x !! 0 == ' '
    then runner (x : xs : xsz)
    else
      if (length x == 1)
        then "<error," ++ x ++ ">\n" ++ runner (xs : xsz)
        else variable_helper (x : xs : xsz) [] False

-- variable_check (x : xs) = "<error," ++ x ++ "> " ++ runner (xs)

-- aaaa
variable_helper (x : xs : xsz) rlist caps =
  if (isUpper (x !! 0))
    then variable_helper ((drop_helper 1 x) : xs : xsz) (rlist ++ [x !! 0]) True
    else
      if ((x !! 0 >= 'a' && x !! 0 <= 'z') || (x !! 0 >= 'A' && x !! 0 <= 'Z') || (x !! 0 >= '0' && x !! 0 <= '9'))
        then variable_helper ((drop_helper 1 x) : xs : xsz) (rlist ++ [x !! 0]) caps
        else
          if (caps == False || length rlist <= 1)
            then "<error," ++ rlist ++ "> " ++ runner ((x) : xs : xsz)
            else "<identifier," ++ rlist ++ "> " ++ runner ((x) : xs : xsz)

take_helper counter [] = []
take_helper counter (x : xs) =
  if counter == 0
    then []
    else [x] ++ take_helper (counter - 1) (xs)

drop_helper _ [] = []
drop_helper counter (x : xs) =
  if counter == 0
    then x : xs
    else drop_helper (counter - 1) xs

-- n
-- if (x!!0 >= '0' && x!!0 )

-- "asd"

-- 'a'
-- identifier
-- identifier(errror) // number // octal // type (int double char' string")
-- next line
--

-- keyword ((x : xs) : y) = [x] ++ runner ((xs) : y)

-- else ([x] !! 0) ++ runner (drop 1 x : xs : xsz)

--   if length x >= 2
--     then
--       if (x !! 0 == 's' && x !! 1 == 't')
--         then "<keyword,if>" ++ keyword (drop 2 x : xs : xsz)
--         else [x] !! 0 ++ runner (drop 1 x : xs : xsz)
--     else [x] !! 0 ++ runner (drop 1 x : xs : xsz)

-- keyword ((x : x1 : x2 : x3) : y : ysz) =
--   if (x == 'i' && x1 == 'n' && x2 == 't')
--     then "<keyword,bool>" ++ runner ((x3) : y : ysz)
--     else [x] ++ runner ((x1 : x2 : x3) : y : ysz)
--  NULL     []
-- keyword ((x : x1 : x2 : x3 : x4) : y : ysz) =
--   if (([x3] ++ x4) == [])
--     then  ++ runner (y : ysz)
--     else
--       if (x == 'b' && x1 == 'o' && x2 == 'o' && x3 == 'l')
--         then "<keyword,bool>" ++ runner ((x4) : y : ysz)
--         else
--           if (x == 'i' && x1 == 'n' && x2 == 't')
--             then "<keyword,int>" ++ runner ((x3 : x4) : y : ysz)
--             else [x] ++ runner ((x1 : x2 : x3 : x4) : y : ysz)

-- keyword ((x : x1 : x2 : x3 : x4) : y : ysz) = "<3" ++ runner ((x1 : x2 : x3 : x4) : y : ysz)
-- keyword ((x : xs) : y) = [x] ++ runner ((xs) : y)

-- caller [] x = []
-- caller list x = seperator (func2 list [] [] !! x) [] [] : caller list (x + 1)

-- seperate [] _ final_list = final_list
-- seperate (x : xs) list final_list =
--   if x == '\n'
--     then seperate xs [] (final_list ++ [list])
--     else seperate xs (list ++ [x]) final_list

-- comment
-- keyword
-- identifier lazmi coderunner

-- seperator [] list final_list = final_list ++ [list]
-- seperator (' ' : xs) list final_list = seperator xs [] (final_list ++ [list])
-- seperator (x : xs) list final_list = seperator xs (list ++ [x]) final_list

-- -- reverse_list :: [Int] -> [Int]
-- reverse_list = \list ->
--   case list of
--     [] -> []
--     x : xs -> reverse_list xs ++ [x]

-- checker (x:xs) = if x =

-- func3

-- main = print (reverse [1, 2, 3])
-- func2 :: [Char] -> [Char] -> [[Char]] -> [[Char]]
-- func2 [] list list2 = []
-- func2 ('\n' : xs) list list2 = func2 xs [] ([list2] ++ [list])
-- func2 (x : xs) list list2 = (func2 xs (list ++ (x)) list2)

-- [[]] ++ []

-- [] ++ [[]]

-- func2 ('\n' : xs) list = []
-- func2 (x : xs) list = x : func2 xs (list : [x])

-- func3 (x : xs) = func2 (x : xs) :

-- main = do
--   let list = []
--   handle <- openFile "code.txt" ReadMode
--   isFileEnd <- hIsEOF handle
--   contents <- hGetContents handle
--   let singlewords = words contents
--         list = singlewords
-- --   print contents
--   print list
--   hClose handle

-- getWords :: FilePath -> IO [String]
-- getWords path = do contents <- readFile "code.txt"
--                    return (lines contents)

-- import System.IO

-- readDataFrom fileHandle =
--     do
--         isFileEnd <- hIsEOF fileHandle
--         if isFileEnd
--             then
--                 return ("")
--             else
--                 do
--                     info <- hGetLine  fileHandle
--                     putStrLn info
--                     readDataFrom fileHandle

-- main =
--     do
--         -- putStrLn "code.txt"
--         fileName <- getLine
--         fileHandle <- openFile "code.txt" ReadMode

--         readDataFrom fileHandle

-- f :: [String] -> [Int]
-- f = map read

-- case 1:
