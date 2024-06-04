import System.IO
import System.Environment
import System.Directory  (getDirectoryContents, doesDirectoryExist, doesFileExist, getHomeDirectory)
import System.FilePath   ((</>), takeFileName)
import Control.Monad     (forM, filterM, mapM)
import Data.List         (isSuffixOf, isPrefixOf, dropWhileEnd, dropWhile)
import Data.Char         (toLower)
import Prelude   hiding  ((*))

{-
    TODO: Add description functionality
    TODO: Add ignore header imports and stuff functionality
    TODO: Write automated readme for all parsed functions
-}

class Multipliable a where (*) :: Int -> a -> a
instance Multipliable String where (*) n str = concat $ replicate n str

sanitize :: [Char] -> [Char]
sanitize [] = []
sanitize (x:xs)
    | x == '\n' = "\n" ++ sanitize xs           -- Sanitize \n
    | x == '"'  = "\\\"" ++ sanitize xs         -- Sanitize "
    | x == '\\' =  "\\\\" ++ sanitize xs        -- Sanitize \
    | otherwise = x : sanitize xs

templ :: String -> String -> (String, String) -> String -> String
templ t p ns b  = t++"\""++n++"\": {\n"++2*t++"\"scope\":"++
                "\""++s++"\",\n"++2*t++"\"prefix\": "++
                "\""++p++map toLower n++"\",\n"++2*t++"\"body\": "++
                "[\n"++b++2*t++"]\n"++t++"},\n"
    where (n,s) = ns

readLines :: FilePath -> IO [String]
readLines filePath = do
    content <- readFile filePath
    return $ lines content

writeLines :: [(String, String)] -> [String] -> FilePath -> String -> String -> IO ()
writeLines ns b pth pre t = writeFile pth $ "{\n" ++ unlines (zipWith (templ t pre) ns b) ++ "\n}"

splitOnDot :: String -> (String, String)
splitOnDot str = (beforeDot, afterDot)
  where
    (beforeDot, rest) = break (== '.') str
    afterDot = if null rest then "" else tail rest

filteredContent :: [[String]] -> [(String, String)] -> [String] -> [String] -> [[String]]
filteredContent content names skip skipignore =
    zipWith filterContent content names
  where
    filterContent :: [String] -> (String, String) -> [String]
    filterContent entry (name1, _) =
      if name1 `elem` skipignore
        then removePadding entry
        else removePadding $ filter (\f -> not (any (`isPrefixOf` f) skip)) entry

removePadding :: [String] -> [String]
removePadding = dropWhile (== "") . dropWhileEnd (== "")

-- Main Method

main :: IO()
main = do

    -- Parameters
    home <- getHomeDirectory
    let pth = "../templates/"                                                                       -- rel path of the code templates
    let trg = home++"/Library/Application Support/Code/User/snippets/icpc.code-snippets"            -- target file 
    let ext = [".cpp", ".py", ".hs"]                                                                -- src template file extensions
    let pre = "$"                                                                                   -- prefix for template prefix
    let tab = "    "
    let skip = ["#include", "typedef", "using"]
    let skipignore = ["Template"]

    exists <- doesDirectoryExist pth
    if  exists
        then do

            all <- getDirectoryContents pth
            let files = filter (\f -> any (`isSuffixOf` f) ext) all
            let names = map (splitOnDot . takeFileName) files

            content <- mapM (readLines . (pth ++)) files
            let res =  map (unlines . map ((++ "\",") . ((3*tab++"\"")++) . sanitize)) $ filteredContent content names skip skipignore
            writeLines names res trg pre tab

    else error "Path does not exist"
