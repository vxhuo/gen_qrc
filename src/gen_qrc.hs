
module Main where

import System.Environment (getArgs)
import System.FilePath
import System.Directory
import Control.Monad (filterM)
import Data.List (sortOn)


is_target :: FilePath -> Bool
is_target f  = takeFileName  f == "qmldir" 
            || takeExtension f == ".qml" 
            || takeExtension f == ".svg"


resource_order :: FilePath -> Int
resource_order f
    | takeFileName  f == "qmldir" = 1
    | takeExtension f == ".qml"   = 2
    | takeExtension f == ".svg"   = 3
    | otherwise                   = 4


count_icon :: [FilePath] -> Int
count_icon files = length (filter (\f -> takeExtension f == ".svg") files)

count_qml :: [FilePath] -> Int
count_qml files = length (filter (\f -> takeExtension f == ".qml") files)

count_qmldir :: [FilePath] -> Int
count_qmldir files = length (filter (\f -> takeFileName f == "qmldir") files)



walk :: FilePath -> IO [FilePath]
walk target_path = do
    files <- listDirectory target_path
    let selected = map (target_path </>)(filter is_target files)
    dirs <- filterM (\f -> doesDirectoryExist (target_path </> f)) files
    out <- mapM (\dir -> walk (target_path </> dir)) dirs
    return (selected ++ concat out)


generate_file :: FilePath -> String
generate_file f
    | takeFileName f  == "qmldir" = "        <file alias=\"" ++ f ++ "\">" ++ f ++ "</file>"
    | takeExtension f == ".qml"   = "        <file alias=\"" ++ f ++ "\">" ++ f ++ "</file>"
    | takeExtension f == ".svg"   = "        <file alias=\"" ++ takeFileName f ++ "\">" ++ f ++ "</file>"
    | otherwise                   = ""


generate :: [FilePath] -> String -> String
generate files prefix =
    "<RCC>\n"
 ++ "    <qresource prefix=\"/" ++ prefix ++ "\">\n\n"
 ++ unlines
    [
        unlines (map generate_file (filter (\f -> resource_order f == 1) files)),
        unlines (map generate_file (filter (\f -> resource_order f == 2) files)),
        unlines (map generate_file (filter (\f -> resource_order f == 3) files))
    ]
 ++ "    </qresource>\n"
 ++ "</RCC>\n"





help_text :: String
help_text = "\n\nusage: qrc_gen <path_to_gui_dir> <output_path> <prefix>\n\n"


finish_text :: [FilePath] -> String -> FilePath -> FilePath -> FilePath -> String
finish_text files generated_file abs_out_path target_path out_path =
    "\n generating qrc file..." 
 ++ "\n checked for files [svg]; [qml]; [qmldir]; target relative path [" ++ target_path ++ "]; output relative path [" ++ out_path ++ "]"
 ++ "\n"
 ++ "\n found:"
 ++ "\n" ++ unlines
            [
                unlines (map (\f -> "  " ++ f) (filter (\f -> resource_order f == 1) files)),
                unlines (map (\f -> "  " ++ f) (filter (\f -> resource_order f == 2) files)),
                unlines (map (\f -> "  " ++ f) (filter (\f -> resource_order f == 3) files))
            ]
 ++ " all    [" ++ show (length files) ++ "]"
 ++ "\n icon   [" ++ show (count_icon files) ++ "]"
 ++ "\n qml    [" ++ show (count_qml files) ++ "]"
 ++ "\n qmldir [" ++ show (count_qmldir files) ++ "]"
 ++ "\n"
 ++ "\n generated [" ++ abs_out_path ++ "]:\n\n" ++ generated_file ++ "\n"


main :: IO ()
main = do
    args <- getArgs
    case args of
        [target_path, output_path, prefix] -> do
            files <- walk target_path
            let gen = generate files prefix
            writeFile output_path gen
            absolute_output_path <- makeAbsolute output_path
            putStrLn (finish_text files gen absolute_output_path target_path output_path)

        _  -> do
            putStrLn help_text


    
