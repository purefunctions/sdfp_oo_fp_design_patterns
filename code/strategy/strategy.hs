type File = String
type Extension = String
type Name = String

archive ::  Extension -> ([File] -> File) -> [File] -> Name -> File
archive ext f files name = (f files) ++ ", " ++ name ++ "." ++ ext

main :: IO ()
main = do
  putStrLn (archiveToZip ["one", "two"] "zipfile")
  putStrLn (archiveToTar ["one", "two"] "tarfile")
  
  where
    archiveToZip :: [File] -> String -> File
    archiveToZip = archive "zip" (\_ -> "compressed to zip")
    
    archiveToTar :: [File] -> String -> File
    archiveToTar = archive "tar" (\_ -> "archived to tar")