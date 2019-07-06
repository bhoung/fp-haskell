import System.IO 

getFile1 :: FilePath -> IO (FilePath, Chars)
getFile1 name = 
  readFile name >>= \contents -> pure (name, contents)
  
-- yes

getFile2 :: FilePath -> IO (FilePath, Chars)
getFile2 name = do
  contents <- readFile name
  pure (name, contents)
  
-- yes
-- a -> f (a, b)
-- a -> b -> c -> f a -> f b -> f c
-- a -> b -> f (a, b) 
-- (,)
-- Functor f => (a -> b) -> f a -> f b
getFile3 :: FilePath -> IO (FilePath, Chars)
getFile3 = 
  lift2 (<$>) (,) readFile

-- yes
  
getFile4 :: FilePath -> IO (FilePath, Chars)
getFile4 name = 
  ((,) name) <$> readFile name

-- yes  
  
getFile5 :: FilePath -> IO (FilePath, Chars)
getFile5 name = 
  lift2 (>>=) readFile (,)

-- yes

getFile6 :: FilePath -> IO (FilePath, Chars)
getFile6 name = 
  (name, readFile)
  
-- no  
  
getFile7 :: FilePath -> IO (FilePath, Chars)
getFile7 name = 
  (\contents -> (name, contents)) <$> readFile name

-- yes
  
getFile8 :: FilePath -> IO (FilePath, Chars)
getFile8 name = 
  pure ((,) name) <*> readFile name
  
-- yes


getFile9 :: FilePath -> IO (FilePath, Chars)
getFile9 name = 
  readFile name >>= ((,) name)          
  
-- yes  
  
