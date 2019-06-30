{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.
  
Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile :: FilePath -> Chars -> IO ()
printFile fp chars = putStrLn ("=============  " ++ fp) >>= \_ ->
                     putStrLn chars

--map putStrLn (fp :. chars:. Nil)

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles :: List (FilePath, Chars) -> IO ()
printFiles xs = void (sequence ((\t -> printFile (fst t) (snd t)) <$> xs) )

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile :: FilePath -> IO (FilePath, Chars)
getFile fp = (\c -> (fp, c)) <$> readFile fp 

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles xs = sequence (getFile <$> xs)

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run :: FilePath -> IO ()
run fp = readFile fp >>= \c -> 
    getFiles (lines c) >>= printFiles
-- run fp = do
--    c <- readFile fp
--    result <- getFiles c
--    printFiles result

-- /Tip:/ use @getArgs@ and @run@
main :: IO ()
main = getArgs >>= \xxs -> 
    case xxs of 
      Nil -> putStrLn "No args"
      x :. _ -> run x

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.


getFile1 :: FilePath -> IO (FilePath, Chars)
getFile1 name = readFile name >>= \contents -> pure (name, contents)
  
-- yes

getFile2 :: FilePath -> IO (FilePath, Chars)
getFile2 name = do
  contents <- readFile name
  pure (name, contents)
  
-- yes

getFile3 :: FilePath -> IO (FilePath, Chars)
getFile3 = lift2 (<$>) (,) readFile

-- yes
  
getFile4 :: FilePath -> IO (FilePath, Chars)
getFile4 name = ((,) name) <$> readFile name

-- yes  
  
--getFile5 :: FilePath -> IO (FilePath, Chars)
--getFile5 name = lift2 (>>=) readFile (,)

-- yes
-- ANSWER: NO

--getFile6 :: FilePath -> IO (FilePath, Chars)
--getFile6 name = (name, readFile)
  
-- no  
-- ANSWER: NO


getFile7 :: FilePath -> IO (FilePath, Chars)
getFile7 name = 
  (\contents -> (name, contents)) <$> readFile name

-- yes
  
getFile8 :: FilePath -> IO (FilePath, Chars)
getFile8 name = 
  pure ((,) name) <*> readFile name
  
-- yes


--getFile9 :: FilePath -> IO (FilePath, Chars)
--getFile9 name = readFile name >>= ((,) name)          
  
-- yes  
-- ANSWER: NO  
-- should be:
getFile9 :: FilePath -> IO (FilePath, Chars)
getFile9 name = readFile name >>= pure . ((,) name)

tmp_file = "C:/cygwin64/home/brendan/tmp.txt"

