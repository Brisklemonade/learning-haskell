import System.Directory.Internal.Prelude (getArgs)
-- Monads are an efficient way to pipe data (wrap|unwrap)
-- IO = {
--   time: : datestamp,
--   user: : user,
--   iobuffer:: *buffer
--   writeBuffer :: char**
-- }

doubleIt :: Int -> Int -> Int
doubleIt x y = x * y

f :: a -> IO a
f x = return x

-- example
-- [1] <> [2] = [1,2]
-- [] <> [1] = [1]

main :: IO ()
main = do
  args <- getArgs
  print args