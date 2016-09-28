import           Control.Monad (void)
import           Data.Functor.Identity (Identity(..))
import           System.FilePath (takeDirectory)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Development.Shake as Shake

import           Zoli

build :: (Pattern tok, MonadIO m) => Rules tok m Identity ()
build = do
  objects <- rule (pat2 "objs/" ".o") $ \name out -> do
    let source = pat2 "src/" ".c" @@ name
    need_ [File source]
    let deps = pat2 "deps/" ".d" @@ name
    () <- cmd_ "mkdir" ["-p", takeDirectory deps]
    () <- cmd_ "cc" ["-MD", "-MF", deps, "-c", "-o", out, source]
    -- The first string is the rule target, which we don't need.
    _ : depsFiles <- filter (\w -> w /= source && w /= "\\") . words <$> liftIO (readFile deps)
    need_ (map File depsFiles)

  bin <- rule (pat1 "bin/myprog") $ \() out -> do
    let names = ["a", "b"]
    objectsFiles <- need (map (Tok objects) names)
    () <- cmd_ "cc" (["-o", out] ++ objectsFiles)
    return ()

  want [Tok bin ()]

  void $ phony "clean" $ do
    () <- cmd_ "rm" ["-rf", "objs"]
    () <- cmd_ "rm" ["-rf", "deps"]
    return ()

main :: IO ()
main = Shake.shakeArgs Shake.shakeOptions (runIdentity (mkRules build))
