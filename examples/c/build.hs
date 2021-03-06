{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (void)
import           System.FilePath (takeDirectory)
import           Control.Monad.IO.Class (liftIO)
import qualified Development.Shake as Shake

import           Zoli

build :: Rules ()
build = do
  objects <- rule [pt|objs//*.o|] $ \name out -> do
    let source = [pt|src//*.c|] @@ name
    needFiles [source]
    let deps = [pt|deps//*.d|] @@ name
    mkdirP (takeDirectory deps)
    () <- cmd_ "cc" ["-MD", "-MF", deps, "-c", "-o", out, source]
    -- The first string is the rule target, which we don't need.
    _ : depsFiles <- filter (\w -> w /= source && w /= "\\") . words <$> liftIO (readFile deps)
    needFiles depsFiles

  bin <- rule (StrPat "bin/myprog") $ \() out -> do
    let names = ["a", "b"]
    objectsFiles <- needToks (map (objects,) (map ("/",) names))
    cmd_ "cc" (["-o", out] ++ objectsFiles)

  wantToks_ [bin]

  void $ phony "clean" $ do
    rmDir "objs"
    rmDir "deps"

main :: IO ()
main = Shake.shakeArgs Shake.shakeOptions (runRules build)
