import qualified Test.Hspec as H

main :: IO ()
main = H.hspec $
  H.it "TODO" $
    H.pendingWith "Test suite to implement."

--networkTest :: MomentIO () -> IO ()
--networkTest description = do
--  bracket_ (SDL.initialize [SDL.InitEvents]) SDL.quit $ 
--  network <- compile description 
--  actuate network
--
--main :: IO ()
--main = H.hspec $
--  H.describe "Poll test" $ do
--    H.describe "Inits Correctly" $ do
--      spec
--
--spec :: H.Spec
--spec = around networkTest $ do
--  H.it "Should" $ do
--    (sdlHandle, release) <- FSDL.addEventWatchHandler
--    sdlEvent <- fromAddHandler sdlHandle
--    interpretFrameworks sdlEvent
