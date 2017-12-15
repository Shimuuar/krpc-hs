{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
import KRPCHS
import KRPCHS.Service.SpaceCenter

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.Typeable

check :: IO a -> IO a
check action = do
  e <- try action
  case e of
    Right a -> return a
    Left (SomeException x) -> do putStrLn "================"
                                 print (typeOf x)
                                 print x
                                 putStrLn "================"
                                 throwIO x
main :: IO ()
main = check $ runKRPC "simple program" "192.168.1.5" "50000" $ do
  vessel    <- call getActiveVessel
  control   <- call $ getVesselControl vessel
  maxStage  <- call $ getControlCurrentStage control
  liftIO $ print maxStage
  --
  liftIO . print =<< call (getControlAntennas control)
  liftIO . print =<< call (getVesselCrewCapacity vessel)
  liftIO . print =<< call (getVesselCrewCount vessel)
 -- -- 
  -- resources <- sequenceA [ call $ vesselResourcesInDecoupleStage vessel i False
  --                        | i <- [0 .. maxStage]]
  -- forM_ [0 .. fromIntegral maxStage] $ \stage -> do
  --   liftIO $ print stage
  --   solidFuel  <- call $ resourcesAmount (resources !! stage) "SolidFuel"
  --   liquidFuel <- call $ resourcesAmount (resources !! stage) "LiquidFuel"
  --   liftIO $ print (solidFuel,liquidFuel)
