{-# LANGUAGE ApplicativeDo #-}
import KRPCHS
import KRPCHS.Service.SpaceCenter

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
  vsl     <- call getActiveVessel
  control <- call $ getVesselControl vsl
  ref     <- call $ getVesselReferenceFrame vsl
  orbit   <- call $ getVesselOrbit vsl

  liftIO . print =<< call (getOrbitOrbitalSpeed orbit)
  liftIO . print =<< call (getOrbitOrbitalSpeed orbit)
  liftIO . print =<< call (getOrbitOrbitalSpeed orbit)
  liftIO . print =<< call (getVesselMET vsl)
  liftIO . print =<< call (getVesselMET vsl)
  liftIO . print =<< call (getVesselMET vsl)
  liftIO $ do
    print vsl
    print control
    print ref
    print orbit
  liftIO . print =<< call (getOrbitNextOrbit orbit)
  return ()
  --
       

                        
