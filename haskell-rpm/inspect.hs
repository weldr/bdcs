import Conduit(($$), (=$), awaitForever, stdinC)
import Control.Monad(void)
import Control.Monad.Except(runExceptT)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Data.Conduit(Consumer)
import Text.PrettyPrint(render)
import Text.PrettyPrint.HughesPJClass(Pretty(pPrint))

import RPM.Parse(parseRPMC)
import RPM.Types(RPM)

consumer :: (MonadIO m) => Consumer RPM m ()
consumer = awaitForever (liftIO . putStrLn . render . pPrint)

main :: IO ()
main =
    void $ runExceptT $ stdinC $$ parseRPMC =$ consumer
