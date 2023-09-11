module Util.Streamly.UDP where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as BSL
import Network.Socket (
    Family (AF_INET),
    PortNumber,
    SockAddr (SockAddrInet),
    SocketType (Datagram),
    bind,
    close,
    defaultProtocol,
    socket,
 )
import Network.Socket.ByteString (recv)
import Streamly.Data.Stream.Prelude qualified as S

stream :: (MonadIO n) => PortNumber -> S.Stream n BSL.ByteString
stream port = S.morphInner liftIO $
    S.bracket
        (socket AF_INET Datagram defaultProtocol >>= \s -> bind s (SockAddrInet port 0) >> pure s)
        close
        \sock -> S.repeatM $ BSL.fromStrict <$> recv sock 4096
