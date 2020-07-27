module Network.Wai.UnliftIO where

import Network.Wai
import UnliftIO

type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived

type MiddlewareT m = ApplicationT m -> ApplicationT m

liftApplication :: MonadUnliftIO m => Application -> ApplicationT m
liftApplication app req respond =
  withRunInIO $ \runInIO -> liftIO $ app req (runInIO . respond)

liftMiddleware :: MonadUnliftIO m => Middleware -> MiddlewareT m
liftMiddleware mid app req respond = do
  app' <- runApplicationT app
  withRunInIO $ \runInIO -> mid app' req (runInIO . respond)

runApplicationT :: MonadUnliftIO m => ApplicationT m -> m Application
runApplicationT app =
  withRunInIO $ \runInIO ->
    pure $ \req respond ->
      runInIO $ app req (liftIO . respond)

runMiddlewareT :: MonadUnliftIO m => MiddlewareT m -> m Middleware
runMiddlewareT mid =
  withRunInIO $ \runInIO ->
    pure $ \app req respond -> do
      app' <- runInIO . runApplicationT . mid $ liftApplication app
      app' req respond
