{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)
import qualified Data.Text as T
import Language.LSP.Server
import Language.LSP.Types
import Text.Printf (printf)

main :: IO Int
main =
  runServer
    ServerDefinition
      { onConfigurationChange = const $ pure $ Right ()
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = defaultOptions
      , defaultConfig = ()
      }

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ handleIntialized
    , handleCodeLens
    ]

handleIntialized :: Handlers (LspT () IO)
handleIntialized = notificationHandler SInitialized $ void . handler
  where
    handler _ = sendRequest SWindowShowMessageRequest params $ \case
      Right (Just (MessageActionItem "Turn on")) -> registerCapabilities
      Right _ ->
        sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
      Left err ->
        sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
    params =
      ShowMessageRequestParams
        MtInfo
        "Turn on code lenses?"
        (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])

handleCodeLens :: Handlers (LspT () IO)
handleCodeLens = requestHandler STextDocumentHover $ \req responder ->
  let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
      Position _l _c' = pos
      rsp = Hover ms (Just range)
      ms = HoverContents $ md "hello world"
      range = Range pos pos
   in responder (Right $ Just rsp)

registerCapabilities :: LspT () IO ()
registerCapabilities = registerCapabilityOfFileChanged *> registerCapabilityOfCodeLens

registerCapabilityOfFileChanged :: LspT () IO ()
registerCapabilityOfFileChanged = void $ registerCapability STextDocumentDidChange chopts $ \(NotificationMessage _ _ params) ->
  sendNotification SWindowShowMessage $ ShowMessageParams MtInfo (getMsg params)
  where
    chopts = TextDocumentChangeRegistrationOptions Nothing TdSyncFull
    getMsg (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) =
      T.pack $ printf "%s changed!" (T.unpack $ getUri _uri)

registerCapabilityOfCodeLens :: LspT () IO ()
registerCapabilityOfCodeLens = void $ registerCapability STextDocumentCodeLens regOpts $ \_req responder -> responder (Right rsp)
  where
    regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)
    cmd = Command "Say hello" "lsp-hello-command" Nothing
    rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]

md :: T.Text -> MarkupContent
md = MarkupContent MkMarkdown