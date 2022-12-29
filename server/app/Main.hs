{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)
import qualified Data.Text as T
import Language.LSP.Diagnostics (partitionBySource)
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
registerCapabilities = registerCapabilityOfFileChanged *> registerCapabilityOfCodeLens *> registerCapabilityOfFileOpened *> registerCapabilityOfFileClosed

registerCapabilityOfFileOpened :: LspT () IO ()
registerCapabilityOfFileOpened = void $ registerCapability STextDocumentDidOpen opts $ \(NotificationMessage _ _ params) ->
  compileFromParams params
  where
    opts = TextDocumentRegistrationOptions Nothing
    compileFromParams (DidOpenTextDocumentParams TextDocumentItem{..}) = compile $ toNormalizedUri _uri

registerCapabilityOfFileChanged :: LspT () IO ()
registerCapabilityOfFileChanged = void $ registerCapability STextDocumentDidChange opts $ \(NotificationMessage _ _ params) ->
  compileFromParams params *> sendNotification SWindowShowMessage (ShowMessageParams MtInfo (getMsg params))
  where
    opts = TextDocumentChangeRegistrationOptions Nothing TdSyncFull
    compileFromParams (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) = compile $ toNormalizedUri _uri
    getMsg (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) =
      T.pack $ printf "%s changed!" (T.unpack $ getUri _uri)

registerCapabilityOfFileClosed :: LspT () IO ()
registerCapabilityOfFileClosed = void $ registerCapability STextDocumentDidClose opts $ \(NotificationMessage _ _ DidCloseTextDocumentParams{_textDocument = TextDocumentIdentifier{_uri}}) ->
  flushDiagnosticsBySource 100 $ Just $ getUri _uri
  where
    opts = TextDocumentRegistrationOptions Nothing

registerCapabilityOfCodeLens :: LspT () IO ()
registerCapabilityOfCodeLens = void $ registerCapability STextDocumentCodeLens opts $ \_req responder -> responder (Right rsp)
  where
    opts = CodeLensRegistrationOptions Nothing Nothing (Just False)
    cmd = Command "Say hello" "lsp-hello-command" Nothing
    rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]

compile :: NormalizedUri -> LspT () IO ()
compile uri = publishDiagnostics 100 uri Nothing (partitionBySource $ diagnostics (fromNormalizedUri uri))
  where
    diagnostics uri =
      [ Diagnostic
          { _range = Range (Position 0 0) (Position 0 5)
          , _severity = Just DsError
          , _code = Nothing
          , _source = Just $ getUri uri
          , _message = "diagnostic message 1"
          , _tags = Nothing
          , _relatedInformation = Nothing
          }
      ]

md :: T.Text -> MarkupContent
md = MarkupContent MkMarkdown