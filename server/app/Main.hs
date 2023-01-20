{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)
import qualified Data.Text as T
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types

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
handleIntialized = notificationHandler SInitialized $ const registerCapabilities

handleCodeLens :: Handlers (LspT () IO)
handleCodeLens = requestHandler STextDocumentHover $ \req responder ->
  let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
      Position _l _c' = pos
      rsp = Hover ms (Just range)
      ms = HoverContents $ md "hello world"
      range = Range pos pos
   in responder (Right $ Just rsp)

registerCapabilities :: LspT () IO ()
registerCapabilities =
  registerCapabilityOfFileChanged
    *> registerCapabilityOfCodeLens
    *> registerCapabilityOfFileOpened
    *> registerCapabilityOfFileClosed

registerCapabilityOfFileOpened :: LspT () IO ()
registerCapabilityOfFileOpened = void $ registerCapability STextDocumentDidOpen opts handler
  where
    opts = TextDocumentRegistrationOptions Nothing
    handler :: Handler (LspT () IO) 'TextDocumentDidOpen
    handler (NotificationMessage{_params = (DidOpenTextDocumentParams TextDocumentItem{_text, _uri})}) =
      compile (toNormalizedUri _uri) _text

registerCapabilityOfFileChanged :: LspT () IO ()
registerCapabilityOfFileChanged = void $ registerCapability STextDocumentDidChange opts handler
  where
    opts = TextDocumentChangeRegistrationOptions Nothing TdSyncFull
    handler :: Handler (LspT () IO) 'TextDocumentDidChange
    handler (NotificationMessage{_params = (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _)}) =
      sendNotification SWindowShowMessage $ ShowMessageParams MtInfo (T.concat [getUri _uri, "changed"])

registerCapabilityOfFileClosed :: LspT () IO ()
registerCapabilityOfFileClosed = void $ registerCapability STextDocumentDidClose opts handler
  where
    opts = TextDocumentRegistrationOptions Nothing
    handler :: Handler (LspT () IO) 'TextDocumentDidClose
    handler (NotificationMessage{_params = DidCloseTextDocumentParams{_textDocument = TextDocumentIdentifier{_uri}}}) =
      flushDiagnosticsBySource 100 $ Just (getUri _uri)

registerCapabilityOfCodeLens :: LspT () IO ()
registerCapabilityOfCodeLens = void $ registerCapability STextDocumentCodeLens opts handler
  where
    opts = CodeLensRegistrationOptions Nothing Nothing (Just False)
    cmd = Command "Say hello" "lsp-hello-command" Nothing
    handler _req responder = responder . Right . List $ [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]

compile :: NormalizedUri -> T.Text -> LspT () IO ()
compile uri msg = publishDiagnostics 100 uri Nothing (partitionBySource $ diagnostics (fromNormalizedUri uri))
  where
    diagnostics uri =
      [ Diagnostic
          { _range = Range (Position 0 0) (Position 0 5)
          , _severity = Just DsError
          , _code = Nothing
          , _source = Just $ getUri uri
          , _message = msg
          , _tags = Nothing
          , _relatedInformation = Nothing
          }
      ]

md :: T.Text -> MarkupContent
md = MarkupContent MkMarkdown