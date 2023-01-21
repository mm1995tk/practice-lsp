{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)
import qualified Data.Text as T
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Capabilities (ClientCapabilities (..), TextDocumentClientCapabilities (..))

main :: IO Int
main =
  runServer
    ServerDefinition
      { onConfigurationChange = const $ pure $ Right conf
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = defaultOptions
      , defaultConfig = conf
      }
  where
    conf =
      LspConfig
        SemanticTokensRegistrationOptions
          { _documentSelector = Nothing
          , _legend = SemanticTokensLegend (List []) (List [])
          , _workDoneProgress = Nothing
          , _range = Nothing
          , _full = Nothing
          , _id = Nothing
          }
        Nothing

data LspConfig = LspConfig {semanticTokensRegistrationOptions :: SemanticTokensRegistrationOptions, dummy :: Maybe Int}

handlers :: Handlers (LspM LspConfig)
handlers =
  mconcat
    [ handleIntialized
    , handleCodeLens
    ]

handleIntialized :: Handlers (LspM LspConfig)
handleIntialized = notificationHandler SInitialized $ const registerCapabilities

handleCodeLens :: Handlers (LspM LspConfig)
handleCodeLens = requestHandler STextDocumentHover $ \req responder ->
  let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
      Position _l _c' = pos
      rsp = Hover ms (Just range)
      ms = HoverContents $ md "hello world"
      range = Range pos pos
   in responder (Right $ Just rsp)

handlerSemanticTokensFull :: Handlers (LspM LspConfig)
handlerSemanticTokensFull = requestHandler STextDocumentSemanticTokensFull handler
  where
    handler :: Handler (LspM LspConfig) 'TextDocumentSemanticTokensFull
    handler (RequestMessage{_params = (SemanticTokensParams{..})}) responder = do
      (LspConfig{semanticTokensRegistrationOptions,..}) <- getConfig
      return ()

registerCapabilities :: LspM LspConfig ()
registerCapabilities =
  registerCapabilityOfFileChanged
    *> registerCapabilityOfCodeLens
    *> registerCapabilityOfFileOpened
    *> registerCapabilityOfFileClosed
    *> registerCapabilityOfSemanticTokens

registerCapabilityOfSemanticTokens :: LspM LspConfig ()
registerCapabilityOfSemanticTokens = do
  (ClientCapabilities{_textDocument}) <- getClientCapabilities
  let maybeOpts = do
        (TextDocumentClientCapabilities{_semanticTokens}) <- _textDocument
        (SemanticTokensClientCapabilities{_tokenTypes, _tokenModifiers}) <- _semanticTokens
        return $ getOpts _tokenTypes _tokenModifiers
  case maybeOpts of
    Nothing -> pure ()
    Just opts -> do
      -- TODO: 1.6.0.0からしかsetConfigがないので対処を考える
      -- setConfig $ LspConfig {semanticTokensRegistrationOptions = opts, dummy = Nothing}
      void $ registerCapability STextDocumentSemanticTokens opts handler
  where
    getOpts _tokenTypes _tokenModifiers =
      SemanticTokensRegistrationOptions
        { _documentSelector = Nothing
        , _legend = SemanticTokensLegend{..}
        , _workDoneProgress = Nothing
        , _range = Nothing
        , _full = Nothing
        , _id = Nothing
        }
    handler :: Handler (LspM LspConfig) 'TextDocumentSemanticTokens
    handler (RequestMessage{}) responder = responder . Right $ Empty

registerCapabilityOfFileOpened :: LspM LspConfig ()
registerCapabilityOfFileOpened = void $ registerCapability STextDocumentDidOpen opts handler
  where
    opts = TextDocumentRegistrationOptions Nothing
    handler :: Handler (LspM LspConfig) 'TextDocumentDidOpen
    handler (NotificationMessage{_params = (DidOpenTextDocumentParams TextDocumentItem{_text, _uri})}) =
      compile (toNormalizedUri _uri) _text

registerCapabilityOfFileChanged :: LspM LspConfig ()
registerCapabilityOfFileChanged = void $ registerCapability STextDocumentDidChange opts handler
  where
    opts = TextDocumentChangeRegistrationOptions Nothing TdSyncFull
    handler :: Handler (LspM LspConfig) 'TextDocumentDidChange
    handler (NotificationMessage{_params = (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _)}) =
      sendNotification SWindowShowMessage $ ShowMessageParams MtInfo (T.concat [getUri _uri, "changed"])

registerCapabilityOfFileClosed :: LspM LspConfig ()
registerCapabilityOfFileClosed = void $ registerCapability STextDocumentDidClose opts handler
  where
    opts = TextDocumentRegistrationOptions Nothing
    handler :: Handler (LspM LspConfig) 'TextDocumentDidClose
    handler (NotificationMessage{_params = DidCloseTextDocumentParams{_textDocument = TextDocumentIdentifier{_uri}}}) =
      flushDiagnosticsBySource 100 $ Just (getUri _uri)

registerCapabilityOfCodeLens :: LspM LspConfig ()
registerCapabilityOfCodeLens = void $ registerCapability STextDocumentCodeLens opts handler
  where
    opts = CodeLensRegistrationOptions Nothing Nothing (Just False)
    cmd = Command "Say hello" "lsp-hello-command" Nothing
    handler _req responder = responder . Right . List $ [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]

compile :: NormalizedUri -> T.Text -> LspM LspConfig ()
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