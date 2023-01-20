import * as path from "path";
import { workspace, ExtensionContext } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export const activate = (context: ExtensionContext) => {
  // Create the language client and start the client.
  client = new LanguageClient(
    "languageServerExample",
    "Language Server Example",
    serverOptions({
      serverModule: context.asAbsolutePath(
        path.join("server", "output", "server-exe")
      ),
    }),
    clientOptions
  );

  // Start the client. This will also launch the server
  context.subscriptions.push(client.start());
};

export const deactivate = (): Thenable<void> | undefined => {
  if (client) {
    return client.stop();
  }
};

// If the extension is launched in debug mode then the debug server options are used
// Otherwise the run options are used
const serverOptions: (deps: {
  /**
   * The server is implemented in node
   */
  serverModule: string;
}) => ServerOptions = ({ serverModule }) => {
  return {
    run: {
      command: serverModule,
      // transport: TransportKind.ipc
    },
    debug: {
      command: serverModule,
      args: [],
      // transport: TransportKind.ipc,
    },
  };
};

// Options to control the language client
const clientOptions: LanguageClientOptions = {
  // Register the server for plain text documents
  documentSelector: [{ scheme: "file", language: "plaintext" }],
  synchronize: {
    // Notify the server about file changes to '.clientrc files contained in the workspace
    fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
  },
};
