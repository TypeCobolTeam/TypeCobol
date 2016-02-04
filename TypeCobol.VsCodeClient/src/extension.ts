/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import * as path from 'path';

import { workspace, Disposable, ExtensionContext } from 'vscode';
import { Executable, LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind } from 'vscode-languageclient';

export function activate(context: ExtensionContext) {

	// The language server is implemented in C#
	let serverProgram = context.asAbsolutePath(path.join('server', 'TypeCobol.LanguageServer.exe'));
	// Command line options for the language server
	let serverTraceOptions = [ "2", // Trace level : 0 Lifecycle, 1 Message, 2 Protocol
                               "TypeCobol.LanguageServer.txt" // Log file
                               ];
	
	// Options to launch the language server
	let serverOptions: Executable = {
		command: serverProgram,
        args: serverTraceOptions 
	}
	
	// Options to control the language client
	let clientOptions: LanguageClientOptions = {        
		// Register the server for TypeCobol documents
		documentSelector: ['typecobol'],		
        synchronize: {
            // Synchronize the setting section 'typecobol' to the server
			configurationSection: 'typecobol',
			// Notify the server about file changes to '.cpy' files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.cpy')
		}
	}
	
	// Create the language client and start the client.
	let disposable = new LanguageClient('TypeCobol', serverOptions, clientOptions).start();
	
	// Push the disposable to the context's subscriptions so that the 
	// client can be deactivated on extension deactivation
	context.subscriptions.push(disposable);
}