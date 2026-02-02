import Main from "./jAgda.AgdaMode.Extension.mjs";
import * as vscode from "vscode";
import * as process from "node:child_process";

export function activate(context) {
    // We make a separate object for AgdaModeImports. Since globalThis is shared
    // across all extensions, and internal libraries, it means that assigning
    // generic names to globalThis has the tendency to break other things in
    // unexpected and unsolvable ways.
    Object.defineProperty(globalThis, "AgdaModeImports", {
        value: { vscode, process, context }
    });
    Main.activate(_ => {});
}

export function deactivate() { }
