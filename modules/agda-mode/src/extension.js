import Main from "./jAgda.AgdaMode.Extension.mjs";
import * as vscode from "vscode";
import * as process from "node:child_process";

export function activate(context) {
    Object.defineProperty(globalThis, "vscode", vscode);
    Object.defineProperty(globalThis, "process", process);
    Object.defineProperty(globalThis, "context", context);
    Main.activate({ vscode, process, context }, _ => {});
}
export function deactivate() { }
