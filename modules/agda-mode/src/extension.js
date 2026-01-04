import Main from "./jAgda.AgdaMode.Extension.mjs";
import * as vscode from "vscode";
import * as process from "node:child_process";

export function activate(context) { Main.activate({ process, vscode, context })(() => {}); }
export function deactivate() { }