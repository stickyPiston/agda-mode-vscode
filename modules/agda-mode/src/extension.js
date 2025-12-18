import Main from "./jAgda.Extension.mjs";
import * as vscode from "vscode";
import * as process from "node:child_process";

export function activate(context) { Main.activate(Main.system(process)(vscode)(context))(() => {}); }
export function deactivate() { }