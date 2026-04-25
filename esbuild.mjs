import * as esbuild from "esbuild";
import { env } from "node:process";

await esbuild.build({
  entryPoints: ["out/extension.js"],
  outfile: "bundle.js",
  platform: "node",
  external: ["vscode"],
  format: "cjs",
  bundle: true,
  minify: env["DEV"] !== "TRUE"
})