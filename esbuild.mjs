import * as esbuild from "esbuild";

await esbuild.build({
  entryPoints: ["out/extension.js"],
  outfile: "bundle.js",
  platform: "node",
  external: ["vscode"],
  format: "cjs",
  bundle: true,
  minify: true
})