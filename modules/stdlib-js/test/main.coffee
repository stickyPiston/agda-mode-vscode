cp = require "node:child_process"
util = require "node:util"
fs = require "node:fs/promises"
path = require "node:path"
exec = util.promisify cp.exec

import_function = (name, erased_params) ->
    parts = name.split "."
    func_name = do parts.pop
    mod_name = parts.join "."

    # Compile the library
    project_root = path.join __dirname, ".."
    stdlib_path = path.join project_root, "src", ...parts[..-2], "#{parts.at -1}.agda"
    await exec "agda --js --js-es6  --no-main --compile-dir=dist #{stdlib_path}", cwd: project_root

    # Find the function and apply erased arguments
    mod = require path.join project_root, "dist", "jAgda.#{mod_name}.mjs"
    f = mod.default[func_name]
    f = f null for _ in [0...erased_params]
    [mod_name, func_name, f]

apply_test_value = (f, input) -> switch input.type
    when "function" then f input.js
    when "intlist" then f input.value.map BigInt

to_agda_list = (l) -> l.reduceRight ((ac, el) -> "#{Number el} ∷ #{ac}"), "[]"

serialise_test_value = (input) -> switch input.type
    when "function" then input.agda
    when "intlist" then to_agda_list input.value

run_stack_safety_tests = ({ inputs, name, erased_params }) ->
    [_, func_name, output] = await import_function name, erased_params
    try input.reduce apply_test_value, output catch then console.error "Stack safety error"

run_consistency_tests = ({ inputs, name, erased_params, meta_information }) ->
    [mod_name, func_name, output] = await import_function name, erased_params

    statements = for input in inputs
        # Apply test inputs and collect their string representations
        test_output = input.reduce apply_test_value, output
        serialised_args = ("(#{serialise_test_value x})" for x in input)

        # Create an equals assertion
        """
        _ : #{func_name} #{meta_information or ''} #{serialised_args.join ' '} ≡ #{to_agda_list test_output}
        _ = refl
        """

    output = path.join __dirname, "Test.agda"
    await fs.writeFile output, """
    module Test where
    open import #{mod_name}
    open import Agda.Builtin.Nat
    open import Agda.Builtin.Equality
    #{statements.join "\n"}
    """

    # Try to compile the file, if it succeeds then the ffi and the agda definition are consistent
    try await exec "agda --js --js-es6 --no-main --compile-dir=dist #{output}", cwd: __dirname
    catch then console.error "Inconsistency error"

    # Profit?

cases = [
    [{ type: "function", js: ((x) -> x + BigInt 1), agda: "λ x → x + 1" }, { type: "intlist", value: [0..20] }]
    [{ type: "function", js: ((x) -> x + BigInt 1), agda: "λ x → x + 1" }, { type: "intlist", value: [20..100] }]
    [{ type: "function", js: ((x) -> x + BigInt 1), agda: "λ x → x + 1" }, { type: "intlist", value: [] }]
    [{ type: "function", js: ((x) -> x + BigInt 1), agda: "λ x → x + 1" }, { type: "intlist", value: [0] }]
    [{ type: "function", js: ((x) -> BigInt 10), agda: "λ x → 10" }, { type: "intlist", value: [0..20] }]
    [{ type: "function", js: ((x) -> BigInt 10), agda: "λ x → 10" }, { type: "intlist", value: [20..100] }]
    [{ type: "function", js: ((x) -> BigInt 10), agda: "λ x → 10" }, { type: "intlist", value: [] }]
    [{ type: "function", js: ((x) -> BigInt 10), agda: "λ x → 10" }, { type: "intlist", value: [0] }]
]
run_consistency_tests name: "Data.List.map", inputs: cases, erased_params: 4, meta_information: "{A = Nat} {B = Nat}"
