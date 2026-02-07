cp = require "node:child_process"
util = require "node:util"
fs = require "node:fs/promises"
path = require "node:path"
os = require "node:os"
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

    # Make a temporary directory and copy the .agda-lib file to it
    tmp_dir = await fs.mkdtemp path.join (do os.tmpdir), path.sep
    await fs.cp (path.join __dirname, ".agda-lib"), (path.join tmp_dir, ".agda-lib")

    # Write the Test module to the temporary directory
    output = path.join tmp_dir, "Test.agda"
    await fs.writeFile output, """
    module Test where
    open import #{mod_name}
    open import Agda.Builtin.Nat
    open import Agda.Builtin.Equality
    #{statements.join "\n"}
    """

    # Try to compile the file, if it succeeds then the ffi and the agda definition are consistent
    try await exec "agda --js --js-es6 --no-main --compile-dir=dist #{output}", cwd: tmp_dir
    catch e then console.error "Inconsistency error", e

    # Remove the temporary directory
    await fs.rm tmp_dir, recursive: yes

cartesian_product = (xs, ys) -> xs.flatMap (x) -> ys.map (y) -> [x, y]

test_lists = [
    (type: "intlist", value: [0..20])
    (type: "intlist", value: [20..100])
    (type: "intlist", value: [])
    (type: "intlist", value: [0])
]

test_functions = [
    (type: "function", js: ((x) -> x + BigInt 1), agda: "λ x → x + 1")
    (type: "function", js: ((x) -> BigInt 10), agda: "λ _ → 10")
]

run_consistency_tests
    name: "Data.List.map"
    inputs: cartesian_product test_functions, test_lists
    erased_params: 4
    meta_information: "{A = Nat} {B = Nat}"

run_consistency_tests
    name: "Data.List._++_"
    inputs: cartesian_product test_lists, test_lists
    erased_params: 2
    meta_information: "{A = Nat}"

run_consistency_tests
    name: "Data.List.concat"
    inputs: { type: "intlist", value: [0..2].map (_) -> test_lists }
    erased_params: 2
    meta_information: "{A = Nat}"
