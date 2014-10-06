# Goals

Make a DSL to express shell programs and generate portable shell
scripts running on bash and pure POSIX shell.

Writing shell scripts is boring and annoying because of the quirks of
shell syntax and semantics.  Writing portable and safe scripts is even
harder.  Furthermore, shell lacks in reasonable functional
abstractions (e.g., parameters to functions cannot even be named
without manual effort).  A DSL could easily smooth out many rough
edges, even adding additional static typing.

For a prototype, the DSL will be a Haskell eDSL.  That isn't really
appropriate for many uses, though, so after the prototype is working,
we can make a separate DSL compiler.  The advantage of a separate DSL
compiler will be that we could more easily capture variable names,
which would make the generated scripts much more readable.

# Features

 * Multiple code generation backends (portable POSIX shell, bash, zsh, csh, python, perl)
 * Types where plausible
 * Additional DSL layer combining awk/sed to add some structure in pipelines
 * Optimization
   * Peephole optimizations for simple rewriting (e.g., `cat foo | bar ~~ bar < foo`)
   * stdout/stderr from a command only used once can be turned into a
     pipeline.  Used more than once it can be stored in a temporary file.

# Example

```{.haskell}
main :: IO ()
main = runSh $ do
  rls <- run "ls" ["-l", "-h"]
  files <- awkmagic (stdout rls)
  rcheck <- runAsync "md5sum" ["-"] (stdout files)
  run "echo" ["Summing files"]
  wait rcheck
```

should produce something like

```{.bash}
set -e
# Some other good bash things
ls -l -h | awkmagic | md5sum - &
PID=$!
echo "Summing files"
wait $PID
```

# General Approach

 * The eDSL will produce an AST, which will have some basic operators
 * The AST can be optimized in various ways (peephole first and then
   again at the end)
 * Walk the AST to produce a shell script
