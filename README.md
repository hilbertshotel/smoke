```
Smoke is a micro project manager for Haskell

Usage:
   smoke [subcommand]

Subcommands:
    new <name       - Create new project
    run             - Run project (if compiled)
    crun            - Compile and run project
    release         - Compile project with -O2 optimization
    restore <name>  - Restore default GHC config string
    count           - Total line count for src/ folder
    path            - Find path of executable
    help            - Helpful and honest opinion

Default GHC config string:
    src/Main.hs -o bin/<name> -i:src -no-keep-hi-files -no-keep-o-files -XLambdaCase

Compilation info:
    - src/Main.hs is the entry point
    - -o bin/ is the compilation folder
    - both are hardcoded and necessary for Smoke to function properly
    - change <name> manually if you want to rename the binary output
    - edit -i:src if you want the compiler to include other source folders (-i:src:pkg)
    - edit ghc.conf file to add other necessary compile options
```
