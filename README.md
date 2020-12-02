```
Smoke is a micro project manager for Haskell

Usage:
   smoke [subcommand]

Subcommands:
    run             - Run project
    compile         - Compile project 
    crun            - Compile and run project
    new <name>      - Create new project
    restore <name>  - Restore default GHC config
    count           - Total line count for src/
    help            - Project structure details

Default compile string:
   ghc -o bin/<name> -no-keep-hi-files -no-keep-o-files -XLambdaCase -i:src Main

```
