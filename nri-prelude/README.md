# NriPrelude

A Prelude inspired by the Elm programming language.

enable ghc option
```
-fplugin=NriPrelude.Plugin
```
to get default imports (List, Maybe, Debug, etc)

and
```
-fno-warn-unused-imports
```
to stop warning that you're importing things you're not using (an unfortunate side effect of this)

Longer term, perhaps we could dynamically import qualified imports only if they're used, to obviate the need to disable warnings


This package re-implements API's and re-uses documentation from [elm-core][] ([license](./licenses/ELM_CORE_LICENSE)) and [elm-test][] ([license](./licenses/ELM_TEST_LICENSE)).

[elm-core]: https://github.com/elm/core
[elm-test]: https://github.com/elm-community/elm-test
