# NriPrelude

A Prelude inspired by the Elm programming language.

enable ghc option

```
-fplugin=NriPrelude.Plugin
```

to get default imports (List, Maybe, Debug, etc)

For tests you're recommended to enable the `ExtendedDefaultRules` language extension, and `-fno-warn-type-defaults` compiler option.

This package re-implements API's and re-uses documentation from [elm-core][] ([license](./licenses/ELM_CORE_LICENSE)) and [elm-test][] ([license](./licenses/ELM_TEST_LICENSE)).

[elm-core]: https://github.com/elm/core
[elm-test]: https://github.com/elm-community/elm-test
