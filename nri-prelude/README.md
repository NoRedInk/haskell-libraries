# NriPrelude

A Prelude inspired by the Elm programming language.

For those interested in learning more about our reasons for developing these libraries, we invite you to check out our blog post titled "Haskell for the Elm Enthusiast" available at https://blog.noredink.com/post/658510851000713216/haskell-for-the-elm-enthusiast. This post provides insights into the motivations behind our efforts.

enable ghc option

```
-fplugin=NriPrelude.Plugin
```

to get default imports (List, Maybe, Debug, etc)

For tests you're recommended to enable the `ExtendedDefaultRules` language extension, and `-fno-warn-type-defaults` compiler option.

This package re-implements API's and re-uses documentation from [elm-core][] ([license](./licenses/ELM_CORE_LICENSE)) and [elm-test][] ([license](./licenses/ELM_TEST_LICENSE)).

[elm-core]: https://github.com/elm/core
[elm-test]: https://github.com/elm-community/elm-test
