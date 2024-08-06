{-# LANGUAGE OverloadedStrings #-}

module GoldenHelpers (goldenResultsDir) where

-- | Historical context:
-- Golden results are slightly different between GHC 9.2.x and 8.10.x due
-- to apparent differences in internal handling of stack frame source locations.
-- In particular, the end of a function call now does not extend to the end of
-- the line but instead to the end of the function name. E.g. for the following:
--
-- > foo
-- >   bar
-- >   baz
-- 
-- In GHC 8.10.x (and possibly GHC 9.0.x?) `srcLocEndLine` and `srcLocEndCol`
-- would correspond to the `z` at the end of `baz`.  Unfortunately, in GHC 9.2.x
-- it corresponds to the second `o` at the end of `foo`.
--
-- We keep this helper around so that if this happens again for future GHC versions
-- we can have different golden results for different GHC versions as necessary.

goldenResultsDir :: Text
goldenResultsDir = "test/golden-results-9.2"
