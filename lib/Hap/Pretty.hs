module Hap.Pretty
  ( oxford
  ) where

import Data.Text.Prettyprint.Doc (Doc)

oxford :: Doc a -> Doc a -> [Doc a] -> [Doc a]
oxford comma andor = go
  where
    go = \ case
      [] -> []
      [one] -> [one]
      [former, latter] -> [former, andor, latter]
      [antepenultimate, penultimate, ultimate] ->
        [ antepenultimate <> comma
        , penultimate <> comma
        , andor
        , ultimate
        ]
      one : more -> one <> comma : go more
