module Format
( formatFloatFloor
)
where

import Text.Printf                          (printf)


-- |
formatFloatFloor
    :: Real num
    => Word -- ^ Number of significant digits. Must be >= 1.
    -> num
    -> String
formatFloatFloor sigCount' num =
    removeInsignificantDigits $ printf "%f" (realToFrac num :: Double)
  where
    sigCount = fromIntegral sigCount'
    beforeAfterPeriod str =
        let (before, after) = span (/= '.') str
        in (before, if length after >= 1 then tail after else "")
    integerSignificantDigits integerStr =
        take sigCount integerStr ++ replicate (length integerStr - sigCount) '0'
    removeInsignificantDigits :: String -> String
    removeInsignificantDigits str
        | (before, "") <- beforeAfterPeriod str = -- no period
            integerSignificantDigits before
        | (before, after) <- beforeAfterPeriod str = -- contains period
            if length before >= sigCount
                then integerSignificantDigits before
                else before ++ "." ++ take (sigCount - length before) after
