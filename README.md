# Haskell-binaryopshelper
a Binary operations helper written in Haskell, for standalone use or add via FFI to C++


How to Use
Import the Library:

haskell
Copy
import BinaryOperationHelper
Test a Binary Operation (for example, addition on integers):

haskell
Copy
main :: IO ()
main = do
  let op = (+)
      sampleValues = [0, 1, 2, 3, 4, 5]
  print $ isAssociative op sampleValues      -- Should be True for (+)
  print $ isCommutative op sampleValues      -- Should be True for (+)
  print $ hasIdentity op sampleValues        -- Likely True (0 is identity for addition)
  print $ identityElement op sampleValues    -- Should be Just 0
  print $ isIdempotent op sampleValues       -- False for addition
  
Extend or Embed: You can wrap these functions in your own higher‐level code or even expose them to C++ via Haskell’s FFI if needed.

