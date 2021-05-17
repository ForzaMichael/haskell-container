import Data.Char
import StrAdd
import Test.QuickCheck

prop_isAllDigit :: String -> Bool
prop_isAllDigit val =
  if isAllDigits val || val == ""
    then onlyDigit == length val
    else onlyDigit /= length val
  where
    onlyDigit = length $ filter isDigit val

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_isAllDigit
  putStrLn "Done"