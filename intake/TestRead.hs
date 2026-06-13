{-# LANGUAGE ScopedTypeVariables #-}
import Chart
import Chart.Examples
import Control.Exception (catch, evaluate, ErrorCall(..))
import Optics.Core

test :: forall a. (Show a, Read a) => String -> a -> IO ()
test name c = do
  result <- (do _ <- evaluate (read (show c) :: a); pure $ Right True)
    `catch` (\(ErrorCall e) -> pure $ Left e)
  case result of
    Right _ -> putStrLn $ name <> ": OK"
    Left e -> putStrLn $ name <> ": FAIL - " <> take 80 e

main = do
  test "defaultLegendOptions" defaultLegendOptions
  test "Priority LegendOptions" ([] :: [Priority LegendOptions])
  test "defaultLegendOptions wrapped" [Priority 5 defaultLegendOptions :: Priority LegendOptions]
