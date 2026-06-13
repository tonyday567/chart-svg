{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
import Chart
import Chart.Examples
import Optics.Core
import Text.Read (readMaybe)

main = do
  let c = lineExample :: ChartOptions
  putStrLn $ "lineExample ChartOptions round-trip: " ++ case readMaybe (show c) of Just (_ :: ChartOptions) -> "OK"; Nothing -> "FAIL"
  let c' = read (show c) :: ChartOptions
  let hud = view #hudOptions c
  let hud' = view #hudOptions c'
  putStrLn $ "axes ==: " ++ show ((hud ^. #axes) == (hud' ^. #axes))
  putStrLn $ "titles ==: " ++ show ((hud ^. #titles) == (hud' ^. #titles))
  putStrLn $ "legends ==: " ++ show ((hud ^. #legends) == (hud' ^. #legends))
  putStrLn $ "chartTree ==: " ++ show ((c ^. #chartTree) == (c' ^. #chartTree))
  putStrLn $ "full ChartOptions ==: " ++ show (c == c')
