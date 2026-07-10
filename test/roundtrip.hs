module Main where

import Chart
import Chart.Examples
import Control.Monad (forM_)

exs :: [(String, ChartOptions)]
exs =
  [ ("mempty", mempty),
    ("unit", unitExample),
    ("rect", rectExample),
    ("line", lineExample),
    ("text", textExample),
    ("glyphs", glyphsExample),
    ("bar", barExample),
    ("sbar", sbarExample),
    ("path", pathExample),
    ("arc", arcFlagsExample),
    ("ellipse", ellipseExample (FixedAspect 1.5)),
    ("quad", quadExample),
    ("cubic", cubicExample),
    ("surface", surfaceExample),
    ("arrow", arrowExample),
    ("gradient", gradientExample),
    ("wheel", wheelExample),
    ("venn", vennExample),
    ("wave", waveExample),
    ("date", dateExample),
    ("compound", compoundExample),
    ("debug", debugExample lineExample)
  ]

main :: IO ()
main = do
  forM_ exs $ \(n, co) -> do
    let ok = (read (show co) :: ChartOptions) == co
    putStrLn (n <> ": roundtrip=" <> show ok)
  let bad = [n | (n, co) <- exs, (read (show co) :: ChartOptions) /= co]
  if null bad
    then putStrLn "FAILURES: none"
    else error ("Read/Show roundtrip failures: " <> show bad)
