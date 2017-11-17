module Examples.ShoeLift.Pillars.Main(run) where

import Examples.ShoeLift.Pillars.GeoxPillarsWithAnkleBrace (runGeoxPillarsWithAnkleBrace)
import Examples.ShoeLift.Pillars.FullScan(runFullTopTreadStlGenerator, runFullBtmTreadStlGenerator)
import Examples.ShoeLift.Pillars.ContourScan(runContourScan)

run = runContourScan
