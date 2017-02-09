module Geometry.CornerPoints(squaredOffAdjustmentFunction) where

{- Base on the idea taken from Christopher Olah's blog
https://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/

However, the function has been modified to cause a cylindrical shape to take on a square shape on the xy plane.
The squareness of the shape depends on the power parameter.

Gets combined with createCornerPointWithSlopeAndXYAdjustmentFx via the squaredOffAdjustment function.

Making it the inverse of (radiusAdjustedForSlope**2) squared it off immensely,
as opposed to just making it x*n + y*n = r*n
Why does it work. What does inversing do.
Making it 1/ instead of (radiusAdjustedForSlope**2) squares it off, but the dimensions
are totally out. What does (radiusAdjustedForSlope**2)\ do compared to 1/
xyAdjustment = (radiusAdjustedForSlope**2)/ (((getX**power) + (getY**power)) **(1/power))
-}
squaredOffAdjustmentFunction :: Double -> Double ->               Double -> Double -> Double
squaredOffAdjustmentFunction    power'    radiusAdjustedForSlope' x         y          =
                                               (radiusAdjustedForSlope'**2)
                                                /
                                                (
                                                   (
                                                      (x**power') + (y**power')
                                                   )
                                                   **(1/power')
                                               )     

