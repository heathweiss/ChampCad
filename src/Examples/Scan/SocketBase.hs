module Examples.Scan.SocketBase(RowReductionFactor(..), WallThickness(..),  AgeCompensatorFactor(..), PixelsPerMillimeter(..),
                                age9LengthCompensator, standardRowReduction, pixelsPerMM, adjustPixelsPerMillimeterForGrowth) where
{- |
Common types, values, and functions for working creating sockets from the socket scan.
-}


-- | Reduce the amount of layers of a scan so that 1 layer ~= 1 cm.
type RowReductionFactor = Int
standardRowReduction :: RowReductionFactor
standardRowReduction = 100

type PixelsPerMillimeter = Double
pixelsPerMM :: PixelsPerMillimeter
pixelsPerMM = 696/38
--how many pixels of the scan images = 1 mm of physical scan

type AgeCompensatorFactor = Double
--Change the pixelsPerMM to lengthen the socket along z_axis to compensate for growth.
age9LengthCompensator :: AgeCompensatorFactor
age9LengthCompensator = 0.95

type WallThickness = Double
--Thickness of things like walls or attachments

{-
Task:
Adjust the PixelsPerMillimeter to adjust the lenght of the scan, to compensate for growth since the scan was taken.
-}
adjustPixelsPerMillimeterForGrowth :: PixelsPerMillimeter -> AgeCompensatorFactor -> RowReductionFactor -> PixelsPerMillimeter
adjustPixelsPerMillimeterForGrowth pixelsPerMillimeter lengthFactor rowReductionFactor = 1/(pixelsPerMillimeter - lengthFactor) * (fromIntegral rowReductionFactor)

