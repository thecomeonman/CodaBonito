#' Pitch dimension coordinates
#' @import data.table
#' @export
fGetPitchDimensions = function (
   nXStart = 0,
   nYStart = 0,
   nXEnd = 120,
   nYEnd = 80
) {

   nXSpan = nXEnd - nXStart
   nYSpan = nYEnd - nYStart

   nPenaltyAreaLength_m = nXSpan * 16.5 / 105
   nPenaltyAreaWidth_m = nYSpan * 40 / 68
   nSixYardBoxLength_m = nXSpan * 5.5 / 105
   nSixYardBoxWidth_m = nYSpan * 18 / 68
   nCentreCircleRadius_m = nXSpan * 9.15 / 105
   nPenaltySpotOffset_m = nXSpan * 11 / 105
   nCornerArcRadius_m = nXSpan * 1 / 105
   nPointRadius_m = nXSpan * 0.05 / 105
   nGoalWidth_m = nXSpan * 4 / 105
   nGoalHeight_m = nXSpan * 2.67 / 105
   nGoalPostRadius_m = nXSpan * 0.04 / 105



   dtPointAliasCircle = data.table(
      Angle_rad = seq(
         0,
         2*pi,
         0.01
      )
   )[,
      x := nXStart + (
         nPointRadius_m * sin(Angle_rad)
      )
   ][,
      y := nYStart + (
         nPointRadius_m * cos(Angle_rad)
      )
   ][,
      list(x, y, z = 0)
   ]

   lPitchDimensions = list(

      lParameters = list(

         nXStart = nXStart,
         nYStart = nYStart,
         nXEnd = nXEnd,
         nYEnd = nYEnd,
         nXSpan = nXSpan,
         nYSpan = nYSpan,

         nPenaltyAreaLength_m = nPenaltyAreaLength_m,
         nPenaltyAreaWidth_m = nPenaltyAreaWidth_m,
         nSixYardBoxLength_m = nSixYardBoxLength_m,
         nSixYardBoxWidth_m = nSixYardBoxWidth_m,
         nCentreCircleRadius_m = nCentreCircleRadius_m,
         nPenaltySpotOffset_m = nPenaltySpotOffset_m,
         nCornerArcRadius_m = nCornerArcRadius_m,
         nPointRadius_m = nPointRadius_m
      ),

      lPitchCoordinates = list(

         dtPitch = data.table(
            x = c( nXStart + 0,  nXStart + 0, nXStart + nXSpan, nXStart + nXSpan),
            y = c(nYStart + 0, nYStart + nYSpan, nYStart + nYSpan, nYStart + 0),
            z = 0
         ),

         dtCentreCircle = data.table(
            Angle_rad = seq(
               0,
               2*pi,
               0.01
            )
         )[,
            x := nXStart + (
               nCentreCircleRadius_m * sin(Angle_rad)
            ) + ( nXSpan / 2 )
         ][,
            y := nYStart + (
               nCentreCircleRadius_m * cos(Angle_rad)
            ) + (
               nYSpan / 2
            )
         ][,
            list(x, y, z = 0)
         ],

         dtCornerArc = data.table(
            Angle_rad = seq(
               0,
               pi/2,
               0.01
            )
         )[,
            x := (
               nCornerArcRadius_m * sin(Angle_rad)
            )
         ][,
            y := (
               nCornerArcRadius_m * cos(Angle_rad)
            )
         ][,
            list(x, y, z = 0)
         ],

         dtPenaltyBoxDefense = data.table(
            y = c(
               nYStart + ( nYSpan / 2 ) - ( nPenaltyAreaWidth_m / 2 ),
               nYStart + ( nYSpan / 2 ) - ( nPenaltyAreaWidth_m / 2 ),
               nYStart + ( nYSpan / 2 ) + ( nPenaltyAreaWidth_m / 2 ),
               nYStart + ( nYSpan / 2 ) + ( nPenaltyAreaWidth_m / 2 )
            ),
            x = c(
               nXStart + 0,
               nXStart + nPenaltyAreaLength_m,
               nXStart + nPenaltyAreaLength_m,
               nXStart + 0
            ),
            z = 0
         ),

         dt6YardBoxDefense = data.table(
            y = c(
               nYStart + ( nYSpan / 2 ) - ( nSixYardBoxWidth_m / 2 ),
               nYStart + ( nYSpan / 2 ) - ( nSixYardBoxWidth_m / 2 ),
               nYStart + ( nYSpan / 2 ) + ( nSixYardBoxWidth_m / 2 ),
               nYStart + ( nYSpan / 2 ) + ( nSixYardBoxWidth_m / 2 )
            ),
            x = c(
               nXStart + 0,
               nXStart + nSixYardBoxLength_m,
               nXStart + nSixYardBoxLength_m,
               nXStart + 0
            ),
            z = 0
         )

      ),

      lGoalCoordinates = list(
         dtGoalFrameDefense = data.table(
            x = c(nXStart, nXStart, nXStart, nXStart),
            y = c(( nYSpan / 2 ) - ( nGoalWidth_m / 2), ( nYSpan / 2 ) + ( nGoalWidth_m / 2), ( nYSpan / 2 ) + ( nGoalWidth_m / 2), ( nYSpan / 2 ) - ( nGoalWidth_m / 2)),
            z = c(nGoalHeight_m / 2, nGoalHeight_m / 2, 0, 0)
         )
      )

   )

   lPitchDimensions$lPitchCoordinates$dtDDefense = lPitchDimensions$lPitchCoordinates$dtCentreCircle[
      (
         x - ( lPitchDimensions$lParameters$nXSpan / 2) + lPitchDimensions$lParameters$nPenaltySpotOffset_m
      ) >= (
         lPitchDimensions$lParameters$nPenaltyAreaLength_m
      )
   ][,
      list(
         x = nXStart + x - ( lPitchDimensions$lParameters$nXSpan / 2) + lPitchDimensions$lParameters$nPenaltySpotOffset_m,
         y = nYStart + y,
         z = 0
      )
   ]

   lPitchDimensions$lPitchCoordinates$dtDOffense = copy(lPitchDimensions$lPitchCoordinates$dtDDefense)
   lPitchDimensions$lPitchCoordinates$dtDOffense[, x := - x + ( lPitchDimensions$lParameters$nXSpan )]

   lPitchDimensions$lPitchCoordinates$dtPenaltyBoxOffense = copy(lPitchDimensions$lPitchCoordinates$dtPenaltyBoxDefense)
   lPitchDimensions$lPitchCoordinates$dtPenaltyBoxOffense[, x :=- x + ( lPitchDimensions$lParameters$nXSpan)]

   lPitchDimensions$lPitchCoordinates$dt6YardBoxOffense = copy(lPitchDimensions$lPitchCoordinates$dt6YardBoxDefense)
   lPitchDimensions$lPitchCoordinates$dt6YardBoxOffense[, x :=- x + ( lPitchDimensions$lParameters$nXSpan )]

   lPitchDimensions$lPitchCoordinates$dtPenaltySpotDefense = dtPointAliasCircle[,
      list(
         x = x + lPitchDimensions$lParameters$nPenaltySpotOffset_m,
         y = y + ( lPitchDimensions$lParameters$nYSpan / 2 ),
         z = 0
      )
   ]

   lPitchDimensions$lPitchCoordinates$dtPenaltySpotOffense = dtPointAliasCircle[,
      list(
         x = x + lPitchDimensions$lParameters$nXSpan - lPitchDimensions$lParameters$nPenaltySpotOffset_m,
         y = y + ( lPitchDimensions$lParameters$nYSpan / 2 ),
         z = 0
      )
   ]

   lPitchDimensions$lPitchCoordinates$dtCentreSpot = dtPointAliasCircle[,
      list(
         x = x + ( lPitchDimensions$lParameters$nXSpan / 2 ),
         y = y + ( lPitchDimensions$lParameters$nYSpan / 2 ),
         z = 0
      )
   ]

   lPitchDimensions$lPitchCoordinates$dtCentreLine = data.table(
      x = c(nXStart + ( lPitchDimensions$lParameters$nXSpan / 2 ), nXStart + ( lPitchDimensions$lParameters$nXSpan / 2 )),
      y = c(nYStart + 0, nYStart + lPitchDimensions$lParameters$nYSpan),
         z = 0
   )

   lPitchDimensions$lPitchCoordinates$dtCornerArcLT = lPitchDimensions$lPitchCoordinates$dtCornerArc[,
      list(
         x = nXStart + x,
         y = nYStart + lPitchDimensions$lParameters$nYSpan - y,
         z = 0
      )
   ]

   lPitchDimensions$lPitchCoordinates$dtCornerArcRB = lPitchDimensions$lPitchCoordinates$dtCornerArc[,
      list(
         x = nXStart + lPitchDimensions$lParameters$nXSpan - x,
         y = nYStart + y,
         z = 0
      )
   ]

   lPitchDimensions$lPitchCoordinates$dtCornerArcRT = lPitchDimensions$lPitchCoordinates$dtCornerArc[,
      list(
         x = nXStart + lPitchDimensions$lParameters$nXSpan - x,
         y = nYStart + lPitchDimensions$lParameters$nYSpan - y,
         z = 0
      )
   ]

   lPitchDimensions$lPitchCoordinates$dtCornerArcLB = lPitchDimensions$lPitchCoordinates$dtCornerArc[,
      list(
         x = nXStart + x,
         y = nYStart + y,
         z = 0
      )
   ]

   lPitchDimensions$lPitchCoordinates$dtCornerArc = NULL

   lPitchDimensions

}
