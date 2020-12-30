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

   iSideNettingRows = 6
   iTopNettingRows = 20
   nPenaltyAreaLength_m = nXSpan * 16.5 / 105
   nPenaltyAreaWidth_m = nYSpan * 40 / 68
   nSixYardBoxLength_m = nXSpan * 5.5 / 105
   nSixYardBoxWidth_m = nYSpan * 18 / 68
   nCentreCircleRadius_m = nXSpan * 9.15 / 105
   nPenaltySpotOffset_m = nXSpan * 11 / 105
   nCornerArcRadius_m = nXSpan * 1 / 105
   nPointRadius_m = nXSpan * 0.2 / 105
   nGoalWidth_m = (nYSpan * 120/80) * 8 / 105
   nGoalHeight_m = ( iSideNettingRows / iTopNettingRows ) * nGoalWidth_m * (80/nYSpan)
   nGoalDepth_m = nGoalHeight_m * (nXSpan/120) # let this be, net spacing is easier
   nGoalPostRadius_m = nXSpan * 0.1 / 105

   dtPointAliasCircle = data.table(
      Angle_rad = seq(
         0,
         2*pi,
         0.01
      )
   )[,
      x := (
         nPointRadius_m * sin(Angle_rad)
      )
   ][,
      y := (
         ( ( nPointRadius_m ^ 2 ) - (x ^ 2) ) * ( ((120 / 80) * (nYSpan / nXSpan) ) ^ 2 )
      ) ^ 0.5
   ][,
      list(
         x = nXStart + x,
         y = nYStart + ( y * ifelse(Angle_rad >= (1.5 * pi) | Angle_rad <= (0.5 * pi), 1, -1)  ),
         z = 0
      )
   ]

   dtCentreCircle = data.table(
      Angle_rad = seq(
         0,
         2*pi,
         0.01
      )
   )[,
      x := (
         nCentreCircleRadius_m * sin(Angle_rad)
      )
   ][,
      # y := nYStart + (
      #    nCentreCircleRadius_m * cos(Angle_rad)
      # ) + (
      #    nYSpan / 2
      # ),
      y := (
         ( (nCentreCircleRadius_m ^ 2) - (x ^ 2) ) * ( ((120 / 80) * (nYSpan / nXSpan) ) ^ 2 )
      ) ^ 0.5
   ][,
      list(
         x = x,
         y = (y * ifelse(Angle_rad >= (1.5 * pi) | Angle_rad <= (0.5 * pi), 1, -1)),
         z = 0
      )
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
         nPointRadius_m = nPointRadius_m,
         nGoalWidth_m = nGoalWidth_m,
         nGoalHeight_m = nGoalHeight_m,
         nGoalDepth_m = nGoalDepth_m,
         nGoalPostRadius_m = nGoalPostRadius_m
      ),

      lPitchCoordinates = list(

         dtPitch = data.table(
            x = c( nXStart + 0, nXStart + 0,      nXStart + nXSpan, nXStart + nXSpan),
            y = c( nYStart + 0, nYStart + nYSpan, nYStart + nYSpan, nYStart + 0     ),
            z = 0
         ),

         dtCentreCircle = copy(dtCentreCircle)[,
            list(
               x = nXStart + ( nXSpan / 2 ) + x,
               y = nYStart + ( nYSpan / 2 ) + y,
               z = 0
            )
         ],

         dtCornerArc = rbind(
            data.table(
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
                  ( ( nCornerArcRadius_m ^ 2 ) - (x ^ 2) ) * ( ((120 / 80) * (nYSpan / nXSpan) ) ^ 2 )
               ) ^ 0.5

            ][,
               list(x,y)
            ],
            data.table(
               x = 0,
               y = 0
            )
         )[,
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

      )

   )

   lPitchDimensions$lPitchCoordinates$dtDDefense = copy(dtCentreCircle)[
      x <= -nSixYardBoxLength_m
   ][,
      list(
         x = x + nXEnd - lPitchDimensions$lParameters$nPenaltySpotOffset_m,
         y = y + nYStart + (nYSpan/2),
         z = 0
      )
   ]

   # lPitchDimensions$lPitchCoordinates$dtDOffense = copy(dtCentreCircle)[
   #    x >= 0
   # ][,
   #    list(
   #       x = x + nXStart + lPitchDimensions$lParameters$nPenaltySpotOffset_m,
   #       y = y + nYStart + (nYSpan/2),
   #       z = 0
   #    )
   # ]

   lPitchDimensions$lPitchCoordinates$dtDOffense = copy(lPitchDimensions$lPitchCoordinates$dtDDefense)
   lPitchDimensions$lPitchCoordinates$dtDOffense[, x := nXStart-x+nXEnd]

   lPitchDimensions$lPitchCoordinates$dtPenaltyBoxOffense = copy(lPitchDimensions$lPitchCoordinates$dtPenaltyBoxDefense)
   lPitchDimensions$lPitchCoordinates$dtPenaltyBoxOffense[, x := nXStart-x+nXEnd]

   lPitchDimensions$lPitchCoordinates$dt6YardBoxOffense = copy(lPitchDimensions$lPitchCoordinates$dt6YardBoxDefense)
   lPitchDimensions$lPitchCoordinates$dt6YardBoxOffense[, x := nXStart-x+nXEnd]

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


   lPitchDimensions$lGoalframes = list()

   if ( T ) {

      # circle of post on the floor
      dtGoalPostFloorDefenseLow = data.table(
         Angle_rad = seq(
            pi,
            3*pi, # so that the missing segment, if at all, is behind
            # 0.01
            pi / 8
         )
      )[,
         x := ( nXStart ) + (
            nGoalPostRadius_m * sin(Angle_rad)
         )
      ][,
         y := nYStart + ( nYSpan / 2) + ( nGoalWidth_m * 0.5 ) + ( nGoalPostRadius_m  ) + (
            nGoalPostRadius_m * cos(Angle_rad)
         )
      ][,
         list(x, y, z = 0)
      ]

      # circle of post on the crossbar
      dtGoalPostAirDefenseLow = copy(dtGoalPostFloorDefenseLow)[,
         z := nGoalHeight_m +
            ( nGoalPostRadius_m * (
               ( y - ( nYStart + ( nYSpan / 2) + ( nGoalWidth_m * 0.5 ) + ( nGoalPostRadius_m ) ) ) /
               nGoalPostRadius_m
            ) )
      ]

      lPitchDimensions$lGoalframes$lGoalPostDefenseLow = lapply(
         seq(nrow(dtGoalPostAirDefenseLow)),
         function ( iRow ) {

            iNextRow = iRow + 1
            iNextRow = ifelse(iNextRow > nrow(dtGoalPostAirDefenseLow), 1, iNextRow)
            rbind(
               dtGoalPostAirDefenseLow[c(iRow)],
               dtGoalPostFloorDefenseLow[c(iRow)]
            )

         }
      )

      lPitchDimensions$lGoalframes$lGoalPostDefenseHigh = lapply(
         lPitchDimensions$lGoalframes$lGoalPostDefenseLow,
         function ( dtRectangle ) {

            copy(dtRectangle)[, y := nYEnd - ( y - nYStart )  ]

         }
      )

      lPitchDimensions$lGoalframes$lGoalCrossbarDefense = lapply(
         seq(nrow(dtGoalPostAirDefenseLow)),
         function ( iRow ) {

            iNextRow = iRow + 1
            iNextRow = ifelse(iNextRow > nrow(dtGoalPostAirDefenseLow), 1, iNextRow)
            rbind(
               dtGoalPostAirDefenseLow[c(iRow)],
               dtGoalPostAirDefenseLow[c(iRow)][, list(x, y = nYEnd - ( y - nYStart ), z)]
            )

         }
      )

      lPitchDimensions$lGoalframes$lGoalPostOffenseLow = lapply(
         lPitchDimensions$lGoalframes$lGoalPostDefenseLow,
         function(dtRectangle) {
            copy(dtRectangle)[, x := nXEnd - ( x - nXStart )]
         }
      )

      lPitchDimensions$lGoalframes$lGoalPostOffenseHigh = lapply(
         lPitchDimensions$lGoalframes$lGoalPostDefenseHigh,
         function(dtRectangle) {
            copy(dtRectangle)[, x := nXEnd - ( x - nXStart )]
         }
      )

      lPitchDimensions$lGoalframes$lGoalCrossbarOffense = lapply(
         lPitchDimensions$lGoalframes$lGoalCrossbarDefense,
         function(dtRectangle) {
            copy(dtRectangle)[, x := nXEnd - ( x - nXStart )]
         }
      )



   }


   lPitchDimensions$lGoalnet = list()

   if ( T ) {

      lPitchDimensions$lGoalnet$dtSideLowDefense = rbind(
         # front to back
         data.table(
            x = nXStart,
            y = ( nYStart + ( nYSpan / 2) - ( nGoalWidth_m * 0.5 ) - ( nGoalPostRadius_m ) ),
            z = seq(0, nGoalHeight_m, nGoalHeight_m / iSideNettingRows)
         )[,
            xend := x - nGoalDepth_m
         ][,
            yend := y
         ][,
            zend := z
         ],
         # bottom to top
         data.table(
            x = nXStart + seq(0, -nGoalDepth_m, -nGoalDepth_m / iSideNettingRows),
            y = ( nYStart + ( nYSpan / 2) - ( nGoalWidth_m * 0.5 ) - ( nGoalPostRadius_m ) ),
            z = 0
         )[,
            xend := x
         ][,
            yend := y
         ][,
            zend := z + nGoalHeight_m
         ]
      )

      lPitchDimensions$lGoalnet$dtSideHighDefense = copy(lPitchDimensions$lGoalnet$dtSideLowDefense)
      lPitchDimensions$lGoalnet$dtSideHighDefense[, y := nYEnd - y + nYStart]
      lPitchDimensions$lGoalnet$dtSideHighDefense[, yend := nYEnd - yend + nYStart]

      lPitchDimensions$lGoalnet$dtTopDefense = rbind(
         # side to side
         data.table(
            x = nXStart - seq(0, nGoalDepth_m, nGoalDepth_m / iSideNettingRows),
            y = ( nYStart + ( nYSpan / 2) + ( nGoalWidth_m * 0.5 ) + ( nGoalPostRadius_m ) ),
            z = nGoalHeight_m
         )[,
            xend := x
         ][,
            yend := y - nGoalWidth_m - nGoalPostRadius_m
         ][,
            zend := z
         ],
         # back to front
         data.table(
            x = nXStart,
            y = seq(
               ( nYStart + ( nYSpan / 2) - ( nGoalWidth_m * 0.5 ) - ( nGoalPostRadius_m ) ),
               ( nYStart + ( nYSpan / 2) + ( nGoalWidth_m * 0.5 ) + ( nGoalPostRadius_m ) ),
               ( nYSpan/80 ) * nGoalHeight_m / iSideNettingRows
            ),
            z = nGoalHeight_m
         )[,
            xend := x - nGoalDepth_m
         ][,
            zend := z
         ][,
            yend := y
         ]
      )

      lPitchDimensions$lGoalnet$dtBackDefense = rbind(
         # side to side
         data.table(
            x = nXStart - nGoalDepth_m,
            y = ( nYStart + ( nYSpan / 2) + ( nGoalWidth_m * 0.5 ) + ( nGoalPostRadius_m ) ),
            z = seq(0, nGoalHeight_m, nGoalHeight_m / iSideNettingRows)
         )[,
            xend := x
         ][,
            yend := y - nGoalWidth_m - nGoalPostRadius_m
         ][,
            zend := z
         ],
         # bottom to top
         data.table(
            x = nXStart - nGoalDepth_m,
            y = seq(
               ( nYStart + ( nYSpan / 2) - ( nGoalWidth_m * 0.5 ) - ( nGoalPostRadius_m ) ),
               ( nYStart + ( nYSpan / 2) + ( nGoalWidth_m * 0.5 ) + ( nGoalPostRadius_m ) ),
               ( nYSpan/80 ) * nGoalHeight_m / iSideNettingRows
            ),
            z = 0
         )[,
            xend := x
         ][,
            yend := y
         ][,
            zend := z + nGoalHeight_m
         ]
      )

      lPitchDimensions$lGoalnet$dtSideHighOffense = copy(lPitchDimensions$lGoalnet$dtSideHighDefense)
      lPitchDimensions$lGoalnet$dtSideHighOffense[, x := -x + (nXStart + nXEnd)]
      lPitchDimensions$lGoalnet$dtSideHighOffense[, xend := -xend + (nXStart + nXEnd)]
      lPitchDimensions$lGoalnet$dtSideLowOffense = copy(lPitchDimensions$lGoalnet$dtSideLowDefense)
      lPitchDimensions$lGoalnet$dtSideLowOffense[, x := -x + (nXStart + nXEnd)]
      lPitchDimensions$lGoalnet$dtSideLowOffense[, xend := -xend + (nXStart + nXEnd)]
      lPitchDimensions$lGoalnet$dtTopOffense = copy(lPitchDimensions$lGoalnet$dtTopDefense)
      lPitchDimensions$lGoalnet$dtTopOffense[, x := -x + (nXStart + nXEnd)]
      lPitchDimensions$lGoalnet$dtTopOffense[, xend := -xend + (nXStart + nXEnd)]
      lPitchDimensions$lGoalnet$dtBackOffense = copy(lPitchDimensions$lGoalnet$dtBackDefense)
      lPitchDimensions$lGoalnet$dtBackOffense[, x := -x + (nXStart + nXEnd)]
      lPitchDimensions$lGoalnet$dtBackOffense[, xend := -xend + (nXStart + nXEnd)]

      qwe = names(lPitchDimensions$lGoalnet)
      lPitchDimensions$lGoalnet = lapply(
         lPitchDimensions$lGoalnet,
         function ( lSegments ) {

            lSegments = lapply(
               seq(nrow(lSegments)),
               function(x) {
                  rbind(
                     lSegments[x, list(x,y,z)],
                     lSegments[x, list(x = xend, y = yend, z = zend)]
                  )
               }
            )

            lSegments

         }
      )

      names(lPitchDimensions$lGoalnet) = qwe

   }


   # appending the start coordinate to explicitly close polygon
   qwe = names(lPitchDimensions$lPitchCoordinates)
   lPitchDimensions$lPitchCoordinates = lapply(
      lPitchDimensions$lPitchCoordinates,
      function( dt ) {
         rbind(
            dt,
            dt[1]
         )[, group := 1]
      }
   )
   names(lPitchDimensions$lPitchCoordinates) = qwe

   lPitchDimensions

}
