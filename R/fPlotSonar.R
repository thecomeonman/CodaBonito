#' A passing sonar alternative to popularly used versions
#'
#' Think of the chart as a bunch of concentric rings. Each ring captures passes
#' falling within a particular range of lengths. The rings are equally spaced
#' so the increase in range from ring 1 to ring 2 is the same as the increase
#' in range from any ring n to ring n + 1.
#' The radius of the circle in the background of each sonar is proportional to
#' the length of the pitch. You can use that as a reference to get an idea of
#' how long a pass actually
#' Each block in the ring captures passes of the respective lengths in a
#' paritcular range of direction - specifically all angles originating from the
#' centre of the circles and passing between the left edge and right edge of
#' the block.
#' The blocks have been calculated in such a way to cover approximately the
#' same sized area of the pitch as any other block, which is why each block
#' spans a lesser part of the circumference on passes of longer lengths
#' compared to the part of the circumference it covers on passes of shorter
#' lengths.
#' The thickness of each block in the ring is proportional to the number of
#' passes of that length and angle. ( It's actuallly proportional to the square
#' root of the number of passes, if I make it proportional then the blocks with
#' few passes become too small to be visible. This is an acceptable workaround
#' to me since the idea is to give an indication of the number of passes, and
#' not really expect people to be able to infer the number of passes. )
#' The colour of each block is proportional to the success percent of the
#' passes associated with that block, going from red to dark green for 0% to
#' 100%
#' This function has only been tested with 120 X 80 pitch dimension dataset.
#' The more complex cases where the sonars are broken by pitch area, players,
#' etc. have also been tested under narrow conditions. Edge cases may break
#' this, in which case please post a bug report on Github or get in touch
#' on Twitter.
#'
#' @examples
#' # Simple overall sonar
#' fPlotSonar(
#'    dtPassesToPlot = dtPasses,
#'    iBlocksInFirstRing = 4,
#'    iNbrRings = 8,
#'    nZoomFactor = NULL,
#'    nXLimit = 120,
#'    nYLimit = 80,
#'    bAddPitchBackground = F,
#'    cTitle = NULL
#' )
#' # Sonar broken up by pitch area
#' fPlotSonar(
#'    dtPassesToPlot = dtPasses[,
#'       list(
#'          playerId,
#'          passLength,
#'          passAngle,
#'          x,
#'          y,
#'          Success,
#'          xBucket = (
#'             ifelse(
#'                x %/% 20 == 120 %/% 20,
#'                ( x %/% 20 ) - 1,
#'                x %/% 20
#'             ) * 20
#'          ) + 10,
#'          yBucket = (
#'             ifelse(
#'                y %/% 20 == 80 %/% 20,
#'                ( y %/% 20 ) - 1,
#'                y %/% 20
#'             ) * 20
#'          ) + 10
#'       )
#'    ],
#'    iBlocksInFirstRing = 4,
#'    iNbrRings = 8,
#'    nZoomFactor = NULL,
#'    nXLimit = 120,
#'    nYLimit = 80,
#'    bAddPitchBackground = T,
#'    cTitle = 'Sample'
#' )
#' # Sonar broken up player, placed at their median passing location
#' fPlotSonar (
#'    dtPassesToPlot = merge(
#'       dtPasses,
#'       merge(
#'          dtPasses[,
#'             list(
#'                xBucket = median(x),
#'                yBucket = median(y)
#'             ),
#'             list(
#'                playerId
#'             )
#'          ],
#'          dtPlayerLabels[,
#'             list(
#'                playerId,
#'                bucketLabel = playerName
#'             )
#'          ],
#'          c(
#'             'playerId'
#'          )
#'       ),
#'       c(
#'          'playerId'
#'       )
#'    ),
#'    iBlocksInFirstRing = 4,
#'    iNbrRings = 8,
#'    nYLimit = 80,
#'    nXLimit = 120,
#'    bAddPitchBackground = T,
#'    cTitle = 'Sample'
#' )
#' # Sonar broken up player, placed at the location dictated by their role
#' # in the formations
#' fPlotSonar (
#'    dtPassesToPlot = merge(
#'       dtPasses,
#'       merge(
#'          dtFormation[,
#'             list(
#'                xBucket = x,
#'                yBucket = y,
#'                playerId
#'             )
#'          ],
#'          dtPlayerLabels[,
#'             list(
#'                playerId,
#'                bucketLabel = playerName
#'             )
#'          ],
#'          c(
#'             'playerId'
#'          )
#'       ),
#'       'playerId'
#'    ),
#'    iBlocksInFirstRing = 4,
#'    iNbrRings = 8,
#'    nXLimit = 120,
#'    nYLimit = 80,
#'    bAddPitchBackground = T,
#'    cTitle = 'Sample'
#' )
#' @import data.table
#' @import ggplot2
#' @export
fPlotSonar = function (
   dtPassesToPlot,
   iBlocksInFirstRing = 4,
   iNbrRings = 8,
   nZoomFactor = NULL,
   nXLimit = 120,
   nYLimit = 80,
   bAddPitchBackground = F,
   cTitle = 'Sample'
) {

   dtPasses = copy(dtPassesToPlot)
   setDT(dtPasses)

   nIncremntalRingRadius = nXLimit / iNbrRings

   # A vector with the same length as the number of rings possible, with each
   # element having the radius for that ring
   vnPassLengthBreaks = seq(0, nXLimit + nYLimit, nIncremntalRingRadius)

   # Each ring, depepnding on the radius, will have different number of breaks
   # to capture passes of the respective length but in a certain direction
   # This logic calculates the number of breaks at each ring
   vnPiR2Difference = ( (tail(vnPassLengthBreaks, -1) ^ 2) - (head(vnPassLengthBreaks, -1) ^ 2) )
   viNbrAngleBreaks = round(vnPiR2Difference / ( vnPiR2Difference[1] / iBlocksInFirstRing ))

   # The coordinates of the pitch will need to be extended to fit multiple
   # sonars in it, since each sonar will individually have a span of
   # (-length,length).
   # The pitch will be extended to nZoomFactor the dimensions to
   # accommodate all the sonars.
   # todo - this needs a more sophisticated calculation. In case use is
   # looking at a smaller area, this calculation may yiel the wrong number

   if ( !'xBucket' %in% colnames(dtPasses)) {

      dtPasses[, xBucket := 0]

   }

   if ( !'yBucket' %in% colnames(dtPasses)) {

      dtPasses[, yBucket := 0]

   }

   if ( is.null(nZoomFactor) ) {

      nZoomFactor = ceiling(
         max(
            dtPasses[, length(unique(xBucket))],
            dtPasses[, length(unique(yBucket)), xBucket][, max(V1)],
            ( nXLimit * dtPasses[, length(unique(yBucket)), xBucket][, max(V1)] ) / nYLimit
         )
      )

   }

   nZoomFactor = nZoomFactor * 2 *  1.1

   # Length bucket number. The bucket number, and not the bucket, is needed
   # to calculate the angle buckets.
   dtPasses[,
      passLengthBucket := passLength %/% nIncremntalRingRadius
   ]

   # Angle buckets.
   # Basically retrieving the number of angle buckets for the resepctive ring
   # which are already calculated in viNbrAngleBreaks and labelling the
   # data accordingly.
   dtPasses[,
      passAngleBucket := passAngle %/% (
         2 * pi / viNbrAngleBreaks[ 1 + passLengthBucket ]
      ),
      passLengthBucket
   ]

   # When angle is exactly 2pi, it falls into a bucket which has only 2*pi
   # angle passes. Forcing them to the previous bucket. Will live with this
   # inconsistency.
   # Not just comparing angle == 2pi because of precision affecting the result
   # of that comparison
   dtPasses[,
      passAngleBucket := ifelse(
         passAngleBucket == ( 2 * pi) %/% (
            2 * pi / viNbrAngleBreaks[ 1 + passLengthBucket ]
         ),
         passAngleBucket - 1,
         passAngleBucket
      ),
      passLengthBucket
   ]

   dtPasses[,
      passAngleBucket.width := (
         2 * pi / viNbrAngleBreaks[ 1 + passLengthBucket ]
      ),
      passLengthBucket
   ]

   dtPasses[,
      passAngleBucket := ( passAngleBucket + 0.5 ) * passAngleBucket.width
   ]

   # Calculating actual pass length buckets
   dtPasses[,
      passLengthBucket := ( passLengthBucket + 0.5 ) * nIncremntalRingRadius,
      passLengthBucket
   ]

   # Preparing the polygons needed for various elements of the chart
   dtAllShapes = dtPasses[
      # Remove throws in, etc. otherwise they might show up beyond
      # the touchlines.
      x < nXLimit &
      y < nYLimit
   ][
      # Some passes don't have this data
      !is.na(passLengthBucket) &
      !is.na(passAngleBucket)
   ][,
      list(
         PassCount = .N,
         Success_pct = sum(Success) / .N
      ),
      list(
         xBucket,
         yBucket,
         passAngleBucket,
         passAngleBucket.width,
         passLengthBucket,
         # the bucket values are defined at the centre of the respective
         # dimension so the min and max span would need to be calcualted
         xmin = passAngleBucket - ( passAngleBucket.width / 2 ),
         xmax = passAngleBucket + ( passAngleBucket.width / 2 ),
         ymin = passLengthBucket - ( nIncremntalRingRadius / 2 ),
         ymax = passLengthBucket + ( nIncremntalRingRadius / 2 )
      )
   ]

   # Changing the dimensions of the block based on the number of passes
   # in that block such that the outer boundary extends all the way to the max
   # radius the ring permits and the inner radius is moved
   dtAllShapes[,
      ymin2 := passLengthBucket +
        ( nIncremntalRingRadius * 1 / 2 ) -
        ( nIncremntalRingRadius * sqrt( PassCount / ( 1.2 * max(PassCount) ) ) )
   ][,
      ymax2 := passLengthBucket + ( nIncremntalRingRadius * 1 / 2 )
   ][,
      xmin2 := passAngleBucket - ( passAngleBucket.width * 1 / 2 )
   ][,
      xmax2 := passAngleBucket + ( passAngleBucket.width * 1 / 2 )
   ]


   # Changed both height and width of the block
   # dtAllShapes[,
   #    xmin2 := passAngleBucket - ( passAngleBucket.width * sqrt( PassCount / max(PassCount) ) / 2 )
   # ][,
   #    xmax2 := passAngleBucket + ( passAngleBucket.width * sqrt( PassCount / max(PassCount) ) / 2 )
   # ][,
   #    ymin2 := passLengthBucket - ( nIncremntalRingRadius * sqrt( PassCount / max(PassCount) ) / 2 )
   # ][,
   #    ymax2 := passLengthBucket + ( nIncremntalRingRadius * sqrt( PassCount / max(PassCount) ) / 2 )
   # ]

   # Changed width of the block and kept height constant
   # dtAllShapes[,
   #    ymin3 := passLengthBucket - ( nIncremntalRingRadius * 1 / 2 )
   # ][,
   #    ymax3 := passLengthBucket + ( nIncremntalRingRadius * 1 / 2 )
   # ][,
   #    xmin3 := passAngleBucket - ( passAngleBucket.width * sqrt( PassCount / ( 1.2 * max(PassCount) ) ) / 2 )
   # ][,
   #    xmax3 := passAngleBucket + ( passAngleBucket.width * sqrt( PassCount / ( 1.2 * max(PassCount) ) ) / 2 )
   # ]

   # Going from the four corner coordinates to a set of points that trace out
   # the entire block. Just the four coordinates would draw a quadrangle but
   # we need an arced sort of a quadrangle which is why we need this.
   dtAllShapes = dtAllShapes[,
      list(
         # The coordinates of the block, scaled for number of passes
         x = fCalculatePolygonCoordinate (
            xmin2,
            xmax2,
            ymin2,
            ymax2,
            fTrigFunction = cos
         ),
         y = fCalculatePolygonCoordinate (
            xmin2,
            xmax2,
            ymin2,
            ymax2,
            fTrigFunction = sin
         ),
         # The coordinates of the black for the max area possible for it
         # Can be used to add a boundary to blocks maybe.
         xBound = fCalculatePolygonCoordinate (
            xmin,
            xmax,
            ymin,
            ymax,
            fTrigFunction = cos
         ),
         yBound = fCalculatePolygonCoordinate (
            xmin,
            xmax,
            ymin,
            ymax,
            fTrigFunction = sin
         )
      ),
      list(
         # Moving the bucket coordinates so that they fit on the
         # stretched out pitch
         xBucket = xBucket * nZoomFactor,
         yBucket = yBucket * nZoomFactor,
         PassCount,
         Success_pct,
         passAngleBucket,
         passLengthBucket
      )
   ]

   # Initialising plot
   p1 = ggplot(
      dtAllShapes
   )

   # Adding a football pitch in the background.
   if ( bAddPitchBackground ) {

      p1 = p1 +
         geom_pitch(
            nXStart = 0,
            nYStart = 0,
            nXEnd = nZoomFactor * nXLimit,
            nYEnd = nZoomFactor * nYLimit,
            cPitchColour = '#111111',
            cLineColour = '#333333'
         )

   } else {

      # If the pitch isn't being added, then the coord_fixed will need to be
      # specified explicitly

      p1 = p1 + coord_fixed()

   }



   # If it's only one sonar, i.e. not multiple blocks, then the background
   # of the plot will need some distinction from the ring around the sonar
   # Adding the background and ring around the sonar for that.
   p1 = p1 +
      geom_polygon(
         data = dtPasses[
            x < nXLimit &
            y < nYLimit
         ][,
            list(
               x = max( nXLimit, nYLimit ) * cos(seq(-pi, pi, pi / 50)),
               y = max( nXLimit, nYLimit ) * sin(seq(-pi, pi, pi / 50))
            ),
            list(
               xBucket = xBucket * nZoomFactor,
               yBucket = yBucket * nZoomFactor
             )
         ],
         aes(
            x = x + xBucket,
            y = y + yBucket,
            group = paste0(
               xBucket,
               yBucket
            )
        ),
        fill = '#000000',
        alpha = ifelse(
           dtPasses[, length(unique(xBucket))] == 1 &
           dtPasses[, length(unique(yBucket))] == 1,
           0,
           0.8
        ),
        color = ifelse(
           dtPasses[, length(unique(xBucket))] == 1 &
           dtPasses[, length(unique(yBucket))] == 1,
           '#222222',
           NA
        ),
        size = ifelse(
           dtPasses[, length(unique(xBucket))] == 1 &
           dtPasses[, length(unique(yBucket))] == 1,
           3,
           0
        )
      )

   p1 = p1 +
      # marking a centre point
      geom_point(
        aes(
            x = xBucket,
            y = yBucket
        ),
        color = '#000000'
      ) +
      # passing block
      geom_polygon(
        aes(
            x = x + xBucket,
            y = y + yBucket,
            fill = Success_pct,
            group = paste0(
               xBucket,
               yBucket,
               passAngleBucket,
               passLengthBucket
            )
        ),
        color = 'black',
        size = 0.1
        # alpha = 0.8
      )

   if ( 'bucketLabel' %in% colnames(dtPasses) ) {

      p1 = p1 +
         geom_text(
            data = dtPasses[,
               list(
                  bucketLabel = bucketLabel[1]
               ),
               list(
                  xBucket = xBucket * nZoomFactor,
                  yBucket = yBucket * nZoomFactor
               )
            ],
            aes(
               x = xBucket,
               y = yBucket - ( nXLimit * 1.1 ),
               label = bucketLabel
            ),
            hjust = 0.5,
            vjust = 0.5,
            color = 'white'
         )

   }

   p1 = p1 +
      scale_fill_gradient2(
         # low = '#89cff0',
         # low = '#0070bb',
         # mid = 'yellow',
         # high = 'red',
         low = 'red',
         mid = 'yellow',
         high = '#006400',
         midpoint = 0.5,
         name = 'Success %',
         labels = percent,
         guide = 'none',
         limits = c(0, 1)
      ) +
      # To prevent gaps around the edges of the blot
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      # aesthetic adjustments
      theme_pitch() +
      theme(
         plot.background = element_rect(fill = '#000000', color = NA),
         panel.background = element_rect(fill = '#000000', color = NA),
         panel.border = element_blank(),
         axis.line = element_blank(),
         legend.text = element_text(colour = 'white'),
         # legend.background = element_rect(fill = '#000000'),
         legend.title = element_text(colour = 'white'),
         title = element_text(size = 14, colour = 'white')
      ) +
      # supporting text
      labs(
         title = cTitle,
         subtitle = paste0(
            nrow(dtPasses), ' passes, ',
            round(100 * dtPasses[, sum(Success) / .N]), '% success rate'
         )
      )

   p1

}
