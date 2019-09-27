#' This function has only been tested with 120 X 80 dimension dataset, in the 
#' two ways shown in the charts section. Any deviation from those may 
#' produce odd results.
#' @import data.table
#' @import ggplot2
#' @export
fPlotSonar = function (
   dtPassesToPlot,
   iBlocksInFirstRing = 4,
   iNbrRings = 8,
   nZoomFactor = NULL,
   nLocation2Size = 80,
   nLocation1Size = 120,
   bAddPitchBackground = F,
   cTitle = NULL
) {
   
   dtPasses = copy(dtPassesToPlot)
   
   nIncremntalRingRadius = nLocation1Size / iNbrRings
   
   # A vector with the same length as the number of rings possible, with each
   # element having the radius for that ring
   vnPassLengthBreaks = seq(0, nLocation1Size + nLocation2Size, nIncremntalRingRadius)
   
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
   
   if ( is.null(nZoomFactor) ) {
      
      nZoomFactor = ceiling(
         max(
            dtPasses[, length(unique(location1.bucket))], 
            dtPasses[, length(unique(location2.bucket)), location1.bucket][, max(V1)],
            ( nLocation1Size * dtPasses[, length(unique(location2.bucket)), location1.bucket][, max(V1)] ) / nLocation2Size
         )
      ) 
      
   }
   
   nZoomFactor = nZoomFactor * 2 *  1.1
   
   # Length bucket number. The bucket number, and not the bucket, is needed
   # to calculate the angle buckets.
   dtPasses[, 
      pass.length.bucket := pass.length %/% nIncremntalRingRadius
   ]
   
   # Angle buckets.
   # Basically retrieving the number of angle buckets for the resepctive ring
   # which are already calculated in viNbrAngleBreaks and labelling the
   # data accordingly.
   dtPasses[, 
      pass.angle.bucket := pass.angle %/% ( 
         2 * pi / viNbrAngleBreaks[ 1 + pass.length.bucket ] 
      ),
      pass.length.bucket
   ]
   
   # When angle is exactly 2pi, it falls into a bucket which has only 2*pi 
   # angle passes. Forcing them to the previous bucket. Will live with this
   # inconsistency.
   # Not just comparing angle == 2pi because of precision affecting the result
   # of that comparison
   dtPasses[, 
      pass.angle.bucket := ifelse(
         pass.angle.bucket == ( 2 * pi) %/% (
            2 * pi / viNbrAngleBreaks[ 1 + pass.length.bucket ]
         ),
         pass.angle.bucket - 1,
         pass.angle.bucket
      ),
      pass.length.bucket
   ]
   
   dtPasses[, 
      pass.angle.bucket.width := (
         2 * pi / viNbrAngleBreaks[ 1 + pass.length.bucket ] 
      ), 
      pass.length.bucket
   ]
   
   dtPasses[, 
      pass.angle.bucket := ( pass.angle.bucket + 0.5 ) * pass.angle.bucket.width
   ]
   
   # Calculating actual pass length buckets
   dtPasses[, 
      pass.length.bucket := ( pass.length.bucket + 0.5 ) * nIncremntalRingRadius, 
      pass.length.bucket
   ]
   
   # Preparing the polygons needed for various elements of the chart
   dtAllShapes = dtPasses[
      # Remove throws in, etc. otherwise they might show up beyond
      # the touchlines.
      location1 < nLocation1Size &
      location2 < nLocation2Size
   ][
      # Some passes don't have this data
      !is.na(pass.length.bucket) &
      !is.na(pass.angle.bucket)
   ][, 
      list(
         PassCount = .N, 
         Success_pct = sum(is.na(pass.outcome.name)) / .N
      ),
      list(
         location1.bucket,
         location2.bucket,
         pass.angle.bucket, 
         pass.angle.bucket.width,
         pass.length.bucket,
         # the bucket values are defined at the centre of the respective
         # dimension so the min and max span would need to be calcualted
         xmin = pass.angle.bucket - ( pass.angle.bucket.width / 2 ),
         xmax = pass.angle.bucket + ( pass.angle.bucket.width / 2 ),
         ymin = pass.length.bucket - ( nIncremntalRingRadius / 2 ),
         ymax = pass.length.bucket + ( nIncremntalRingRadius / 2 )
      )
   ]
   
   # Changing the dimensions of the block based on the number of passes 
   # in that block such that the outer boundary extends all the way to the max
   # radius the ring permits and the inner radius is moved
   dtAllShapes[,
      ymin2 := pass.length.bucket + 
        ( nIncremntalRingRadius * 1 / 2 ) - 
        ( nIncremntalRingRadius * sqrt( PassCount / ( 1.2 * max(PassCount) ) ) )
   ][,
      ymax2 := pass.length.bucket + ( nIncremntalRingRadius * 1 / 2 )
   ][,
      xmin2 := pass.angle.bucket - ( pass.angle.bucket.width * 1 / 2 )
   ][,
      xmax2 := pass.angle.bucket + ( pass.angle.bucket.width * 1 / 2 )
   ]
   
   
   # Changed both height and width of the block
   # dtAllShapes[,
   #    xmin2 := pass.angle.bucket - ( pass.angle.bucket.width * sqrt( PassCount / max(PassCount) ) / 2 )
   # ][,
   #    xmax2 := pass.angle.bucket + ( pass.angle.bucket.width * sqrt( PassCount / max(PassCount) ) / 2 )
   # ][,
   #    ymin2 := pass.length.bucket - ( nIncremntalRingRadius * sqrt( PassCount / max(PassCount) ) / 2 )
   # ][,
   #    ymax2 := pass.length.bucket + ( nIncremntalRingRadius * sqrt( PassCount / max(PassCount) ) / 2 )
   # ]
   
   # Changed width of the block and kept height constant
   # dtAllShapes[,
   #    ymin3 := pass.length.bucket - ( nIncremntalRingRadius * 1 / 2 )
   # ][,
   #    ymax3 := pass.length.bucket + ( nIncremntalRingRadius * 1 / 2 )
   # ][,
   #    xmin3 := pass.angle.bucket - ( pass.angle.bucket.width * sqrt( PassCount / ( 1.2 * max(PassCount) ) ) / 2 )
   # ][,
   #    xmax3 := pass.angle.bucket + ( pass.angle.bucket.width * sqrt( PassCount / ( 1.2 * max(PassCount) ) ) / 2 )
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
         location1.bucket = location1.bucket * nZoomFactor,
         location2.bucket = location2.bucket * nZoomFactor,
         PassCount,
         Success_pct,
         pass.angle.bucket,
         pass.length.bucket
      )
   ]
   
   # Initialising plot
   p1 = ggplot(
      dtAllShapes
   )
   
   # Adding a football pitch in the background.
   if ( bAddPitchBackground ) {
      
      p1 = fAddPitchLines(
         p1, 
         nXLimit = nZoomFactor * nLocation1Size, 
         nYLimit = nZoomFactor * nLocation2Size,
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
            location1 < nLocation1Size &
            location2 < nLocation2Size
         ][,
            list(
               x = max( nLocation1Size, nLocation2Size ) * cos(seq(-pi, pi, pi / 50)),
               y = max( nLocation1Size, nLocation2Size ) * sin(seq(-pi, pi, pi / 50))
            ),
            list(
               location1.bucket = location1.bucket * nZoomFactor,
               location2.bucket = location2.bucket * nZoomFactor
             )
         ],
         aes(
            x = x + location1.bucket,
            y = y + location2.bucket,
            group = paste0(
               location1.bucket,
               location2.bucket
            )
        ),
        fill = '#000000',
        alpha = ifelse(
           dtPasses[, length(unique(location1.bucket))] == 1 &
           dtPasses[, length(unique(location2.bucket))] == 1,
           0,
           0.8
        ),
        color = ifelse(
           dtPasses[, length(unique(location1.bucket))] == 1 &
           dtPasses[, length(unique(location2.bucket))] == 1,
           '#222222',
           NA
        ),
        size = ifelse(
           dtPasses[, length(unique(location1.bucket))] == 1 &
           dtPasses[, length(unique(location2.bucket))] == 1,
           3,
           0
        )
      )
   
   p1 = p1 +
      # marking a centre point
      geom_point(
        aes(
            x = location1.bucket,
            y = location2.bucket
        ),
        color = '#000000'
      ) +
      # passing block
      geom_polygon(
        aes(
            x = x + location1.bucket,
            y = y + location2.bucket,
            fill = Success_pct,
            group = paste0(
               location1.bucket,
               location2.bucket,
               pass.angle.bucket,
               pass.length.bucket
            )
        ),
        color = 'black',
        size = 0.1
        # alpha = 0.8
      )
   
   if ( 'location.bucket.label' %in% colnames(dtPasses) ) {
      
      p1 = p1 +
         geom_text(
            data = dtPasses[, 
               list(
                  location.bucket.label = location.bucket.label[1]
               ), 
               list(
                  location1.bucket = location1.bucket * nZoomFactor, 
                  location2.bucket = location2.bucket * nZoomFactor
               )
            ],
            aes(
               x = location1.bucket,
               y = location2.bucket - ( nLocation1Size * 1.1 ),
               label = location.bucket.label
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
            round(100 * dtPasses[, sum(is.na(pass.outcome.name)) / .N]), '% success rate'
         )
      )
   
   p1

}