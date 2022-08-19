#' part of the geom_pitch backend
geom_pitch_from_coords_obj = function(
   lPitchCoordinates,
   cLineColour = '#BBBBBB',
   cPitchColour = '#038253',
   vcToPlot = c('Markings','Stripes','Goalframe','Goalnet')
) {

   # https://github.com/tidyverse/ggplot2/issues/2799
   # cf = coord_fixed()
   # cf$default = TRUE

   lPitchElements = list()

   if ( 'Markings' %in% vcToPlot ) {

      lPitchElements = append(
         lPitchElements,
         lapply(
            lPitchCoordinates$lMarkings[
               sapply(lPitchCoordinates$lMarkings, nrow) > 0
            ],
            function( dtPolygon ) {
               geom_polygon(
                  data = dtPolygon,
                  aes(
                     x = x,
                     y = y
                  ),
                  color = NA,
                  fill = cPitchColour
               )
            }
         )
      )

      lPitchElements = append(
         lPitchElements,
         lapply(
            lPitchCoordinates$lMarkings[
               sapply(lPitchCoordinates$lMarkings, nrow) > 0
            ],
            function( dtPolygon ) {
               geom_path(
                  data = dtPolygon,
                  aes(
                     x = x,
                     y = y,
                     group = group
                  ),
                  color = cLineColour
                  # fill = NA
               )
            }
         )
      )

   }

   if ( 'Stripes' %in% vcToPlot & nrow(lPitchCoordinates$dtPitchStripes) ) {

      lPitchElements = append(
         lPitchElements,
         list(
            geom_polygon(
               data = lPitchCoordinates$dtPitchStripes,
               aes(
                  x = x,
                  y = y,
                  group = stripe_id
               ),
               fill = 'white',
               alpha = 0.05
            )
         )
      )

   }

   if ( 'Goalframe' %in% vcToPlot ) {

      if ( F ) {

         lGoalFrameElements = lapply(
            lPitchCoordinates$lGoalframes[
               sapply(lPitchCoordinates$lGoalframes, nrow) > 0
            ],
            function( dtPolygon ) {
               geom_polygon(
                  data = dtPolygon,
                  aes(
                     x = x,
                     y = y
                  ),
                  color = cLineColour,
                  fill = cLineColour
               )
            }
         )

         lGoalFrameElements = append(
            lGoalFrameElements,
            list(
               geom_polygon(
                  data = rbind(
                     lPitchCoordinates$lGoalframes$dtGoalPostFloorDefenseLow[x %in% range(x)][order(x)],
                     lPitchCoordinates$lGoalframes$dtGoalPostAirDefenseLow[x %in% range(x)][rev(order(x))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                     lPitchCoordinates$lGoalframes$dtGoalPostFloorDefenseHigh[x %in% range(x)][order(x)],
                     lPitchCoordinates$lGoalframes$dtGoalPostAirDefenseHigh[x %in% range(x)][rev(order(x))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                     lPitchCoordinates$lGoalframes$dtGoalPostAirDefenseLow[y %in% range(y)][order(y)],
                     lPitchCoordinates$lGoalframes$dtGoalPostAirDefenseHigh[y %in% range(y)][rev(order(y))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour,
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                     lPitchCoordinates$lGoalframes$dtGoalPostFloorOffenseLow[x %in% range(x)][order(x)],
                     lPitchCoordinates$lGoalframes$dtGoalPostAirOffenseLow[x %in% range(x)][rev(order(x))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                     lPitchCoordinates$lGoalframes$dtGoalPostFloorOffenseHigh[x %in% range(x)][order(x)],
                     lPitchCoordinates$lGoalframes$dtGoalPostAirOffenseHigh[x %in% range(x)][rev(order(x))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                     lPitchCoordinates$lGoalframes$dtGoalPostAirOffenseLow[y %in% range(y)][order(y)],
                     lPitchCoordinates$lGoalframes$dtGoalPostAirOffenseHigh[y %in% range(y)][rev(order(y))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour,
                  # color = 'black'
               )
            )
         )

      }

      lGoalFrameElements = list()
      for ( i in seq(length(lPitchCoordinates$lGoalframes)) ) {

         lGoalFrameElements = append(
            lGoalFrameElements,
            lapply(
               seq(length(lPitchCoordinates$lGoalframes[[i]])),
               function (j) {

                  nextJ = j + 1
                  nextJ = ifelse(nextJ > length(lPitchCoordinates$lGoalframes[[i]]), 1, nextJ)

                  # dtCoordinates = rbind(
                  #    lPitchCoordinates$lGoalframes[[i]][[j]],
                  #    lPitchCoordinates$lGoalframes[[i]][[nextJ]][.N:1]
                  # )

                  dtCoordinates = lPitchCoordinates$lGoalframes[[i]][[j]]

                  if ( nrow(dtCoordinates) ) {

                     returnObject = geom_polygon(
                        data = dtCoordinates,
                        aes(
                           x = x,
                           y = y
                        ),
                        color = cLineColour,
                        fill = cLineColour
                     )

                  } else {

                     returnObject = NULL

                  }

                  return ( returnObject )

               }
            )
         )

      }
      lGoalFrameElements = lGoalFrameElements[!sapply(lGoalFrameElements, is.null)]
      lPitchElements = append(
         lPitchElements,
         lGoalFrameElements
      )
      rm(lGoalFrameElements)

   }

   if ( 'Goalnet' %in% vcToPlot ) {

      dtGoalNetElements = rbindlist(
         lapply(
            lPitchCoordinates$lGoalnet,
            function( lSegments ) {

               # lSegments = lPitchCoordinates$lGoalnet[[5]]

               dtSegments = rbindlist(
                  lapply(
                     lSegments,
                     function( dtSegment ) {

                        if ( nrow(dtSegment) ) {

                           dtSegment = data.table(
                              x = dtSegment[1, x],
                              y = dtSegment[1, y],
                              xend = dtSegment[2, x],
                              yend = dtSegment[2, y]
                           )

                        } else {

                           dtSegment = data.table()

                        }

                        dtSegment

                     }
                  ),
                  fill = T
               )

               dtSegments

            }
         ),
         fill = T
      )

      lPitchElements = append(
         lPitchElements,
         geom_segment(
            data = dtGoalNetElements,
            aes(
               x = x,
               y = y,
               xend = xend,
               yend = yend
            ),
            color = cLineColour,
            alpha = 0.4
         )
      )
   }

   if ( F ) {

      lPitchElements = append(
         lPitchElements,
         list(
            # direction of attack
            geom_rect(
               data = data.table(
                  x = ( lPitchCoordinates$lParameters$nXSpan / 2 ) - ( lPitchCoordinates$lParameters$nXSpan * 2 / 105 ),
                  xend = ( lPitchCoordinates$lParameters$nXSpan / 2 ),
                  # xend = ( lPitchCoordinates$lParameters$nXSpan / 2 ) + ( lPitchCoordinates$lParameters$nXSpan * 2 / 105 ),
                  y = ( lPitchCoordinates$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchCoordinates$lParameters$nXSpan * 0.9 / 105 ),
                  yend = ( lPitchCoordinates$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchCoordinates$lParameters$nXSpan * 1.1 / 105 )
               ),
               aes(
                  xmin = nXStart + x,
                  xmax = nXStart + xend,
                  ymin = nYStart + y,
                  ymax = nYStart + yend
               ),
               # arrow = arrow(length = unit(0.03, "npc")),
               color = cLineColour
            )
            ,
            geom_polygon(
               data = data.table(
                  x = c(
                     ( lPitchCoordinates$lParameters$nXSpan / 2 ),
                     ( lPitchCoordinates$lParameters$nXSpan / 2 ) + ( lPitchCoordinates$lParameters$nXSpan * 2 / 105 ),
                     ( lPitchCoordinates$lParameters$nXSpan / 2 )
                  ),
                  y = c(
                     lPitchCoordinates$lParameters$nYSpan * -2.5 / 68,
                     ( lPitchCoordinates$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchCoordinates$lParameters$nXSpan * 1 / 105 ),
                     ( lPitchCoordinates$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchCoordinates$lParameters$nXSpan * 2 / 105 )
                  )
               ),
               aes(
                  x = nXStart + x,
                  y = nYStart + y
               ),
               fill = cLineColour
            )
         )
      )

   }

   # lPitchElements = append(
   #    lPitchElements,
   #    cf
   # )

   return ( lPitchElements )

}
