#' Pitch background
#'
#' Adds pitch markings, green background, etc. for plot background to look like
#' a football pitch. ggplot adds things in order so initiate a ggplot object,
#' add the pitch marking with this function, and then add the data you want to
#' add
#'
#' @param plotObject a ggplot object. Can just be a blank ggplot object too
#' @param cLineColour the colour of the line markings on the pitch
#' @param cPitchColour the colour of the grass on the pitch
#' @param nXStart Start x coordinate of the pitch
#' @param nYStart Start y coordinate of the pitch
#' @param nXEnd End x coordinate of the pitch
#' @param nYEnd Start y coordinate of the pitch
#' @examples
#' ggplot() + geom_pitch()
#' @import data.table
#' @import ggplot2
#' @export
geom_pitch = function (
   nXStart = 0,
   nYStart = 0,
   nXEnd = 120,
   nYEnd = 80,
   mOriginCoordinates = NULL,
   mScreenCoordinates = NULL,
   mViewBeginsFromCoordinates = NULL,
   mZAxisVector = c(0,0,1),
   cLineColour = '#BBBBBB',
   cPitchColour = '#038253',
   vcToPlot = c('Markings','Goalframe','Goalnet')
) {

   # nXStart = 0; nYStart = 0; nXEnd = 120; nYEnd = 80; mZAxisVector = c(0,0,1)

   lPitchDimensions = fGetPitchDimensions (
      nXStart = nXStart,
      nYStart = nYStart,
      nXEnd = nXEnd,
      nYEnd = nYEnd
   )


   if (requireNamespace("POV", quietly = TRUE)) {
      if ( !is.null(mOriginCoordinates) )  {

      lPitchDimensions$lPitchCoordinates = lapply(
          lPitchDimensions$lPitchCoordinates,
          function( dtDimensions ) {

              # dtDimensions = lPitchDimensions$lPitchCoordinates[[1]]

              mCoordinates = cbind(
                  dtDimensions[, x],
                  dtDimensions[, y],
                  dtDimensions[, z]
              )

              mTransformedCoordinates = POV::fGetTransformedCoordinates(
                  mCoordinates = mCoordinates,
                  mOriginCoordinates = mOriginCoordinates,
                  mScreenCoordinates = mScreenCoordinates,
                  iTreatAs = 2,
                  mZAxisVector = mZAxisVector,
                  mViewBeginsFromCoordinates = mViewBeginsFromCoordinates
              )

              data.table(
                  x = mTransformedCoordinates[, 1],
                  y = mTransformedCoordinates[, 2],
                  group = mTransformedCoordinates[, 3]
              )

          }
      )
      # names(lPitchDimensions$lPitchCoordinates) = names(lPitchDimensions$lPitchCoordinates)

      if ( F ) {

         lPitchDimensions$lGoalframes = lapply(
             lPitchDimensions$lGoalframes,
             function( dtDimensions ) {

                 # dtDimensions = lPitchDimensions$lGoalframes[[1]]

                 mCoordinates = cbind(
                     dtDimensions[, x],
                     dtDimensions[, y],
                     dtDimensions[, z]
                 )

                 mTransformedCoordinates = POV::fGetTransformedCoordinates(
                     mCoordinates = mCoordinates,
                     mOriginCoordinates = mOriginCoordinates,
                     mScreenCoordinates = mScreenCoordinates,
                     mZAxisVector = mZAxisVector,
                     mViewBeginsFromCoordinates = mViewBeginsFromCoordinates
                 )

                 data.table(
                     x = mTransformedCoordinates[, 1],
                     y = mTransformedCoordinates[, 2]
                 )

             }
         )
         # names(lPitchDimensions$lGoalframes) = names(lPitchDimensions$lGoalframes)

      }


      lPitchDimensions$lGoalframes = lapply(
          lPitchDimensions$lGoalframes,
          function( lRectangles ) {
             lapply(
                lRectangles,
                function( dtDimensions ) {

                    # dtDimensions = lPitchDimensions$lGoalframes[[1]]

                    mCoordinates = cbind(
                        dtDimensions[, x],
                        dtDimensions[, y],
                        dtDimensions[, z]
                    )

                    mTransformedCoordinates = POV::fGetTransformedCoordinates(
                        mCoordinates = mCoordinates,
                        mOriginCoordinates = mOriginCoordinates,
                        mScreenCoordinates = mScreenCoordinates,
                        mZAxisVector = mZAxisVector,
                        mViewBeginsFromCoordinates = mViewBeginsFromCoordinates
                    )


                    data.table(
                        x = mTransformedCoordinates[, 1],
                        y = mTransformedCoordinates[, 2]
                    )

                }
            )
          }
      )

      lPitchDimensions$lGoalnet = lapply(
          lPitchDimensions$lGoalnet,
          function( dtSegments ) {

              # dtSegments = lPitchDimensions$lGoalnet[[5]]

              lapply(
                  dtSegments,
                  function( dtSegment ) {

                      mCoordinates = cbind(
                          dtSegment[, x],
                          dtSegment[, y],
                          dtSegment[, z]
                      )

                      mTransformedCoordinates = POV::fGetTransformedCoordinates(
                           mCoordinates = mCoordinates,
                           mOriginCoordinates = mOriginCoordinates,
                           mScreenCoordinates = mScreenCoordinates,
                           mZAxisVector = mZAxisVector,
                           iTreatAs = 2,
                           mViewBeginsFromCoordinates = mViewBeginsFromCoordinates
                       )

                      data.table(
                          x = mTransformedCoordinates[, 1],
                          y = mTransformedCoordinates[, 2]
                      )
                  }
              )


          }
      )
      # names(lPitchDimensions$lGoalnet) = names(lPitchDimensions$lGoalnet)

      }

   } else {
      warning('POV package not installed, can\t use the mOrigin argument')
   }



   # https://github.com/tidyverse/ggplot2/issues/2799
   cf = coord_fixed()
   cf$default = TRUE

   lPitchElements = list()

   if ( 'Markings' %in% vcToPlot ) {

      lPitchElements = append(
         lPitchElements,
         lapply(
            lPitchDimensions$lPitchCoordinates[sapply(lPitchDimensions$lPitchCoordinates, nrow) > 0],
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
            lPitchDimensions$lPitchCoordinates[sapply(lPitchDimensions$lPitchCoordinates, nrow) > 0],
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

   if ( 'Goalframe' %in% vcToPlot ) {

      if ( F ) {

         lGoalFrameElements = lapply(
            lPitchDimensions$lGoalframes[sapply(lPitchDimensions$lGoalframes, nrow) > 0],
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
                      lPitchDimensions$lGoalframes$dtGoalPostFloorDefenseLow[x %in% range(x)][order(x)],
                      lPitchDimensions$lGoalframes$dtGoalPostAirDefenseLow[x %in% range(x)][rev(order(x))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                      lPitchDimensions$lGoalframes$dtGoalPostFloorDefenseHigh[x %in% range(x)][order(x)],
                      lPitchDimensions$lGoalframes$dtGoalPostAirDefenseHigh[x %in% range(x)][rev(order(x))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                      lPitchDimensions$lGoalframes$dtGoalPostAirDefenseLow[y %in% range(y)][order(y)],
                      lPitchDimensions$lGoalframes$dtGoalPostAirDefenseHigh[y %in% range(y)][rev(order(y))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour,
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                      lPitchDimensions$lGoalframes$dtGoalPostFloorOffenseLow[x %in% range(x)][order(x)],
                      lPitchDimensions$lGoalframes$dtGoalPostAirOffenseLow[x %in% range(x)][rev(order(x))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                      lPitchDimensions$lGoalframes$dtGoalPostFloorOffenseHigh[x %in% range(x)][order(x)],
                      lPitchDimensions$lGoalframes$dtGoalPostAirOffenseHigh[x %in% range(x)][rev(order(x))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour
                  # color = 'black'
               ),
               geom_polygon(
                  data = rbind(
                      lPitchDimensions$lGoalframes$dtGoalPostAirOffenseLow[y %in% range(y)][order(y)],
                      lPitchDimensions$lGoalframes$dtGoalPostAirOffenseHigh[y %in% range(y)][rev(order(y))]
                  ),
                  aes(x = x, y = y),
                  fill = cLineColour,
                  # color = 'black'
               )
            )
         )

      }

      lGoalFrameElements = list()
      for ( i in seq(length(lPitchDimensions$lGoalframes)) ) {

         lGoalFrameElements = append(
            lGoalFrameElements,
            lapply(
               seq(length(lPitchDimensions$lGoalframes[[i]])),
               function (j) {

                  nextJ = j + 1
                  nextJ = ifelse(nextJ > length(lPitchDimensions$lGoalframes[[i]]), 1, nextJ)

                  dtCoordinates = rbind(
                     lPitchDimensions$lGoalframes[[i]][[j]],
                     lPitchDimensions$lGoalframes[[i]][[nextJ]][.N:1]
                  )

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
            lPitchDimensions$lGoalnet,
            function( lSegments ) {

               # lSegments = lPitchDimensions$lGoalnet[[5]]

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
                  x = ( lPitchDimensions$lParameters$nXSpan / 2 ) - ( lPitchDimensions$lParameters$nXSpan * 2 / 105 ),
                  xend = ( lPitchDimensions$lParameters$nXSpan / 2 ),
                  # xend = ( lPitchDimensions$lParameters$nXSpan / 2 ) + ( lPitchDimensions$lParameters$nXSpan * 2 / 105 ),
                  y = ( lPitchDimensions$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchDimensions$lParameters$nXSpan * 0.9 / 105 ),
                  yend = ( lPitchDimensions$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchDimensions$lParameters$nXSpan * 1.1 / 105 )
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
                     ( lPitchDimensions$lParameters$nXSpan / 2 ),
                     ( lPitchDimensions$lParameters$nXSpan / 2 ) + ( lPitchDimensions$lParameters$nXSpan * 2 / 105 ),
                     ( lPitchDimensions$lParameters$nXSpan / 2 )
                  ),
                  y = c(
                     lPitchDimensions$lParameters$nYSpan * -2.5 / 68,
                     ( lPitchDimensions$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchDimensions$lParameters$nXSpan * 1 / 105 ),
                     ( lPitchDimensions$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchDimensions$lParameters$nXSpan * 2 / 105 )
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

   lPitchElements = append(
      lPitchElements,
      cf
   )

   return ( lPitchElements )

}
