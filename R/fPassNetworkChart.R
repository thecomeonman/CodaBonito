#' Pass network ( WIP )
#'
#' Plots a marker for each player at their median passing position, and draws
#' connections between players to represent the number of passes exchanged 
#' between them
#'
#' @param dtPasses a data.table with the columns playerId ( the player who
#' made the pass, ) recipientPlayerId ( the player who received the pass, )
#' Success ( 1/0 for whether the pass reached the recipient, ) x ( the 
#' coordinate along the length of the pitch, 0 is defensive end, nXLimit is 
#' offensive end, ) and y ( along the breadth of the pitch, 0 is right wing and 
#' nYLimit is left wing. ) Refer to the dtPasses dataset packaged with the 
#' library 
#' @param dtPlayerLabels a data.table with the colums playerId ( same as 
#' dtPasses, ) and playerName ( the label that the point of the respective 
#' player should be labelled as. ) Leaving this blank will mean no labels in 
#' the diagram. Refer to the dtPlayerLabels dataset packaged with the library.
#' @param nXLimit Length of the pitch
#' @param nYLimit Breadth of the pitch
#' @examples
#' fPassNetworkChart(
#'    dtPasses,
#'    dtPlayerLabels
#' )
#' @import ggplot2
#' @import data.table
#' @export
fPassNetworkChart = function(
   dtPasses,
   dtPlayerLabels, 
   nXLimit = 120,
   nYLimit = 80,
   nSegmentWidth = 1
) {

   vnAngleSequence = seq(0, 2*pi, pi/50)   

   dtNodes = dtPasses[
      Success == 1,
      list(
         Count = .N,
         x = median(x),
         y = median(y)
      ),
      playerId
   ]

   dtSegments = merge(
      dtNodes[, list(playerId, k = 'k')],
      dtNodes[, list(recipientPlayerId = playerId, k = 'k')],
      'k',
      allow.cartesian = T
   )
   dtSegments[, k := NULL]
   dtSegments = dtSegments[playerId != recipientPlayerId]

   dtSegments = merge(
      dtSegments,
      dtPasses[
         Success == 1,
         list(
            CountBetween = .N
         ),
         list(playerId, recipientPlayerId)
      ],
      c('playerId', 'recipientPlayerId'),
      all = T
   )

   dtSegments = merge(
      dtSegments,
      dtNodes[, list(playerId, x, y)],
      'playerId'
   )

   dtSegments = merge(
      dtSegments,
      dtNodes[, list(recipientPlayerId = playerId, receipientX = x, receipientY = y)],
      'recipientPlayerId'
   )

   dtSegments[
      is.na(CountBetween),
      CountBetween := 0
   ]

   if ( !is.null(dtPlayerLabels) ) {

      dtNodes = merge(
         dtNodes,
         dtPlayerLabels,
         'playerId'
      )

   }

   p1 = ggplot()

   p1 = fAddPitchLines(
      p1, 
      nXLimit = nXLimit,
      nYLimit = nYLimit,
      cLineColour = 'green',
      cPitchColour = 'white'
   )


   p1 = p1 + 
      geom_polygon(
         data = dtNodes[,
            list(
               xPath = 5 * ( Count / dtNodes[, max(Count)] ) * cos(vnAngleSequence),
               yPath = 5 * ( Count / dtNodes[, max(Count)] ) * sin(vnAngleSequence)
            ),
            list(
               playerId,
               x,
               y
            )
         ],
         aes(
            x = xPath + x,
            y = yPath + y,
            group = playerId
         ),
         color = 'red',
         alpha = 0,
         size = 2
      )

   if ( F ) {
         
      p1 = p1 +
         geom_segment(
         # geom_curve(
            data = dtSegments[order(CountBetween)],
            aes(
               x = receipientX,
               y = receipientY,
               xend = ( receipientX + x ) / 2,
               yend = ( receipientY + y ) / 2,
               color = CountBetween
            ),
            size = 4,
            # curvature = 0.1,
            # arrow = arrow(length = unit(0.03,"npc")),
            # arrow.fill = 'black'
         )

   } else if ( F ) {

      p1 = p1 +
         geom_segment(
         # geom_curve(
            data = dtSegments[order(CountBetween)],
            aes(
               x    = ( receipientX             ) + ( 1 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
               y    = ( receipientY             ) + ( 1 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
               xend = ( ( ( 0.1 * receipientX ) + ( 0.9 * x ) ) ) + ( 1 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
               yend = ( ( ( 0.1 * receipientY ) + ( 0.9 * y ) ) ) + ( 1 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
               alpha = CountBetween
            ),
            size = 4,
            color = 'black'
            # curvature = 0.1,
            # arrow = arrow(length = unit(0.03,"npc")),
            # arrow.fill = 'black'
         )

   } else if ( F ) {

      p1 = p1 +
         geom_polygon(
            data = dtSegments[
               order(CountBetween),
               list(
                  x = c(
                        ( receipientX ) - ( 1 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( receipientX ) + ( 0 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( x           ) + ( 0 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( x           ) - ( 1 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) )
                  ),
                  y = c(
                        ( receipientY ) - ( 1 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( receipientY ) + ( 0 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( y           ) + ( 0 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( y           ) - ( 1 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) )
                  )
               ),
               list(
                  playerId,
                  recipientPlayerId,
                  CountBetween
               )
            ],
            aes(
               x = x,
               y = y,
               group = paste(formatC(CountBetween, flag='0', width = 3), playerId, recipientPlayerId),
               fill = CountBetween
            ),
            color = 'black'
         )

   } else if ( T ) {

      # ascending order of max of back and fro between each two players is the 
      # order in which the segments get placed. Highest passing between two 
      # players should be visible on top
      dtSegmentOutlines = rbind(
         dtSegments[, list(playerId, recipientPlayerId, CountBetween)],
         dtSegments[, list(playerId = recipientPlayerId, recipientPlayerId = playerId, CountBetween)]
      )[
         playerId < recipientPlayerId
      ][, 
         .SD[which.max(CountBetween)][1],
         list(playerId, recipientPlayerId)
      ]

      # for each pair of back and fro, adding the segments
      for ( i in seq(nrow(dtSegmentOutlines))) {

         dtSegmentSubset = dtSegments[
            playerId %in% dtSegmentOutlines[i, c(playerId, recipientPlayerId)] &
            recipientPlayerId %in% dtSegmentOutlines[i, c(playerId, recipientPlayerId)],
         ]

         # adding the pass segment
         p1 = p1 +
            geom_polygon(
               data = dtSegmentSubset[,
                  list(
                     x = c(
                           ( receipientX ) - ( nSegmentWidth * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( receipientX ) + ( 0 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( x           ) + ( 0 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( x           ) - ( nSegmentWidth * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) )
                     ),
                     y = c(
                           ( receipientY ) - ( nSegmentWidth * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( receipientY ) + ( 0 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( y           ) + ( 0 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( y           ) - ( nSegmentWidth * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) )
                     )
                  ),
                  list(
                     playerId,
                     recipientPlayerId,
                     CountBetween
                  )
               ],
               aes(
                  x = x,
                  y = y,
                  group = paste(formatC(CountBetween, flag='0', width = 3), playerId, recipientPlayerId),
                  # alpha = CountBetween
                  fill = CountBetween
               ),
               # fill = 'black'
            )


         # adding the direction
         p1 = p1 +
            geom_path(
               data = dtSegmentSubset[,
                  list(
                     x = c(
                        ( ( ( receipientX * 0.45 ) + ( x * 0.55 ) ) ) - ( nSegmentWidth * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( ( ( receipientX * 0.45 ) + ( x * 0.55 ) ) ) - ( nSegmentWidth * cos( (pi/6) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( ( ( receipientX * 0.45 ) + ( x * 0.55 ) ) ) + ( 0 * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) )
                     ),
                     y = c(
                        ( ( ( receipientY * 0.45 ) + ( y * 0.55 ) ) ) - ( nSegmentWidth * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( ( ( receipientY * 0.45 ) + ( y * 0.55 ) ) ) - ( nSegmentWidth * sin( (pi/6) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                        ( ( ( receipientY * 0.45 ) + ( y * 0.55 ) ) ) + ( 0 * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) )
                     )
                  ),
                  list(
                     CountBetween,
                     playerId,
                     recipientPlayerId
                  )
               ],
               aes(
                  x = x,
                  y = y,
                  group = paste(formatC(CountBetween, flag='0', width = 3), playerId, recipientPlayerId),
                  color = ifelse(
                  # fill = ifelse(
                     CountBetween < dtSegments[, max(CountBetween) / 2],
                     dtSegments[, max(CountBetween)],
                     0
                  )
               ),
            )

         p1 = p1 +
            geom_polygon(
               data = dtSegmentSubset[1][,
                  list(
                     x = c(
                           ( receipientX ) - ( nSegmentWidth * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( receipientX ) + ( nSegmentWidth * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( x           ) + ( nSegmentWidth * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( x           ) - ( nSegmentWidth * cos( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) )
                     ),
                     y = c(
                           ( receipientY ) - ( nSegmentWidth * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( receipientY ) + ( nSegmentWidth * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( y           ) + ( nSegmentWidth * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) ),
                           ( y           ) - ( nSegmentWidth * sin( (pi/2) + atan2( y = ( receipientY - y ), x = ( receipientX - x ) )) )
                     )
                  ),
                  list(
                     playerId,
                     recipientPlayerId,
                     CountBetween
                  )
               ],
               aes(
                  x = x,
                  y = y,
                  group = paste(formatC(CountBetween, flag='0', width = 3), playerId, recipientPlayerId),
               ),
               alpha = 0,
               color = 'black'
            )

      }         

   }

   # p1 = p1 + 
   #    geom_segment(
   #       data = dtSegments[playerId < recipientPlayerId],
   #       aes(
   #          x = x,
   #          y = y,
   #          xend = receipientX,
   #          yend = receipientY
   #       ),
   #       color = 'black'
   #    )
   
   p1 = p1 +
      scale_fill_continuous(
         low = 'white',
         high = 'black',
         limits = c(
            0,
            dtSegments[, max(CountBetween)]
         ),
         guide = 'none'
      ) +
      scale_color_continuous(
         low = 'white',
         high = 'black',
         guide = 'none'
      ) +
      theme_pitch()

   if ( !is.null(dtPlayerLabels) ) {
      
      p1 = p1 + 
         geom_label(
            data = dtNodes,
            aes(
               x = x,
               y = y,
               label = playerName
            ),
            # vjust = 1,
            color = 'red'
         )
   }

   p1

}
