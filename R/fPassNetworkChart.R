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
#' nYLimit is left wing )
#' @param dtPlayerLabels a data.table with the colums playerId ( same as 
#' dtPasses, ) and playerName ( the label that the point of the respective 
#' player should be labelled as. ) Leaving this blank will mean no labels in 
#' the diagram.
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
   nYLimit = 80
) {

   dtSegments = dtPasses[
      Success == 1,
      list(
         CountBetween = .N
      ),
      list(playerId, recipientPlayerId)
   ]

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
      dtSegments,
      dtNodes[, list(playerId, x, y)],
      'playerId'
   )

   dtSegments = merge(
      dtSegments,
      dtNodes[, list(recipientPlayerId = playerId, receipientX = x, receipientY = y)],
      'recipientPlayerId'
   )

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
      nYLimit = nYLimit
   )

   vnAngleSequence = seq(0, 2*pi, pi/50)
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
         fill = 'red'
      )
   
   p1 = p1 +
      geom_curve(
         data = dtSegments[order(CountBetween)],
         aes(
            x = receipientX,
            y = receipientY,
            xend = x,
            yend = y,
            color = CountBetween
         ),
         size = 4,
         curvature = 0.1,
         arrow = arrow(length = unit(0.03,"npc")),
         arrow.fill = 'black'
      )
      
   p1 = p1 +
      scale_color_continuous(
         low = 'white',
         high = 'black'
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
            vjust = 1,
            color = 'red'
         )
   }

   p1

}
