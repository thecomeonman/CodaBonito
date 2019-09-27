#' @import ggplot2
#' @import data.table
#' @export
fPassNetworkChart = function(
   dtPasses
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

   ggplot() +
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
         curvature = 0.1
      ) +
      geom_point(
         data = dtNodes,
         aes(
            x = x,
            y = y,
            size = Count
         )
      ) +
      scale_color_continuous(
         low = 'white',
         high = 'black'
      ) +
      coord_fixed()

}
