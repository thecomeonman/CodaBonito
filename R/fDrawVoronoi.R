#' Draws a Voronoi diagram for a time slice
#'
#' @param dtTrackingSlice
#' @param xMinBB
#' @param yMinBB
#' @param xMaxBB
#' @param yMaxBB
#' @examples
#' @import ggplot2
#' @import data.table
#' @export

fDrawVoronoi = function(
   dtTrackingSlice,
   xMinBB = 0,
   yMinBB = 0,
   xMaxBB = 1,
   yMaxBB = 1
) {

   dtTrackingSlice = fConvertTrackingDataWideToLong(
      dtTrackingSlice
   )

   dtVoronoiCoordinates = fGetVoronoiFromDataTable(
      dtTrackingSlice[Player != 'Ball'],
      # dtTrackingSlice[Player != 'Ball'][c(c(1:5),15:20)],
      xMinBB = 0,
      yMinBB = 0,
      xMaxBB = 1,
      yMaxBB = 1
   )

   plotVoronoi = ggplot() +
      geom_polygon(
         data = dtVoronoiCoordinates,
         aes(
            x = x,
            y = y,
            group = Player,
            # group = factor(ind),
            fill = Tag
            # fill = factor(Player)
            # fill = factor(ind)
         ),
         alpha = 0.2,
         color = 'black'
      ) +
      geom_point(
         data = dtTrackingSlice,
         aes(
            x = X,
            y = Y,
            color = Tag
         ),
         size = 3
      )

   if ( dtTrackingSlice[, any(Player == 'Ball')] ) {

      plotVoronoi = plotVoronoi +
         geom_point(
            data = dtTrackingSlice[Player == 'Ball'],
            aes(
               x = X,
               y = Y,
               color = 'Ball',
               fill = 'Ball'
            ),
            size = 3
         )

   }

   plotVoronoi = plotVoronoi+
      scale_color_manual(
         labels = c('Home','Ball','Away'),
         values = c('green','black','red')
      ) +
      scale_fill_manual(
         labels = c('Home','Ball','Away'),
         values = c('green','black','red'),
         guide = FALSE
      ) +
      coord_fixed()

   plotVoronoi

}
