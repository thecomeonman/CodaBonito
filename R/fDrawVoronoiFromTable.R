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

fDrawVoronoiFromTable = function(
    dtTrackingSlice,
    nXLimit = 120,
    nYlimit = 80
) {

    setDT(dtTrackingSlice)

    dtTrackingSlice = fConvertTrackingDataWideToLong(
       dtTrackingSlice
    )

    dtVoronoiCoordinates = fGetVoronoiFromTable(
       dtTrackingSlice[Player != 'Ball'],
        nXLimit = 120,
        nYlimit = 80
    )

    plotVoronoi = ggplot()
    plotVoronoi = fAddPitchLines(plotVoronoi)
    plotVoronoi = plotVoronoi +
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

    plotVoronoi = plotVoronoi +
        scale_color_manual(
            values = c('Home' = 'blue','Ball' = 'black','Away' = 'red')
        ) +
        scale_fill_manual(
            values = c('Home' = 'blue','Ball' = 'black','Away' = 'red'),
            guide = FALSE
        ) +
        coord_fixed() + 
        theme_pitch()

    plotVoronoi

}
