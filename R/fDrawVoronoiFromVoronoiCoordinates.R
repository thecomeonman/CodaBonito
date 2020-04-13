#' Draws the Voronoi plot from the Voronoi coordinates
#'
#' @param dtVoronoiCoordinates
#' @examples
#' @import deldir
#' @import ggplot2
#' @import data.table
#' @export
fDrawVoronoiFromVoronoiCoordinates = function(
    dtVoronoiCoordinates,
    dtTrackingSlice,
    nXLimit,
    nYLimit,
    markFutureTrajectoryFor,
    dtTrackingSliceFuture
) {

    plotVoronoi = ggplot()

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
            alpha = 0.6,
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

    if ( !is.null(markFutureTrajectoryFor) ) {

        plotVoronoi = plotVoronoi +
            geom_path(
                data = dtTrackingSliceFuture[
                    Player %in% markFutureTrajectoryFor
                ][
                    order(Frame)
                ],
                aes(
                    x = X,
                    y = Y,
                    group = Player,
                    color = Tag
                )
            )

    }


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

    plotVoronoi = fAddPitchLines(
        plotVoronoi,
        nXLimit,
        nYLimit,
        cLineColour = 'white',
        cPitchColour = NA
    )

    plotVoronoi = plotVoronoi +
        scale_color_manual(
            values = c('Home' = '#1f78b4','Ball' = 'black','Away' = '#33a02c'),
            guide = FALSE
        ) +
        scale_fill_manual(
            values = c('Home' = '#1f78b4','Ball' = 'black','Away' = '#33a02c'),
            guide = FALSE
        ) + 
        theme_pitch()
    
    plotVoronoi = plotVoronoi +
        coord_fixed(
            xlim = c(0,nXLimit),
            ylim = c(0,nYLimit)
        )

    plotVoronoi

}