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
    nYLimit
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
        geom_pitch(
            nXStart = -nXLimit/2,
            nYStart = -nYLimit/2,
            nXEnd = nXLimit/2,
            nYEnd = nYLimit/2,
            cLineColour = 'white',
            cPitchColour = NA
        ) +
        theme_pitch()

    if ( F ) {

        plotVoronoi = plotVoronoi +
            scale_color_manual(
                values = c('Home' = '#1f78b4','Ball' = 'black','Away' = '#33a02c'),
                guide = FALSE
            ) +
            scale_fill_manual(
                values = c('Home' = '#1f78b4','Ball' = 'black','Away' = '#33a02c'),
                guide = FALSE
            )

    }

    cf = coord_fixed(
        xlim = c(-nXLimit/2,nXLimit/2),
        ylim = c(-nYLimit/2,nYLimit/2)
    )
    cf$default = T

    plotVoronoi = plotVoronoi +
        cf

    plotVoronoi

}
