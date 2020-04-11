#' Draws a Voronoi diagram for a time slice
#'
#' @param dtTrackingSlice
#' @param xMinBB
#' @param yMinBB
#' @param xMaxBB
#' @param yMaxBB
#' @examples
#' @import magick
#' @import ggplot2
#' @import data.table
#' @export

fDrawVoronoiFromTable = function(
    dtTrackingSlice,
    nXLimit = 120,
    nYlimit = 80,
    UseOneFrameEvery = 1
) {

    setDT(dtTrackingSlice)

    if ( nrow(dtTrackingSlice) > UseOneFrameEvery ) {

        dtTrackingSlice = dtTrackingSlice[
            Frame %% UseOneFrameEvery == min(Frame) %% UseOneFrameEvery
        ]

    }

    dtTrackingSlice = fConvertTrackingDataWideToLong(
       dtTrackingSlice
    )

    dtVoronoiCoordinates = rbindlist(
        lapply(
            dtTrackingSlice[, unique(Frame)],
            function( iFrame ) {

                dtFrame = dtTrackingSlice[
                    Frame == iFrame
                ][
                    Player != 'Ball'
                ]

                dtVoronoiCoordinates = fGetVoronoiFromTable(
                    dtFrame,
                    nXLimit = 120,
                    nYlimit = 80
                )

                dtVoronoiCoordinates[, Frame := iFrame]

                dtVoronoiCoordinates

            }
        )
    )

    if ( F ) {

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

        if ( dtTrackingSlice[, length(unique(Frame)) > 1] ) {
            
            plotVoronoi = plotVoronoi +
                transition_time(Frame) +
                ease_aes('linear')

        }

        plotVoronoi

    }

    if ( dtTrackingSlice[, length(unique(Frame)) > 1] ) {

        cTempdir = paste0(
            tempdir(),
            # '~/Desktop/',
            '/Voronois/'
        )

        dir.create(
            cTempdir,
            recursive = T,
            showWarnings = FALSE
        )

        for ( iFrame in dtTrackingSlice[, unique(Frame)] ) {

            plotVoronoi = fDrawVoronoiFromVoronoiCoordinates(
                dtVoronoiCoordinates[Frame == iFrame],
                dtTrackingSlice[Frame == iFrame],
                nXLimit,
                nYLimit
            )

            ggsave(
                plotVoronoi,
                filename = paste0(
                    cTempdir,
                    '/',
                    iFrame,
                    '.png'
                )
            )

            rm(plotVoronoi)

        }

        cGIFFile = paste0(
            tempfile(),
            '.gif'
        )

        print(
            paste0(
                'Multiple frames given. Saving a GIF at - ',
                cGIFFile,
                ' and returning the file path instead of the plot itself'
            )
        )

        system(
            paste0(
                'convert -delay 5 ',
                paste0(
                    paste0(
                        cTempdir,'/',
                        dtTrackingSlice[, unique(Frame)], 
                        '.png'
                    ), 
                    collapse = ' '
                ),
                    ' ',
                cGIFFile
            )
        )

        plotVoronoi = cGIFFile

        # list.files(
        #     path = cTempdir, 
        #     pattern = '*.png', 
        #     full.names = TRUE
        # ) %>% 
        #     image_read() %>% # reads each path file
        #     image_join() %>% # joins image
        #     image_animate(fps=4) %>% # animates, can opt for number of loops
        #     image_write("FileName.gif") # write to current dir

    } else {


        plotVoronoi = fDrawVoronoiFromVoronoiCoordinates(
            dtVoronoiCoordinates,
            dtTrackingSlice,
            nXLimit,
            nYLimit
        )


    }

    plotVoronoi    

}
