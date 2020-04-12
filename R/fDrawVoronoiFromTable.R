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
    UseOneFrameEvery = 1,
    DelayBetweenFrames = 5,
    suppressWarnings = F
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

            suppressMessages(
                ggsave(
                    plotVoronoi,
                    filename = paste0(
                        cTempdir,
                        '/',
                        iFrame,
                        '.png'
                    )
                )
            )

            rm(plotVoronoi)

        }

        cGIFFile = paste0(
            tempfile(),
            '.gif'
        )

        if ( !suppressWarnings ) { 

            warning(
                paste0(
                    'Multiple frames given. Saving a GIF at - ',
                    cGIFFile,
                    ' and returning the file path instead of the plot itself'
                )
            )

        }

        system(
            paste0(
                'convert -delay ',
                DelayBetweenFrames, ' ',
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
