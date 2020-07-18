#' Draws a Voronoi diagram for a time slice
#'
#' @param dtTrackingSlice
#' @examples
#' @import magick
#' @import ggplot2
#' @import data.table
#' @export


fDrawVoronoiFromTable = function(
    dtTrackingSlice,
    nXLimit,
    nYLimit,
    UseOneFrameEvery = 1,
    DelayBetweenFrames = 5,
    markTrajectoryFor = NULL,
    markOffsideLineFor = NULL,
    markLineBetween = NULL
) {

    setDT(dtTrackingSlice)

    if ( nrow(dtTrackingSlice) > UseOneFrameEvery ) {

        dtTrackingSlice = dtTrackingSlice[
            Frame %% UseOneFrameEvery == min(Frame) %% UseOneFrameEvery
        ]

    }



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
                    nYLimit = 80
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

    }

    for ( iFrame in dtTrackingSlice[, unique(Frame)] ) {

        plotVoronoi = fDrawVoronoiFromVoronoiCoordinates(
            dtVoronoiCoordinates = dtVoronoiCoordinates[Frame == iFrame],
            dtTrackingSlice = dtTrackingSlice[Frame == iFrame],
            nXLimit = nXLimit,
            nYLimit = nYLimit
        )


        if ( !is.null(markLineBetween) ) {

            for ( i in seq(length((markLineBetween))) ) {

                dtFormationLine = dtTrackingSlice[
                    Frame == iFrame
                ][
                    Player %in% markLineBetween[[i]]
                ][
                    order(Y)
                ]

                dtFormationLine[,
                    Tag := Tag[
                        Player == markLineBetween[[i]][1]
                    ]
                ]

                plotVoronoi = plotVoronoi +
                    geom_path(
                        data = dtFormationLine,
                        aes(
                            x = X,
                            y = Y,
                            color = Tag
                        )
                    )

            }

        }

        if ( !is.null(markOffsideLineFor) ) {

            for ( i in seq(length((markOffsideLineFor))) ) {

                plotVoronoi = plotVoronoi +
                    geom_vline(
                        aes(
                            xintercept = dtTrackingSlice[
                                Frame == iFrame
                            ][
                                Tag == markOffsideLineFor[[i]][1]
                            ][
                                Player != markOffsideLineFor[[i]][2]
                            ][,
                                markOffsideLineFor[[i]][[3]](X)
                            ],
                            color = unlist(markOffsideLineFor[[i]][1])
                        )
                    )

            }

        }

        if ( !is.null(markTrajectoryFor) ) {

            plotVoronoi = plotVoronoi +
                geom_path(
                    data = dtTrackingSlice[
                        Player %in% markTrajectoryFor
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


        plotVoronoi = plotVoronoi +
            geom_pitch(
                nXStart = 0,
                nYStart = 0,
                nXEnd = nXLimit,
                nYEnd = nYLimit,
                cLineColour = 'white',
                cPitchColour = NA
            )

        cf = coord_fixed(
            xlim = c(0, nXLimit),
            ylim = c(0, nYLimit)
        )
        cf$default = T

        plotVoronoi = plotVoronoi +
            cf

        if ( dtTrackingSlice[, length(unique(Frame)) > 1] ) {

            suppressMessages(
                ggsave(
                    plotVoronoi,
                    filename = paste0(
                        cTempdir,
                        '/',
                        iFrame,
                        '.png'
                    ),
                    width = 20,
                    height = 15,
                    units = 'cm'
                )
            )

            rm(plotVoronoi)

        }

    }


    if ( dtTrackingSlice[, length(unique(Frame)) > 1] ) {

        cGIFFile = paste0(
            tempfile(),
            '.gif'
        )

        cCommand = 'convert'
        if ( .Platform$OS.type == 'windows' ) {
            cCommand = 'magick'
        }

        system(
            paste0(
                cCommand,
                ' -delay ',
                DelayBetweenFrames, ' ',
                paste0(
                    paste0(
                        cTempdir,'/',
                        dtTrackingSlice[, sort(unique(Frame))],
                        '.png'
                    ),
                    collapse = ' '
                ),
                    ' ',
                cGIFFile
            )
        )
        # convert image1.jpg image2.jpg +append output.jpg

        plotVoronoi = cGIFFile

    }

    plotVoronoi

}
