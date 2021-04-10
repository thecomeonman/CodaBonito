#' Pitch control support function
#'
#' @param lPitchControl
#' @param cFolderPathToSaveImagesSubdir
#' @examples
#' @import data.table
#' @import ggplot2
#' @export
fPlotPitchControl = function(
    lPitchControl,
    cFolderPathToSaveImagesSubdir = NULL,
    DelayBetweenFrames = 10,
    nXLimit = 120,
    nYLimit = 80,
    vcColourAssignment = c('Home' = 'red', 'Away' = 'blue', 'Ball' = 'black')
) {

    viTrackingFrame = lPitchControl$dtDetails[, unique(Frame)]

    if ( length(viTrackingFrame) > 0 ) {

        if ( is.null(cFolderPathToSaveImagesSubdir) ) {

            cFolderPathToSaveImagesSubdir = paste0(
                tempdir(), '/',
                min(viTrackingFrame), '_',
                max(viTrackingFrame), '/'
            )

        }

        cFolderPathToSaveImagesSubdir = paste0(
            cFolderPathToSaveImagesSubdir, '/'
        )

        dir.create(
            cFolderPathToSaveImagesSubdir,
            recursive = T,
            showWarnings = F
        )

    }

    for ( iTrackingFrame in viTrackingFrame ) {

        plotPitchControl = ggplot() +
            geom_tile(
                data = lPitchControl$dtDetails[
                    Frame == iTrackingFrame
                ],
                aes(
                    x = TargetX,
                    y = TargetY,
                    fill =  AttackProbability
                )
            ) +
            geom_point(
                data = lPitchControl$dtTrackingSlice[
                    Frame == iTrackingFrame
                ][
                    Player != 'Ball'
                ],
                aes(x = X, y = Y),
                color = 'white',
                size = 7
            ) +
            geom_point(
                data = lPitchControl$dtTrackingSlice[
                    Frame == iTrackingFrame
                ][
                    Player != 'Ball'
                ],
                aes(x = X, y = Y, color = Tag),
                size = 6
            ) +
            geom_point(
                data = lPitchControl$dtTrackingSlice[
                    Frame == iTrackingFrame
                ][
                    Player == 'Ball'
                ],
                aes(x = X, y = Y, color = Tag),
                size = 4
            ) +
            geom_segment(
                data = lPitchControl$dtTrackingSlice[
                    Frame == iTrackingFrame
                ][
                    Player != 'Ball'
                ],
                aes(
                    x = X, y = Y,
                    xend = X + VelocityX,
                    yend = Y + VelocityY,
                    color = Tag
                )
            ) +
            # geom_text(aes(x = X, y = Y, label = Player)) +
            # geom_segment(
            #    data = lData$dtEventsData[StartFrame == iTrackingFrame],
            #    aes(
            #       x = EventStartX,
            #       y = EventStartY,
            #       xend = EventEndX,
            #       yend = EventEndY
            #    )
            # ) +
            scale_fill_gradient2(
                low = vcColourAssignment[
                    lPitchControl$dtDetails[Frame == iTrackingFrame, setdiff(c('Home','Away'), AttackingTeam[1])]
                ],
                mid = 'white',
                high = vcColourAssignment[
                    lPitchControl$dtDetails[Frame == iTrackingFrame, AttackingTeam[1]]
                ],
                midpoint = 0.5,
                guide = FALSE
            ) +
            scale_color_manual(
                values = vcColourAssignment,
                guide = FALSE
            )

        plotPitchControl = plotPitchControl +
            geom_pitch(
                nXStart = 0,
                nYStart = 0,
                nXEnd = nXLimit,
                nYEnd = nYLimit,
                cLineColour = 'black',
                cPitchColour = NA
            )

        plotPitchControl = plotPitchControl +
            theme_pitch()

        if ( length(viTrackingFrame) > 1 ) {

            ggsave(
                plotPitchControl,
                file = paste0(
                    cFolderPathToSaveImagesSubdir,
                    iTrackingFrame,
                    '.png'
                ),
                width = 20,
                height = 14,
                units = 'cm'
            )

        }

    }

    if ( length(viTrackingFrame) > 1 ) {

        plotPitchControl = cFolderPathToSaveImagesSubdir

        cGIFFile = paste0(
            cFolderPathToSaveImagesSubdir,
            '/PitchControl.gif'
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
                        cFolderPathToSaveImagesSubdir,'/',
                        sort(viTrackingFrame),
                        '.png'
                    ),
                    collapse = ' '
                ),
                    ' ',
                cGIFFile
            )
        )
        # convert image1.jpg image2.jpg +append output.jpg

        plotPitchControl = cGIFFile


    }

    plotPitchControl

}
