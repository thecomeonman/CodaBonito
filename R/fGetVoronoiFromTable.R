#' Pass network ( WIP )
#'
#' Plots a marker for each player at their median passing position, and draws
#' connections between players to represent the number of passes exchanged
#' between them
#'
#' @param dtFrame
#' @param xMinBB
#' @param yMinBB
#' @param xMaxBB
#' @param nYLimit
#' @examples
#' @import deldir
#' @import data.table
#' @export
fGetVoronoiFromTable = function(
    dtFrame,
    nXLimit = 120,
    nYLimit = 80
) {

    # vnXCoordinates = unlist(dtFrame[, grep(x = colnames(dtFrame), pattern = 'Player.*X$', value = T), with = F])
    # vnXCoordinates = vnXCoordinates[!is.na(vnXCoordinates)]
    # vnYCoordinates = unlist(dtFrame[, grep(x = colnames(dtFrame), pattern = 'Player.*Y$', value = T), with = F])
    # vnYCoordinates = vnYCoordinates[!is.na(vnYCoordinates)]
    # dtFrame = dtRandomSlice
    
    setDT(dtFrame)

    dtPlayerTag = data.table(
        Tag = dtFrame[, Tag],
        Player = dtFrame[, Player],
        X = dtFrame[, X],
        Y = dtFrame[, Y]
    )

    dtPlayerTag[, ind := .I]

#     nXLimit = 120
#     nYLimit = 1








    Zero = 0
    Zero = 0

    x = dtPlayerTag[, X]
    y = dtPlayerTag[, Y]
    # Zero = 0
    # Zero = 0
    # nXLimit = 1
    # nYLimit = 1

    iNbrPoints = length(x)

    if ( F ) {

        x = c(
            x,
            Zero - nXLimit,
            ( Zero + nXLimit ) / 2,
            ( nXLimit + nXLimit ),
            ( Zero + nXLimit ) / 2
        )


        y = c(
            y,
            ( Zero + nYLimit ) / 2,
            ( nYLimit + nYLimit ),
            ( Zero + nYLimit ) / 2,
            Zero - nYLimit
        )

    }

    bPointsOnPitch = 
        x >= Zero & x <= nXLimit &
        y >= Zero & y <= nYLimit

    BufferedX = x
    BufferedY = y

    BufferedX = c(
        BufferedX,
        Zero - x[bPointsOnPitch]
    )

    BufferedY = c(
        BufferedY,
        y[bPointsOnPitch]
    )

    BufferedX = c(
        BufferedX,
        nXLimit + (nXLimit - x[bPointsOnPitch] )
    )

    BufferedY = c(
        BufferedY,
        y[bPointsOnPitch]
    )





    BufferedX = c(
        BufferedX,
        x[bPointsOnPitch]
    )

    BufferedY = c(
        BufferedY,
        nYLimit + (nYLimit - y[bPointsOnPitch])
    )

    BufferedX = c(
        BufferedX,
        x[bPointsOnPitch]
    )

    BufferedY = c(
        BufferedY,
        -y[bPointsOnPitch]
    )




    # plot(x,y, xlim = c(0,1), ylim = c(0,1))

    tv = deldir(
        x = BufferedX, y = BufferedY,
     #    list(ndx=2,ndy=2),
        rw = c(
            Zero - nXLimit - nXLimit,
            nXLimit + nXLimit + nXLimit,
            Zero - nYLimit - nYLimit,
            nYLimit + nYLimit + nYLimit
        ),
        suppressMsge = T
    )

    dtVoronoiCoordinates = tv$dirsgs

    setDT(dtVoronoiCoordinates)

    dtVoronoiCoordinates[, length(unique(c(ind1, ind2)))]

    if ( F ) {

        dtVoronoiCoordinates[
            ind1 == iNbrPoints + 1 |
            ind2 == iNbrPoints + 1,
            c('x1','y1','x2','y2') := list(Zero, Zero, Zero, Zero)
        ]

        dtVoronoiCoordinates[
            ind1 == iNbrPoints + 2 |
            ind2 == iNbrPoints + 2,
            c('x1','y1','x2','y2') := list(nXLimit, Zero, nXLimit, Zero)
        ]

        dtVoronoiCoordinates[
            ind1 == iNbrPoints + 3 |
            ind2 == iNbrPoints + 3,
            c('x1','y1','x2','y2') := list(Zero, nYLimit, Zero, nYLimit)
        ]

        dtVoronoiCoordinates[
            ind1 == iNbrPoints + 4 |
            ind2 == iNbrPoints + 4,
            c('x1','y1','x2','y2') := list(nXLimit, nYLimit, nXLimit, nYLimit)
        ]

    }

    dtVoronoiCoordinates[, VoronoiEdgeId := .I]

    dtVoronoiCoordinates = rbind(
        dtVoronoiCoordinates[, list(VoronoiEdgeId, x1, y1, x2, y2, ind = ind1)],
        dtVoronoiCoordinates[, list(VoronoiEdgeId, x1, y1, x2, y2, ind = ind2)]
    )

    dtVoronoiCoordinates = rbind(
        dtVoronoiCoordinates[, list(VoronoiEdgeId, x = x1, y = y1, ind)],
        dtVoronoiCoordinates[, list(VoronoiEdgeId, x = x2, y = y2, ind)]
    )

    dtVoronoiCoordinates[, VoronoiPointId := .GRP, list(x, y)]

    dtVoronoiCoordinates = dtVoronoiCoordinates[,
        .SD[
            chull(x,y)
        ],
        ind
    ]

    dtVoronoiCoordinates[,
        PolygonOrder := .I - min(.I) + 1,
        ind
    ]

    dtVoronoiCoordinates = dtVoronoiCoordinates[
        ind <= iNbrPoints
    ]

    if ( F ) {

        dtVoronoiCoordinates[
            x < Zero,
            x := Zero
        ]

        dtVoronoiCoordinates[
            x > nXLimit,
            x := nXLimit
        ]

        dtVoronoiCoordinates[
            y < Zero,
            y := Zero
        ]

        dtVoronoiCoordinates[
            y > nYLimit,
            y := nYLimit
        ]

    }



    dtVoronoiCoordinates = merge(
        dtVoronoiCoordinates,
        dtPlayerTag[, list(ind, Tag, Player)],
        'ind'
    )

    dtVoronoiCoordinates[, ind := NULL]

    dtVoronoiCoordinates

}
