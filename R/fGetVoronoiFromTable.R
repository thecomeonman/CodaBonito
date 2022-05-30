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








    Zero = -nXLimit/2
    # Zero = -nXLimit

    x = dtPlayerTag[, X]
    y = dtPlayerTag[, Y]
    # Zero = 0
    # Zero = 0
    # nXLimit = 1
    # nYLimit = 1

    iNbrPoints = length(x)


    bPointsOnPitch =
        x >= -nXLimit/2 & x <= nXLimit/2 &
        y >= -nYLimit/2 & y <= nYLimit/2

    BufferedX = x
    BufferedY = y

    BufferedX = c(
        BufferedX,
        (-nXLimit/2) - (x[bPointsOnPitch] - (-nXLimit/2) )
    )

    BufferedY = c(
        BufferedY,
        y[bPointsOnPitch]
    )

    BufferedX = c(
        BufferedX,
        (nXLimit/2) + ((nXLimit/2) - x[bPointsOnPitch] )
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
        (nYLimit/2) + ((nYLimit/2) - y[bPointsOnPitch])
    )

    BufferedX = c(
        BufferedX,
        x[bPointsOnPitch]
    )

    BufferedY = c(
        BufferedY,
        (-nYLimit/2) - (y[bPointsOnPitch] - (-nYLimit/2))
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

    # we lose out on information that the same point will be on two edges
    # when we do this. Also removing voronoin edge id since evey edge will
    # just be a unique combination of two points anyway
    dtVoronoiCoordinates = dtVoronoiCoordinates[,
        .SD[1],
        list(
            ind,
            VoronoiPointId
        )
    ]

    dtVoronoiCoordinates[, VoronoiEdgeId := NULL ]

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

    dtVoronoiCoordinates = merge(
        dtVoronoiCoordinates,
        dtPlayerTag[, list(ind, Tag, Player)],
        'ind'
    )

    dtVoronoiCoordinates[, ind := NULL]

    dtVoronoiCoordinates

}
