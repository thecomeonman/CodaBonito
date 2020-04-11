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
#' @param nYlimit
#' @examples
#' @import deldir
#' @import data.table
#' @export
fGetVoronoiFromTable = function(
    dtFrame,
    nXLimit = 120,
    nYlimit = 80
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
#     nYlimit = 1

    dtVoronoiCoordinates = fGetVoronoiFromVectors (
        x = dtPlayerTag[, X],
        y = dtPlayerTag[, Y],
        nXLimit = nXLimit,
        nYLimit = nYLimit
    )

    dtVoronoiCoordinates = merge(
        dtVoronoiCoordinates,
        dtPlayerTag[, list(ind, Tag, Player)],
        'ind'
    )

    dtVoronoiCoordinates[, ind := NULL]

    dtVoronoiCoordinates

}
