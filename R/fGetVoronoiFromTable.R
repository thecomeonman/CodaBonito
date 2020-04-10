#' Pass network ( WIP )
#'
#' Plots a marker for each player at their median passing position, and draws
#' connections between players to represent the number of passes exchanged
#' between them
#'
#' @param dtTrackingSlice
#' @param xMinBB
#' @param yMinBB
#' @param xMaxBB
#' @param nYlimit
#' @examples
#' @import deldir
#' @import data.table
#' @export
fGetVoronoiFromTable = function(
    dtTrackingSlice,
    nXLimit = 120,
    nYlimit = 80
) {

    # vnXCoordinates = unlist(dtTrackingSlice[, grep(x = colnames(dtTrackingSlice), pattern = 'Player.*X$', value = T), with = F])
    # vnXCoordinates = vnXCoordinates[!is.na(vnXCoordinates)]
    # vnYCoordinates = unlist(dtTrackingSlice[, grep(x = colnames(dtTrackingSlice), pattern = 'Player.*Y$', value = T), with = F])
    # vnYCoordinates = vnYCoordinates[!is.na(vnYCoordinates)]
    # dtTrackingSlice = dtRandomSlice

    setDT(dtTrackingSlice)

    dtPlayerTag = data.table(
        Tag = dtTrackingSlice[, Tag],
        Player = dtTrackingSlice[, Player],
        X = dtTrackingSlice[, X],
        Y = dtTrackingSlice[, Y]
    )

    dtPlayerTag[, ind := .I]

#     nXLimit = 120
#     nYlimit = 1

    dtVoronoiCoordinates = fGetVoronoiFromVectors (
        x = dtPlayerTag[, X],
        y = dtPlayerTag[, Y],
        nXLimit = nXLimit,
        nYlimit = nYlimit
    )

    dtVoronoiCoordinates = merge(
        dtVoronoiCoordinates,
        dtPlayerTag[, list(ind, Tag, Player)],
        'ind'
    )

    dtVoronoiCoordinates[, ind := NULL]

    dtVoronoiCoordinates

}
