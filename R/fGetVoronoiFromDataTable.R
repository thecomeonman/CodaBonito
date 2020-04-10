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
#' @param yMaxBB
#' @examples
#' @import deldir
#' @import data.table
#' @export
fGetVoronoiFromDataTable = function(
   dtTrackingSlice,
   xMinBB = 0,
   yMinBB = 0,
   xMaxBB = 1,
   yMaxBB = 1
) {

   # vnXCoordinates = unlist(dtTrackingSlice[, grep(x = colnames(dtTrackingSlice), pattern = 'Player.*X$', value = T), with = F])
   # vnXCoordinates = vnXCoordinates[!is.na(vnXCoordinates)]
   # vnYCoordinates = unlist(dtTrackingSlice[, grep(x = colnames(dtTrackingSlice), pattern = 'Player.*Y$', value = T), with = F])
   # vnYCoordinates = vnYCoordinates[!is.na(vnYCoordinates)]
   # dtTrackingSlice = dtRandomSlice

   dtPlayerTag = data.table(
      Tag = dtTrackingSlice[, Tag],
      Player = dtTrackingSlice[, Player],
      X = dtTrackingSlice[, X],
      Y = dtTrackingSlice[, Y]
   )

   dtPlayerTag[, ind := .I]

#    xMinBB = 0
#    yMinBB = 0
#    xMaxBB = 1
#    yMaxBB = 1

   dtVoronoiCoordinates = fGetVoronoiFromVectors (
      x = dtPlayerTag[, X],
      y = dtPlayerTag[, Y],
      xMinBB = xMinBB,
      yMinBB = yMinBB,
      xMaxBB = xMaxBB,
      yMaxBB = yMaxBB
   )

   dtVoronoiCoordinates = merge(
      dtVoronoiCoordinates,
      dtPlayerTag[, list(ind, Tag, Player)],
      'ind'
   )

   dtVoronoiCoordinates[, ind := NULL]

   dtVoronoiCoordinates

}
