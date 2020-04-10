#' Pass network ( WIP )
#'
#' Plots a marker for each player at their median passing position, and draws
#' connections between players to represent the number of passes exchanged
#' between them
#'
#' @param x
#' @param y
#' @param Zero
#' @param Zero
#' @param nXLimit
#' @param nYlimit
#' @examples
#' @import deldir
#' @import data.table
#' @export
fGetVoronoiFromVectors = function (
   x = NULL,
   y = NULL,
   nXLimit = NULL,
   nYlimit = NULL
) {

    Zero = 0
    Zero = 0

   # x = dtPlayerTag[, X]
   # y = dtPlayerTag[, Y]
   # Zero = 0
   # Zero = 0
   # nXLimit = 1
   # nYlimit = 1

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
         ( Zero + nYlimit ) / 2,
         ( nYlimit + nYlimit ),
         ( Zero + nYlimit ) / 2,
         Zero - nYlimit
      )

   }

   BufferedX = x
   BufferedY = y

   BufferedX = c(
      BufferedX,
      Zero - x
   )

   BufferedY = c(
      BufferedY,
      y
   )

   BufferedX = c(
      BufferedX,
      nXLimit + (nXLimit - x )
   )

   BufferedY = c(
      BufferedY,
      y
   )





   BufferedX = c(
      BufferedX,
      x
   )

   BufferedY = c(
      BufferedY,
      nYlimit + (nYlimit - y)
   )

   BufferedX = c(
      BufferedX,
      x
   )

   BufferedY = c(
      BufferedY,
      -y
   )




   # plot(x,y, xlim = c(0,1), ylim = c(0,1))

   tv = deldir(
      x = BufferedX, y = BufferedY,
    #   list(ndx=2,ndy=2),
      rw = c(
         Zero - nXLimit,
         nXLimit + nXLimit,
         Zero - nYlimit,
         nYlimit + nYlimit
      )
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
         c('x1','y1','x2','y2') := list(Zero, nYlimit, Zero, nYlimit)
      ]

      dtVoronoiCoordinates[
         ind1 == iNbrPoints + 4 |
         ind2 == iNbrPoints + 4,
         c('x1','y1','x2','y2') := list(nXLimit, nYlimit, nXLimit, nYlimit)
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
         y > nYlimit,
         y := nYlimit
      ]

   }

   dtVoronoiCoordinates

}
