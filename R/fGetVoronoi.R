#' Pass network ( WIP )
#'
#' Plots a marker for each player at their median passing position, and draws
#' connections between players to represent the number of passes exchanged
#' between them
#'
#' @param x
#' @param y
#' @param xMinBB
#' @param yMinBB
#' @param xMaxBB
#' @param yMaxBB
#' @examples
#' @import deldir
#' @import data.table
#' @export
fGetVoronoiFromVectors = function (
   x = NULL,
   y = NULL,
   xMinBB = NULL,
   yMinBB = NULL,
   xMaxBB = NULL,
   yMaxBB = NULL
) {

   # x = dtPlayerTag[, X]
   # y = dtPlayerTag[, Y]
   # xMinBB = 0
   # yMinBB = 0
   # xMaxBB = 1
   # yMaxBB = 1

   iNbrPoints = length(x)

   if ( F ) {

      x = c(
         x,
         xMinBB - xMaxBB,
         ( xMinBB + xMaxBB ) / 2,
         ( xMaxBB + xMaxBB ),
         ( xMinBB + xMaxBB ) / 2
      )


      y = c(
         y,
         ( yMinBB + yMaxBB ) / 2,
         ( yMaxBB + yMaxBB ),
         ( yMinBB + yMaxBB ) / 2,
         yMinBB - yMaxBB
      )

   }

   BufferedX = x
   BufferedY = y

   BufferedX = c(
      BufferedX,
      xMinBB - x
   )

   BufferedY = c(
      BufferedY,
      y
   )

   BufferedX = c(
      BufferedX,
      xMaxBB + (xMaxBB - x )
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
      yMaxBB + (yMaxBB - y)
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
      BufferedX, BufferedY,
      list(ndx=2,ndy=2),
      c(
         xMinBB - xMaxBB,
         xMaxBB + xMaxBB,
         yMinBB - yMaxBB,
         yMaxBB + yMaxBB
      )
   )

   dtVoronoiCoordinates = tv$dirsgs

   setDT(dtVoronoiCoordinates)

   dtVoronoiCoordinates[, length(unique(c(ind1, ind2)))]

   if ( F ) {

      dtVoronoiCoordinates[
         ind1 == iNbrPoints + 1 |
         ind2 == iNbrPoints + 1,
         c('x1','y1','x2','y2') := list(xMinBB, yMinBB, xMinBB, yMinBB)
      ]

      dtVoronoiCoordinates[
         ind1 == iNbrPoints + 2 |
         ind2 == iNbrPoints + 2,
         c('x1','y1','x2','y2') := list(xMaxBB, yMinBB, xMaxBB, yMinBB)
      ]

      dtVoronoiCoordinates[
         ind1 == iNbrPoints + 3 |
         ind2 == iNbrPoints + 3,
         c('x1','y1','x2','y2') := list(xMinBB, yMaxBB, xMinBB, yMaxBB)
      ]

      dtVoronoiCoordinates[
         ind1 == iNbrPoints + 4 |
         ind2 == iNbrPoints + 4,
         c('x1','y1','x2','y2') := list(xMaxBB, yMaxBB, xMaxBB, yMaxBB)
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
         x < xMinBB,
         x := xMinBB
      ]

      dtVoronoiCoordinates[
         x > xMaxBB,
         x := xMaxBB
      ]

      dtVoronoiCoordinates[
         y < yMinBB,
         y := yMinBB
      ]

      dtVoronoiCoordinates[
         y > yMaxBB,
         y := yMaxBB
      ]

   }

   dtVoronoiCoordinates

}
