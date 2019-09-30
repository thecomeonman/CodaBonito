#' Draws a block based on the angle bucket and length bucket, respectively 
#' indicated by x and y, used by fPlotSonar
#'
#' Hard to describe it. Play with the example
#'
#' @examples
#' plot(
#'    fCalculatePolygonCoordinate(
#'       nAngleMin_rad = pi/4,
#'       nAngleMax_rad = pi/2,
#'       nRadiusMin = 1,
#'       nRadiusMax = 2,
#'       cos   
#'    ),
#'    fCalculatePolygonCoordinate(
#'       nAngleMin_rad = pi/4,
#'       nAngleMax_rad = pi/2,
#'       nRadiusMin = 1,
#'       nRadiusMax = 2,
#'       sin   
#'    )      
#' )
#' @import ggplot2
fCalculatePolygonCoordinate = function (
   nAngleMin_rad,
   nAngleMax_rad,
   nRadiusMin,
   nRadiusMax,
   fTrigFunction
) {
   
   c(
      # radial path from in to out along nAngleMin_rad 
      seq(
         nRadiusMin,
         nRadiusMax,
         ( nRadiusMax - nRadiusMin ) / 50
      ) * fTrigFunction(nAngleMin_rad),

      # arc at nRadiusMax from angle min to max
      fTrigFunction(
         seq(
            nAngleMin_rad,
            nAngleMax_rad,
            pi/50
         )
      ) * nRadiusMax,

      # radial path from out to in along nAngleMax_rad 
      seq(
         nRadiusMax,
         nRadiusMin,
         ( nRadiusMin - nRadiusMax ) / 50
      ) * fTrigFunction(nAngleMax_rad),

      # arc at nRadiusMin from angle max to min
      fTrigFunction(
         seq(
            nAngleMax_rad,
            nAngleMin_rad,
            -pi/50
         )
      ) * nRadiusMin

   )
   
}
