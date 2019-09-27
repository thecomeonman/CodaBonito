#' Draws a block based on the angle bucket and length bucket, respectively 
#' indicated by x and y
#' @import ggplot2
fCalculatePolygonCoordinate = function (
   xmin2,
   xmax2,
   ymin2,
   ymax2,
   fTrigFunction
) {
   
   c(
      seq(
         ymin2,
         ymax2,
         ( ymax2 - ymin2 ) / 50
      ) * fTrigFunction(xmin2),
      fTrigFunction(
         seq(
            xmin2,
            xmax2,
            ( xmax2 - xmin2 ) / 50
         )
      ) * ymax2,
      seq(
         ymax2,
         ymin2,
         ( ymin2 - ymax2 ) / 50
      ) * fTrigFunction(xmax2),
      fTrigFunction(
         seq(
            xmax2,
            xmin2,
            ( xmin2 - xmax2 ) / 50
         )
      ) * ymin2
   )
   
}
