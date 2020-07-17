#' Pitch background
#'
#' Adds pitch markings, green background, etc. for plot background to look like
#' a football pitch. ggplot adds things in order so initiate a ggplot object,
#' add the pitch marking with this function, and then add the data you want to
#' add
#'
#' @param plotObject a ggplot object. Can just be a blank ggplot object too
#' @param cLineColour the colour of the line markings on the pitch
#' @param cPitchColour the colour of the grass on the pitch
#' @param nXStart Start x coordinate of the pitch
#' @param nYStart Start y coordinate of the pitch
#' @param nXEnd End x coordinate of the pitch
#' @param nYEnd Start y coordinate of the pitch
#' @examples
#' ggplot() + geom_pitch()
#' @import data.table
#' @import ggplot2
#' @export
geom_pitch = function (
   nXStart = 0,
   nYStart = 0,
   nXEnd = 120,
   nYEnd = 80,
   cLineColour = 'white',
   cPitchColour = '#038253',
   bMarkDirectionOfAttack = T
) {

   nXStart = 0
   nYStart = 0
   nXEnd = 120
   nYEnd = 80


   lPitchDimensions = fGetPitchDimensions (
      nXStart = nXStart,
      nYStart = nYStart,
      nXEnd = nXEnd,
      nYEnd = nYEnd
   )

   # https://github.com/tidyverse/ggplot2/issues/2799
   cf = coord_fixed()
   cf$default = TRUE


   lPitchElements = list(

      # background
      # geom_rect(
      #    aes(
      #       xmin = nXStart + 0 - (5*lPitchDimensions$lParameters$nXSpan/120),
      #       ymin = nYStart + 0 - (5*lPitchDimensions$lParameters$nXSpan/120),
      #       xmax = nXStart + lPitchDimensions$lParameters$nXSpan + (5*lPitchDimensions$lParameters$nXSpan/120),
      #       ymax = nYStart + lPitchDimensions$lParameters$nYSpan + (5*lPitchDimensions$lParameters$nXSpan/120)
      #    ),
      #    fill = cPitchColour,
      #    size = 0
      # ),

      # pitch
      geom_polygon(
         data = lPitchDimensions$lPitchCoordinates$dtPitch,
         aes(
            x = x,
            y = y
         ),
         fill = cPitchColour,
         color = cLineColour
      ),

      # D defense
      geom_polygon(
         data = lPitchDimensions$lPitchCoordinates$dtDDefense,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour,
         fill = cPitchColour
      ),

      # D offense
      geom_polygon(
         data = lPitchDimensions$lPitchCoordinates$dtDOffense,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour,
         fill = cPitchColour
      ),

      # penalty box defense
      geom_polygon(
         data = lPitchDimensions$lPitchCoordinates$dtPenaltyBoxDefense,
         aes(
            x = x,
            y = y
         ),
         # alpha = 0,
         color = cLineColour,
         fill = cPitchColour
      ),

      # penalty box attack
      geom_polygon(
         data = lPitchDimensions$lPitchCoordinates$dtPenaltyBoxOffense,
         aes(
            x = x,
            y = y
         ),
         # alpha = 0,
         color = cLineColour,
         fill = cPitchColour
      ),

      # six yard box defense
      geom_polygon(
         data = lPitchDimensions$lPitchCoordinates$dt6YardBoxDefense,
         aes(
            x = x,
            y = y
         ),
         # alpha = 0,
         color = cLineColour,
         fill = cPitchColour
      ),

      # six yard attack
      geom_polygon(
         data = lPitchDimensions$lPitchCoordinates$dt6YardBoxOffense,
         aes(
            x = x,
            y = y
         ),
         # alpha = 0,
         color = cLineColour,
         fill = cPitchColour
      ),

      # centre circle
      # this doesn't work, size changes with plot size
      # geom_point(
      #    aes(x = lPitchDimensions$lParameters$nXSpan / 2, y = lPitchDimensions$lParameters$nYSpan / 2),
      #    size = 30,
      #    shape = 1,
      #    color = "white"
      # ),

      # penalty spot defense
      geom_point(
         data = lPitchDimensions$lPitchCoordinates$dtPenaltySpotDefense,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
      ),

      # penalty spot offense
      geom_point(
         data = lPitchDimensions$lPitchCoordinates$dtPenaltySpotOffense,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
      ),

      # centre spot
      geom_point(
         data = lPitchDimensions$lPitchCoordinates$dtCentreSpot,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
      ),

      # centre circle
      geom_polygon(
         data = lPitchDimensions$lPitchCoordinates$dtCentreCircle,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour,
         alpha = 0
      ),

      # centre line
      geom_path(
         data = lPitchDimensions$lPitchCoordinates$dtCentreLine,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
      ),

      # left bottom corner arc
      geom_path(
         data = lPitchDimensions$lPitchCoordinates$dtCornerArcLB,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
         # alpha = 0
      ),

      # left top corner arc
      geom_path(
         data = lPitchDimensions$lPitchCoordinates$dtCornerArcLT,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
      ),

      # right bottom corner arc
      geom_path(
         data = lPitchDimensions$lPitchCoordinates$dtCornerArcRT,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
      ),

      # right top corner arc
      geom_path(
         data = lPitchDimensions$lPitchCoordinates$dtCornerArcRB,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
      ),

      # # goal frame defense
      # geom_segment(
      #    data = lPitchDimensions$lPitchCoordinates$dtGoalFrame,
      #    aes(
      #       x = x - lPitchDimensions$lParameters$nPointRadius_m,
      #       xend = x + lPitchDimensions$lParameters$nPointRadius_m,
      #       y = y,
      #       yend = y
      #    ),
      #    color = cLineColour
      # ),
      #
      # # goal frame offense
      # geom_segment(
      #    data = lPitchDimensions$lPitchCoordinates$dtGoalFrame,
      #    aes(
      #       x = x + lPitchDimensions$lParameters$nXSpan - lPitchDimensions$lParameters$nPointRadius_m,
      #       xend = x + lPitchDimensions$lParameters$nXSpan + lPitchDimensions$lParameters$nPointRadius_m,
      #       y = y,
      #       yend = y
      #    ),
      #    color = cLineColour
      # ),

      cf,
      xlab(NULL),
      ylab(NULL)
   )

   if ( bMarkDirectionOfAttack == T ) {

      lPitchElements = append(
         lPitchElements,
         list(
            # direction of attack
            geom_rect(
               data = data.table(
                  x = ( lPitchDimensions$lParameters$nXSpan / 2 ) - ( lPitchDimensions$lParameters$nXSpan * 2 / 105 ),
                  xend = ( lPitchDimensions$lParameters$nXSpan / 2 ),
                  # xend = ( lPitchDimensions$lParameters$nXSpan / 2 ) + ( lPitchDimensions$lParameters$nXSpan * 2 / 105 ),
                  y = ( lPitchDimensions$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchDimensions$lParameters$nXSpan * 0.9 / 105 ),
                  yend = ( lPitchDimensions$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchDimensions$lParameters$nXSpan * 1.1 / 105 )
               ),
               aes(
                  xmin = nXStart + x,
                  xmax = nXStart + xend,
                  ymin = nYStart + y,
                  ymax = nYStart + yend
               ),
               # arrow = arrow(length = unit(0.03, "npc")),
               color = cLineColour
            )
            ,
            geom_polygon(
               data = data.table(
                  x = c(
                     ( lPitchDimensions$lParameters$nXSpan / 2 ),
                     ( lPitchDimensions$lParameters$nXSpan / 2 ) + ( lPitchDimensions$lParameters$nXSpan * 2 / 105 ),
                     ( lPitchDimensions$lParameters$nXSpan / 2 )
                  ),
                  y = c(
                     lPitchDimensions$lParameters$nYSpan * -2.5 / 68,
                     ( lPitchDimensions$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchDimensions$lParameters$nXSpan * 1 / 105 ),
                     ( lPitchDimensions$lParameters$nYSpan * -2.5 / 68 ) + ( lPitchDimensions$lParameters$nXSpan * 2 / 105 )
                  )
               ),
               aes(
                  x = nXStart + x,
                  y = nYStart + y
               ),
               fill = cLineColour
            )
         )
      )

   }

   return ( lPitchElements )

}
