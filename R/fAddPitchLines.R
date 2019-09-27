#' Adds pitch markings, green background, etc. for plot background to look like
#' a football pitch.
#' @import data.table
#' @import ggplot2
fAddPitchLines = function (
   plotObject,
   nXLimit = 105,
   nYLimit = 68,
   cLineColour = 'white',
   cPitchColour = '#038253'
) {

   nPenaltyAreaLength_m = nXLimit * 16.5 / 105
   nPenaltyAreaWidth_m = nYLimit * 40 / 68
   nSixYardBoxLength_m = nXLimit * 5.5 / 105
   nSixYardBoxWidth_m = nYLimit * 18 / 68
   nCentreCircleRadius_m = nXLimit * 9.15 / 105
   nPenaltySpotOffset_m = nXLimit * 11 / 105
   nCornerArcRadius_m = nXLimit * 1 / 105

   dtCentreCircle = data.table(
      Angle_rad = seq(
         0,
         2*pi,
         0.01
      )
   )[,
      x := ( 
         nCentreCircleRadius_m * sin(Angle_rad)
      ) + ( nXLimit / 2 )
   ][,
      y := ( 
         nCentreCircleRadius_m * cos(Angle_rad)
      ) + ( 
         nYLimit / 2
      )
   ]

   dtCornerArc = data.table(
      Angle_rad = seq(
         0,
         pi/2,
         0.01
      )
   )[,
      x := ( 
         nCornerArcRadius_m * sin(Angle_rad)
      )
   ][,
      y := ( 
         nCornerArcRadius_m * cos(Angle_rad)
      )
   ]

   
   plotObject = plotObject +
      # background
      geom_rect(
         aes(
            xmin = 0 - 5, 
            ymin = 0 - 5,
            xmax = nXLimit + 5,
            ymax = nYLimit + 5
         ),
         fill = cPitchColour
      ) +
      # pitch
      geom_rect(
         aes(
            xmin = 0, 
            ymin = 0,
            xmax = nXLimit,
            ymax = nYLimit
         ),
         fill = cPitchColour,
         color = cLineColour
      ) +
      # D defense 
      geom_polygon(
         data = dtCentreCircle,
         aes(
            x = x - ( nXLimit / 2) + nPenaltySpotOffset_m,
            y = y
         ),
         color = cLineColour,
         fill = cPitchColour
      ) +
      # D offense 
      geom_polygon(
         data = dtCentreCircle,
         aes(
            x = x + ( nXLimit / 2) - nPenaltySpotOffset_m,
            y = y
         ),
         color = cLineColour,
         fill = cPitchColour
      ) +
      # penalty box defense
      geom_rect(
         aes(
            ymin = ( nYLimit / 2 ) - ( nPenaltyAreaWidth_m / 2 ), 
            ymax = ( nYLimit / 2 ) + ( nPenaltyAreaWidth_m / 2 ),
            xmin = 0,
            xmax = nPenaltyAreaLength_m
         ),
         # alpha = 0,
         color = cLineColour,
         fill = cPitchColour
      ) +
      # penalty box attack
      geom_rect(
         aes(
            ymin = ( nYLimit / 2 ) - ( nPenaltyAreaWidth_m / 2 ), 
            ymax = ( nYLimit / 2 ) + ( nPenaltyAreaWidth_m / 2 ),
            xmin = nXLimit,
            xmax = nXLimit - nPenaltyAreaLength_m
         ),
         # alpha = 0,
         color = cLineColour,
         fill = cPitchColour
      ) +
      # six yard box defense
      geom_rect(
         aes(
            ymin = ( nYLimit / 2 ) - ( nSixYardBoxWidth_m / 2 ), 
            ymax = ( nYLimit / 2 ) + ( nSixYardBoxWidth_m / 2 ),
            xmin = 0,
            xmax = nSixYardBoxLength_m
         ),
         # alpha = 0,
         color = cLineColour,
         fill = cPitchColour
      ) +
      # six yard attack
      geom_rect(
         aes(
            ymin = ( nYLimit / 2 ) - ( nSixYardBoxWidth_m / 2 ), 
            ymax = ( nYLimit / 2 ) + ( nSixYardBoxWidth_m / 2 ),
            xmin = nXLimit,
            xmax = nXLimit - nSixYardBoxLength_m
         ),
         # alpha = 0,
         color = cLineColour,
         fill = cPitchColour
      ) +
      # centre circle
      # this doesn't work, size changes with plot size
      # geom_point(
      #    aes(x = nXLimit / 2, y = nYLimit / 2),
      #    size = 30, 
      #    shape = 1, 
      #    color = "white"
      # ) +
      # penalty spot defense
      geom_point(
         aes(
            x = nPenaltySpotOffset_m,
            y = ( nYLimit / 2 )
         ),
         color = cLineColour
      ) +
      # penalty spot offense
      geom_point(
         aes(
            x = nXLimit - nPenaltySpotOffset_m,
            y = ( nYLimit / 2 )
         ),
         color = cLineColour
      ) +
      # centre circle
      geom_polygon(
         data = dtCentreCircle,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour,
         alpha = 0
      ) +
      # centre line
      geom_segment(
         aes(
            x = nXLimit / 2,
            xend = nXLimit / 2,
            y = 0,
            yend = nYLimit
         ),
         color = cLineColour
      ) +
      # centre spot
      geom_point(
         aes(
            x = ( nXLimit / 2 ),
            y = ( nYLimit / 2 )
         ),
         color = cLineColour
      ) + 
      # left bottom corner arc
      geom_path(
         data = dtCornerArc,
         aes(
            x = x,
            y = y
         ),
         color = cLineColour
         # alpha = 0
      ) +
      # left top corner arc
      geom_path(
         data = dtCornerArc,
         aes(
            x = x,
            y = nYLimit - y
         ),
         color = cLineColour
      ) +
      # right bottom corner arc
      geom_path(
         data = dtCornerArc,
         aes(
            x = nXLimit - x,
            y = y
         ),
         color = cLineColour
      ) +
      # right top corner arc
      geom_path(
         data = dtCornerArc,
         aes(
            x = nXLimit - x,
            y = nYLimit - y
         ),
         color = cLineColour
      ) +
      # direction of attack
      geom_rect(
         data = data.table(
            x = ( nXLimit / 2 ) - ( nXLimit * 2 / 105 ),
            xend = ( nXLimit / 2 ) + ( nXLimit * 2 / 105 ),
            y = ( nYLimit * -2.5 / 68 ) + ( nXLimit * 0.9 / 105 ),
            yend = ( nYLimit * -2.5 / 68 ) + ( nXLimit * 1.1 / 105 )
         ),
         aes(
            xmin = x,
            xmax = xend,
            ymin = y,
            ymax = yend
         ),
         # arrow = arrow(length = unit(0.03, "npc")),
         color = cLineColour
      ) +
      geom_polygon(
         data = data.table(
            x = c(
               ( nXLimit / 2 ),
               ( nXLimit / 2 ) + ( nXLimit * 2 / 105 ),
               ( nXLimit / 2 )
            ),
            y = c(
               nYLimit * -2.5 / 68,
               ( nYLimit * -2.5 / 68 ) + ( nXLimit * 1 / 105 ),
               ( nYLimit * -2.5 / 68 ) + ( nXLimit * 2 / 105 )
            )
         ),
         aes(
            x = x,
            y = y
         ),
         fill = cLineColour
      ) +
      xlab(NULL) + 
      ylab(NULL) + 
      coord_fixed()

   return ( plotObject )

}