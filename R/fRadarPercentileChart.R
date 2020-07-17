#' A worse but more popular alternative to percentile bar charts
#'
#' The radar stretch out across each metric. The farther the radar stretches
#' on a particular metric, the higher percentile the player is on that metric.
#' You can create percentile bar charts with the same arguments using the
#' fPercentileBarChart function.
#'
#' @param dtPlayerMetrics A dataset with one row for each PlayerName, and various
#' metrics about the PlayerName declared in separate columns. Refer to the
#' dtPlayerMetrics dataset packaged with the library for an example
#' @param vcColumnsToIndex The non-metric columns in your dataset, these are
#' typically columns like name, age, team, position, etc.
#' @param dtMetricCategorisation A table with metadata about the variables in
#' dtPlayerMetrics. Refer to the dtMetricCategorisation object declared in the
#' library for an example.
#' @param iPlayerId The ID of the player you want visualised
#' @param cTitle The title on the chart
#' @examples
#' fRadarPercentileChart (
#'    dtPlayerMetrics = dtPlayerMetrics,
#'    vcColumnsToIndex = c('playerId','PlayerName','TeamName'),
#'    dtMetricCategorisation = dtMetricCategorisation,
#'    iPlayerId = 2,
#'    cTitle = 'Sample'
#' )
#' @import data.table
#' @import ggplot2
#' @import ggrepel
#' @export
fRadarPercentileChart = function (
   dtPlayerMetrics,
   vcColumnsToIndex,
   dtMetricCategorisation,
   iPlayerId,
   cTitle,
   cFontFamily = 'arial',
   cForegroundColour = 'green',
   cBackgroundColour = 'black',
   cFontColour = 'white',
   vnExpand = c(0.1, 0.1)
) {

   setDT(dtPlayerMetrics)
   setDT(dtMetricCategorisation)

   warning(
      'Radar charts are bad. Use fPercentileBarChart instead.'
   )

   viColumnNameOccurrence = table(
      colnames(
         dtPlayerMetrics
      )
   )

   vcDuplicateColumnNames = names(
      names(viColumnNameOccurrence)[
         viColumnNameOccurrence > 1
      ]
   )

   if ( length(vcDuplicateColumnNames) > 0 ) {

      stop(
         paste0(
            'Some column names are being used for multiple columns. ',
            'Please have unique column names.\n',
            'The offending column names are -',
            paste0(
               vcDuplicateColumnNames,
               collapse = ', '
            ),
            '\nPlease rename and try again.'
         )
      )

   }

   dtPlayerMetrics = melt(
      dtPlayerMetrics,
      id.vars = vcColumnsToIndex
   )

   dtPlayerMetrics = merge(
      dtPlayerMetrics,
      dtMetricCategorisation,
      'variable'
   )

   dtPlayerMetrics[,
      MappedValue := rank(
         value,
         ties.method = 'average'
      ) / .N,
      variable
   ]

   dtPlayerMetrics[
      HighValueIsBad == T,
      MappedValue := 1 - MappedValue
   ]


   dtPlayerMetrics[, Angle := .GRP, variable]
   dtPlayerMetrics[, Angle := 2 * pi * Angle / max(Angle)]
   dtPlayerMetrics[, RadarX := cos(Angle) * MappedValue]
   dtPlayerMetrics[, RadarY := sin(Angle) * MappedValue]

   dtPlayer = dtPlayerMetrics[playerId == iPlayerId]
   dtPlayerMetrics = dtPlayerMetrics[!playerId == iPlayerId]

   ggplot() +
      geom_path(
         data = data.table(
           Radius = c(1:5) / 5
         )[,
            list(Angle = seq(0, 2 * pi, 1/100)),
            Radius
         ][,
            list(
               RadarX = cos(Angle) * Radius,
               RadarY = sin(Angle) * Radius
            ),
           Radius
         ],
         aes(x = RadarX, y = RadarY, group = Radius),
         alpha = 0.2,
         color = cFontColour
      ) +
      geom_polygon(
         data = dtPlayer,
         aes(x = RadarX, y = RadarY),
         alpha = 0.1,
         fill = cForegroundColour,
         color = cForegroundColour,
         size = 0.2
      ) +
      geom_segment(
         data = dtPlayer,
         aes(xend = RadarX, yend = RadarY, x = 0, y = 0),
         color = cForegroundColour,
         size = 3
      ) +
      geom_text(
         data = dtPlayer[,
            list(
               RadarX,
               RadarY,
               value,
               Angle,
               MappedValue
            )
         ][
            MappedValue > 0.5,
            MappedValueoffset := MappedValue - 0.1
         ][
            MappedValue <= 0.5,
            MappedValueoffset := MappedValue + 0.1
         ][,
             list(
                RadarX = cos(Angle) * MappedValueoffset,
                RadarY = sin(Angle) * MappedValueoffset,
                value
            )
         ],
         aes(x = RadarX, y = RadarY, label = round(value, 2)),
         color = cFontColour,
         size = 4,
         fontface = 'bold',
         family = cFontFamily
      ) +
      geom_segment(
         data = rbind(
            dtPlayerMetrics,
            dtPlayer
         )[,
            .SD[
               which.max(MappedValue)
            ],
            variable
         ][,
            list(
               RadarX = 1.04 * cos(Angle),
               RadarY = 1.04 * sin(Angle)
               # RadarX = RadarX * 1.03,
               # RadarY = RadarY * 1.03
            ),
            variable
         ],
         aes(
            x = 0,
            xend = RadarX,
            y = 0,
            yend = RadarY
         ),
         alpha = 0.2,
         color = cFontColour
      ) +
      geom_text(
         data = rbind(
            dtPlayerMetrics,
            dtPlayer
         )[
            Angle <= pi
         ][,
            .SD[
               which.max(MappedValue)
            ],
            list(
               variableLabel,
               variable
            )
         ][,
            list(
               # RadarX = RadarX * 1.1,
               # RadarY = RadarY * 1.1
               RadarX = 1.04 * cos(Angle),
               RadarY = 1.04 * sin(Angle)
            ),
            list(
               # Calling this column angle messes with the radarx and radary
               # calculation. I think this gets evaluated before that, so you
               # end up getting all the labels on one side of the circle with
               # overlap
               AngleOrientation = ifelse(
                  Angle <= pi,
                  Angle + pi,
                  Angle
               ),
               variable,
               variableLabel
            )
         ],
         aes(
            x = RadarX,
            y = RadarY,
            label = variableLabel,
            angle = ( 90 ) + ( 180 * AngleOrientation /  pi )
         ),
         fontface = 'bold',
         color = cFontColour,
         family = cFontFamily,
         vjust = 0
      ) +
      geom_text(
         data = rbind(
            dtPlayerMetrics,
            dtPlayer
         )[
            Angle > pi
         ][,
            .SD[
               which.max(MappedValue)
            ],
            list(
               variableLabel,
               variable
            )
         ][,
            list(
               # RadarX = RadarX * 1.1,
               # RadarY = RadarY * 1.1
               RadarX = 1.1 * cos(Angle),
               RadarY = 1.1 * sin(Angle)
            ),
            list(
               # Calling this column angle messes with the radarx and radary
               # calculation. I think this gets evaluated before that, so you
               # end up getting all the labels on one side of the circle with
               # overlap
               AngleOrientation = ifelse(
                  Angle <= pi,
                  Angle + pi,
                  Angle
               ),
               variable,
               variableLabel
            )
         ],
         aes(
            x = RadarX,
            y = RadarY,
            label = variableLabel,
            angle = ( 90 ) + ( 180 * AngleOrientation /  pi )
         ),
         fontface = 'bold',
         color = cFontColour,
         family = cFontFamily,
         vjust = 0
         # hjust = 0.5
      ) +
      geom_point(
         aes(
            x = 0,
            y = 0
         ),
         color = cFontColour
      ) +
      coord_fixed() +
      scale_x_continuous(expand = expand_scale(mult = 0, add = vnExpand[1])) +
      scale_y_continuous(expand = expand_scale(mult = 0, add = vnExpand[2])) +
      theme(
         panel.background = element_rect(fill = cBackgroundColour),
         panel.border = element_blank(),
         plot.background = element_rect(fill = cBackgroundColour),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title = element_blank(),
         panel.spacing = unit(3, "lines"),
         strip.placement = 'outside',
         strip.background = element_rect(
            fill = cForegroundColour
         ),
         strip.text = element_text(
            size = 15,
            face = 'bold',
            color = cFontColour,
            family = cFontFamily
         ),
         plot.title = element_text(
            color = cFontColour,
            size = 15,
            hjust = 0.5,
            margin = margin(15, 15, 15, 15),
            face = 'bold',
            family = cFontFamily
         )
      ) +
      labs(
         title = cTitle
      )

}
