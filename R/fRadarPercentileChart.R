#' @import data.table
#' @import ggplot2
#' @import ggrepel
#' @export
fRadarPercentileChart = function (
   dtDataset,
   vcColumnsToIndex = c('Player','Team'),
   dtMetricCategorisation,
   cPlayerName = "gjn xfv",
   cTitle = 'Sample'
) {

   viColumnNameOccurrence = table(
      colnames(
         dtDataset
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

   dtDataset = melt(
      dtDataset,
      id.vars = vcColumnsToIndex
   )

   dtDataset = merge(
      dtDataset,
      dtMetricCategorisation,
      'variable'
   )

   dtDataset[,
      MappedValue := rank(
         value,
         ties.method = 'average'
      ) / .N,
      variable
   ]

   dtDataset[
      HighValueIsBad == T,
      MappedValue := 1 - MappedValue
   ]


   dtDataset[, Angle := .GRP, variable]
   dtDataset[, Angle := 2 * pi * Angle / max(Angle)]
   dtDataset[, RadarX := cos(Angle) * MappedValue]
   dtDataset[, RadarY := sin(Angle) * MappedValue]

   dtPlayer = dtDataset[Player == cPlayerName]
   dtDataset = dtDataset[!Player == cPlayerName]

   ggplot() +
      geom_polygon(
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
         alpha = 0,
         color = 'grey20'
      ) +
      geom_polygon(
         data = dtPlayer,
         aes(x = RadarX, y = RadarY),
         alpha = 0,
         fill = 'green',
         color = 'green',
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
         color = 'white',
         size = 4,
         fontface = 'bold'
      ) +
      geom_segment(
         data = rbind(
            dtDataset,
            dtPlayer
         )[,
            .SD[
               which.max(MappedValue)
            ],
            variable
         ][,
            list(
               RadarX = RadarX * 1.03,
               RadarY = RadarY * 1.03
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
         color = 'white'
      ) +
      geom_text(
         data = rbind(
            dtDataset,
            dtPlayer
         )[,
            .SD[
               which.max(MappedValue)
            ],
            variable
         ][,
            list(
               RadarX = RadarX * 1.1,
               RadarY = RadarY * 1.1
            ),
            list(
               Angle = ifelse(
                  Angle <= pi,
                  Angle + pi,
                  Angle
               ),
               variable
            )
         ],
         aes(
            x = RadarX,
            y = RadarY,
            label = variable,
            angle = ( 90 ) + ( 180 * Angle /  pi )
         ),
         fontface = 'bold',
         color = 'white'
      ) +
      geom_point(
         aes(
            x = 0,
            y = 0
         )
      ) +
      coord_fixed() +
      theme(
         panel.background = element_rect(fill = 'black'),
         panel.border = element_blank(),
         plot.background = element_rect(fill = 'black'),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title = element_blank(),
         panel.spacing = unit(3, "lines"),
         strip.placement = 'outside',
         strip.background = element_rect(fill = 'green'),
         strip.text = element_text(size = 15, face = 'bold', color = 'white'),
         plot.title = element_text(
            color = 'white',
            size = 15,
            hjust = 0.5,
            margin = margin(15, 15, 15, 15),
            face = 'bold'
         )
      ) +
      labs(
         title = cTitle
      )

}
