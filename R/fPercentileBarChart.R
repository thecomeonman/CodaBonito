#' Ashwin Raman's percentile bar charts
#'
#' Each bar denotes the percentile the player is for that respective stat.
#' This is a much better alternative to radar plots.
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
#' @param bAddAbsoluteIndicator Percentiles can be a little misleading if the
#' underlying numbers aren't uniformly distributed. You can use the vertical
#' dimension to add annotations for an indicator of the absolute spread of the
#' values and where this particular player's values fall within that spread.
#' @examples
#' fPercentileBarChart(
#'    dtDataset = dtPlayerMetrics,
#'    vcColumnsToIndex = c('playerId','PlayerName','TeamName'),
#'    dtMetricCategorisation,
#'    iPlayerId = 2,
#'    cTitle = 'Sample'
#' )
#' @import data.table
#' @import ggplot2
#' @import scales
#' @export
fPercentileBarChart = function(
   dtDataset,
   vcColumnsToIndex,
   dtMetricCategorisation,
   iPlayerId,
   cTitle = NULL,
   nColumnWidthByTwo = 0.25,
   nBufferForTextHorizontal = 0.005,
   nBufferForTextVertical = 0.05,
   vnQuantileMarkers = NULL,
   bAddAbsoluteIndicator = F
) {

   dtDataset = melt(
      dtDataset,
      id.vars = intersect(
         vcColumnsToIndex,
         colnames(dtDataset)
      )
   )


   dtDataset[,
      MappedValue := rank(value)/ .N,
      variable
   ]

   dtDataset = merge(
      dtMetricCategorisation,
      dtDataset,
      'variable'
   )

   dtDataset[,
      MappedValue := rank(
         value,
         ties.method = 'average'
      ) / .N,
      variable
   ]

   setkey(
      dtDataset,
      variableCategory,
      variable
   )

   dtDataset[,
      variableIndex := .GRP,
      list(
         variableCategory,
         variable
      )
   ]

   p1 = ggplot() +
      geom_rect(
         data = dtDataset[
            playerId == iPlayerId
         ],
         aes(
            xmin = 0,
            ymin = variableIndex - nColumnWidthByTwo,
            xmax = MappedValue,
            ymax = variableIndex + nColumnWidthByTwo,
            group = paste(PlayerName, variable)
         ),
         fill = 'green'
      ) +
      # geom_text(
      #    data = dtDataset[
      #       playerId == iPlayerId
      #    ],
      #    aes(
      #       x = MappedValue + nBufferForTextHorizontal,
      #       y = variableIndex,
      #       group = paste(PlayerName, variable),
      #       label = round(value, 2)
      #    ),
      #    hjust = 0
      # ) +
      scale_y_continuous(
         breaks = dtDataset[, sort(unique(variableIndex))],
         labels = dtDataset[, list(variable = variable[1]), variableIndex][order(variableIndex), variable]
      ) +
      labs(
         title = cTitle,
         x = NULL,
         y = NULL
      ) +
      facet_grid(
         variableCategory~.,
         # ncol = 1,
         scale = 'free_y',
         # strip.position = 'left',
         space = 'free',
         switch = 'y'
      ) +
      theme(
         panel.background = element_rect(fill = 'black'),
         panel.border = element_blank(),
         plot.background = element_rect(fill = 'black'),
         panel.grid.major.x = element_line(size = 1, color = 'grey20'),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(
            color = 'white',
            hjust = 1,
            margin = margin(15,0,0,15)
         ),
         axis.title = element_blank(),
         panel.spacing = unit(3, "lines"),
         strip.placement = 'outside',
         strip.background = element_rect(fill = 'green'),
         strip.text = element_text(
            size = 15,
            face = 'bold',
            color = 'white'
         ),
         plot.title = element_text(
            color = 'white',
            size = 15,
            hjust = 0.5,
            margin = margin(15, 15, 15, 15),
            face = 'bold'
         )
      )

   if ( F & !is.null(vnQuantileMarkers) ) {

      p1 = p1 +
         geom_point(
            data = dtDataset[,
               list(Value_pile = vnQuantileMarkers),
               list(
                  variableIndex,
                  variableCategory
               )
            ],
            aes(
               x = Value_pile,
               y = variableIndex - nColumnWidthByTwo
            ),
            color = 'white'
         ) +
         geom_segment(
            data = dtDataset[,
               list(
                  Value_pileMin = 0,
                  Value_pileMax = 1
               ),
               list(
                  variableIndex,
                  variableCategory
               )
            ],
            aes(
               x = Value_pileMin,
               xend = Value_pileMax,
               y = variableIndex - nColumnWidthByTwo,
               yend = variableIndex - nColumnWidthByTwo
            ),
            color = 'white'
         ) +
         geom_text(
            data = dtDataset[,
               list(
                  value = quantile(value, vnQuantileMarkers),
                  Value_pile = vnQuantileMarkers
               ),
               list(
                  variable,
                  variableIndex,
                  variableCategory
               )
            ],
            aes(
               x = Value_pile,
               y = variableIndex - nColumnWidthByTwo - nBufferForTextVertical,
               label = round(value, 2)
            ),
            color = 'white',
            vjust = 1
         )

   }

   if ( bAddAbsoluteIndicator ) {

      dtDataset[,
         Value_pile := rank(value)/ .N,
         variable
      ]

      dtAnnotations = dtDataset[,
         list(
            playerId,
            Value_pile,
            value,
            valueScaled = (
               (value - min(value)) / ( 2 * (max(value) - min(value)))
            ) - nColumnWidthByTwo
         ),
         list(
            variableIndex,
            variableCategory
         )
      ][
         playerId == iPlayerId |
         Value_pile %in% c(min(Value_pile), max(Value_pile))
      ]

      p1 = p1 +
         geom_segment(
            data = dtDataset[
               playerId == iPlayerId
            ],
            aes(
               x = Value_pile,
               xend = Value_pile,
               y = variableIndex + nColumnWidthByTwo,
               yend = variableIndex - nColumnWidthByTwo,
               group = paste(PlayerName, variable)
            ),
            color = 'white'
         ) +
         geom_segment(
            data = dtAnnotations[
               playerId == iPlayerId
            ],
            aes(
               x = Value_pile - nBufferForTextHorizontal,
               xend = Value_pile + nBufferForTextHorizontal,
               y = variableIndex + valueScaled,
               yend = variableIndex + valueScaled
            ),
            color = 'white'
         ) +
         geom_text(
            data = dtAnnotations[
               playerId == iPlayerId
            ],
            aes(
               x = Value_pile + nBufferForTextHorizontal,
               y = variableIndex + valueScaled,
               label = round(value, 2)
            ),
            color = 'white',
            hjust = 0
         ) +
         geom_text(
            data = merge(
               dtAnnotations[
                  Value_pile %in% c(min(Value_pile)),
                  list(valueScaled, value, variableIndex, variableCategory)
               ],
               dtAnnotations[
                  playerId == iPlayerId,
                  list(variableIndex, Value_pile)
               ],
               'variableIndex'
            ),
            aes(
               x = Value_pile - nBufferForTextHorizontal,
               y = variableIndex + valueScaled - nBufferForTextVertical,
               label = round(value, 2)
            ),
            color = 'white',
            hjust = 1,
            vjust = 1
         ) +
         geom_text(
            data = merge(
               dtAnnotations[
                  Value_pile %in% c(max(Value_pile)),
                  list(valueScaled, value, variableIndex, variableCategory)
               ],
               dtAnnotations[
                  playerId == iPlayerId, list(variableIndex, Value_pile)
               ],
               'variableIndex'
            ),
            aes(
               x = Value_pile - nBufferForTextHorizontal,
               y = variableIndex + valueScaled + nBufferForTextVertical,
               label = round(value, 2)
            ),
            color = 'white',
            hjust = 1,
            vjust = 0
         )

   }

   p1

}
