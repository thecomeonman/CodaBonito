#' Those charts with normailsed metric values and a bright mark indicating
#' where the PlayerName of interest lies
#'
#' @param dtPlayerMetrics A dataset with one row for each PlayerName, and various
#' metrics about the PlayerName declared in separate columns. Refer to the
#' dtPlayerMetrics dataset packaged with the library for an example
#' @param vcColumnsToIndex The non-metric columns in your dataset, these are
#' typically columns like name, age, team, position, etc.
#' @param dtMetricCategorisation A table with metadata about the variables in
#' dtPlayerMetrics. Refer to the `CodaBonito::dtMetricCategorisation` object
#' declared in the library for an example.
#' @param iPlayerId The ID of the player you want visualised
#' @param cTitle The title on the chart
#' @param vnExpand The X axis stretches between 0 and 1 but you need space on both
#' sides to fit the annotations. Specify what values the category label, the
#' stat value, and the comparison stat value should come at
#' @param compareWith, either 'median', another playerId, or NULL. If not null,
#' will add a second set of points for either the median or another player to
#' have some reference for the player you're looking at.
#' @examples
#' fStripChart (
#'    dtPlayerMetrics,
#'    vcColumnsToIndex = c('playerId','PlayerName','TeamName'),
#'    dtMetricCategorisation,
#'    iPlayerId = 2,
#'    cTitle = 'Sample'
#' )
#' @import data.table
#' @import ggplot2
#' @import colorspace
#' @export
fStripChart = function (
   dtPlayerMetrics,
   vcColumnsToIndex,
   dtMetricCategorisation,
   iPlayerId,
   cTitle,
   cFontFamily = 'arial',
   cPlayerColour = 'red',
   cBackgroundColour = 'black',
   cComparisonColour = 'white',
   cNeutralColour = 'grey50',
   vnExpand = c(-0.25, -0.03, 1.03, 1.1, 1.15),
   bShrinkOtherPlayerPoints = T,
   compareWith = 'median',
   bDisplayCategories = T,
   bDisplayValue = T
) {

   setDT(dtPlayerMetrics)
   setDT(dtMetricCategorisation)

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
      MappedValue := ( value - min(value) ) / ( max(value) - min(value) ),
      variable
   ]

   dtPlayerMetrics[
      HighValueIsBad == T,
      MappedValue := 1 - MappedValue
   ]

   dtPlayerMetrics = dtPlayerMetrics[
      order(
         variableCategory, variableLabel
      )
   ]

   dtPlayerMetrics[,
      variablePosition := cumsum(
         c(
            1,
            (tail(variableLabel, -1) != head(variableLabel, -1)) +
            (tail(variableCategory, -1) != head(variableCategory, -1))
         )
      )
   ]

   dtPlayer = dtPlayerMetrics[get(vcColumnsToIndex[1]) == iPlayerId]
   dtPlayerMetrics = dtPlayerMetrics[!get(vcColumnsToIndex[1]) == iPlayerId]

   dtPlayerMetrics[, MappedValueAlternatingColour := ( ( (rank(MappedValue) / .N ) %/% 0.25 ) ), variable]
   dtPlayerMetrics[MappedValueAlternatingColour == max(MappedValueAlternatingColour), MappedValueAlternatingColour := MappedValueAlternatingColour - 1]

   p1 = ggplot(
      # dtPlayerMetrics
   ) +
      geom_segment(
         data = dtPlayerMetrics[,
            list(
               variablePositionMin = min(variablePosition),
               variablePositionMax = max(variablePosition),
               x = seq(0, 1, 0.25)
            )
         ],
         aes(
            y = variablePositionMin,
            yend = variablePositionMax,
            x = x,
            xend = x
         ),
         color = cNeutralColour,
         alpha = 0.5
      )

   viMappedValueAlternatingColour = dtPlayerMetrics[, sort(unique(MappedValueAlternatingColour))]
   for ( iMappedValueAlternatingColour in viMappedValueAlternatingColour ) {

      p1 = p1 +
         geom_jitter(
            data = dtPlayerMetrics[MappedValueAlternatingColour == iMappedValueAlternatingColour],
            aes(
               x = MappedValue,
               y = variablePosition
            ),
            shape = 21,
            # alpha = pmax(0.25, 5 / nrow(dtPlayerMetrics)),
            color = lighten(cNeutralColour, ( 0.5 * ( iMappedValueAlternatingColour - median(viMappedValueAlternatingColour) ) ) / max(viMappedValueAlternatingColour)),
            fill = NA,
            size = ifelse(bShrinkOtherPlayerPoints, 1, 1),
            height = ifelse(bShrinkOtherPlayerPoints, 0.25, 0.25),
         )
   }

   if ( is.null(compareWith) ) {

      dtComparison = dtPlayer[, list(median(MappedValue), median(value)), list(variableLabel)]

   } else {

      if ( compareWith == 'median' ) {

         dtComparison = dtPlayerMetrics[, list(median(MappedValue), median(value)), list(variableLabel)]

      } else if ( compareWith %in% dtPlayerMetrics[, get(vcColumnsToIndex[1])] ) {

         dtComparison = dtPlayerMetrics[compareWith == get(vcColumnsToIndex[1]), list(median(MappedValue), median(value)), list(variableLabel)]

      }

   }

   dtComparison = merge(
      dtPlayer,
      dtComparison,
      'variableLabel'
   )

   dtComparison[, NumericLabel := '']
   dtComparison[V1 == MappedValue, NumericLabel := round(value, 2)]
   dtComparison[V1 != MappedValue, NumericLabel := paste0(round(value, 2), ' | ', round(V2, 2))]

   p1 = p1 +
      geom_segment(
         data = dtComparison[ MappedValue < V1 ],
         aes(
            x = V1,
            xend = MappedValue,
            y = variablePosition,
            yend = variablePosition
         ),
         color = cComparisonColour,
         size = 2,
         alpha = 0.8
      ) +
      geom_segment(
         data = dtComparison[ MappedValue >= V1 ],
         aes(
            x = V1,
            xend = MappedValue,
            y = variablePosition,
            yend = variablePosition
         ),
         color = cPlayerColour,
         size = 2,
         alpha = 0.8
      ) +
      geom_point(
         data = dtComparison,
         aes(
            x = V1,
            y = variablePosition
         ),
         color = cPlayerColour,
         size = 3,
         alpha = 0
      )

   p1 = p1 +
      # geom_point(
      #     data = dtPlayer,
      #     aes(x = MappedValue, y = variable2),
      #     color = 'black',
      #     size = 6
      # ) +
      geom_point(
         data = dtComparison[ MappedValue >= V1 ],
         aes(
            x = MappedValue,
            y = variablePosition
         ),
         color = cPlayerColour,
         size = 3
      ) +
      geom_point(
         data = dtComparison[ MappedValue < V1 ],
         aes(
            x = MappedValue,
            y = variablePosition
         ),
         color = cComparisonColour,
         size = 3
      )

   p1 = p1 +
      geom_text(
         data = dtComparison[
            MappedValue < V1,
            list(
               x = c(vnExpand[2]),
               hjust = c(1),
               # Label = c(NumericLabel, variableLabel)
               Label = c(
                  variableLabel
               )
            ),
            list(variableCategory, variableLabel, variablePosition)
         ],
         aes(
            x = x,
            y = variablePosition,
            label = Label,
            hjust = hjust
         ),
         color = cComparisonColour,
         size = 5,
         fontface = 'bold',
         family = cFontFamily
      ) +
      geom_text(
         data = dtComparison[
            MappedValue >= V1,
            list(
               x = c(vnExpand[2]),
               hjust = c(1),
               # Label = c(NumericLabel, variableLabel)
               Label = c(
                  variableLabel
               )
            ),
            list(variableCategory, variableLabel, variablePosition)
         ],
         aes(
            x = x,
            y = variablePosition,
            label = Label,
            hjust = hjust
         ),
         color = cPlayerColour,
         size = 5,
         fontface = 'bold',
         family = cFontFamily
      )

   if ( bDisplayValue ) {

      p1 = p1 +
         geom_text(
            data = dtComparison[
               MappedValue < V1,
               list(
                  x = c(vnExpand[3]),
                  hjust = c(0),
                  # Label = c(NumericLabel, variableLabel)
                  Label = c(
                     paste0(c(round(value, ifelse(suffix == '%', 0, 2))), suffix)
                  )
               ),
               list(variableCategory, variableLabel, variablePosition)
            ],
            aes(
               x = x,
               y = variablePosition,
               label = Label,
               hjust = hjust
            ),
            color = cComparisonColour,
            size = 5,
            fontface = 'bold',
            family = cFontFamily
         ) +
         geom_text(
            data = dtComparison[
               MappedValue >= V1,
               list(
                  x = c(vnExpand[3]),
                  hjust = c(0),
                  # Label = c(NumericLabel, variableLabel)
                  Label = c(
                     paste0(c(round(value, ifelse(suffix == '%', 0, 2))), suffix)
                  )
               ),
               list(variableCategory, variableLabel, variablePosition)
            ],
            aes(
               x = x,
               y = variablePosition,
               label = Label,
               hjust = hjust
            ),
            color = cPlayerColour,
            size = 5,
            fontface = 'bold',
            family = cFontFamily
         )

      if ( dtComparison[, any(V1 != MappedValue) ] ) {

         p1 = p1 +
            geom_text(
               data = dtComparison[,
                  list(
                     x = c(vnExpand[4]),
                     hjust = c(0),
                     # Label = c(NumericLabel, variableLabel)
                     Label = paste0(c(round(V2, ifelse(suffix == '%', 0, 2))), suffix)
                  ),
                  list(variableCategory, variableLabel, variablePosition)
               ],
               aes(
                  x = x,
                  y = variablePosition,
                  label = Label,
                  hjust = hjust
               ),
               color = cNeutralColour,
               size = 5,
               fontface = 'bold',
               family = cFontFamily
            )

         if ( compareWith %in% 'median' ) {

            p1 = p1 +
               geom_text(
                  aes(
                     x = vnExpand[4],
                     y = dtComparison[, max(variablePosition) + 0.6],
                     label = 'median'
                  ),
                  hjust = 0,
                  color = cNeutralColour,
                  size = 3,
                  family = cFontFamily
               )

         }

      }

   }


   if ( bDisplayCategories ) {

      p1 = p1 +
         geom_text(
            data = dtComparison[, list(variablePosition = sum(range(variablePosition)) / 2, x = vnExpand[1]), variableCategory],
            aes(
               x = x,
               y = variablePosition,
               label = variableCategory
            ),
            color = cNeutralColour,
            size = 5,
            fontface = 'bold',
            family = cFontFamily,
            angle = 90,
            vjust = 1.1,
            lineheight = 0.7
         )
   }

   p1 = p1 +
      scale_x_continuous(
         breaks = c(0:10) / 10,
         name = NULL,
         limits = c(vnExpand[1], vnExpand[5]),
         expand = expansion()
      ) +
      scale_y_discrete(
         # expand = c(0.2,0),
         expand = expansion(mult = 0, add = c(0.5, 0.1)),
         name = NULL
      ) +
      theme(
         panel.background = element_rect(fill = cBackgroundColour),
         panel.border = element_blank(),
         plot.background = element_rect(fill = cBackgroundColour),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         axis.line = element_blank(),
         axis.ticks = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title = element_blank(),
         panel.spacing = unit(3, "lines"),
         strip.placement = 'outside',
         strip.background = element_blank(),
         strip.text = element_blank(),
         plot.title = element_text(
            color = cNeutralColour,
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


   p1

}
