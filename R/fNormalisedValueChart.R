#' Those charts with normailsed metric values and a bright mark indicating
#' where the PlayerName of interest lies
#'
#' @param dtPlayerMetrics A dataset with one row for each PlayerName, and various
#' metrics about the PlayerName declared in separate columns. Refer to the 
#' dtPlayerMetrics dataset packaged with the library for an example
#' @param vcColumnsToIndex The non-metric columns in your dataset, these are
#' typically columns like name, age, team, position, etc.
#' @param dtMetricCategorisation A table with metadata about the variables in 
#' dtPlayerMetrics. Refer to the dtMetricCategorisation object declared in the 
#' library for an example. 
#' @param cPlayerName The name of the PlayerName you want visualised
#' @param cTitle The title on the chart
#' @examples
#' fNormalisedValueChart (
#'    dtPlayerMetrics,
#'    vcColumnsToIndex = c('PlayerName','TeamName'),
#'    dtMetricCategorisation,
#'    cPlayerName = "gjn xfv",
#'    cTitle = 'Sample'
#' )
#' @import data.table
#' @import ggplot2
#' @export
fNormalisedValueChart = function (
   dtPlayerMetrics,
   vcColumnsToIndex,
   dtMetricCategorisation,
   cPlayerName,
   cTitle
) {

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

   dtPlayer = dtPlayerMetrics[PlayerName == cPlayerName]
   dtPlayerMetrics = dtPlayerMetrics[!PlayerName == cPlayerName]

   p1 = ggplot(
      dtPlayerMetrics
   ) +
       geom_jitter(
         aes(
            x = MappedValue,
            y = variable
         ),
         alpha = 0.25,
         color = 'grey50',
         size = 5,
         height = 0.05
      ) +
      # geom_point(
      #     data = dtPlayer,
      #     aes(x = MappedValue, y = variable2),
      #     color = 'black',
      #     size = 6
      # ) +
      geom_point(
         data = dtPlayer,
         aes(
            x = MappedValue,
            y = variable
         ),
         color = 'green',
         size = 7
      ) +
      geom_text(
         data = dtPlayer,
         aes(
            x = 1.03,
            y = variable,
            label = round(value, 2)
         ),
         color = 'green',
         size = 5,
         hjust = 0,
         fontface = 'bold'
      ) +
      scale_x_continuous(
         breaks = c(0:10) / 10,
         name = NULL
      ) +
      scale_y_discrete(
         # expand = c(0.2,0),
         name = 'Stat category names'
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
      ) +
      labs(
         title = cTitle
      )

   p1

}
