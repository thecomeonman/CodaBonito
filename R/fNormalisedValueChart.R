#' @import data.table
#' @import ggplot2
#' @export
fNormalisedValueChart = function (
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
      MappedValue := ( value - min(value) ) / ( max(value) - min(value) ),
      variable
   ]

   dtDataset[
      HighValueIsBad == T,
      MappedValue := 1 - MappedValue
   ]

   dtPlayer = dtDataset[Player == cPlayerName]
   dtDataset = dtDataset[!Player == cPlayerName]

   p1 = ggplot(
      dtDataset
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
