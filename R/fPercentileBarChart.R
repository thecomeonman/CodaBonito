# Paste the below lines into your console
#' @import data.table
#' @import ggplot2
#' @import scales
#' @export
fPercentileBarChart = function(
   dtDataset,
   vcColumnsToIndex = c('PlayerName','TeamName'),
   dtMetricCategorisation,
   cPlayerName = "gjn xfv",
   cTitle = NULL,
   nColumnWidthByTwo = 0.25,
   nBufferForTextHorizontal = 0.005,
   nBufferForTextVertical = 0.05,
   vnQuantileMarkers = c(0.01, 0.25, 0.5, 0.75, 0.99)
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

   plotVanillaDesign = ggplot() +
      geom_rect(
         data = dtDataset[
            PlayerName == cPlayerName
         ],
         aes(
            xmin = 0,
            ymin = variableIndex - nColumnWidthByTwo,
            xmax = MappedValue,
            ymax = variableIndex + nColumnWidthByTwo,
            group = paste(PlayerName, variable)
         )
      ) +
      geom_text(
         data = dtDataset[
            PlayerName == cPlayerName
         ],
         aes(
            x = MappedValue + nBufferForTextHorizontal,
            y = variableIndex,
            group = paste(PlayerName, variable),
            label = round(value, 2)
         ),
         hjust = 0
      ) +
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

   print(plotVanillaDesign)

}
