#' @import data.table
#' @import ggplot2
#' @import scales
#' @export
fPercentileBarCharts = function(
   dtDataset = dtDataset,
   nColumnWidthByTwo = 0.25,
   nBufferForTextHorizontal = 0.005,
   nBufferForTextVertical = 0.05,
   vnQuantileMarkers = c(0.0, 0.25, 0.5, 0.75, 1),
   cPlayerNameToAnalyse
) {


   dtDataset = fread(cDataFilePath)

   dtDatasetMelted = melt(
      dtDataset,
      id.vars = intersect(
         vcIDColumns,
         colnames(dtDataset)
      )
   )

   dtDatasetMelted[,
      variableIndex := .GRP,
      variable
   ]

   dtDatasetMelted[,
      Value_pile := rank(value)/ .N,
      variable
   ]

   plotVanillaDesign = ggplot() + 
      geom_rect(
         data = dtDatasetMelted[
            PlayerName == cPlayerNameToAnalyse
         ],
         aes(
            xmin = 0,
            ymin = variableIndex - nColumnWidthByTwo,
            xmax = Value_pile, 
            ymax = variableIndex + nColumnWidthByTwo,
            group = paste(PlayerName, variable)
         )
      ) +
      geom_text(
         data = dtDatasetMelted[
            PlayerName == cPlayerNameToAnalyse
         ],
         aes(
            x = Value_pile + nBufferForTextHorizontal, 
            y = variableIndex,
            group = paste(PlayerName, variable),
            label = round(value, 2)
         ),
         hjust = 0
      ) +
      scale_y_continuous(
         breaks = dtDatasetMelted[, sort(unique(variableIndex))],
         labels = dtDatasetMelted[, list(variable = variable[1]), variableIndex][order(variableIndex), variable]
      ) +
      labs(
         title = 'Original Design',
         x = NULL,
         y = NULL
      ) + 
      theme(
         axis.text.x = element_blank()
      )

   plotVanillaDesign

}