## ----SetupChunk, include = FALSE----------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>",
#   fig.width = 6,
#   fig.height = 3
# )

library(data.table)
library(CodaBonito)
library(ggplot2)
library(lpSolveAPI)



## ----DataDescriptionPlayerMetrics, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----

kable(head(dtPlayerMetrics))

## ----DataDescriptionMetricCategorisation, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
kable(head(dtMetricCategorisation))

## ----DataDescriptionPasses, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
kable(head(dtPasses))

## ----DataDescriptionFormation, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
kable(head(dtFormation))

## ----DataDescriptionPlayerLabels, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
kable(head(dtPlayerLabels))


## ----fAddPitchLines, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----

pPitch = ggplot()
pPitch = fAddPitchLines(pPitch)

print(pPitch)

## ----fAddPitchLinesData, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----

# adding passing data on top now
pPitch = pPitch + 
   geom_point(
      data = dtPasses,
      aes(x = x , y = y)
   )

print(pPitch)

## ----theme_pitch, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----

pPitch = pPitch + 
   theme_pitch()

print(pPitch)



## ----fNormalisedValueChart, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----

pNormalisedValueChart = fNormalisedValueChart (
   dtPlayerMetrics,
   vcColumnsToIndex = c('PlayerName','TeamName'),
   dtMetricCategorisation,
   cPlayerName = "gjn xfv",
   cTitle = 'Sample'
)

print(pNormalisedValueChart)

## ----fPercentileBarChart, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
pPercentileBarChart = fPercentileBarChart(
   dtDataset = dtPlayerMetrics,
   vcColumnsToIndex = c('PlayerName','TeamName'),
   dtMetricCategorisation,
   cPlayerName = "gjn xfv",
   cTitle = 'Sample'
)

print(pPercentileBarChart)

## ----fPlotSonar, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
pPlotSonar = fPlotSonar(
   dtPassesToPlot = dtPasses,
   iBlocksInFirstRing = 4,
   iNbrRings = 8,
   nZoomFactor = NULL,
   nXLimit = 120,
   nYLimit = 80,
   bAddPitchBackground = F,
   cTitle = NULL
)
print(pPlotSonar)

# Sonar broken up by pitch area 
pPlotSonarVariation1 = fPlotSonar(
   dtPassesToPlot = dtPasses[,
      list(
         playerId,
         passLength,
         passAngle,      
         x,
         y,
         Success,
         xBucket = (
            ifelse(
               x %/% 20 == 120 %/% 20,
               ( x %/% 20 ) - 1,
               x %/% 20
            ) * 20
         ) + 10,
         yBucket = ( 
            ifelse(
               y %/% 20 == 80 %/% 20,
               ( y %/% 20 ) - 1,
               y %/% 20
            ) * 20
         ) + 10
      )
   ],
   iBlocksInFirstRing = 4,
   iNbrRings = 8,
   nZoomFactor = NULL,
   nXLimit = 120,
   nYLimit = 80,
   bAddPitchBackground = T,
   cTitle = 'Sample by Area of Pitch'
)
print(pPlotSonarVariation1)

# Sonar broken up player, placed at their median passing location 
pPlotSonarVariation2 = fPlotSonar (
   dtPassesToPlot = merge(
      dtPasses,
      merge(
         dtPasses[, 
            list(
               xBucket = median(x), 
               yBucket = median(y)
            ), 
            list(
               playerId
            )
         ],
         dtPlayerLabels[, 
            list(
               playerId,
               bucketLabel = playerName
            )
         ],
         c(
            'playerId'
         )
      ),
      c(
         'playerId'
      )
   ),
   iBlocksInFirstRing = 4,
   iNbrRings = 8,
   nYLimit = 80,
   nXLimit = 120,
   bAddPitchBackground = T,
   cTitle = 'Sample By Median Position On Pitch'
)
print(pPlotSonarVariation2)
# Sonar broken up player, placed at the location dictated by their role
# in the formations
pPlotSonarVariation3 = fPlotSonar(
   dtPassesToPlot = merge(
      dtPasses,
      merge(
         dtFormation[,
            list(
               xBucket = x,
               yBucket = y,
               playerId
            )
         ],
         dtPlayerLabels[, 
            list(
               playerId,
               bucketLabel = playerName
            )
         ],
         c(
            'playerId'
         )
      ),
      'playerId'
   ),
   iBlocksInFirstRing = 4,
   iNbrRings = 8,
   nXLimit = 120,
   nYLimit = 80,
   bAddPitchBackground = T,
   cTitle = 'Sample By Formation'
)
print(pPlotSonarVariation3)


## ----fRadarPercentileChart, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
pRadarPercentileChart = fRadarPercentileChart (
   dtPlayerMetrics = dtPlayerMetrics,
   vcColumnsToIndex = c('PlayerName','TeamName'),
   dtMetricCategorisation = dtMetricCategorisation,
   cPlayerName = "gjn xfv",
   cTitle = 'Sample'
)
print(pRadarPercentileChart)


## ----fEMDDetailed, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----

# Two random datasets of three dimension
a = data.table(matrix(runif(21), ncol = 3))
b = data.table(matrix(runif(30), ncol = 3))

# adding serial numbers to each observation
a[, SNO := .I]
b[, SNO := .I]

# evaluating distance between all combinations of data in the two datasets
a[, k := 'k']
b[, k := 'k']
dtDistances = merge(a,b,'k',allow.cartesian = T)
dtDistances[,
   Distance := (
      (( V1.x - V1.y) ^ 2) +
      (( V2.x - V2.y) ^ 2) +
      (( V3.x - V3.y) ^ 2)
   ) ^ 0.5
]

# getting EMD between this dataet
lprec = fEMDDetailed(
   SNO1 = dtDistances[, SNO.x],
   SNO2 = dtDistances[, SNO.y],
   Distance = dtDistances[, Distance]
)

print(fGetEMDFromDetailedEMD(lprec))

# This value should be the same as that computed by emdist package's emd function.
# EMD needs the weightage of each point, which is assigned as equal in our 
# function, so giving 1/N weightage to each data point
# emdist::emd(
#    as.matrix(
#       a[, list(1/.N, V1,V2,V3)]
#    ),
#    as.matrix(
#       b[, list(1/.N, V1,V2,V3)]
#    )
# ))

## ----fEMDDetailedExtra, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
dtDistances[, EMDWeightage := get.variables(lprec)]
ggplot(dtDistances) +
   geom_point(
      data = dtDistances,
      aes(
         x = factor(SNO.x),
         y = factor(SNO.y),
         size = Distance,
         color = EMDWeightage
      )
   ) +
   scale_colour_continuous(
      low = 'black',
      high = 'red'
   ) +
   coord_fixed() +
   xlab('SNO.x') +
   ylab('SNO.y')