## ----Setup, cache=FALSE, echo=FALSE, warning=FALSE, message=FALSE, fig.width = 15, fig.height = 8, results = 'hide'----

library(knitr)
read_chunk('./CodaBonito.R')


## ----SetupChunk, echo = F-----------------------------------------------------
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

nXLimit = 120
nYLimit = 80

## ----DataDescriptionPlayerMetrics, echo = F-----------------------------------
kable(head(dtPlayerMetrics))

## ----DataDescriptionMetricCategorisation, echo = F----------------------------
kable(head(dtMetricCategorisation))

## ----DataDescriptionPasses, echo = F------------------------------------------
kable(head(dtPasses))

## ----DataDescriptionFormation, echo = F---------------------------------------
kable(head(dtFormation))

## ----DataDescriptionPlayerLabels, echo = F------------------------------------
kable(head(dtPlayerLabels))

## ----DataDescriptionTrackingSlice, echo = F------------------------------------
kable(head(dtTrackingSlice))

## ----fAddPitchLines-----------------------------------------------------------
pPitch = ggplot()
pPitch = fAddPitchLines(pPitch)

print(pPitch)

## ----fAddPitchLinesData-------------------------------------------------------
# adding passing data on top now
pPitch = pPitch +
   geom_point(
      data = dtPasses,
      aes(x = x , y = y)
   )

print(pPitch)

## ----theme_pitch--------------------------------------------------------------
pPitch = pPitch +
   theme_pitch()

print(pPitch)

## ----fNormalisedValueChart----------------------------------------------------
pNormalisedValueChart = fNormalisedValueChart (
   dtPlayerMetrics,
   vcColumnsToIndex = c('playerId','PlayerName','TeamName'),
   dtMetricCategorisation,
   iPlayerId = 2,
   cTitle = 'Sample'
)

print(pNormalisedValueChart)

## ----fPercentileBarChart------------------------------------------------------
pPercentileBarChart = fPercentileBarChart(
   dtDataset = dtPlayerMetrics,
   vcColumnsToIndex = c('playerId','PlayerName','TeamName'),
   dtMetricCategorisation,
   iPlayerId = 2,
   cTitle = 'Sample'
)
print(pPercentileBarChart)

## ----fPercentileBarChartAbsoluteIndicator-------------------------------------
pPercentileBarChart = fPercentileBarChart(
   dtDataset = dtPlayerMetrics,
   vcColumnsToIndex = c('playerId','PlayerName','TeamName'),
   dtMetricCategorisation,
   iPlayerId = 2,
   cTitle = 'Sample',
   # vnQuantileMarkers = c(0.01, 0.25, 0.5, 0.75, 0.99),
   bAddAbsoluteIndicator = T
)

print(pPercentileBarChart)

## ----fRadarPercentileChart----------------------------------------------------
pRadarPercentileChart = fRadarPercentileChart (
   dtPlayerMetrics = dtPlayerMetrics,
   vcColumnsToIndex = c('playerId','PlayerName','TeamName'),
   dtMetricCategorisation = dtMetricCategorisation,
   iPlayerId = 2,
   cTitle = 'Sample'
)
print(pRadarPercentileChart)

## ----fPlotSonar---------------------------------------------------------------
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
dtPassesByPitchArea = dtPasses[,
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
]

pPlotSonarVariation1 = fPlotSonar(
   dtPassesToPlot = dtPassesByPitchArea,
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
dtPassesByPlayer = merge(
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
)

pPlotSonarVariation2 = fPlotSonar (
   dtPassesToPlot = dtPassesByPlayer,
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

dtPassesByPlayerFormation = merge(
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
)
pPlotSonarVariation3 = fPlotSonar(
   dtPassesToPlot = dtPassesByPlayerFormation,
   iBlocksInFirstRing = 4,
   iNbrRings = 8,
   nXLimit = 120,
   nYLimit = 80,
   bAddPitchBackground = T,
   cTitle = 'Sample By Formation'
)
print(pPlotSonarVariation3)

## ----fPassNetworkChart--------------------------------------------------------
pPassNetworkChart = fPassNetworkChart(
   dtPasses,
   dtPlayerLabels
)
print(pPassNetworkChart)

## ----fXgBuildUpComparison-----------------------------------------------------
pXgBuildUpComparison = fXgBuildUpComparison(
   dtXg,
   dtTeamLabels
)
print(pXgBuildUpComparison)

## ----fDrawVoronoiFromTable-----------------------------------------------------


pVoronoi = fDrawVoronoiFromTable(
   dtTrackingSlice[1],
   nXLimit = nXLimit,
   nYlimit = nYlimit
)

print(pVoronoi)


## ----fDrawVoronoiFromTableAnimated--------------------------------------------

voronoiOutput = suppressWarnings(
   fDrawVoronoiFromTable(
      dtTrackingSlice,
      nXLimit = nXLimit,
      nYlimit = nYlimit,
      UseOneFrameEvery = 1,
      DelayBetweenFrames = 5,
      suppressWarnings = T
   )
)

if ( !interactive() ) {

   qwe = suppressWarnings(
      file.remove('./README_files/figure-markdown_strict/Voronoi.gif')
   )
   rm(qwe)

   qwe = file.copy(
      voronoiOutput,
      './README_files/figure-markdown_strict/Voronoi.gif'
   )

   rm(qwe)

}


## ----fEMDDetailed-------------------------------------------------------------
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

## ----fEMDDetailedExtra--------------------------------------------------------
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

