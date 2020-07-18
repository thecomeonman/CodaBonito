if ( F ) {

   library(data.table)

   set.seed(1)

   nXLimit = 120
   nYLimit = 80

   iPlayers = 200

   dtPlayerMetrics = data.table(
      PlayerName = unique(
         sapply(
            1:iPlayers,
            function ( x ) {

               paste(
                  paste0(
                     sample(letters, 3),
                     collapse = ''
                  ),
                  paste0(
                     sample(letters, 3),
                     collapse = ''
                  )
               )

            }
         )
      )
   )

   dtPlayerMetrics[,
      TeamName := sapply(
         1:.N,
         function ( x ) {

            paste0(
               sample(letters, 3),
               collapse = ''
            )

         }
      )
   ]

   dtPlayerMetrics[, playerId := .I]

   dtPlayerMetrics[,
      Metric1 := abs(rnorm(200, mean = 3, sd = 0.5))
   ]

   dtPlayerMetrics[,
      Metric2 := runif(.N)
   ]

   dtPlayerMetrics[,
      Metric3 := rbeta(
         200,
         runif(1),
         runif(1)
      )
   ]

   dtPlayerMetrics[,
      Metric4 := 1 - rbeta(
         200,
         runif(1),
         runif(1)
      )
   ]

   dtPlayerMetrics[,
      Metric5 := rgamma(200, 1)
   ]

   dtPlayerMetrics[,
      Metric6 := rgamma(200, 5)
   ]

   dtPlayerMetrics[,
      Metric7 := rgamma(200, 0.1)
   ]

   save(
      list = 'dtPlayerMetrics',
      file = './data/PlayerMetrics.rda'
   )

   dtMetricCategorisation = data.table(
      variable = c('Metric1','Metric2','Metric3','Metric4','Metric5','Metric7'),
      variableLabel = c('Metric 1','Metric 2','Metric 3','Metric 4','Metric 5','Metric 7'),
      variableCategory = c('Offense','Offense','Defense','Offense','Defense','Defense'),
      HighValueIsBad = c(F,F,F,T,F,F),
      suffix = c('','','%','','','%')
   )

   save(
      list = 'dtMetricCategorisation',
      file = './data/PlqyerMetricCategorisation.rda'
   )

   # very defensive 5 a side team
   dtPasses = rbindlist(
      list(
         # GK
         data.table(
            playerId = 1,
            x = ( runif(20) * 15 ) + 0,
            y = ( runif(20) * 40 ) + 20,
            endX = ( runif(20) * 75 ) + 10,
            endY = ( runif(20) * 70 ) + 5
         ),
         # LB
         data.table(
            playerId = 2,
            x = ( runif(25) * 90 ) + 15,
            y = ( runif(25) * 15 ) + 5
         )[,
            endX := x + ( runif(25) * 20 ) - 10
         ][,
            endY := y + ( runif(25) * 40 )
         ],
         # RB
         data.table(
            playerId = 3,
            x = ( runif(25) * 90 ) + 15,
            y = ( runif(25) * 15 ) + 60
         )[,
            endX := x + ( runif(25) * 20 ) - 10,
         ][,
            endY := y - ( runif(25) * 40 )
         ],
         # CM
         data.table(
            playerId = 8,
            x = ( runif(40) * 60 ) + 35,
            y = ( runif(40) * 60 ) + 10
         )[,
            endX := x + ( runif(40) * 15 ) - 10
         ][,
            endY := ( runif(40) * 50 ) + 15
         ],
         # CF
         data.table(
            playerId = 9,
            x = ( runif(40) * 50 ) + 55,
            y = ( runif(40) * 40 ) + 20
         )[,
            endX := x + ( runif(40) * 15 ) - 20
         ][,
            endY := ( runif(40) * 50 ) + 15
         ]
      )
   )

   dtPasses[,
      passLength := (
         (( x - endX) ^ 2) +
         (( y - endY) ^ 2)
      ) ^ 0.5
   ]

   dtPasses[,
      passAngle := atan2(
         endY - y, endX - y
      )
   ]

   dtPasses[,
      Success := as.integer( ( runif(.N) + ( endX / 600 ) ) < 0.9 )
   ]

   viReceivingPlayerId = sapply(
      dtPasses[, which(Success == 1)],
      function( iRow ) {

         dtPass = dtPasses[iRow]

         dtCandidates = dtPasses[
            playerId != dtPass[, playerId]
         ]

         vnBeingNextPassPDF = dtCandidates[,
            1 / (
               ( ( x - dtPass[, endX] ) ^ 2 ) +
               ( ( y - dtPass[, endY] ) ^ 2 )
            )
         ]

         vnBeingNextPassPDF = (vnBeingNextPassPDF) / sum(vnBeingNextPassPDF)
         vnBeingNextPassCDF = cumsum(vnBeingNextPassPDF)

         iEndRow = min( which( vnBeingNextPassCDF >= runif(1) ) )

         dtCandidates[
            iEndRow,
            playerId
         ]
      }
   )

   dtPasses[
      Success == 1,
      recipientPlayerId := viReceivingPlayerId
   ]

   rm(viReceivingPlayerId)

   save(
      list = 'dtPasses',
      file = './data/Passes.rda'
   )

   dtFormation = data.table(
      playerId = c(1, 2, 3, 8, 9),
      x = c(15, 35, 35, 60, 90),
      y = c(40, 20, 60, 40, 40)
   )

   save(
      list = 'dtFormation',
      file = './data/Formation.rda'
   )

   dtPlayerMetrics = data.table(
      playerId = 1:iPlayers,
      PlayerName = unique(
         sapply(
            1:iPlayers,
            function ( x ) {

               paste(
                  paste0(
                     sample(letters, 3),
                     collapse = ''
                  ),
                  paste0(
                     sample(letters, 3),
                     collapse = ''
                  )
               )

            }
         )
      )
   )


   save(
      list = 'dtPlayerLabels',
      file = './data/PlayerLabels.rda'
   )


   nEvents = 40
   nGoals = 6
   set.seed(34)
   dtXg = data.table(
      minute = runif(nEvents) * 95,
      second = runif(nEvents) * 60,
      teamId = round(runif(nEvents), 0) + 1,
      xG = rbeta(nEvents, 0.4, 2),
      Goal = 1:nEvents %in% sample(1:nEvents, nGoals)
   )

   save(
      list = 'dtXg',
      file = './data/dtXg.rda'
   )

   dtTeamLabels = data.table(
      teamId = c(1,2),
      teamName = c(
         'FC ABC',
         'QWE FC'
      )
   )

   save(
      list = 'dtTeamLabels',
      file = './data/dtTeamLabels.rda'
   )









    iNbrFrames = 10
    iNbrPlayers = 23
    pitchBuffer = 0.25
    set.seed(1)
    # minimal tracking slice example

    lTrackingData = list()
    # lTrackingData$dtTrackingData = data.table(
    #     X = abs(rnorm(iNbrPlayers * iNbrFrames, 0, 0.06)),
    #     Y = abs(rnorm(iNbrPlayers * iNbrFrames, 0, 0.03))
    # )

    lTrackingData$dtTrackingData = data.table(
        X = (runif(iNbrPlayers) * (nXLimit/2)) + (nXLimit/5),
        Y = (runif(iNbrPlayers) * (nYLimit/2)) + (nYLimit/4)
    )

    lTrackingData$dtTrackingData[, PlayerID := .I %% 23]
    lTrackingData$dtTrackingData[PlayerID == 0, c('Tag','Player') := list('Ball','Ball')]
    lTrackingData$dtTrackingData[
      PlayerID > 0 & PlayerID <= 11,
      c('Tag','Player') := list('Away', paste0('AwayPlayer', PlayerID))
    ]

    lTrackingData$dtTrackingData[
      PlayerID > 11 & PlayerID <= 23,
      c('Tag','Player') := list('Home', paste0('HomePlayer', PlayerID))
    ]

    lTrackingData$dtTrackingData[, XEnd := X + ( (nXLimit/30) * runif(.N) )]
    lTrackingData$dtTrackingData[, YEnd := Y + ( (nXLimit/30) * runif(.N) )]

    lTrackingData$dtTrackingData = lTrackingData$dtTrackingData[,
      list(
        Frame = 0:iNbrFrames,
        X = ( ( X - XEnd ) * (0:iNbrFrames) / iNbrFrames ) + X,
        Y = ( ( Y - YEnd ) * (0:iNbrFrames) / iNbrFrames ) + Y
      ),
      list(
        Tag,
        Player
      )
    ]

    lTrackingData$dtEventsData = data.table(
        StartFrame = min(lTrackingData$dtTrackingData[, Frame]),
        EndFrame = min(lTrackingData$dtTrackingData[, Frame]),
        Type = 'PASS',
        Subtype = '',
        Team = 'Home'
    )

    lTrackingData$dtTrackingData[, Time_s := Frame * 0.2]

    lTrackingData$dtTrackingData[,
        VelocityX := c(
            0,
            diff(X) / diff(Time_s)
        ),
        list(
            Player
        )
    ]

    lTrackingData$dtTrackingData[,
        VelocityY := c(
            0,
            diff(Y) / diff(Time_s)
        ),
        list(
            Player
        )
    ]


   save(
      list = 'lTrackingData',
      file = './data/lTrackingData$lTrackingData.rda'
   )


}
