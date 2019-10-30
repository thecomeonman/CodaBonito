#' @import ggplot2
#' @import data.table
#' @import zoo
#' @export
fXgBuildUpComparison = function(
   dtXg,
   dtTeamLabels = NULL,
   iSimulationIterations = 1000
) {

   vcTeamIds = dtXg[, unique(teamId)]

   dtXg[, time := minute + ( second / 60 )]
   dtXg = dtXg[order(time)]
   dtXg = dtXg[,
      list(
         xG,
         CumXg = cumsum(xG),
         Goal = Goal,
         time
      ),
      list(
         teamId
      )
   ]
   dtXg[, CumGoals := cumsum(Goal), teamId]

   dtXg[, ActualEvent := T ]
   dtXg = rbind(
      dtXg,
      dtXg[, .SD[which.max(time)], teamId][, ActualEvent := time == max(time)][time != max(time)][, time := dtXg[,max(time)]]
   )

   dtXg = rbind(
      dtXg,
      dtXg[, list(time = 0, xG = 0, CumXg = 0, Goal = F, CumGoals = 0, ActualEvent = F), teamId]
   )

   dtXg = merge(
      dtXg[, list(teamId = vcTeamIds), time],
      dtXg,
      c('time','teamId'),
      all = T
   )

   dtXg[, xG := na.locf(xG), teamId]
   dtXg[, CumXg := na.locf(CumXg), teamId]
   dtXg[, CumGoals := na.locf(CumGoals), teamId]
   dtXg[is.na(Goal), Goal := F]
   dtXg[is.na(ActualEvent), ActualEvent := F]

   dtXgLong = melt(
      dtXg,
      id.vars = c('teamId','time','Goal','ActualEvent')
   )

   dtCumXgComparison = dcast(dtXg, time~teamId, value.var = 'CumXg')
   setnames(
      dtCumXgComparison,
      as.character(vcTeamIds),
      c('teamId1','teamId2')
   )

   dtCumXgComparison[, Winner := teamId1 - teamId2]
   # dtCumXgComparison = dtCumXgComparison[time > 0]

   dtGoalsComparison = dcast(dtXg, time~teamId, value.var = 'CumGoals')
   setnames(
      dtGoalsComparison,
      as.character(vcTeamIds),
      c('teamId1','teamId2')
   )
   dtGoalsComparison[, Winner := teamId1 - teamId2]
   # dtGoalsComparison = dtGoalsComparison[time > 0]

   dtComparisons = rbind(
      dtCumXgComparison[order(time), list(Winner, time, variable = 'ComparisonCumXg')],
      dtGoalsComparison[order(time), list(Winner, time, variable = 'ComparisonGoals')]
   )

   dtComparisons[,
      PlottingGroup := cumsum(
         c(
            1,
            abs(
               diff(
                  # ifelse( Winner > 0, 1, ifelse( Winner < 0, -1, 0))
                  round(Winner / ( abs(Winner) + 0.00000001 ), 0)
               )
            ) > 0
         )
      ),
      variable
   ]

   dtComparisons[,
      Nexttime := c(tail(time, -1), max(time)),
      variable
   ]

   dtSimGoalsComparison = rbindlist(
      lapply(
         1:iSimulationIterations,
         function ( i ) {

            dtXg[, SimGoals := runif(.N) < xG]
            dtXg[, CumSimGoals := cumsum(SimGoals), teamId]

            dtSimGoalsComparison = dcast(dtXg, time~teamId, value.var = 'CumSimGoals')
            setnames(
               dtSimGoalsComparison,
               as.character(vcTeamIds),
               c('teamId1','teamId2')
            )
            dtSimGoalsComparison[, Winner := teamId1 - teamId2]
            # dtSimGoalsComparison = dtSimGoalsComparison[time > 0]
            dtSimGoalsComparison[, Iteration := i]

            dtSimGoalsComparison

         }
      )
   )

   dtSimGoalsComparison = dtSimGoalsComparison[,
      .N,
      list(
         time,
         Winner = round(Winner / ( abs(Winner) + 0.00000001 ), 0)
      )
   ]

   dtSimGoalsComparison = merge(
      c,
      dtSimGoalsComparison[,
         list(
            Winner = c(-1, 0, 1)
         ),
         time
      ],
      c('time','Winner'),
      all = T
   )

   dtSimGoalsComparison[is.na(N), N := 0]

   setkey(
      dtSimGoalsComparison,
      time, Winner
   )

   dtSimGoalsComparison[,
      Nexttime := c(tail(time, -1), max(time)),
      Winner
   ]


   dtSimGoalsComparison[,
      CumN := cumsum(N),
      time
   ]

   dtSimGoalsComparison[,
      PrevCumN := c(0, head(CumN, -1)),
      time
   ]

   dtComparisons[
      Winner < 0,
      WinningTeam := vcTeamIds[2]
   ]

   dtComparisons[
      Winner > 0,
      WinningTeam := vcTeamIds[1]
   ]

   dtSimGoalsComparison[
      Winner < 0,
      WinningTeam := vcTeamIds[2]
   ]

   dtSimGoalsComparison[
      Winner > 0,
      WinningTeam := vcTeamIds[1]
   ]

   dtSimGoalsComparison[, variable := 'ComparisonSimWinProbability']

   dtXg[, teamId := factor(teamId, ordered = T, levels = c(NA, vcTeamIds))]
   dtComparisons[, WinningTeam := factor(WinningTeam, ordered = T, levels = c(NA, vcTeamIds))]
   dtSimGoalsComparison[, WinningTeam := factor(WinningTeam, ordered = T, levels = c(NA, vcTeamIds))]
   # dtSimGoalsComparison[time == sort(unique(time))[2]]

   p1 = ggplot() +
      geom_step(
         data = dtXgLong[variable == 'CumXg'],
         aes(
            x = time,
            y = value,
            group = teamId,
            color = factor(teamId)
         )
      ) +
      geom_point(
         data = dtXgLong[ActualEvent == T][variable == 'CumXg'],
         aes(
            x = time,
            y = value,
            group = teamId,
            color = factor(teamId)
         ),
         size = 1,
      ) +
      geom_point(
         data = dtXgLong[ActualEvent == T][variable == 'CumXg'][Goal == T],
         aes(
            x = time,
            y = value,
            group = teamId
         ),
         size = 2,
      ) +
      geom_segment(
         data = dtXgLong[ActualEvent == T][variable == 'xG'],
         aes(
            x = time,
            y = value,
            xend = time,
            yend = 0,
            color = factor(teamId)
         ),
         stat = 'identity'
      ) +
      geom_point(
         data = dtXgLong[ActualEvent == T][variable == 'xG'][Goal == T],
         aes(
            x = time,
            y = value
         ),
         size = 2,
         stat = 'identity'
      ) +
      geom_point(
         data = dtXgLong[ActualEvent == T][variable == 'xG'],
         aes(
            x = time,
            y = value,
            fill = factor(teamId),
            color = factor(teamId)
         ),
         size = 1,
         stat = 'identity'
      ) +
      geom_rect(
         data = dtComparisons,
         aes(
            xmin = time,
            xmax = Nexttime,
            ymin = 0,
            ymax = Winner,
            fill = factor(WinningTeam)
         )
      )  +
      geom_step(
         data = dtComparisons,
         aes(
            x = time,
            y = Winner
         )
      ) +
      geom_rect(
         data = dtSimGoalsComparison,
         aes(
            xmin = time,
            xmax = Nexttime,
            ymin = PrevCumN / iSimulationIterations,
            ymax = CumN / iSimulationIterations,
            fill = factor(WinningTeam)
         )
      ) +
      facet_grid(
         variable~.,
         scale = 'free_y',
         # space = 'free_y'
      ) +
      ylab(NULL)

   if ( !is.null(dtTeamLabels) ) {

      p1 = p1 +
         scale_fill_discrete(
            name = 'Team',
            # values = c('black','grey'),
            breaks = dtTeamLabels[, teamId],
            labels = dtTeamLabels[, teamName]
         ) +
         scale_colour_discrete(
            guide = FALSE
         )

   }

   p1

}
