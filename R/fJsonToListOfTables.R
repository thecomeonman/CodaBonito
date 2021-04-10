#' Primarily to convert statsbomb files to a list of data.tables
#' 
#' The function has been named generically so that tomorrow it can be used 
#' with other json syntaxes as well. As of right now, it has been coded up
#' in a manner that it works with StatsBomb data only.
#' 
#' @param lMatchJson refer example
#' @param vcTagsToExtract refer example
#' @param vcTagsToExtract refer example
#' @param nXLimit Length of the pitch
#' @param nYLimit Breadth of the pitch
#' @examples
#' # read in the raw json as a list
#' lMatchJson = rjson::fromJSON(file = cMatchFileName)
#' 
#' # get one table each for the starting XI, passes, and shots
#' lEvents = fJsonToListOfTables (
#'    lMatchJson = lMatchJson,
#'    vcTagsToExtract = c('Starting XI','Pass','Shot')
#' )
#' @import data.table
#' @export
fJsonToListOfTables = function (
   lMatchJson,
   vcTagsToExtract,
   nXLimit = 120,
   nYLimit = 80
) {

   lEvents = list()

   for ( lEvent in lMatchJson ) {

      if ( lEvent$type$name %in% vcTagsToExtract ) {

         if ( lEvent$type$name %in% 'Starting XI') {

            dtEvent = rbindlist(
               lapply(
                  lEvent[['tactics']][['lineup']],
                  function (lEvent) {

                     fJsonToTabular(
                        lEvent
                     )

                  }
               )
            )

            lEvent[['tactics']][['lineup']] = NULL

         }

         dtEventMore = fJsonToTabular(
            lEvent
         )

         if ( exists('dtEvent')) {

            dtEvent = cbind(
               dtEvent,
               dtEventMore
            )

         } else {

            dtEvent = dtEventMore

         }

         rm(dtEventMore)

         lEvents[[lEvent$type$name]][[length(lEvents[[lEvent$type$name]]) + 1]] = dtEvent

         rm(lEvent)
         rm(dtEvent)

      }

   }


   for ( iEventIndex in seq(length(lEvents)) ) {

      # print(iEventIndex)

      lEvents[[iEventIndex]] = rbindlist(lEvents[[iEventIndex]], fill = T)

      for ( i in grep(colnames(lEvents[[iEventIndex]]), pattern = 'location1', value = T) ) {

         lEvents[[iEventIndex]][, i := as.numeric(as.character(get(i)))]
         setnames(
            lEvents[[iEventIndex]],
            i,
            'delete'
         )
         setnames(
            lEvents[[iEventIndex]],
            'i',
            i
         )
         lEvents[[iEventIndex]][, delete := NULL]

      }

      for ( i in grep(colnames(lEvents[[iEventIndex]]), pattern = 'location2', value = T) ) {

         lEvents[[iEventIndex]][, i := nYLimit - as.numeric(as.character(get(i)))]
         setnames(
            lEvents[[iEventIndex]],
            i,
            'delete'
         )
         setnames(
            lEvents[[iEventIndex]],
            'i',
            i
         )
         lEvents[[iEventIndex]][, delete := NULL]

      }

      if ( names(lEvents)[iEventIndex] == 'Pass' ) {

         lEvents[[iEventIndex]][, pass.angle := as.numeric(as.character(pass.angle))]
         lEvents[[iEventIndex]][, pass.length := as.numeric(as.character(pass.length))]

         # 0 points from defense to offense
         # pi / 2 points from centre to left
         # -pi / 2 or 3pi / 2 points from centre to right
         lEvents[[iEventIndex]][, pass.angle := -pass.angle]

      }

      # todo - add all formations
      if ( names(lEvents)[iEventIndex] == 'Starting XI' ) {

         # Attaching formation position details
         # todo - move to a separate function evnetually
         lEvents[[iEventIndex]][, location1 := as.numeric(NA)]
         lEvents[[iEventIndex]][, location2 := as.numeric(NA)]


         # 433 with one CDM
         viteam.id = lEvents[[iEventIndex]][
            (tactics.formation == '433' &  position.name == 'Center Defensive Midfield'),
            unique(team.id)
         ]

         if ( length(viteam.id) > 0 ) {

            lEvents[[iEventIndex]] = merge(
               lEvents[[iEventIndex]],
               rbindlist(
                  lapply(
                     viteam.id,
                     function(iteam.id) {
                        data.table(
                           team.id = iteam.id,
                           position.name = c(
                              'Goalkeeper',
                              'Right Back','Right Center Back','Left Center Back','Left Back',
                              'Center Defensive Midfield',
                              'Right Center Midfield','Left Center Midfield',
                              'Right Wing','Center Forward', 'Left Wing'
                           ),
                           location1 = c(
                              rep(1 * nXLimit/5, 1),
                              rep(2 * nXLimit/5, 4),
                              rep(3 * nXLimit/5, 1),
                              rep(4 * nXLimit/5, 2),
                              rep(5 * nXLimit/5, 3)
                           ) - ( (nXLimit/5) / 2),
                           location2 = c(
                              ( c(1) - 0.5 ) * ( nYLimit / 1 ),
                              ( c(1:4) - 0.5 ) * ( nYLimit / 4 ),
                              ( c(1) - 0.5 ) * ( nYLimit / 1 ),
                              ( c(1:2) - 0.5 ) * ( nYLimit / 2 ),
                              ( c(1:3) - 0.5 ) * ( nYLimit / 3 )
                           )
                        )
                     }
                  )
               ),
               c('team.id','position.name'),
               suffixes = c('','.new'),
               all.x = T
            )

            lEvents[[iEventIndex]][is.na(location1), location1 := location1.new]
            lEvents[[iEventIndex]][is.na(location2), location2 := location2.new]
            lEvents[[iEventIndex]][, c('location1.new','location2.new') := NULL]

         }



         # 3511
         viteam.id = lEvents[[iEventIndex]][
            (tactics.formation == '3511'),
            unique(team.id)
         ]

         if ( length(viteam.id) > 0 ) {

            lEvents[[iEventIndex]] = merge(
               lEvents[[iEventIndex]],
               rbindlist(
                  lapply(
                     viteam.id,
                     function(iteam.id) {

                        data.table(
                           team.id = iteam.id,
                           position.name = c(
                              'Goalkeeper',
                              'Right Center Back','Center Back','Left Center Back',
                              'Right Wing Back','Right Center Midfield','Center Defensive Midfield','Left Center Midfield','Left Wing Back',
                              'Center Attacking Midfield',
                              'Center Forward'
                           ),
                           location1 = c(
                              rep(1 * nXLimit/5, 1),
                              rep(2 * nXLimit/5, 3),
                              rep(3 * nXLimit/5, 5),
                              rep(4 * nXLimit/5, 1),
                              rep(5 * nXLimit/5, 1)
                           ) - ( (nXLimit/5) / 2),
                           location2 = c(
                              ( c(1:1) - 0.5 ) * ( nYLimit / 1 ),
                              ( c(1:3) - 0.5 ) * ( nYLimit / 3 ),
                              ( c(1:5) - 0.5 ) * ( nYLimit / 5 ),
                              ( c(1:1) - 0.5 ) * ( nYLimit / 1 ),
                              ( c(1:1) - 0.5 ) * ( nYLimit / 1 )
                           )
                        )

                     }
                  )
               ),
               c('team.id','position.name'),
               suffixes = c('','.new'),
               all.x = T
            )

            lEvents[[iEventIndex]][is.na(location1), location1 := location1.new]
            lEvents[[iEventIndex]][is.na(location2), location2 := location2.new]
            lEvents[[iEventIndex]][, c('location1.new','location2.new') := NULL]

         }

      }

   }

   names(lEvents) = make.names(names(lEvents))

   lEvents

}
