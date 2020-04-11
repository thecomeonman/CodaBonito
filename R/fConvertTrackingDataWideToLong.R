#' Converts tracking data from wide to long
#'
#' Very tightly tied to how https://github.com/metrica-sports/sample-data is
#' organised right now since I don't know how tracking data is otherwise
#' organised
#'
#' @param dtTrackingSlice
#' @examples
#' @import data.table
#' @export
fConvertTrackingDataWideToLong = function(
   dtTrackingSlice
) {

   if ( F ) {

      dtAllData = copy(dtTrackingSlice)

      dtTrackingSlice = melt(
         dtTrackingSlice[,
            c(
               c('Period','Frame','Time_s'),
               grep(
                   colnames(dtTrackingSlice),
                   pattern = 'Player',
                   value = T
               )
            ),
            with = F
         ],
         intersect(
            c('Period','Frame','Time_s'),
            colnames(dtTrackingSlice)
         )
      )

   } else {

    #    dtTrackingSlice = copy(dtTrackingSlice)

      dtTrackingSlice = melt(
         dtTrackingSlice,
         intersect(
            colnames(dtTrackingSlice),
            c('Period','Frame','Time_s','Team','Type','Subtype','From','To')
         )
      )

   }

   dtTrackingSlice = dtTrackingSlice[!is.na(value)]

   dtTrackingSlice[,
       Coordinate := gsub(
          x = variable,
          pattern = '.*Player[[:digit:]]*|.*Ball[[:digit:]]*',
          replacement = ''
       )
   ]

   dtTrackingSlice[
       !Coordinate %in% c('X','Y'),
       Coordinate := NA
   ]

   dtTrackingSlice[,
      Tag := gsub(
         x = variable,
         pattern = 'Ball.*|Player.*',
         replacement = ''
      )
   ]

   dtTrackingSlice[
       !Tag %in% c('Home','Away'),
       Tag := NA
   ]

   dtTrackingSlice[,
       Player := gsub(
         x = variable,
         pattern = paste0(paste0(unique(Coordinate), '$*'), collapse = '|'),
         replacement = ''
      )
   ]

   dtTrackingSlice[
        Player %in% c("EndFrame", "EndTime_s"),
        Player := NA
   ]

   dtTrackingSlice[
      grepl(
         Player,
         pattern = 'Ball'
      ),
      c('Player','Tag') := list('Ball', 'Ball')
   ]

   dtTrackingSlice[, variable := NULL]

   dtTrackingSlice = merge(
      dtTrackingSlice[
         1,
         intersect(
            colnames(dtTrackingSlice),
            c(
               'Period',
               'Frame',
               'Time_s',
               'Team',
               'Type',
               'Subtype',
               'From',
               'To'
            )
         ),
         with = F
      ],
      dcast(
         dtTrackingSlice[
             !is.na(Coordinate),
            intersect(
               colnames(dtTrackingSlice),
               c(
                  'Period',
                  'Frame',
                  'Time_s',
                  'Tag',
                  'Player',
                  'Coordinate',
                  'value'
               )
            ),
            with = F
         ],
         as.formula(
            paste0(
               paste0(
                  intersect(
                     colnames(dtTrackingSlice),
                     c('Period','Frame','Time_s','Tag','Player')
                  ),
                  collapse = '+'
               ),
               '~ Coordinate'
            )
         ),
         value.var = 'value',
         fun.aggregate = mean
      ),
      c(intersect(
         colnames(dtTrackingSlice),
         c('Period', 'Frame', 'Time_s')
      )),
      all = T
   )

   dtTrackingSlice

}
