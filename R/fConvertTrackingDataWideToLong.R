#' Converts tracking data from wide to long
#'
#' Very tightly tied to how https://github.com/metrica-sports/sample-data is
#' organised right now since I don't know how tracking data is otherwise
#' organised
#'
#' @param dtData
#' @examples
#' @import data.table
#' @export
fConvertTrackingDataWideToLong = function(
   dtData
) {

   if ( F ) {

      dtAllData = copy(dtData)

      dtData = melt(
         dtData[,
            c(
               c('Period','Frame','Time_s'),
               grep(
                   colnames(dtData),
                   pattern = 'Player',
                   value = T
               )
            ),
            with = F
         ],
         intersect(
            c('Period','Frame','Time_s'),
            colnames(dtData)
         )
      )

   } else {

      dtData = melt(
         dtData,
         intersect(
            colnames(dtData),
            c('Period','Frame','Time_s','Team','Type','Subtype','From','To')
         )
      )

   }

   dtData = dtData[!is.na(value)]

   dtData[,
      Coordinate := gsub(
         x = variable,
         pattern = '.*Player[[:digit:]]*|.*Ball[[:digit:]]*',
         replacement = ''
      )
   ]

   dtData[,
      Tag := gsub(
         x = variable,
         pattern = 'Ball.*|Player.*',
         replacement = ''
      )
   ]


   dtData[,
       Player := gsub(
         x = variable,
         pattern = paste0(paste0(unique(Coordinate), '$*'), collapse = '|'),
         replacement = ''
      )
   ]

   dtData[
      grepl(
         Player,
         pattern = 'Ball'
      ),
      c('Player','Tag') := list('Ball', 'Ball')
   ]

   dtData[, variable := NULL]

   dtData = merge(
      dtData[
         1,
         intersect(
            colnames(dtData),
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
         dtData[,
            intersect(
               colnames(dtData),
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
                     colnames(dtData),
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
         colnames(dtData),
         c('Period', 'Frame', 'Time_s')
      ))
   )

   dtData

}
