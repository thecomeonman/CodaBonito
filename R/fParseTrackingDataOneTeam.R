#' Pass network ( WIP )
#'
#' Plots a marker for each player at their median passing position, and draws
#' connections between players to represent the number of passes exchanged
#' between them
#'
#' @param cFilePath
#' @param cTag
#' @examples
#' @import data.table
#' @import zoo
#' @export
fParseTrackingDataOneTeam = function (
   cFilePath,
   cTag = '',
   nXSpan = 120,
   nYSpan = 80,
   xMaxBB = 1,
   yMaxBB = 1
) {

   dtRawData = fread(
      cFilePath,
      skip = 3,
      header = F
   )

   dtMetaData = fread(
      cFilePath,
      skip = 2,
      nrows = 1,
      header = F
   )

   vcColnames = na.locf(unlist(dtMetaData))
   rm(dtMetaData)
   vcColnames[vcColnames == 'Time [s]'] = 'Time_s'
   vcColnames[
      grepl(
         vcColnames,
         pattern = 'Player|Ball'
      )
   ] = paste0(
      cTag,
      paste0(
         grep(
            vcColnames[
               grepl(
                  vcColnames,
                  pattern = 'Player|Ball'
               )
            ],
            pattern = 'Player|Ball',
            value = T
         ),
         c('X','Y')
      )
   )

   vcColnames = gsub(
      vcColnames,
      pattern = 'Ball',
      replacement = 'Player0'
   )

   setnames(
      dtRawData,
      vcColnames
   )

   for ( cColname in colnames(dtRawData) ) {

      if ( grepl(cColname, pattern = 'X$') ) {

         dtRawData[,
            (cColname) := ( dtRawData[, cColname, with = F] * nXSpan / xMaxBB ) - ( nXSpan * 0.5 )
         ]

      }

      # y coordinates are flipped
      if ( grepl(cColname, pattern = 'Y$') ) {

         dtRawData[,
            (cColname) := ( ( yMaxBB - dtRawData[, cColname, with = F] )  * nYSpan / yMaxBB ) - ( nYSpan * 0.5 )
         ]

      }

   }

   dtRawData

}
