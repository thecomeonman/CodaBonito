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
fParseTrackingData = function (
   cFilePath,
   cTag = ''
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

   setnames(
      dtRawData,
      vcColnames
   )

   dtRawData

}
