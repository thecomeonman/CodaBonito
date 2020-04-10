#' Gives cleanly parsed data
#'
#' Very tightly tied to how https://github.com/metrica-sports/sample-data is
#' organised right now since I don't know how tracking data is otherwise
#' organised
#'
#' @param cRootPath
#' @examples
#' @import data.table
#' @export
fParseTrackingDataBothTeams = function (
   cRootPath,
   cGameName,
   bOutputLong,
   nXLimit = 120,
   nYLimit = 80,
   xMaxBB = 1,
   yMaxBB = 1
) {

   dtTrackingData = merge(
      fParseTrackingDataOneTeam(
         paste0(
            cRootPath, '/',
            cGameName, '/',
            cGameName,
            '_RawTrackingData_Away_Team.csv'
         ),
         cTag = 'Away',
         nXLimit = 120,
         nYLimit = 80,
         xMaxBB = 1,
         yMaxBB = 1
      ),
      fParseTrackingDataOneTeam(
         paste0(
            cRootPath, '/',
            cGameName, '/',
            cGameName,
            '_RawTrackingData_Home_Team.csv'
         ),
         cTag = 'Home',
         nXLimit = 120,
         nYLimit = 80,
         xMaxBB = 1,
         yMaxBB = 1
      ),
      c('Period','Frame','Time_s'),
      all = T
   )

   dtEventsData = fread(
      paste0(
         paste0(
            cRootPath, '/',
            cGameName, '/',
            cGameName,
            '_RawEventsData.csv'
         )
      )
   )

   setnames(
      dtEventsData,
      c('Start Frame','Start Time [s]','End Frame','End Time [s]'),
      c('Frame','Time_s','EndFrame','EndTime_s'),
   )

   setnames(
      dtEventsData,
      c('Start X','Start Y','End X','End Y'),
      c('EventStartX','EventStartY','EventEndX','EventEndY'),
   )

   dtEventsData[, EventStartX := EventStartX * nXLimit / xMaxBB]
   dtEventsData[, EventEndX := EventEndX * nXLimit / xMaxBB]
   dtEventsData[, EventStartY := EventStartY * nYLimit / xMaxBB]
   dtEventsData[, EventEndY := EventEndY * nYLimit / yMaxBB]

   # if ( bOutputLong ) {
   if ( F ) {

      dtData = fConvertTrackingDataWideToLong(
         dtTrackingData
      )

      # dtEventsData = merge(
      #
      # )

   } else {

      dtTrackingData = merge(
         dtEventsData,
         dtTrackingData,
         c('Period','Frame','Time_s'),
         all = T
      )

   }

   dtTrackingData

}
