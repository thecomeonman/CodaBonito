#' Helper function to fJsonToListOfTables
#'
#' Breaks individual json chunks into tables, takes care of duplicate 
#' column names.
#'
#' @import data.table
fJsonToTabular = function(
   lEvent
) {

   dtEvent = data.table(
      t(unlist(lEvent))
   )

   vcNewColnames = colnames(dtEvent)
   
   vcNewColnames = sapply(
      seq(length(vcNewColnames)),
      function ( iNewColnameIndex ) {
         
         cNewColname = vcNewColnames[iNewColnameIndex]
         
         if ( 
            sum(
               cNewColname == vcNewColnames[1:iNewColnameIndex]
            ) > 1
         ) {
            
            cNewColname = paste0(
               cNewColname,
               '.',
               formatC(
                  width = 2,
                  flag = '0',
                  sum(
                     cNewColname == vcNewColnames[1:iNewColnameIndex]
                  )
               )
            )
            
         }
         
         cNewColname
         
      }
   )

   setnames(
      dtEvent,
      vcNewColnames
   )

}