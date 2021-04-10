#' Refer to fEMDDetailed example
#' @import lpSolveAPI
#' @export
fGetEMDFromDetailedEMD = function (
   lprec
) {

   get.objective(lprec) / sum(get.variables(lprec))

}
