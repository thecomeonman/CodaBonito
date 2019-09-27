#' @import lpSolveAPI
#' @export
fGetEMDFromDetailedEMD = function (
   lprec
) {

   get.objective(lprec) / sum(get.variables(lprec))

}
