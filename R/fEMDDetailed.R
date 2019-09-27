#' @import data.table
#' @import lpSolveAPI
#' @export
fEMDDetailed = function(
   SNO1,
   SNO2,
   Distance
) {

   library(data.table)

   # dtMatrix = dtMatrix[, list(SNO1, SNO2, Distance)]
   dtMatrix = data.table(SNO1, SNO2, Distance)

   lprec = make.lp(
      nrow = dtMatrix[, length(unique(SNO1))] + dtMatrix[, length(unique(SNO2))] + nrow(dtMatrix),
      ncol = nrow(dtMatrix)
   )

   nScaleUpFactor = dtMatrix[, length(unique(SNO1))] / dtMatrix[, length(unique(SNO2))]

   for ( iSNO1 in dtMatrix[, unique(SNO1) ]) {

      add.constraint(
         lprec,
         xt = rep(
            1,
            dtMatrix[, sum(iSNO1 == SNO1)]
         ),
         type = c("="),
         rhs = ifelse(
            nScaleUpFactor < 1,
            1 / nScaleUpFactor,
            1
         ),
         indices = dtMatrix[, which(iSNO1 == SNO1)]
      )

   }

   for ( iSNO2 in dtMatrix[, unique(SNO2) ]) {

      add.constraint(
         lprec,
         xt = rep(
            1,
            dtMatrix[, sum(iSNO2 == SNO2)]
         ),
         type = c("="),
         rhs = ifelse(
            nScaleUpFactor < 1,
            1,
            nScaleUpFactor
         ),
         indices = dtMatrix[, which(iSNO2 == SNO2)]
      )

   }

   for ( iRow in dtMatrix[, seq(.N) ]) {

      add.constraint(
         lprec,
         xt = 1,
         type = c(">="),
         rhs = 0,
         indices = iRow
      )

   }

   set.type(lprec, c(1:nrow(dtMatrix)), type = c("real"))

   set.objfn(
      lprec,
      dtMatrix[, Distance]
   )

   solve(lprec)


   lprec

}
