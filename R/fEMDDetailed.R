#' emdist:emd is quite restrictive. A more libral alternative.
#'
#' So long as you can calculate a distance matrix between the two datasets, you
#' can use this function to calculate the earth mover's distance between them. 
#' This lets you calculate EMD beteen any two datasets, unlike emdist::emd 
#' only lets compare numberic datasets of up to four dimensions.
#' This additionally gives you the weights when transforming one dataset to 
#' another so you can make more detailed inferences about which data is 
#' contributing the most to the distances, etc.
#' 
#' The output is an lpExtPtr type of object. The lpSolveAPI library has many 
#' operations that you can perform on such an object.
#'
#' @param SNO1, index of the obervation number from dataset one,
#' eg. c(1,2,3,4,1,2,3,4,1,2,3,4)
#' @param SNO2, index of the obervation number from dataset one,
#' eg. c(1,1,1,1,2,2,2,2,3,3,3,3)
#' @param Distance The distance between the data associated with the 
#' respective SNO1 and SNO2 values,
#' @examples
#' # Two random datasets of three dimension
#' a = data.table(matrix(runif(21), ncol = 3))
#' b = data.table(matrix(runif(30), ncol = 3))
#' # adding serial numbers to each observation
#' a[, SNO := .I]
#' b[, SNO := .I]
#' # evaluating distance between all combinations of data in the two datasets
#' a[, k := 'k']
#' b[, k := 'k']
#' dtDistances = merge(a,b,'k',allow.cartesian = T)
#' dtDistances[,
#'    Distance := (
#'       (( V1.x - V1.y) ^ 2) +
#'       (( V2.x - V2.y) ^ 2) +
#'       (( V3.x - V3.y) ^ 2)
#'    ) ^ 0.5
#' ]
#' # getting EMD between this dataet
#' lprec = fEMDDetailed(
#'    SNO1 = dtDistances[, SNO.x],
#'    SNO2 = dtDistances[, SNO.y],
#'    Distance = dtDistances[, Distance]
#' )
#' fGetEMDFromDetailedEMD(lprec)
#' # This value should be the same as that computed by EMD
#' # EMD needs the weightage of each point, which is assigned as equal in our 
#' # function, so giving 1/N weightage to each data point
#' emdist::emd(
#'    as.matrix(
#'       a[, list(1/.N, V1,V2,V3)]
#'    ),
#'    as.matrix(
#'       b[, list(1/.N, V1,V2,V3)]
#'    )
#' )
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
