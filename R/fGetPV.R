#' Uses Laurie Shaw's PV grid by default
#' @import data.table
#' @export
fGetPV = function(
   vnX,
   vnY,
   nXSpan = 120,
   nYSpan = 80,
   dtEPV = NULL
) {

   if ( is.null(dtEPV) ) {

      dtEPV = fread(
         'https://raw.githubusercontent.com/Friends-of-Tracking-Data-FoTD/LaurieOnTracking/master/EPV_grid.csv',
         header = F
      )

   }

   dtEPV[, y := .I]
   dtEPV = melt(dtEPV, 'y')
   setnames(dtEPV, 'variable','x')
   dtEPV[, x := gsub(x, pattern = 'V', replacement = '')]
   dtEPV[, x := as.integer(x)]

   # slightly corrupting the grid for better interpolation
   dtEPV[, x := x - min(x)]
   dtEPV[, y := y - min(y)]
   dtEPV[, x := x * ( nXSpan - (1/(5*nXSpan)) )  / max(x)]
   dtEPV[, y := y * ( nYSpan - (1/(5*nYSpan)) ) / max(y)]

   qwe = interp(
      dtEPV[,x],
      dtEPV[,y],
      dtEPV[,value],
      xo = seq(0, nXSpan - (1/(5*nXSpan)), length = ( 5 * nXSpan )),
      yo = seq(0, nYSpan - (1/(5*nYSpan)), length = ( 5 * nYSpan )),
      extrap = T,
   )

   dtEPV = data.table(t(qwe$z))

   vnEPVGridX = seq(0, nXSpan - (1/(5*nXSpan)), nXSpan/(ncol(dtEPV)))
   vnEPVGridY = seq(0, nYSpan - (1/(5*nYSpan)), nYSpan/(nrow(dtEPV)))

   # vnEPVGridX[1] = vnEPVGridX[1] - 0.000000001
   # vnEPVGridY[1] = vnEPVGridY[1] - 0.000000001

   vnX = vnX + (nXSpan/2)
   vnY = vnY + (nYSpan/2)

   vnXIndex = 1 + ( vnX %/% (nXSpan/(ncol(dtEPV))) )
   vnXIndex[vnXIndex <= 0] = 1L
   vnXIndex[vnXIndex > ncol(dtEPV)] = ncol(dtEPV)
   vnYIndex = 1 + ( vnY %/% (nYSpan/nrow(dtEPV)) )
   vnYIndex[vnYIndex <= 0] = 1L
   vnYIndex[vnYIndex > nrow(dtEPV)] = nrow(dtEPV)

   dtEPV[, Y := .I]
   dtEPV = melt(dtEPV, 'Y')
   dtEPV[, X := as.integer(gsub(variable, pattern = 'V', replacement = ''))]

   merge(
      data.table(X = vnXIndex, Y = vnYIndex)[, SNO := .I],
      dtEPV,
      c('X','Y'),
      all.x = T
   )[order(SNO), value]

}
