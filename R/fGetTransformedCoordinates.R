#' @import zoo
#' @export
fGetTransformedCoordinates = function (
    mCoordinates,
    mOrigin,
    mOriginDirection
) {


   fCrossProduct = function(mZAxisVectorNew, mYAxisVectorNew) {

       matrix(
           c(
               ( mZAxisVectorNew[2] * mYAxisVectorNew[3] ) - ( mZAxisVectorNew[3] * mYAxisVectorNew[2] ),
               ( mZAxisVectorNew[3] * mYAxisVectorNew[1] ) - ( mZAxisVectorNew[1] * mYAxisVectorNew[3] ),
               ( mZAxisVectorNew[1] * mYAxisVectorNew[2] ) - ( mZAxisVectorNew[2] * mYAxisVectorNew[1] )
           ),
           ncol = 3
       )

   }

    # mCoordinates = cbind(
    #     # x = c( 0, -1, 1,-1, 1,-1, 1),
    #     # y = c( 5,  1, 1, 0, 0, 0, 0),
    #     # z = c( 2,  0, 0, 1, 1, 5, 5)
    #     x = c( 0, -2,  0, 2 ),
    #     y = c( 5,  0, -2, 0 ),
    #     z = c( 2,  0,  2, 0 )
    # )

    # mOrigin = cbind(0,2,2)
    # mOriginDirection = cbind(0,0,0)

    mTruncatingOrigin = ( ( mOrigin - mOriginDirection) * 0.9999 ) + mOriginDirection
    mTruncatingOrigin = mOriginDirection

    mZAxisVector = c(mOriginDirection[,1:2], mOriginDirection[,3] + 1)

    # normal vector = (a,b,c)
    # a(x - x1) + b(y - y1) + c(z - z1) = 0
    nScreenPlaneCoefficients = c(
        mOriginDirection[, 1] - mOrigin[, 1],
        mOriginDirection[, 2] - mOrigin[, 2],
        mOriginDirection[, 3] - mOrigin[, 3],
        0
        + ( ( mOriginDirection[, 1] - mOrigin[, 1] ) * mOriginDirection[, 1] )
        + ( ( mOriginDirection[, 2] - mOrigin[, 2] ) * mOriginDirection[, 2]  )
        + ( ( mOriginDirection[, 3] - mOrigin[, 3] ) * mOriginDirection[, 3]  )
    )

    # points which lie on the other side of truncating origin -> origin direction
    mAnotherDivisionPlaneAxisVector = fCrossProduct(
        mZAxisVector - mOriginDirection,
        mTruncatingOrigin - mOriginDirection
    )
    # if the above two vectors are parallel, i.e. viewing direction is along z axis
    if ( sum(mAnotherDivisionPlaneAxisVector) == 0 ) {
        nDivisionPlaneCoefficients = c(
            nScreenPlaneCoefficients[1:3],
            nScreenPlaneCoefficients[4] - cbind(mTruncatingOrigin, 1) %*% nScreenPlaneCoefficients
        )
    } else {

        mAnotherDivisionPlaneAxisVector = ( mAnotherDivisionPlaneAxisVector / sum(mAnotherDivisionPlaneAxisVector ^ 2 ) ^ 0.5 )
        nDivisionPlaneCoefficients = fCrossProduct(mZAxisVector - mOriginDirection, mAnotherDivisionPlaneAxisVector)
        nDivisionPlaneCoefficients = c(
            nDivisionPlaneCoefficients,
            -sum(nDivisionPlaneCoefficients * mTruncatingOrigin)
        )
    }

    bOriginDestinationInPositiveDirection = sum(nDivisionPlaneCoefficients * c(mOrigin, 1)) < 0

    # removing points behind the screen with adjacent points also behind the screen
    # at most two points behind the screen should remain for each stretch of points
    # behind the screen
    vbCoordinatesToTransform = c(
        bOriginDestinationInPositiveDirection == (
            cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
        )
    )

    if ( all(!vbCoordinatesToTransform) ) {
        return ( mOrigin[0,] )
    }

    viRelativeScreenPositionChunks = cumsum(c(1,abs(diff(vbCoordinatesToTransform))))

    mCoordinates = mCoordinates[
        setdiff(
            seq(nrow(mCoordinates)),
            unlist(c(lapply(
                unique(viRelativeScreenPositionChunks[vbCoordinatesToTransform == F]),
                function( iChunk ) {

                    viIndices = which(viRelativeScreenPositionChunks == iChunk)

                    if ( length(viIndices) > 2 ) {
                        viIndicesToRemove = setdiff(viIndices, range(viIndices) )
                    } else {
                        viIndicesToRemove = -1
                    }

                    viIndicesToRemove

                }
            )))
        ),
    ]

    mCoordinates = matrix(mCoordinates, ncol = 3)

    vbCoordinatesToTransform = c(
        bOriginDestinationInPositiveDirection == (
            cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
        )
    )


    viRelativeScreenPositionChunks = cumsum(c(1,abs(diff(vbCoordinatesToTransform))))

    # if start and end of the sequence of coordinates are both behind the screen then special handling
    # because the logic becloe won't realise it's one chunk
    if ( all(!vbCoordinatesToTransform[viRelativeScreenPositionChunks %in% range(viRelativeScreenPositionChunks)]) ) {

        viIndicestoKeep = max(which(viRelativeScreenPositionChunks == 1)):min(which(viRelativeScreenPositionChunks == max(viRelativeScreenPositionChunks)))
        viIndicestoKeep = c(viIndicestoKeep[-1],viIndicestoKeep[1])
        mCoordinates = mCoordinates[viIndicestoKeep, ]

    }


    repeat {

        vbCoordinatesToTransform = c(
            bOriginDestinationInPositiveDirection == (
                cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
            )
        )

        if ( all(vbCoordinatesToTransform[!is.na(vbCoordinatesToTransform)]) ) {
            break
        }

        viRelativeScreenPositionChunks = cumsum(
            c(
                1,
                abs(
                    diff(
                        # sapply(
                        #     vbCoordinatesToTransform,
                        #     function(x) ifelse(is.na(x),-1,x)
                        # )
                        zoo::na.locf(vbCoordinatesToTransform)
                    ) != 0
                )
            )
        )
        viCoordinatesToTransform = table(viRelativeScreenPositionChunks[!vbCoordinatesToTransform])
        viCoordinatesToTransform = which(
            viRelativeScreenPositionChunks == as.integer(names(viCoordinatesToTransform)[1])
        )
        # in caise it's just one point
        viCoordinatesToTransform = range(viCoordinatesToTransform)

        iPrevPoint = viCoordinatesToTransform[1] - 1
        iNextPoint = viCoordinatesToTransform[2] + 1

        iPrevPoint[iPrevPoint == 0] = nrow(mCoordinates)
        iNextPoint[iNextPoint == nrow(mCoordinates) + 1] = 1

        vnDistancesFromPlane = cbind(mCoordinates[c(iPrevPoint, viCoordinatesToTransform, iNextPoint),], 1) %*% nDivisionPlaneCoefficients

        mReplacementPoints = rbind(
            mCoordinates[viCoordinatesToTransform[1], ] - ( diff(mCoordinates[c(iPrevPoint, viCoordinatesToTransform[1]),]) * abs(vnDistancesFromPlane[2]) / ( abs(vnDistancesFromPlane[1]) + abs(vnDistancesFromPlane[2]) ) ),
            cbind(NA,NA,NA),
            mCoordinates[viCoordinatesToTransform[2], ] - ( diff(mCoordinates[c(iNextPoint, viCoordinatesToTransform[2]),]) * abs(vnDistancesFromPlane[3]) / ( abs(vnDistancesFromPlane[3]) + abs(vnDistancesFromPlane[4]) ) )
        )

        if ( viCoordinatesToTransform[1] == 1 ) {

            mCoordinates = rbind(
                mReplacementPoints,
                mCoordinates[-viCoordinatesToTransform, ]
            )

        } else if ( viCoordinatesToTransform[2] == length(vbCoordinatesToTransform) ) {

            mCoordinates = rbind(
                mCoordinates[-viCoordinatesToTransform, ],
                mReplacementPoints
            )

        } else {

            mCoordinates = rbind(
                mCoordinates[1:(viCoordinatesToTransform[1]-1), ],
                mReplacementPoints,
                mCoordinates[(viCoordinatesToTransform[2]+1):nrow(mCoordinates), ]
            )

        }

    }

    # solutions for points in front
    if ( T ) {

        # adding a vertical vector for knowing which way points up
        # can parameterise this also maybe
        mCoordinates = rbind(
            mCoordinates,
            mZAxisVector
        )

        mLHSBase = matrix(nScreenPlaneCoefficients[1:3], ncol = 3)
        mRHSBase = matrix(nScreenPlaneCoefficients[4], ncol = 1)
        mSolutions = matrix(c(0,0,0), ncol = 3)

        for ( iCoordinatesRow in seq(nrow(mCoordinates)) ) {

            if ( iCoordinatesRow == 1 + length(vbCoordinatesToTransform) ) {
                bEvaluate = T
            } else {
                bEvaluate = vbCoordinatesToTransform[iCoordinatesRow]
            }

            bEvaluate = ifelse(is.na(bEvaluate), F, bEvaluate)

            if ( bEvaluate ) {

                nOtherPoint = mCoordinates[iCoordinatesRow,]

                if ( all(!is.na(nOtherPoint))) {
                    if ( all(nOtherPoint == mOriginDirection ) ) {

                        mSolution = mOriginDirection

                    } else if ( all(nOtherPoint == mOrigin ) ) {

                        mSolution = mOrigin

                    } else if (
                        sum(nScreenPlaneCoefficients[1:3] * nOtherPoint) == nScreenPlaneCoefficients[4]
                    ) {

                        mSolution = nOtherPoint

                    } else if (
                        all ( ( nOtherPoint - mOrigin ) * ( mOriginDirection - mOrigin ) == 0 )
                    ) {

                        mSolution = cbind(NA, NA, NA)

                    } else {

                        mLHS = mLHSBase
                        mRHS = mRHSBase

                        viCoefficientsToIncorporate = which(nOtherPoint != mOrigin)

                        for ( iCoeff in setdiff(1:3, viCoefficientsToIncorporate) ) {

                            mLHSIncrement = matrix(c(0,0,0), ncol = 3)
                            mLHSIncrement[iCoeff] = 1

                            mRHSIncrement = mOrigin[,iCoeff]

                            mLHS = rbind(
                                mLHS,
                                mLHSIncrement
                            )

                            mRHS = rbind(
                                mRHS,
                                mRHSIncrement
                            )

                        }

                        if ( length(viCoefficientsToIncorporate) >= 2 ) {

                            mCoeffCombinations = t(combn(viCoefficientsToIncorporate, 2))

                            for ( iCoefficientCombnRow in seq(pmin(2,nrow(mCoeffCombinations))) ) {

                                mLHSIncrement = matrix(c(0,0,0), ncol = 3)

                                iIndex1 = mCoeffCombinations[iCoefficientCombnRow, 1]
                                iIndex2 = mCoeffCombinations[iCoefficientCombnRow, 2]

                                # ( x - x0 ) / a = ( y - y0)  / b
                                # a = x1 - x0
                                # b = y1 - y0
                                # bx - ay = bx0 - ay0
                                b = ( mOrigin[,iIndex2] - nOtherPoint[iIndex2] )
                                a = ( mOrigin[,iIndex1] - nOtherPoint[iIndex1] )
                                mLHSIncrement[iIndex1] = + b
                                mLHSIncrement[iIndex2] = - a

                                mRHSIncrement = (
                                    ( + b * ( nOtherPoint[iIndex1] ) ) +
                                    ( - a * ( nOtherPoint[iIndex2] ) )
                                )

                                mLHS = rbind(
                                    mLHS,
                                    mLHSIncrement
                                )

                                mRHS = rbind(
                                    mRHS,
                                    mRHSIncrement
                                )

                            }

                        }

                        mSolution = t(solve(mLHS, mRHS))

                    }

                } else {

                    mSolution = cbind(NA, NA, NA)

                }

            } else {

                mSolution = cbind(NA, NA, NA)

            }

            mSolutions = rbind(
                mSolutions,
                mSolution
            )


        }

        mSolutions = mSolutions[-1,]
        mYAxis = mSolutions[nrow(mSolutions), ]
        mSolutions = matrix(mSolutions[-nrow(mSolutions), ], ncol = 3)
        mCoordinates = matrix(mCoordinates[-nrow(mCoordinates), ], ncol = 3)

    }

    # mCoordinates
    # mSolutions
    # mYAxis

    # projections
    if ( T ) {

        mYAxisVectorNew = mYAxis - mOriginDirection
        if ( all(mYAxisVectorNew == 0)) {
            mYAxisVectorNew = matrix(c(0,1,0), ncol = 3)
        }
        mYAxisVectorNew = mYAxisVectorNew / sum(mYAxisVectorNew^2) ^ 0.5

        mZAxisVectorNew = mOriginDirection - mOrigin
        mZAxisVectorNew = mZAxisVectorNew / sum(mZAxisVectorNew^2) ^ 0.5

        mXAxisVectorNew = fCrossProduct(mZAxisVectorNew, mYAxisVectorNew)
        mXAxisVectorNew = mXAxisVectorNew / sum(mXAxisVectorNew^2) ^ 0.5

        mPlaneVectors = mSolutions - matrix( rep(mOriginDirection, nrow(mSolutions)), ncol = 3, byrow = T )

        vnYCoord = mPlaneVectors %*% t(mYAxisVectorNew)
        vnXCoord = mPlaneVectors %*% t(mXAxisVectorNew)

    }

    mResult = cbind(
        vnXCoord,
        vnYCoord
    )

    mResult[
        which(is.na(mResult[,1])),
        2
    ] = min(mResult[,2], na.rm = T) - ( 0.0000001 * diff(range(mResult[,2], na.rm = T)) )

    mResult[
        which(is.na(mResult[,1])),
        1
    ] = 0

    row.names(mResult) = NULL

    mResult

}
