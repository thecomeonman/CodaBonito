#' Transforming coordinate spaces
#' @param mCoordinates matrix with three columns - x,y,z coorinates.
#' if mCoordinates is an isolated point then it is treated
#' as an isolated point, if it is two points, it is treated as a line segment,
#' but 3 or more points are treated as closed polygons unless bTreatAsClosedPolyon = F.
#' Recommendation is to ensure start point is repeated as end point in mCoordinates
#' and bTreatAsClosedPolyon = F. That way, where the polygon / path went behind the
#' screen, there will be a marker specifying that the line has to be broken there
#' when plotting or anywhere else
#' @param mOrigin three column, one row matrix specifying coordinates of where
#' the scene is being viewed from
#' @param mScreenCoordinate three column, one row matrix specifying coordinates of
#' where the screen on which the scene is being projected on. Also dictates the
#' direction along which which the scene is being viewed from
#' @param bTreatAsClosedPolyon
#' @return a matrix with 2 columns for the x,y coordinates on the screen + an
#' optional column linking continuous stretches in front of the screening plane
#' which can be used in geom_path(aes(group = V3))
#' @import zoo
#' @export
fGetTransformedCoordinates = function (
    mCoordinates,
    mOrigin,
    mScreenCoordinate,
    bTreatAsClosedPolyon = T
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

    # removing repeat points
    if ( nrow(mCoordinates) > 1 ) {

        mCoordinates = mCoordinates[c(T, !rowSums(matrix(apply(mCoordinates, 2, diff), ncol = 3) ^ 2) == 0), ]

        if ( bTreatAsClosedPolyon & nrow(mCoordinates) > 1 ) {

            if (
                all(
                    mCoordinates[1,] == mCoordinates[nrow(mCoordinates),]
                )
            ) {
                mCoordinates = mCoordinates[-nrow(mCoordinates),]
            }

        }

    }

    # mScreenCoordinate = ( ( mOrigin - mScreenCoordinate) * 0.9999 ) + mScreenCoordinate
    # mScreenCoordinate = mScreenCoordinate

    mZAxisVector = c(mScreenCoordinate[,1:2], mScreenCoordinate[,3] + 1)



    # this is the plane on which to project the data
    # normal vector = (a,b,c)
    # a(x - x1) + b(y - y1) + c(z - z1) = 0
    nScreenPlaneCoefficients = c(
        mScreenCoordinate[, 1] - mOrigin[, 1],
        mScreenCoordinate[, 2] - mOrigin[, 2],
        mScreenCoordinate[, 3] - mOrigin[, 3],
        0
        + ( ( mScreenCoordinate[, 1] - mOrigin[, 1] ) * mScreenCoordinate[, 1] )
        + ( ( mScreenCoordinate[, 2] - mOrigin[, 2] ) * mScreenCoordinate[, 2]  )
        + ( ( mScreenCoordinate[, 3] - mOrigin[, 3] ) * mScreenCoordinate[, 3]  )
    )



    # We can't let points all the way till on the screen plane be visualised because
    # the coordinates for them will be ~inf. So we'll only include points which
    # are at least a little ahead of mScreenCoordinate from the direction of mOrigin
    # nDivisionPlaneCoefficients = nScreenPlaneCoefficients, right? Why isn't that working?
    mAnotherDivisionPlaneAxisVector = fCrossProduct(
        mZAxisVector - mScreenCoordinate,
        mScreenCoordinate - mScreenCoordinate
    )
    # if the above two vectors are parallel, i.e. viewing direction is along z axis
    if ( sum(mAnotherDivisionPlaneAxisVector) == 0 ) {
        nDivisionPlaneCoefficients = c(
            nScreenPlaneCoefficients[1:3],
            nScreenPlaneCoefficients[4] - cbind(mScreenCoordinate, 1) %*% nScreenPlaneCoefficients
        )
    } else {

        mAnotherDivisionPlaneAxisVector = ( mAnotherDivisionPlaneAxisVector / sum(mAnotherDivisionPlaneAxisVector ^ 2 ) ^ 0.5 )
        nDivisionPlaneCoefficients = fCrossProduct(mZAxisVector - mScreenCoordinate, mAnotherDivisionPlaneAxisVector)
        nDivisionPlaneCoefficients = c(
            nDivisionPlaneCoefficients,
            -sum(nDivisionPlaneCoefficients * mScreenCoordinate)
        )
    }
    # ... i.e on the other side of the plane as the origin
    bOriginDestinationInPositiveDirection = sum(nDivisionPlaneCoefficients * c(mOrigin, 1)) < 0

    # removing points behind the screen
    # retaining at most two points behind the screen should remain for each stretch of points
    # behind the screen so that closed polygons from the points ahead of the screen can be
    # computed
    vbCoordinatesToTransform = c(
        bOriginDestinationInPositiveDirection == (
            cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
        )
    )

    # return empty dataset if all points are behind screen
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

    # if the start and the end stretches of the coordinates are both behind the polygon
    # then compress them into a single stretch and cycle the polygon coordinates
    # such that the starting coordinate is ahead of the screen
    # THIS CHANGES THE ORDER OF POINTS for closed polygons

    vbCoordinatesToTransform = c(
        bOriginDestinationInPositiveDirection == (
            cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
        )
    )

    viRelativeScreenPositionChunks = cumsum(c(1,abs(diff(vbCoordinatesToTransform))))

    if ( all(!vbCoordinatesToTransform[viRelativeScreenPositionChunks %in% range(viRelativeScreenPositionChunks)]) ) {

        viIndicestoKeep = max(which(viRelativeScreenPositionChunks == 1)):min(which(viRelativeScreenPositionChunks == max(viRelativeScreenPositionChunks)))

        if ( bTreatAsClosedPolyon ) {

            viIndicestoKeep = c(viIndicestoKeep[-1],viIndicestoKeep[1])

        }

        mCoordinates = mCoordinates[viIndicestoKeep, ]

    }

    mCoordinates = matrix(mCoordinates, ncol = 3)

    if ( nrow(mCoordinates) >= 2 ) {

        vbCoordinatesToTransform = c(
            bOriginDestinationInPositiveDirection == (
                cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
            )
        )

        if ( vbCoordinatesToTransform[1] == F & vbCoordinatesToTransform[2] == F & !bTreatAsClosedPolyon ) {

            mCoordinates = mCoordinates[-1,]

        }

    }


    if ( nrow(mCoordinates) >= 2 ) {

        vbCoordinatesToTransform = c(
            bOriginDestinationInPositiveDirection == (
                cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
            )
        )

        if ( vbCoordinatesToTransform[length(vbCoordinatesToTransform)] == F & vbCoordinatesToTransform[length(vbCoordinatesToTransform)-1] == F & !bTreatAsClosedPolyon ) {

            mCoordinates = mCoordinates[-length(mCoordinates), ]

        }

    }

    # interpolating the connecting points between the adjacent behind screen - in front of screen points
    # such that the connecting point is a point on the screen
    # if there are two consecutive behind points then adding a place holder for an inbetween point between the two behind points
    # so that the polygon closes elegantly
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

        # in case it's just one point
        viCoordinatesToTransform = range(viCoordinatesToTransform)

        if ( viCoordinatesToTransform[2] == 1 & !bTreatAsClosedPolyon ) {

            iNextPoint = viCoordinatesToTransform[2] + 1

            vnDistancesFromPlane = cbind(mCoordinates[c(viCoordinatesToTransform[2], iNextPoint),], 1) %*% nDivisionPlaneCoefficients

            mReplacementPoints = mCoordinates[viCoordinatesToTransform[2], ] - ( diff(mCoordinates[c(iNextPoint, viCoordinatesToTransform[2]),]) * abs(vnDistancesFromPlane[1]) / ( abs(vnDistancesFromPlane[1]) + abs(vnDistancesFromPlane[2]) ) )

        } else if ( viCoordinatesToTransform[1] == nrow(mCoordinates) & !bTreatAsClosedPolyon ) {

            iPrevPoint = viCoordinatesToTransform[1] - 1

            vnDistancesFromPlane = cbind(
                mCoordinates[c(iPrevPoint, viCoordinatesToTransform[1]),],
            1) %*% nDivisionPlaneCoefficients

            mReplacementPoints = mCoordinates[viCoordinatesToTransform[1], ] - ( diff(mCoordinates[c(iPrevPoint, viCoordinatesToTransform[1]),]) * abs(vnDistancesFromPlane[2]) / ( abs(vnDistancesFromPlane[1]) + abs(vnDistancesFromPlane[2]) ) )

        } else {

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

        }

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

    # pojrection on screen for points in front
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
                    if ( all(nOtherPoint == mScreenCoordinate ) ) {

                        mSolution = mScreenCoordinate

                    } else if ( all(nOtherPoint == mOrigin ) ) {

                        mSolution = mOrigin

                    } else if (
                        sum(nScreenPlaneCoefficients[1:3] * nOtherPoint) == nScreenPlaneCoefficients[4]
                    ) {

                        mSolution = nOtherPoint

                    } else if (
                        all ( ( nOtherPoint - mOrigin ) * ( mScreenCoordinate - mOrigin ) == 0 )
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

    # rotating projections WRT to screening plane so as to get points in
    # two coordinates
    if ( T ) {

        mYAxisVectorNew = mYAxis - mScreenCoordinate
        if ( all(mYAxisVectorNew == 0)) {
            mYAxisVectorNew = matrix(c(0,1,0), ncol = 3)
        }
        mYAxisVectorNew = mYAxisVectorNew / sum(mYAxisVectorNew^2) ^ 0.5

        mZAxisVectorNew = mScreenCoordinate - mOrigin
        mZAxisVectorNew = mZAxisVectorNew / sum(mZAxisVectorNew^2) ^ 0.5

        mXAxisVectorNew = fCrossProduct(mZAxisVectorNew, mYAxisVectorNew)
        mXAxisVectorNew = mXAxisVectorNew / sum(mXAxisVectorNew^2) ^ 0.5

        mPlaneVectors = mSolutions - matrix( rep(mScreenCoordinate, nrow(mSolutions)), ncol = 3, byrow = T )

        vnYCoord = mPlaneVectors %*% t(mYAxisVectorNew)
        vnXCoord = mPlaneVectors %*% t(mXAxisVectorNew)

    }

    mResult = cbind(
        vnXCoord,
        vnYCoord
    )

    # the placeholders between the two behind points being filled with a value
    # that lies just a little below the lowest point in the viz so it can be
    # chopped off with a ylim
    if ( F ) {

        mResult[
            which(is.na(mResult[,1])),
            2
        ] = min(mResult[,2], na.rm = T) - ( 0.0000001 * diff(range(mResult[,2], na.rm = T)) )

        mResult[
            which(is.na(mResult[,1])),
            1
        ] = 0

    } else {

        if ( !bTreatAsClosedPolyon ) {
            mResult = cbind(mResult, 1)
        }

        viPointsToFillIn = which(is.na(mResult[,1]))

        if ( length(viPointsToFillIn) ) {

            if ( bTreatAsClosedPolyon ) {

                viPointsHowToFillInMax = mResult[viPointsToFillIn - 1, 2] > mean(range(mResult[,2], na.rm = T))

                mResult[viPointsToFillIn[!viPointsHowToFillInMax], 2] =
                    min(mResult[,2], na.rm = T) -
                    # ( 0.0000001 * diff(range(mResult[,2], na.rm = T)) )
                    0

                mResult[viPointsToFillIn[viPointsHowToFillInMax], 2] =
                    max(mResult[,2], na.rm = T) +
                    # ( 0.0000001 * diff(range(mResult[,2], na.rm = T)) )
                    0

                mResult[viPointsToFillIn, 1] = ( mResult[viPointsToFillIn - 1, 1] + mResult[viPointsToFillIn + 1, 1] ) / 2

            } else {

                mResult = cbind(mResult[,1:2], cumsum(is.na(mResult[,1])))
                mResult = mResult[-viPointsToFillIn,]
            }
        }

    }

    row.names(mResult) = NULL

    mResult

}
