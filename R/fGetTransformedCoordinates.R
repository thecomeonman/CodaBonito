#' Function to easily transform coordinate spaces
#'
#' @param mCoordinates matrix with three columns [x,y,z] with >= one rows
#' @param mOriginCoordinates three column, one row matrix specifying coordinates
#' of where the scene is being viewed from
#' @param mScreenCoordinates three column, one row matrix specifying coordinates
#' of the screen on which the scene is being projected on. The function
#' calculates a plane which is perpendicular to the vector from mOriginCoordinates
#' to mScreenCoordinates which contains mScreenCoordinates. This plane is the
#' screen on which things are projected finally.
#' @param iTreatAs 1 for isolated points, 2 for path, 3 for polygon. This matters
#' if you have data going behind the mOriginCoordinates / mViewBeginsFromCoordinates
#' as explained in mViewBeginsFromCoordinates.
#' @param mVectorPointingUpOnScreen Which way is up for the viewer
#' @param mViewBeginsFromCoordinates if NULL, all objects in front of origin, where
#' front is the direciton in which the screen coordinates are, are projected. If
#' a set of coordinates is given then all objects in front of that coordinate, where
#' front is the direciton in which the screen coordinates are, are retained, and
#' other objects are treated as behind the view. If this coordinate is on the opposite
#' side of the origin as the screen then you'll delete all objects and get an empty
#' array in the results. This dividing plane and iTreatAs together also affect
#' how points are treated when data is crossing the dividing plane. Points behind the
#' dividing plane are just deleted but paths / polygons get an interpolated coordinate
#' on the dividing plane which helps retain a continuity to the data when plotted
#' @return If inputs are valid, a matrix with 2 columns for the x,y coordinates
#' on the screen, an optional column, group: linking continuous stretches in front of
#' the screening plane which can be used in geom_path(aes(group = group)), an optional
#' column inputOrder: which lets you map the output back to the data that was sent
#' in. If invalid inputs then you'll get a NULL.
#' @example
#' # standing two units away from the screen and looking at a square of side length 2 units, placed parallel to the screen, one unit away from the screen
#' fGetTransformedCoordinates(
#'   mCoordinates = rbind(cbind(1,1,1), cbind(1,-1,1), cbind(-1,-1,1), cbind(-1,1,1)), # a square of side length 2 on the XY plane,
#'   mOriginCoordinates = cbind(0,0,2), # a point on the Z axis
#'   mScreenCoordinates = cbind(0,0,0), # another point on the Z axis
#'   mViewBeginsFromCoordinates = NULL,
#'   mVectorPointingUpOnScreen = c(0,1,0), # looking along the Z axis so
#'   iTreatAs = 3 # treat as a
#' )
#' @export
fGetTransformedCoordinates = function (
   mCoordinates,
   mOriginCoordinates,
   mScreenCoordinates,
   mViewBeginsFromCoordinates = NULL,
   mVectorPointingUpOnScreen = c(0,0,1),
   iTreatAs = 3
) {

   # browser()
   # some input parameters that need to be pre computed
   if ( T ) {


      # Getting the shadow coordinates
      # mYAxisVectorOnScreen = fGetProjectionsOnPlane(
      #     mScreenCoordinates + t(cbind(mVectorPointingUpOnScreen)),
      #     mOriginCoordinates,
      #     fGetPlaneAt(
      #         mOriginCoordinates = mScreenCoordinates,
      #         mNormalVector = mOriginCoordinates - mScreenCoordinates
      #     )
      # )
      # mYAxisVectorOnScreen = mYAxisVectorOnScreen / sum(mYAxisVectorOnScreen ^ 2 )

      # This is needed only for y axis of final projection
      mYAxisVectorOnScreen = mScreenCoordinates + ( mVectorPointingUpOnScreen / ( sum(mVectorPointingUpOnScreen^2) ^ 0.5 ) )


      # this is the plane on which to project the data
      # normal vector = (a,b,c)
      # a(x - x1) + b(y - y1) + c(z - z1) = 0
      # This ia plane perpendicular to the origin - screen vector with
      # the screen coordinates being one of the points on this plane
      nScreenPlaneCoefficients = c(
         mScreenCoordinates - mOriginCoordinates,
         sum(
            ( mScreenCoordinates - mOriginCoordinates ) * mScreenCoordinates
         )
      )


      # We can't let points all the way till on the screen plane be visualised because
      # the coordinates for them will be ~inf. So we'll only include points which
      # are at least a little ahead of mScreenCoordinates from the direction of mOriginCoordinates#
      mVectorInDividingPlane = fCrossProduct(
         mScreenCoordinates - mOriginCoordinates,
         mVectorPointingUpOnScreen
      )

      if ( is.null(mViewBeginsFromCoordinates) ) {

         nDivisionPlaneCoefficients = fGetPlaneAt(
            mOriginCoordinates = mOriginCoordinates,
            mNormalVector = mScreenCoordinates - mOriginCoordinates
            # nScreenPlaneCoefficients
         )

      } else {

         nDivisionPlaneCoefficients = fGetPlaneAt(
            mOriginCoordinates = mViewBeginsFromCoordinates,
            mNormalVector = mScreenCoordinates - mViewBeginsFromCoordinates
            # nScreenPlaneCoefficients
         )

      }

      bOriginDestinationInPositiveDirection = sum(nDivisionPlaneCoefficients * c(mOriginCoordinates, 1)) < -0.00000000001

   }


   # dropping repeat points
   if ( T ) {

      viInputPoints = seq(nrow(mCoordinates))

      # removing repeat points
      if ( nrow(mCoordinates) > 1 & iTreatAs %in% c(2,3) ) {

         viPointsToKeep = c(
            T,
            !rowSums(
               matrix(
                  apply(
                     mCoordinates,
                     2,
                     diff
                  ),
                  ncol = 3
               ) ^ 2
            ) == 0
         )

         viInputPoints = viInputPoints[viPointsToKeep]
         mCoordinates = mCoordinates[viPointsToKeep,]

         if ( iTreatAs == 3 & nrow(mCoordinates) > 1 ) {

            if (
               all(
                  mCoordinates[1,] == mCoordinates[nrow(mCoordinates),]
               )
            ) {
               viInputPoints = viInputPoints[-nrow(mCoordinates)]
               mCoordinates = mCoordinates[-nrow(mCoordinates),]
            }

         }

      }

   }


   # adding points to handle cases where a path or a polygon is going across
   # the division plane
   if ( T ) {

      # print('m0')
      # print(head(mCoordinates))
      # browser()
      viPointsToKeep = fRemovePointsBehindDividingPlane(
         mCoordinates = mCoordinates,
         nDivisionPlaneCoefficients = nDivisionPlaneCoefficients,
         bOriginDestinationInPositiveDirection = bOriginDestinationInPositiveDirection,
         iTreatAs = iTreatAs
      )

      # print('return')
      # print(viPointsToKeep)

      if ( length(viPointsToKeep) == 0 ) {

         return ( NULL )

      }

      mCoordinates = mCoordinates[viPointsToKeep,]
      viInputPoints = viInputPoints[viPointsToKeep]
      mCoordinates = matrix(mCoordinates, ncol = 3)




      # interpolating the connecting points between the adjacent behind screen - in front of screen points
      # such that the connecting point is a point on the screen
      # if there are two consecutive behind points then adding a place holder for an inbetween point between the two behind points
      # so that the polygon closes elegantly

      lReturn = fGetInterpolatedPointsAtDivisionPlane(
         mCoordinates = mCoordinates,
         nDivisionPlaneCoefficients = nDivisionPlaneCoefficients,
         bOriginDestinationInPositiveDirection = bOriginDestinationInPositiveDirection,
         iTreatAs = iTreatAs,
         viInputPoints = viInputPoints
      )

      viInputPoints = lReturn$viInputPoints
      mCoordinates = lReturn$mCoordinates
      rm(lReturn)


   }


   # projection on screen for points in front in 3d coordinates
   if ( T ) {

      # adding a vertical vector for knowing which way points up
      # can parameterise this also maybe

      mSolutions = fGetProjectionsOnPlane(
         mCoordinates,
         mOriginCoordinates,
         nScreenPlaneCoefficients
      )

      mSolutions = matrix(mSolutions, ncol = 3)

   }


   # rotating projections WRT to screening plane so as to get points in
   # two coordinates
   if ( T ) {

      mYAxis = fGetProjectionsOnPlane(
         mYAxisVectorOnScreen,
         mOriginCoordinates,
         nScreenPlaneCoefficients
      )
      mYAxis = matrix(mYAxis, ncol = 3)

      mVectorPointingUpOnScreenNew = mScreenCoordinates - mOriginCoordinates
      mVectorPointingUpOnScreenNew = mVectorPointingUpOnScreenNew / sum(mVectorPointingUpOnScreenNew^2) ^ 0.5

      mXAxisVectorNew = fCrossProduct(mVectorPointingUpOnScreenNew, mYAxis - mScreenCoordinates)

      mYAxisVectorNew = mYAxis / sum(mYAxis^2) ^ 0.5
      mXAxisVectorNew = mXAxisVectorNew / sum(mXAxisVectorNew^2) ^ 0.5

      mResult = fRelativeXYPositionOnPlane(
         mCoordinates = mSolutions,
         mScreenCoordinates = mScreenCoordinates,
         mYAxis = mYAxis - mScreenCoordinates,
         mXAxis = mXAxisVectorNew
      )

   }

   # the placeholders between the two behind points being filled with a value
   # that lies just a little below the lowest point in the viz or just a little
   # aboe the highest point in the viz so it can be chopped off with a ylim
   # you won't have poitns spilling outsdie the x boundaries
   if ( T ) {

      viPointsToFillIn = which(is.na(mResult[, 1]))

      if ( length(viPointsToFillIn) ) {

         if ( iTreatAs == 3 ) {

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

         } else if ( iTreatAs == 2 ) {

            mResult = cbind(mResult[,c('x','y')], group = cumsum(is.na(mResult[,1])))
            mResult = mResult[-viPointsToFillIn,]
            viInputPoints = viInputPoints[-viPointsToFillIn]
         }

      } else if (
         iTreatAs == 2
      ) {

         mResult = cbind(
            mResult[,c('x','y')],
            group = 1
         )

      }

   }

   row.names(mResult) = NULL

   mResult = cbind(
      mResult,
      inputOrder = viInputPoints
   )

   return ( mResult )

}
