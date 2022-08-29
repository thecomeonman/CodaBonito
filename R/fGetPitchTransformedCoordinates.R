#' Uses POV and fGetPitchCoordinates to give reoriented xy coords of pitch for plotting
#' @import data.table
#' @export
fGetPitchTransformedCoordinates = function(
   lPitchCoordinates,
   mOriginCoordinates = NULL,
   mScreenCoordinates = NULL,
   mViewBeginsFromCoordinates = NULL,
   mVectorPointingUpOnScreen = cbind(0,0,1)
) {

   if (requireNamespace("POV", quietly = TRUE)) {
      if ( !is.null(mOriginCoordinates) )  {

         qwe = names(lPitchCoordinates$lMarkings)
         lPitchCoordinates$lMarkings = lapply(
            lPitchCoordinates$lMarkings,
            function( dtDimensions ) {

               # dtDimensions = lPitchCoordinates$lMarkings[[1]]

               mCoordinates = cbind(
                  dtDimensions[, x],
                  dtDimensions[, y],
                  dtDimensions[, z]
               )

               mTransformedCoordinates = POV::fGetTransformedCoordinates(
                  mCoordinates = mCoordinates,
                  mOriginCoordinates = mOriginCoordinates,
                  mScreenCoordinates = mScreenCoordinates,
                  iTreatAs = 2,
                  mVectorPointingUpOnScreen = mVectorPointingUpOnScreen,
                  mViewBeginsFromCoordinates = mViewBeginsFromCoordinates
               )

               data.table(data.frame(mTransformedCoordinates))

            }
         )
         names(lPitchCoordinates$lMarkings) = qwe

         if ( nrow(lPitchCoordinates$dtPitchStripes) ) {

            dtPitchStripes = fGetTransformedCoordinates(
               mCoordinates = lPitchCoordinates$dtPitchStripes[, cbind(x,y,0)],
               mOriginCoordinates = mOriginCoordinates,
               mScreenCoordinates = mScreenCoordinates,
               mViewBeginsFromCoordinates = mViewBeginsFromCoordinates,
               mVectorPointingUpOnScreen = mVectorPointingUpOnScreen,
               iTreatAs = 2
            )
            dtPitchStripes = data.table(data.frame(dtPitchStripes))
            # dt_stripes_3d[, inputOrder := NULL]
            dtPitchStripes[, stripe_id := (inputOrder-1) %/% 5]
            lPitchCoordinates$dtPitchStripes = dtPitchStripes

         }

         qwe = names(lPitchCoordinates$lGoalframes)
         lPitchCoordinates$lGoalframes = lapply(
            lPitchCoordinates$lGoalframes,
            function( lRectangles ) {

               lRectangles = lapply(
                  lRectangles,
                  function( dtDimensions ) {

                     # dtDimensions = lPitchCoordinates$lGoalframes[[1]]

                     mCoordinates = cbind(
                        dtDimensions[, x],
                        dtDimensions[, y],
                        dtDimensions[, z]
                     )

                     mTransformedCoordinates = POV::fGetTransformedCoordinates(
                        mCoordinates = mCoordinates,
                        mOriginCoordinates = mOriginCoordinates,
                        mScreenCoordinates = mScreenCoordinates,
                        mVectorPointingUpOnScreen = mVectorPointingUpOnScreen,
                        mViewBeginsFromCoordinates = mViewBeginsFromCoordinates,
                        iTreatAs = 3
                     )

                     data.table(data.frame(mTransformedCoordinates))

                  }
               )

               lRectangles

            }
         )
         names(lPitchCoordinates$lGoalframes) = qwe

         qwe = names(lPitchCoordinates$lGoalnet)
         lPitchCoordinates$lGoalnet = lapply(
            lPitchCoordinates$lGoalnet,
            function( lSegments ) {

               # dtSegments = lPitchCoordinates$lGoalnet[[5]]

               lapply(
                  lSegments,
                  function( dtSegment ) {

                     mCoordinates = cbind(
                        dtSegment[, x],
                        dtSegment[, y],
                        dtSegment[, z]
                     )

                     mTransformedCoordinates = POV::fGetTransformedCoordinates(
                        mCoordinates = mCoordinates,
                        mOriginCoordinates = mOriginCoordinates,
                        mScreenCoordinates = mScreenCoordinates,
                        mVectorPointingUpOnScreen = mVectorPointingUpOnScreen,
                        mViewBeginsFromCoordinates = mViewBeginsFromCoordinates,
                        iTreatAs = 2
                     )

                     data.table(data.frame(mTransformedCoordinates))

                  }
               )


            }
         )
         names(lPitchCoordinates$lGoalnet) = qwe

         # names(lPitchCoordinates$lGoalnet) = names(lPitchCoordinates$lGoalnet)

      }

   } else {

      warning('POV package not installed, can\t use the mOrigin argument')

   }

   lPitchCoordinates

}
