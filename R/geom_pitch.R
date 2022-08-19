#' Pitch background
#'
#' Adds pitch markings, green background, etc. for plot background to look like
#' a football pitch. ggplot adds things in order so initiate a ggplot object,
#' add the pitch marking with this function, and then add the data you want to
#' add
#'
#' @param plotObject a ggplot object. Can just be a blank ggplot object too
#' @param cLineColour the colour of the line markings on the pitch
#' @param cPitchColour the colour of the grass on the pitch
#' @param nXStart Start x coordinate of the pitch
#' @param nYStart Start y coordinate of the pitch
#' @param nXEnd End x coordinate of the pitch
#' @param nYEnd Start y coordinate of the pitch
#' @examples
#' ggplot() + geom_pitch()
#' @import data.table
#' @import ggplot2
#' @export
geom_pitch = function (
   nXStart = -60,
   nYStart = -40,
   nXEnd = 60,
   nYEnd = 40,
   mOriginCoordinates = NULL,
   mScreenCoordinates = NULL,
   mViewBeginsFromCoordinates = NULL,
   mVectorPointingUpOnScreen = cbind(0,0,1),
   cLineColour = '#BBBBBB',
   cPitchColour = '#038253',
   vcToPlot = c('Markings','Stripes','Goalframe','Goalnet'),
   iXStripes = 12,
   iYStripes = 8
) {

   # nXStart = 0; nYStart = 0; nXEnd = 120; nYEnd = 80; mVectorPointingUpOnScreen = c(0,0,1)

   lPitchCoordinates = fGetPitchCoordinates (
      nXStart = nXStart,
      nYStart = nYStart,
      nXEnd = nXEnd,
      nYEnd = nYEnd,
      iXStripes = iXStripes,
      iYStripes = iYStripes
   )

   lPitchCoordinates = fGetPitchTransformedCoordinates(
      lPitchCoordinates = lPitchCoordinates,
      mOriginCoordinates = mOriginCoordinates,
      mScreenCoordinates = mScreenCoordinates,
      mViewBeginsFromCoordinates = mViewBeginsFromCoordinates,
      mVectorPointingUpOnScreen = mVectorPointingUpOnScreen
   )

   lPitchElements = geom_pitch_from_coords_obj(
      lPitchCoordinates = lPitchCoordinates,
      cLineColour = cLineColour,
      cPitchColour = cPitchColour,
      vcToPlot =vcToPlot
   )

   return ( lPitchElements )

}
