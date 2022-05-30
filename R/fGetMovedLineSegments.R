#' Calculates coordiantes of a line segment moved from a base line segment
#'
#' @examples
#'
#' line_segment = data.table(
#'    x1 = c(0,10), y1 = c(0,10),
#'    x2 = c(1,5), y2 = c(1,5)
#' )
#'
#' line_segment_top = fGetMovedLineSegments(
#'    x1 = line_segment[,x1], y1 = line_segment[,y1],
#'    x2 = line_segment[,x2], y2 = line_segment[,y2],
#'    distance = c(1,5),
#'    angle = c(pi/2,pi/4)
#' )
#' line_segment_top = data.frame(line_segment_top)
#'
#' line_segment_bottom = fGetMovedLineSegments(
#'    x1 = line_segment[,x1], y1 = line_segment[,y1],
#'    x2 = line_segment[,x2], y2 = line_segment[,y2],
#'    distance = -c(1,5),
#'    angle = c(pi/2,pi/4)
#' )
#' line_segment_bottom = data.frame(line_segment_bottom)
#'
#' library(ggplot2)
#'
#' ggplot() +
#'    geom_segment(
#'       data = line_segment,
#'       aes(
#'          x = x1,
#'          y = y1,
#'          xend = x2,
#'          yend = y2
#'       )
#'    ) +
#'    geom_segment(
#'       data = line_segment_top,
#'       aes(
#'          x = x1,
#'          y = y1,
#'          xend = x2,
#'          yend = y2
#'       ),
#'       color = 'red'
#'    ) +
#'    geom_segment(
#'       data = line_segment_bottom,
#'       aes(
#'          x = x1,
#'          y = y1,
#'          xend = x2,
#'          yend = y2
#'       ),
#'       color = 'cyan'
#'    ) +
#'    coord_fixed()
#'
#' @export
#'
fGetMovedLineSegments = function(
   x1,y1,
   x2,y2,
   distance,
   angle = pi/2
) {

   # x1 y1 x2 y2 need to be equal length vectors

   # get angle perpendicular to the line segment
   m_theta = atan2(( y2 - y1 ), ( x2 - x1 ))
   m_2_theta = (m_theta + (angle))

   # calculate the distance by which to move on x and y
   x_displacement = distance * cos(m_2_theta)
   y_displacement = distance * sin(m_2_theta)

   # overrides for when the angles are painful values
   # x_equals = x1 == x2
   # y_equals = y1 == y2
   #
   # x_displacement[x_equals & y_equals] = NA
   # y_displacement[x_equals & y_equals] = NA
   #
   # x_displacement[x_equals & !y_equals] = distance
   # y_displacement[x_equals & !y_equals] = 0
   #
   # x_displacement[!x_equals & y_equals] = 0
   # y_displacement[!x_equals & y_equals] = distance

   list(
      'x1' = x1 + x_displacement,
      'y1' = y1 + y_displacement,
      'x2' = x2 + x_displacement,
      'y2' = y2 + y_displacement
   )

}


