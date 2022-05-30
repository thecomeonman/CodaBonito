#' Identifies the blocks in a grid through which a pass (
#' or more generally a line segment ) goes
#'
#' @param dt_passes a data.table with x, y, xend, yend, and optionally id
#' @param x_lim the span of x coordinates on your grid - c(min x, max x)
#' @param y_lim the span of x coordinates on your grid - c(min y, max y)
#' @param x_gap the spacing between blocks of the grid on the x dim
#' @param y_gap the spacing between blocks of the grid on the y dim
#' @examples
#'
#' library(data.table)
#' dt_passes = rbind(
#'    data.table(x = -27, y = 7, xend = -37, yend = 16, id = 1),
#'    data.table(x = 27, y = 7, xend = 37, yend = 16, id = 2),
#'    data.table(x = 27, y = -7, xend = 37, yend = -16, id = 3),
#'    data.table(x = -27, y = -7, xend = -37, yend = -16, id = 4),
#'    data.table(x = -25, y = 25, xend = -10, yend = 35, id = 5),
#'    data.table(x = 25, y = 25, xend = 10, yend = 35, id = 6),
#'    data.table(x = 25, y = -25, xend = 10, yend = -35, id = 7),
#'    data.table(x = -25, y = -25, xend = -10, yend = -35, id = 8),
#'    data.table(x = -8, y = -20, xend = 8, yend = -20, id = 9),
#'    data.table(x = 0, y = 0, xend = 0, yend = 20, id = 10)
#' )
#' dt_grids_pass = fGetBlocksThroughWhichPassesWent(
#'    dt_passes = dt_passes,
#'    x_lim = c(-60,60),
#'    y_lim = c(-40,40),
#'    x_gap = 5,
#'    y_gap = 5
#' )
#'
#'
#' library(ggplot2)
#' ggplot() +
#'    geom_pitch(nXStart = -60, nYStart = -40, nXEnd = 60, nYEnd = 40) +
#'    geom_tile(
#'       data = dt_grids_pass,
#'       aes(x = block_x, y = block_y),
#'       fill = 'cyan',
#'       color = 'black'
#'    ) +
#'    geom_segment(
#'       data = dt_passes,
#'       aes(x = x, y = y, xend = xend, yend = yend),
#'       color = 'red',
#'       arrow=arrow(type="closed",ends="last",length=unit(0.1,"cm"))
#'    )
#'
#' @import data.table
#' @export

fGetBlocksThroughWhichPassesWent =  function(
   dt_passes,
   x_lim = c(-60,60),
   y_lim = c(-40,40),
   x_gap = 5,
   y_gap = 5
) {

   if ( !'id' %in% colnames(dt_passes) ) {
      dt_passes[, id := .I]
   }

   x_cuts = seq(x_lim[1], x_lim[2] - x_gap, x_gap)
   y_cuts = seq(y_lim[1], y_lim[2] - y_gap, y_gap)
   # x_cuts = seq(x_lim[1]+(x_gap/2),x_lim[2]-(x_gap/2),x_gap)
   # y_cuts = seq(y_lim[1]+(y_gap/2),y_lim[2]-(y_gap/2),y_gap)

   dt_passes_x_cuts = rbindlist(
      lapply(
         x_cuts,
         function ( x_cut ) {

            dt_passes[,
               y_intersection := (  ( x_cut - x ) * ( y - yend ) / ( x - xend ) ) + y
            ]

            dt_passes_cut = dt_passes[
               (
                  ( ( x_cut - x ) * ( x_cut - xend ) ) <= 0
               ) &
                  (
                     ( ( y_intersection - y ) * ( y_intersection - yend ) ) <= 0
                  ) & {
                     !is.infinite(y_intersection)
                  },
               list(
                  y,
                  yend,
                  id,
                  y_intersection
               )
            ]

            if ( nrow(dt_passes_cut) > 0 ) {

               dt_passes_cut = dt_passes_cut[
                  ( y == yend ) |
                  (
                     ( y_intersection != y ) &
                     ( y_intersection != yend )
                  )
               ]

               dt_passes_cut = dt_passes_cut[,list(id,y_intersection)]
               dt_passes_cut[, x_cut := x_cut]
               # dt_passes_cut = rbind(
               #    dt_passes_cut,
               #    copy(dt_passes_cut)[, x_cut := x_cut + x_gap][, next_y_intersection := y_intersection][, y_intersection := NA],
               #    fill = T
               # )

            }

            dt_passes_cut

         }
      ),
      fill = T
   )


   dt_passes_y_cuts = rbindlist(
      lapply(
         y_cuts,
         function ( y_cut ) {

            dt_passes[,
                      x_intersection := ( ( y_cut - y ) * ( x - xend ) / ( y - yend ) ) + x
            ]

            dt_passes_cut = dt_passes[
               (
                  ( ( y_cut - y ) * ( y_cut - yend ) ) <= 0
               ) &
                  (
                     ( ( x_intersection - x ) * ( x_intersection - xend ) ) <= 0
                  ) & {
                     !is.infinite(x_intersection)
                  },
               list(
                  x,
                  xend,
                  id,
                  x_intersection
               )
            ]


            if ( nrow(dt_passes_cut) > 0 ) {

               dt_passes_cut = dt_passes_cut[
                  ( x == xend ) |
                     (
                        ( x_intersection != x ) &
                        ( x_intersection != xend )
                     )
               ]

               dt_passes_cut = dt_passes_cut[,list(id,x_intersection)]

               dt_passes_cut[, y_cut := y_cut]
               # dt_passes_cut = rbind(
               #    dt_passes_cut,
               #    copy(dt_passes_cut)[, y_cut := y_cut + y_gap][, next_x_intersection := x_intersection][, x_intersection := NA],
               #    fill = T
               # )

            }

            dt_passes_cut

         }
      ),
      fill = T
   )

   dt_blocks = data.table(expand.grid(x_cuts, y_cuts))

   setnames(dt_blocks, c('x','y'))

   dt_passing_through = rbindlist(lapply(
      seq(nrow(dt_blocks)),
      function( block_index ) {

         block = dt_blocks[block_index, c(x,y)]

         dt_through = data.table()

         ids = unique(c(
            dt_passes_x_cuts[
               ( x_cut == block[1] ) &
               ( y_intersection >= block[2] ) &
               ( y_intersection <= block[2] + y_gap ),
               id
            ]
         ))

         if ( length(ids) > 0 ) {

            dt_through = rbind(
               dt_through,
               data.table(
                  block_x = block[1] + (x_gap/2),
                  block_y = block[2] + (y_gap/2),
                  id = ids,
                  passing_through = T
               ),
               data.table(
                  block_x = block[1] - (x_gap/2),
                  block_y = block[2] + (y_gap/2),
                  id = ids,
                  passing_through = F
               ),
               fill = T
            )

         }


         ids = unique(c(
            dt_passes_y_cuts[
               ( x_intersection >= block[1] ) &
                  ( x_intersection <= block[1] + x_gap ) &
                  ( y_cut == block[2] ),
               id
            ]
         ))


         if ( length(ids) > 0 ) {

            dt_through = rbind(
               dt_through,
               data.table(
                  block_x = block[1] + (x_gap/2),
                  block_y = block[2] + (y_gap/2),
                  id = ids,
                  passing_through = T
               ),
               data.table(
                  block_x = block[1] + (x_gap/2),
                  block_y = block[2] - (y_gap/2),
                  id = ids,
                  passing_through = F
               ),
               fill = T
            )

         }


         dt_through

      }
   ), fill = T)

   dt_passing_through = dt_passing_through[,
      list(
         passing_through = .N == 2
      ),
      list(id, block_x, block_y)
   ]

   dt_passing_through

}
