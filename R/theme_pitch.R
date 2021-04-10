#' Lipstick for the pitch background plots
#' Refer to fAddPitchLines
#' @import ggplot2
#' @export
theme_pitch <- function () {

   theme_bw(
      base_size = 12
   ) +
      theme(
         # panel.background = element_rect(
         #    fill = "white",
         #    colour = NA
         # ),
         plot.background = element_rect(fill = "white", colour = NA),
         legend.background = element_rect(fill = "transparent", colour = NA),
         legend.key = element_rect(fill = "transparent", colour = NA),
         plot.caption = element_text(hjust = 0),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         # axis.line = element_line(colour = "black"),
         axis.line = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank()
      )

}
