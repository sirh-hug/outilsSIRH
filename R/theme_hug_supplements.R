#' Theme a ajouter avec des facets
#'
#' @export

theme_small_wrap <- function(){
  theme(axis.title = element_text(size=14),
        strip.text = element_text(size=10),
        axis.text = element_text(size=10))
}

#' @describeIn Theme a ajouter avec coord_flip()
#'
#' @export

theme_flip <- function(){
  theme(panel.grid.major.x = element_line(color="#cbcbcb"),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size=12))
}
