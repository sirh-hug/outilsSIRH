#' Formater un tableau croise
#'
#' @param table a table.
#' @param bg.col color. Background color
#' @param title character.
#' @param caption character.
#'
#' @export
#'

print_table <- function(data, bg.col = "#a0d0ab", title=NULL, caption = "" ){
  tab <- data %>%
    # Creer le tableau
    flextable::flextable() %>%
    # colorer une ligne sur deux
    flextable::theme_zebra() %>%
    # colorer l'entete et la derniere ligne
    flextable::bg(bg = bg.col, part="header") %>%
    flextable::bg(i=nrow(data), bg = bg.col) %>%
    # aligner la premiere colonne a gauche, le reste a droite
    flextable::align(j = 1, align="left", part="all") %>%
    flextable::align(j = 2:ncol(data), align="right", part="all") %>%
    # Ajouter la note en bas
    flextable::add_footer_lines(caption)  %>%
    flextable::merge_at(j=1:ncol(data), part="footer") %>%
    # changer taile de police
    flextable::fontsize(size=9, part = "body") %>%
    flextable::fontsize(size=8, part = "footer") %>%
    flextable::fontsize(size=9, part = "header") %>%
    flextable::italic(part = "footer") %>%
    # Autofit
    flextable::autofit(0,0)

  ## titre
  if(!is.null(title)){
    tab <- tab %>%
      flextable::add_header_lines(title) %>%
      flextable::merge_at(i=1,j = 1:ncol(data), part="header")
  }


  return(tab)
}
