#' Couleurs officielles HUG
#'
#' @param type character. "principales", "secondaires bleu" ou "secondaires rouge"
#' @export
#'
#' @examples
#' couleurs_hug("principales")

couleurs_hug <- function(type = "principales"){
  if( type == "principales" ){
    c("#a0d0ab", "#00b0af", "#3abff0", "#72c5c3", "#585757")
  } else if ( type == "secondaires bleu" ){
    c("#343d98","#2974bf","#008cba","#009ba2","#009875","#009656")
  } else if ( type == "secondaires rouge"){
    c("#eb5c2e","#e4032e","#ae0d1b","#e5005a","#a71681","#662382")
  } else {
    message("type inconnu")
  }
}
