#' Reduit les lignes de dates
#'
#' Reduire les lignes d'une table selon la date de debut et la date de fin. Utiles pour analyser les differentes saisies d'absences ou les differents contrats agents qui se suivent, etc.
#'
#' @param data une table contenant au moins une colonne date de debut et une colonne date de fin (voir autres arugments)
#' @param groups le(s) (differents) groupe(s) dont il faut tenir compte variable d'identification (ex: Matricule, Statut)
#' @param from une seule variable qui indique la date de debut (debut agent, debut absences, ...)
#' @param to une seule variable qui indique la date de fin (fin agent, fin absences, ...)
#' @param tronquer_debut Date entre guillemets `ymd("2018-01-01")` dans le format "YYYY-MM-JJ" pour tronquer les dates de debut (utile pour le calcul du nombre de jours)
#' @param tronquer_fin Date entre guillemets `ymd("2018-01-01")` dans le format "YYYY-MM-JJ" pour tronquer les dates de fin (utile pour le calcul du nombre de jours)
#' @param tolerance_weekend si TRUE, alors tolere une difference de jours deux lignes si cette difference est uniquement sur le weekend (date de fin un vendredi/samedi/dimanche et date de debut suivante est un lundi)
#' @param tolerance_n_days default 1. Nombre de jours de tolerance entre la date de fin et la prochaine date de debut (par exemple 31 jours pour les contrats agents ou 1 jour pour les absences a la suite)
#'
#' @return une table reduite
#' @examples
#' X <- data.frame(id = 1:1,
#'   debut = as.Date.character("2020-01-01") + c(0, 60),
#'   fin = as.Date.character("2020-01-01") + c(30, 90))
#' head(X)
#'
#' distinct_dates(X, id, debut, fin)
#' distinct_dates(X, id, debut, fin, tolerance_n_days = 31)
#'
#' @export

distinct_dates <- function(data, groups, from, to,
                           tronquer_debut = NULL, tronquer_fin = NULL,
                           tolerance_weekend = TRUE, tolerance_n_days = 1,
                           add_days = FALSE){
  # param check
  if( missing(data) | missing(groups) |
      missing(from) | missing(to) ){stop("Missing parameters")}

  if(!is.data.frame(data)){stop("data must be a data.frame")}

  if( !lubridate::is.Date(dplyr::pull(data, {{from}}) ) |
      !lubridate::is.Date(dplyr::pull(data, {{to}}) ) ){
    stop("from & to must be dates")}

  # fonction interne
  paste_collapse <- function(x){base::paste0(base::unique(x), collapse = ";")}

  # faire la reduction
  data_preparee <- data %>%
    dplyr::mutate( .row_number = dplyr::row_number() ) %>%
    # tronquer debut
    { if(!is.null(tronquer_debut)){
      dplyr::mutate_at(., dplyr::vars({{from}}), ~ base::pmax(.x, tronquer_debut) )
      } else {.} } %>%
    # tronquer fin
    { if(!is.null(tronquer_debut)){
      dplyr::mutate_at(., dplyr::vars({{to}}), ~ base::pmin(.x, tronquer_fin) )
      } else {.} } %>%
    # group
    dplyr::group_by( !!!rlang::enquos(groups) ) %>%
    # arrange
    dplyr::arrange( {{from}}, {{to}} ) %>%
    # define starting period
    dplyr::mutate( .id_group_case = dplyr::case_when(
                     dplyr::row_number() == 1 ~ 0,
                     ({{from}} - dplyr::lag({{to}})) <= tolerance_n_days ~ 0,
                     tolerance_weekend & ( lubridate::wday({{from}},  week_start = 1) == 1 & ({{from}} - dplyr::lag({{to}})) <= 3  ) ~ 0,
                     TRUE ~ 1),
                   .id_group_case = base::cumsum( base::as.numeric(.id_group_case) ) + 1) %>%
    dplyr::group_by( .id_group_case, add = TRUE)

  reduction <- data_preparee %>%
    dplyr::summarize( {{from}} := base::min({{from}}), # on prend la 1ere date de debut
                      {{to}} := base::max({{to}}),  # la derniere date de fin
                      .row_number = paste_collapse(.row_number)) %>%
    {if(add_days){
      dplyr::mutate(., .days = to - from + 1,
                     .days_diff_lag = from - dplyr::lag(to) )
      } else {.}}

  autre_info <- data_preparee %>%
    dplyr::select( -.row_number, - {{from}}, - {{to}}) %>%
    dplyr::summarize_all(paste_collapse)

  return( dplyr::full_join(reduction, autre_info,
                           by = dplyr::intersect(names(reduction), names(autre_info)) ) )
}
