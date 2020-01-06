#' @title Reduit les lignes de dates
#' @description Reduire les lignes d'une table selon la date de debut et la date de fin. Utiles pour analyser les differentes saisies d'absences ou les differents contrats agents qui se suivent, etc.
#'
#' @param data une table contenant au moins une colonne date de debut et une colonne date de fin (voir autres arugments)
#' @param ... le(s) (differents) groupe(s) dont il faut tenir compte variable d'identification (ex: Matricule, Statut)
#' @param variable_date_debut une seule variable qui indique la date de debut (debut agent, debut absences, ...)
#' @param variable_date_fin une seule variable qui indique la date de fin (fin agent, fin absences, ...)
#' @param tronquer_debut a venir
#' @param tronquer_fin a venir
#' @param tolerance_weekend si TRUE, alors tolere une difference de jours deux lignes si cette difference est uniquement sur le weekend (date de fin un vendredi/samedi/dimanche et date de debut suivante est un lundi)
#' @param tolerance_nb_jours default 1. Nombre de jours de tolerance entre la date de fin et la prochaine date de debut (par exemple 31 jours pour les contrats agents ou 1 jour pour les absences a la suite)
#'
#' @return une table reduite
#' @examples distinct_dates(data, MATRICULE, STATUT, variable_date_debut = `date debut agent`, variable_date_fin = `date fin agent`, tolerance_nb_jours = 31)
#'
#' @export

distinct_dates <- function(data, ..., variable_date_debut, variable_date_fin,
                           tronquer_debut = FALSE, tronquer_fin = FALSE,
                           tolerance_weekend = TRUE, tolerance_nb_jours = 1){
  # check les parametres

  # fonction interne
  jour_de_la_semaine <- function(x){
    # Fonction qui extrait le jour de la date, de 1 (lundi) à 7 (dimanche)
    lubridate::wday(x, week_start = 1)
  }

  paste_collapse <- function(x){
    paste0(unique(x), collapse = ";")
  }

  # faire la reduction
  data_preparee <- data %>%
    mutate( ligne_id = row_number() ) %>%
    mutate_at(.vars = vars({{variable_date_debut}}, {{variable_date_fin}}), lubridate::ymd) %>%
    group_by( ... ) %>%
    arrange( {{variable_date_debut}}, {{variable_date_fin}}) %>%
    mutate( ligne_id_groupes = row_number(),
            temp_start_id = case_when(
              ligne_id_groupes == 1 ~ 0,
              ({{variable_date_debut}} - lag({{variable_date_fin}})) <= tolerance_nb_jours ~ 0,
              tolerance_weekend & ( jour_de_la_semaine({{variable_date_debut}}) == 1 & ({{variable_date_debut}} - lag({{variable_date_fin}})) <= 3  ) ~ 0,
              TRUE ~ 1
            ),
            id_reduction = cumsum( as.numeric(temp_start_id) )
    ) %>%
    group_by( id_reduction, add = TRUE)

  reduction <- data_preparee %>%
    summarize( min_date_debut = min({{variable_date_debut}}), # on prend la 1ere date de debut
               max_date_fin = max({{variable_date_fin}}),  # la derniere date de fin
               liste_lignes = paste_collapse(ligne_id)) %>%
    mutate( jours_entre = max_date_fin - min_date_debut + 1,
            jours_ecart_ligne_precedente = min_date_debut - lag(max_date_fin) )
  autre_info <- data_preparee %>%
    summarize_all(paste_collapse)

  return( full_join(reduction, autre_info) )

}
