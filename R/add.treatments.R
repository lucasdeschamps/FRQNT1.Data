#' Add more descriptive treatments to the data set
#'
#' @param D The dataset you want to add variables to
#'
#' @return The same dataset, but with new variables describing fertilization treatments and geomorphology
#' @export
#'
#' @examples
#'
add.treatments <- function(D){

  require(tidyverse)

  D_added <- D %>%
    mutate(Geomorpho = car::recode(Traitement, recodes = "c('1','4','6','10','14') = 'PH'"),
           Fertilization_N = car::recode(Traitement, recodes = "c('1', 'PA', 'PR', 'PO', 'PS') = 0;
                                  '4' = 1;'6' = 5;'10' = 0;'14' = 5"),
           Fertilization_P = car::recode(Traitement,recodes = "c('1', '4', '6', 'PA', 'PR', 'PO', 'PS') = 0;
                                  c('14') = 1; c('10') = 3"),
           Fertilization = car::recode(Traitement, recodes = "c('1', 'PA', 'PR', 'PO', 'PS') = 0"),
           Traitement = ifelse(Parcelle %in% c("26", "43"), "PAS", Traitement)) %>%
    mutate(Geomorpho = replace(Geomorpho, Parcelle == "ROC6", "PS")) %>%
    mutate_at(vars(Fertilization_N, Fertilization_P, Fertilization), as.character) %>%
    mutate(Parcelle = factor(Parcelle),
           Geomorpho = factor(Geomorpho, levels = c("PH", "PS", "PA", "PO", "PR")),
           Exclos = factor(Exclos, levels = c("Temoin", "Exclos")),
           Fertilization = factor(Fertilization, levels = c(0,4,10, 6,14)),
           Fertilization_N = ordered(Fertilization_N, levels = c(0,1,5)),
           Fertilization_P = factor(Fertilization_P, levels = c(0,1,3)),
           Fertilization_P_bin = factor(ifelse(Fertilization_P == 0, 0, 1))
    )

  return(D_added)
}
