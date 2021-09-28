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

  D_added <- D  %>%
    mutate(Geomorpho = recode(Traitement, "1" = "PH", "4" = "PH", "6" = "PH", "10" = "PH", "14" = "PH"),
           Fertilization_N = recode(Traitement, "1" = "0", PA = "0", PAS = "0", PR = "0", PO = "0", PS = "0",
                                    "4" = "1", "6" = "6", "10" = "0", "14" = "5"),
           Fertilization_P = recode(Traitement, "1" = "0", "4" = "0", "6" = "0",
                                    PA = "0", PAS = "0", PR = "0", PO = "0", PS = "0",
                                    "14" = "1", "10" = "3"),
           Fertilization = recode(Traitement, "1" = "Control",
                                  PA = "0", PAS = "0", PR = "0", PO = "0", PS = "0",
                                  "4" = "Low N", "6" = "High N", "10" = "High P", "14" = "High N+P"),
           Traitement = ifelse(Parcelle %in% c("26", "43"), "PAS", Traitement)) %>%
    mutate(Geomorpho = replace(Geomorpho, Parcelle == "ROC6", "PS")) %>%
    mutate_at(vars(Fertilization_N, Fertilization_P, Fertilization), as.character) %>%
    mutate(Parcelle = factor(Parcelle),
           Geomorpho = factor(Geomorpho, levels = c("PH", "PS", "PA", "PAS", "PO", "PR")),
           Exclos = factor(Exclos, levels = c("Temoin", "Exclos")),
           Fertilization = factor(Fertilization, levels = c(0, "Control", "Low N", "High P", "High N", "High N+P")),
           Fertilization_N = factor(Fertilization_N, levels = c(0,1,5)),
           Fertilization_P = factor(Fertilization_P, levels = c(0,1,3)),
           Fertilization_P_bin = factor(ifelse(Fertilization_P == 0, 0, 1))
    )

  return(D_added)
}
