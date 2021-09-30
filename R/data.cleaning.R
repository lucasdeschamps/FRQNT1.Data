#' Clean variables identifying observations
#'
#' @param D The dataset you want to clean.
#'
#' @return The same dataset, but with uniformized Parcelle, Traitement and Exclos columns
#' @export
#'
#' @examples
#'
data.cleaning <- function(D){

  require(tidyverse)

  D_clean <- D  %>%
    ## First be sure Traitement columns are compatibles
    mutate(Traitement = replace(Traitement, Traitement == "1.0", "1")) %>%
    mutate(Traitement = replace(Traitement, Traitement == "4.0", "4")) %>%
    mutate(Traitement = replace(Traitement, Traitement == "6.0", "6")) %>%
    mutate(Traitement = replace(Traitement, Traitement == "10.0", "10")) %>%
    mutate(Traitement = replace(Traitement, Traitement == "14.0", "14")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "26.0", "26")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "33.0", "33")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "35.0", "35")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "36.0", "36")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "43.0", "43")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "71.0", "71")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "72.0", "72")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "Roc7", "ROC7")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "33-1", "33")) %>%
    mutate(Parcelle = replace(Parcelle, Parcelle == "33-2", "35")) %>%
    ## Make 26 and 43 Plots with the PAS treatments
    mutate(Traitement = ifelse(Parcelle %in% c("26", "43"), "PAS", Traitement))

  ## Ensure Exclos are written uniformely
  if("Exclos" %in% colnames(D_clean)){
    D_clean <- D_clean %>%
      mutate(Exclos = replace(Exclos, Parcelle == "ROC8" & Exclos == "Temoin/exclos", "Temoin")) %>%
      mutate(Exclos = replace(Exclos, Parcelle == "ROC8" & Exclos == "Temoin/Exclos", "Temoin")) %>%
      mutate(Exclos = replace(Exclos, Exclos == "Témoin/Exclos", "Temoin")) %>%
      mutate(Exclos = replace(Exclos, Exclos == "Exclos/Temoin", "Exclos")) %>%
      mutate(Exclos = replace(Exclos, Exclos == "Témoin", "Temoin")) %>%
      mutate(Exclos = replace(Exclos, Exclos == "Temoin Exclos?", "Temoin")) %>%
      mutate(Exclos = replace(Exclos, Exclos == "Exclos Temoin?", "Exclos"))
  }

  ## Create the Exclos/Variable from Herbivorie_enveloppe
  if("Herbivorie_enveloppe" %in% colnames(D_clean)){
    D_clean <- D_clean %>%
      mutate(Exclos = ifelse(Herbivorie_enveloppe == "Non Brouté", "Exclos", "Temoin"))
  }

  ## Create exclos from Herbivorie
  if("Herbivorie" %in% colnames(D_clean)){
    D_clean <- D_clean %>%
      mutate(Exclos = recode(Herbivorie, "Non-Broute" = "Exclos", "Broute" = "Temoin", "Brouté" = "Temoin",
                             "Témoin" = "Temoin"))
  }

  ## Create a clean Herbivorie variable
  D_clean <- D_clean %>%
    mutate(Grazing = ifelse(Exclos == "Temoin", "Grazed", "Ungrazed"))

  return(D_clean)
}
