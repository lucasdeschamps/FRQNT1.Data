#' @title Abundances of Vascular plants in clods
#' @description This data set contains different measures of the abundance of the different species in each clod. 3 to 4 clods had been
#'  harvested randomly in a grid in each plot. Each individual in these clods had been sorted, counted, each species weighted at the plot level.
#' @format A data frame with 1327 rows and 15 variables:
#' \describe{
#'   \item{\code{Parcelle}}{character Name of the site}
#'   \item{\code{Traitement}}{character Name of the treatment. PA : Polygons slightly drained, PAS : Polygons drained by a secondary ice wedge,
#'   PS : polygons close to the shore, submitted to abundant eolian silt deposition, PO : flat to high-center mesic polygons,  PR : mesic meadow,
#'   1 : low-centered, wet polygon; 4 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3; 6 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3,
#'   10 : wet polygons fertilized yearly with 3 g.m-2 of H3PO4; 14 : wet polygons fertilized yearly with 1 g.m-2 of H3PO4 amd 5 g.m-2 of NH4NO3}
#'   \item{\code{Exclos}}{character Fencing. Exclos : plots protected constantly by a fence; Temoin : unprotected plots}
#'   \item{\code{Herbivorie}}{character Grazing (reformulation of Exclos). Ungrazed : plots protected constantly by a fence; Grazed : unprotected plots}
#'   \item{\code{Motte}}{character Identifiant of the clod}``
#'   \item{\code{Motte_enveloppe}}{double Numerical identifiant of the clod}
#'   \item{\code{Sp}}{character Code of species}
#'   \item{\code{Density_ind_m2}}{double Number of individuals of the species in each clod, scaled to m2}
#'   \item{\code{Density_rel}}{double Number of individuals of species in the clod divided by the total number of individuals in the clod}
#'   \item{\code{Biomass_g_m2}}{double Biomass of the species in each clod, scaled to m2.
#'   Computed indirectly by multiplying the mean mass of individuals of the species in the plot by the number of individuals in the clod}
#'   \item{\code{Biomass_rel}}{double Biomass of the species in the clod divided by the total biomass in the clod}
#'   \item{\code{Productivity_g_m2}}{double Biomass of the species in each clod, scaled to m2. Correspond to the biomass created in one year, without potential older woods.
#'   Equal biomass in case of herbaceous species.
#'   Computed indirectly by multiplying the mean productivity of individuals of the species in the plot by the number of individuals in the clod}
#'   \item{\code{Productivity_rel}}{double Productivity of the species in the clod divided by the total productivity in the clod}
#'   \item{\code{LAI_m2_m2}}{double Total leaf area of species in the clod, scaled to m2.
#'   Computed indirectly as mean number of leaves per individual of the species in the plot multpiplied by
#'  the mean leaf area of the species in the treatment and the number of individual in the clod}
#'   \item{\code{LAI_rel}}{double LAI of the species in the clod divided by the total LAI in the clod}
#'   #'   \item{\code{Geomorpho}}{integer A formulation of the Traitement columns focusing on discrete
#'   habitat differences}
#'   \item{\code{Fertilization_N}}{integer Specifies the quantity of N added each year in g.m-2 of NH4NO3}
#'   \item{\code{Fertilization_P}}{integer Specifies the quantity of P added each year in g.m-2 of H3PO4}
#'   \item{\code{Fertilization}}{integer A formulation of the Traitement columns with a zero to each unfertilized plots}
#'   \item{\code{Fertilization_P_bin}}{integer Binary variable indicating if the plot receive annual P fertilization}
#'}
#' @source \url{http://somewhere.important.com/}
"Vascular_Abundances_Clod"

#' @title Traits of Vascular Plants in each plot
#' @description This dataset contains the mean traits of each species in each plot,
#' measured either on 10-12 individuals or on every individuals pulled together, depending on their abundances.
#' @format A data frame with 340 rows and 22 variables:
#' \describe{
#'   \item{\code{Parcelle}}{integer Name of the site}
#'   \item{\code{Traitement}}{character Name of the treatment. PA : Polygons slightly drained, PAS : Polygons drained by a secondary ice wedge,
#'   PS : polygons close to the shore, submitted to abundant eolian silt deposition, PO : flat to high-center mesic polygons,  PR : mesic meadow,
#'   1 : low-centered, wet polygon; 4 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3; 6 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3,
#'   10 : wet polygons fertilized yearly with 3 g.m-2 of H3PO4; 14 : wet polygons fertilized yearly with 1 g.m-2 of H3PO4 amd 5 g.m-2 of NH4NO3}
#'   \item{\code{Exclos}}{integer Exclos : plots protected constantly by a fence; Temoin : unprotected plots}
#'   \item{\code{Herbivorie}}{character Grazing (reformulation of Exclos). Ungrazed : plots protected constantly by a fence; Grazed : unprotected plots}
#'   \item{\code{Sp}}{character Code of species}
#'   \item{\code{Leaf_N15}}{double Leaf d15N, in per mill}
#'   \item{\code{LNC}}{double Leaf Nitrogen Content, in percent}
#'   \item{\code{Leaf_C13}}{double Leaf d13C, in per mill}
#'   \item{\code{LCC}}{double Leaf Carbon Content, in percent}
#'   \item{\code{Root_N15}}{double Root d15N, in per mill}
#'   \item{\code{RNC}}{double Root Nitrogen Content, in percent}
#'   \item{\code{Leaf_pH}}{double Leaf Hydrogen potential}
#'   \item{\code{Hveg}}{double Vegetative height in cm, measured as the distance between the base of the plant
#'   and the extremity of the extended leaves. Measured on height individuals per plot or less}
#'   \item{\code{LDMC}}{double Leaf Dry Matter Content, in g_dry.g_fresh^-1}
#'   \item{\code{RDMC}}{double Root Dry Matter Content, in g_dry.g_fresh^-1}
#'   \item{\code{LA}}{double Leaf Area, in cm^2}
#'   \item{\code{SLA}}{double Specific Leaf Area, in cm^2.g^-1}
#'   \item{\code{Geomorpho}}{integer A formulation of the Traitement columns focusing on discrete
#'   habitat differences}
#'   \item{\code{Fertilization_N}}{integer Specifies the quantity of N added each year in g.m-2 of NH4NO3}
#'   \item{\code{Fertilization_P}}{integer Specifies the quantity of P added each year in g.m-2 of H3PO4}
#'   \item{\code{Fertilization}}{integer A formulation of the Traitement columns with a zero to each unfertilized plots}
#'   \item{\code{Fertilization_P_bin}}{integer Binary variable indicating if the plot receive annual P fertilization}
#'}
#' @source \url{http://somewhere.important.com/}
"Vascular_Traits_Plot"
