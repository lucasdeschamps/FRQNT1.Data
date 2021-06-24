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

#' @title Physical properties of soil horizons
#' @description This dataset describes the physical properties of each homogeneous horizon
#' @format A data frame with 280 rows and 35 variables:
#' \describe{
#'   \item{\code{Date}}{character Date at which the sampled has been processed}
#'   \item{\code{Parcelle}}{integer Name of the site}
#'   \item{\code{Traitement}}{character Name of the treatment. PA : Polygons slightly drained, PAS : Polygons drained by a secondary ice wedge,
#'   PS : polygons close to the shore, submitted to abundant eolian silt deposition, PO : flat to high-center mesic polygons,  PR : mesic meadow,
#'   1 : low-centered, wet polygon; 4 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3; 6 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3,
#'   10 : wet polygons fertilized yearly with 3 g.m-2 of H3PO4; 14 : wet polygons fertilized yearly with 1 g.m-2 of H3PO4 amd 5 g.m-2 of NH4NO3}
#'   \item{\code{Exclos}}{integer Fencing. Exclos : plots protected constantly by a fence; Temoin : unprotected plots}
#'   \item{\code{Depth_up}}{double Upper depth of the horizon}
#'   \item{\code{Depth_down}}{double Lower depth of the horizon}
#'   \item{\code{Depth_mean}}{double Mean depth of the horizon}
#'   \item{\code{Oxydo_Reduction}}{double Did the horizon displayed trace of oxydo-reduction?}
#'   \item{\code{Volume}}{double Volume of the horizon, cm-3}
#'   \item{\code{Density}}{double Bulk density of the horizon, g.cm-3}
#'   \item{\code{LOI}}{double Proportion of organic matter, as lost on ignition, (0-1)}
#'   \item{\code{V_om}}{double Volumic fraction of organic matter}
#'   \item{\code{V_oms}}{double Proportion of the volume of the solide attributed to organic matter}
#'   \item{\code{Mineral}}{double Proportion of mineral material (1-LOI, 0-1)}
#'   \item{\code{V_min}}{double Volumic fraction of mineral material}
#'   \item{\code{V_mins}}{double Proportion of the volume of the solide attributed to minerals}
#'   \item{\code{Particle_Density}}{double Particle density as approximated by weighting a known volume of powder, g.cm-3}
#'   \item{\code{Particle_Density_computed}}{double Particle density computed following Balland and Arp (2005), g.cm-3}
#'   \item{\code{Porosity}}{double Porosity computed using approximation of particle density (0-1)}
#'   \item{\code{Porosity_computed}}{double Porosity computed using computed particle density (0-1)}
#'   \item{\code{GWC}}{double Gravimetric Water Content, g.g-1}
#'   \item{\code{VWC}}{double Volumetric Water Content, g.cm-3}
#'   \item{\code{theta_sat}}{double Proportion of the porosity filled with water}
#'   \item{\code{K_solid}}{double Thermal conductivity of solids}
#'   \item{\code{K_dry}}{double Thermal conductivity of dry materials}
#'   \item{\code{K_sat}}{double Thermal conductivity of the saturated soil}
#'   \item{\code{K_e}}{double Impact of water content on soil thermal conductivity}
#'   \item{\code{K_soil}}{double Raw thermal conductivity of soil}
#'   \item{\code{VolHeatCap}}{double Volumetric heat capacity of soil, as a weighted mean of its components}
#'   \item{\code{ThermDiff}}{double Thermal diffusivity of soil}
#'   \item{\code{Geomorpho}}{integer A formulation of the Traitement columns focusing on discrete
#'   habitat differences}
#'   \item{\code{Fertilization_N}}{integer Specifies the quantity of N added each year in g.m-2 of NH4NO3}
#'   \item{\code{Fertilization_P}}{integer Specifies the quantity of P added each year in g.m-2 of H3PO4}
#'   \item{\code{Fertilization}}{integer A formulation of the Traitement columns with a zero to each unfertilized plots}
#'   \item{\code{Fertilization_P_bin}}{integer Binary variable indicating if the plot receive annual P fertilization}
#'}
#' @source \url{http://somewhere.important.com/}
"Soil_Physic_Horizon"

#' @title Physical properties of 5mm thick soil slices
#' @description This dataset describes the physical properties of each homogeneous horizon, divided in slice 5mm thick
#' @format A data frame with 280 rows and 36 variables:
#' \describe{
#'   \item{\code{Date}}{character Date at which the sampled has been processed}
#'   \item{\code{Parcelle}}{integer Name of the site}
#'   \item{\code{Traitement}}{character Name of the treatment. PA : Polygons slightly drained, PAS : Polygons drained by a secondary ice wedge,
#'   PS : polygons close to the shore, submitted to abundant eolian silt deposition, PO : flat to high-center mesic polygons,  PR : mesic meadow,
#'   1 : low-centered, wet polygon; 4 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3; 6 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3,
#'   10 : wet polygons fertilized yearly with 3 g.m-2 of H3PO4; 14 : wet polygons fertilized yearly with 1 g.m-2 of H3PO4 amd 5 g.m-2 of NH4NO3}
#'   \item{\code{Exclos}}{integer Fencing. Exclos : plots protected constantly by a fence; Temoin : unprotected plots}
#'   \item{\code{Depth_up}}{double Upper depth of the horizon}
#'   \item{\code{Depth_down}}{double Lower depth of the horizon}
#'   \item{\code{Depth_mean}}{double Mean depth of the horizon}
#'   \item{\code{Oxydo_Reduction}}{double Did the horizon displayed trace of oxydo-reduction?}
#'   \item{\code{Volume}}{double Volume of the horizon, cm-3}
#'   \item{\code{Density}}{double Bulk density of the horizon, g.cm-3}
#'   \item{\code{LOI}}{double Proportion of organic matter, as lost on ignition, (0-1)}
#'   \item{\code{V_om}}{double Volumic fraction of organic matter}
#'   \item{\code{V_oms}}{double Proportion of the volume of the solide attributed to organic matter}
#'   \item{\code{Mineral}}{double Proportion of mineral material (1-LOI, 0-1)}
#'   \item{\code{V_min}}{double Volumic fraction of mineral material}
#'   \item{\code{V_mins}}{double Proportion of the volume of the solide attributed to minerals}
#'   \item{\code{Particle_Density}}{double Particle density as approximated by weighting a known volume of powder, g.cm-3}
#'   \item{\code{Particle_Density_computed}}{double Particle density computed following Balland and Arp (2005), g.cm-3}
#'   \item{\code{Porosity}}{double Porosity computed using approximation of particle density (0-1)}
#'   \item{\code{Porosity_computed}}{double Porosity computed using computed particle density (0-1)}
#'   \item{\code{GWC}}{double Gravimetric Water Content, g.g-1}
#'   \item{\code{VWC}}{double Volumetric Water Content, g.cm-3}
#'   \item{\code{theta_sat}}{double Proportion of the porosity filled with water}
#'   \item{\code{K_solid}}{double Thermal conductivity of solids}
#'   \item{\code{K_dry}}{double Thermal conductivity of dry materials}
#'   \item{\code{K_sat}}{double Thermal conductivity of the saturated soil}
#'   \item{\code{K_e}}{double Impact of water content on soil thermal conductivity}
#'   \item{\code{K_soil}}{double Raw thermal conductivity of soil}
#'   \item{\code{VolHeatCap}}{double Volumetric heat capacity of soil, as a weighted mean of its components}
#'   \item{\code{ThermDiff}}{double Thermal diffusivity of soil}
#'   \item{\code{Geomorpho}}{integer A formulation of the Traitement columns focusing on discrete
#'   habitat differences}
#'   \item{\code{Fertilization_N}}{integer Specifies the quantity of N added each year in g.m-2 of NH4NO3}
#'   \item{\code{Fertilization_P}}{integer Specifies the quantity of P added each year in g.m-2 of H3PO4}
#'   \item{\code{Fertilization}}{integer A formulation of the Traitement columns with a zero to each unfertilized plots}
#'   \item{\code{Fertilization_P_bin}}{integer Binary variable indicating if the plot receive annual P fertilization}
#'   \item{\code{Depth}}{double Depth of the slice}
#'}
#' @source \url{http://somewhere.important.com/}
"Soil_Physic_mm"

#' @title Measured thermal conductivity of soil at 5, 10 (and 15cm)
#' @description This dataset contains the measured thermal conductivity of soil at different depth,
#' joined to the average physical properties 2.5cm around this depth
#' @format A data frame with 139 rows and 34 variables:
#' \describe{
#'   \item{\code{Date}}{character Date at which the sampled has been processed}
#'   \item{\code{Parcelle}}{integer Name of the site}
#'   \item{\code{Traitement}}{character Name of the treatment. PA : Polygons slightly drained, PAS : Polygons drained by a secondary ice wedge,
#'   PS : polygons close to the shore, submitted to abundant eolian silt deposition, PO : flat to high-center mesic polygons,  PR : mesic meadow,
#'   1 : low-centered, wet polygon; 4 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3; 6 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3,
#'   10 : wet polygons fertilized yearly with 3 g.m-2 of H3PO4; 14 : wet polygons fertilized yearly with 1 g.m-2 of H3PO4 amd 5 g.m-2 of NH4NO3}
#'   \item{\code{Exclos}}{integer Fencing. Exclos : plots protected constantly by a fence; Temoin : unprotected plots}
#'   \item{\code{Depth}}{double Depth of the measurement}
#'   \item{\code{K_measured}}{double Measured thermal conductivity, averaged over replicates, W.m-1.k-1}
#'   \item{\code{Temp_mean}}{double Mean temperature during thermal conductivity measurement, Celsius degree}
#'   \item{\code{Volume}}{double Volume of the horizon, cm-3}
#'   \item{\code{Density}}{double Bulk density of the horizon, g.cm-3}
#'   \item{\code{LOI}}{double Proportion of organic matter, as lost on ignition, (0-1)}
#'   \item{\code{V_om}}{double Volumic fraction of organic matter}
#'   \item{\code{V_oms}}{double Proportion of the volume of the solide attributed to organic matter}
#'   \item{\code{Mineral}}{double Proportion of mineral material (1-LOI, 0-1)}
#'   \item{\code{V_min}}{double Volumic fraction of mineral material}
#'   \item{\code{V_mins}}{double Proportion of the volume of the solide attributed to minerals}
#'   \item{\code{Particle_Density}}{double Particle density as approximated by weighting a known volume of powder, g.cm-3}
#'   \item{\code{Particle_Density_computed}}{double Particle density computed following Balland and Arp (2005), g.cm-3}
#'   \item{\code{Porosity}}{double Porosity computed using approximation of particle density (0-1)}
#'   \item{\code{Porosity_computed}}{double Porosity computed using computed particle density (0-1)}
#'   \item{\code{GWC}}{double Gravimetric Water Content, g.g-1}
#'   \item{\code{VWC}}{double Volumetric Water Content, g.cm-3}
#'   \item{\code{theta_sat}}{double Proportion of the porosity filled with water}
#'   \item{\code{K_solid}}{double Thermal conductivity of solids}
#'   \item{\code{K_dry}}{double Thermal conductivity of dry materials}
#'   \item{\code{K_sat}}{double Thermal conductivity of the saturated soil}
#'   \item{\code{K_e}}{double Impact of water content on soil thermal conductivity}
#'   \item{\code{K_soil}}{double Raw thermal conductivity of soil}
#'   \item{\code{VolHeatCap}}{double Volumetric heat capacity of soil, as a weighted mean of its components}
#'   \item{\code{ThermDiff}}{double Thermal diffusivity of soil}
#'   \item{\code{Geomorpho}}{integer A formulation of the Traitement columns focusing on discrete
#'   habitat differences}
#'   \item{\code{Fertilization_N}}{integer Specifies the quantity of N added each year in g.m-2 of NH4NO3}
#'   \item{\code{Fertilization_P}}{integer Specifies the quantity of P added each year in g.m-2 of H3PO4}
#'   \item{\code{Fertilization}}{integer A formulation of the Traitement columns with a zero to each unfertilized plots}
#'   \item{\code{Fertilization_P_bin}}{integer Binary variable indicating if the plot receive annual P fertilization}
#'}
#' @source \url{http://somewhere.important.com/}
"Soil_ThermalConductivity_Horizon"

#' @title Surface environment of plots
#' @description This dataset compiles measures of environmental describing the plots
#' @format A data frame with 542 rows and 22 variables:
#' \describe{
#'   \item{\code{Date}}{double Date at which the measurement has been performed}
#'   \item{\code{Parcelle}}{integer Name of the site}
#'   \item{\code{Traitement}}{character Name of the treatment. PA : Polygons slightly drained, PAS : Polygons drained by a secondary ice wedge,
#'   PS : polygons close to the shore, submitted to abundant eolian silt deposition, PO : flat to high-center mesic polygons,  PR : mesic meadow,
#'   1 : low-centered, wet polygon; 4 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3; 6 : wet polygons fertilized yearly with 1 g.m-2 of NH4NO3,
#'   10 : wet polygons fertilized yearly with 3 g.m-2 of H3PO4; 14 : wet polygons fertilized yearly with 1 g.m-2 of H3PO4 amd 5 g.m-2 of NH4NO3}
#'   \item{\code{Exclos}}{integer Fencing. Exclos : plots protected constantly by a fence; Temoin : unprotected plots}
#'   \item{\code{Thaw_depth}}{double Depth of the thaw front, as measured physically with a graduated probe, cm}
#'   \item{\code{Soil_temp}}{double Soil temperature at ~5cm, Celsius degree}
#'   \item{\code{SVWC}}{double Soil Volumetric Water content as derived from permittivity measurements, cm-3.cm-3}
#'   \item{\code{Theta_sat}}{double Proportion of soil porosity filled by water, 0-1}
#'   \item{\code{WaterTable_depth}}{double Depth of the water table, as measured in a hole, cm}
#'   \item{\code{pH}}{double Shallow soil pH, unitless}
#'   \item{\code{Dead_prop}}{double Proportion of the surface of the plot covered by dead plant material,
#'   as estimated from orthogonal photographs, 0-1}
#'   \item{\code{NDVI}}{double Normalized Difference Vegetation Index, 0-1}
#'   \item{\code{PRI}}{double Photochemical Reflectance index, -1 - 1}
#'   \item{\code{Reflectance_blue}}{double Reflectance in the blue color band, 0-1}
#'   \item{\code{Reflectance_green}}{double Reflectance in the green color band, 0-1}
#'   \item{\code{Reflectance_red}}{double Reflectance in the red color band, 0-1}
#'   \item{\code{Reflectance_visible}}{double Reflectance in the visible, 0-1}
#'   \item{\code{Reflectance_NIR}}{double Reflectance in the NIR color band, 0-1}
#'   \item{\code{Albedo}}{double Surface albedo, 0-1}
#'   \item{\code{Soil_Density}}{double Averaged density of soil in the first 5cm, g.cm-3}
#'   \item{\code{Soil_LOI}}{double Averaged soil organic matter content in the first 5cm, g.g-1}
#'   \item{\code{Soil_Porosity}}{double Averaged soil porosity in the first 5cm, 0-1}
#'   \item{\code{Geomorpho}}{integer A formulation of the Traitement columns focusing on discrete
#'   habitat differences}
#'   \item{\code{Fertilization_N}}{integer Specifies the quantity of N added each year in g.m-2 of NH4NO3}
#'   \item{\code{Fertilization_P}}{integer Specifies the quantity of P added each year in g.m-2 of H3PO4}
#'   \item{\code{Fertilization}}{integer A formulation of the Traitement columns with a zero to each unfertilized plots}
#'   \item{\code{Fertilization_P_bin}}{integer Binary variable indicating if the plot receive annual P fertilization}
#'}
#' @details DETAILS
"Environment_Date"
