## Function to create a complete species list, calculate species richness and species diversity for
## combined SCREAM and DRM data (FL only)

# Purpose:
# creates csv files with species list, species richness and species diversity


## Tag: data analysis


# outputs created in this file --------------
# species_list
# richness_site
# unwh_richness_strata
# Domain_est
# species_diversity_site
# species_diversity_strata


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Viehman, Bauer, Groves
# Last update: Sept 2018


##############################################################################################################################

#' Creates species list, species richness and species diversity dataframes from NCRMP and FRRP data
#'
#'
#'
#'
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'



DRM_SCREAM_calculate_species_richness_diversity <- function(){

  # Load data
  dat <- preNCRMP_SCREAM_DRM_coral_demographics %>%
    # Set LAT and LON to the same # of digits - helps with grouping
    dplyr::mutate(LAT_DEGREES = sprintf("%0.5f", LAT_DEGREES),
                  LON_DEGREES = sprintf("%0.5f", LON_DEGREES)) %>%
    # Filter out strata with no NTOT
    dplyr::filter(STRAT != "SAND_NA")

  # Create a list of all species found in data set
  species_2stage <- dat %>%
    dplyr::filter(N == 1,
                  !grepl('SPE.', SPECIES_CD),
                  !grepl('ANCX', SPECIES_CD)) %>%  # Filter out SPE
    dplyr::select(SPECIES_NAME, SPECIES_CD) %>%
    dplyr::distinct(SPECIES_NAME, SPECIES_CD)

  species_list <- species_2stage%>%
    unique(.)


  # Calculate species richness at the site level
  dat1 <- dat %>%
    dplyr::filter(N == 1,
                  SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                  !grepl('SPE.', SPECIES_CD),
                  !grepl('ANCX', SPECIES_CD)) %>% # Filter out SPE
    dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
    dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, PROT, SPECIES_NAME) %>% #No need to include region, will be added from ntot in wh. function
    dplyr::summarise(IndSumSite = sum(N))  %>%
    dplyr::mutate(present = 1) %>%
    dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>%
    dplyr::summarise(SppSumSite = sum(present)) %>%
    dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>%
    dplyr::summarise(SPP_RICHNESS = mean(SppSumSite)) %>%
    dplyr::ungroup()


  richness_site <- dat1

  # Add NTOT, # Cells sampled and calculate sampling weights in weighting function

  tmp  <- DRM_SCREAM_make_weighted_demo_data(inputdata = richness_site, datatype = "richness")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  # Site level
  sites <- dat %>%
    dplyr::filter(N == 1,
                  SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                  !grepl('SPE.', SPECIES_CD),
                  !grepl('ANCX', SPECIES_CD))  %>%
    dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT)) %>%
    dplyr::group_by(REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>%
    dplyr::summarize(PSU = unique(PRIMARY_SAMPLE_UNIT))

  species_wide_site <- dat %>%
    dplyr::filter(N == 1,
                  SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                  !grepl('SPE.', SPECIES_CD),
                  !grepl('ANCX', SPECIES_CD)) %>%
    dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT)) %>% #Convert PSU to character to make sure vegan::diversity does not use it as counts
    dplyr::group_by(REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT, SPECIES_NAME) %>%
    dplyr::summarise(SPP_Count = sum(N)) %>%
    tidyr::spread(., key = SPECIES_NAME, value = SPP_Count, fill = 0)

  species_only_site <-  species_wide_site[, c(4, 8:ncol(species_wide_site))]

  species_diversity_site <- species_only_site %>%
    dplyr::mutate(Simpson =  vegan::diversity(species_only_site[, -1], index = "simpson")) %>%
    dplyr::mutate(Inv_Simpson =  vegan::diversity(species_only_site[, -1], index = "invsimpson")) %>%
    dplyr::mutate(Shannon =  vegan::diversity(species_only_site[, -1], index = "shannon")) %>%
    dplyr::select(PRIMARY_SAMPLE_UNIT, Simpson, Inv_Simpson, Shannon ) %>%
    dplyr::inner_join(., sites) %>%
    dplyr::select(REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT, Simpson, Inv_Simpson, Shannon)





  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "species_list" = species_list,
    "richness_site" = richness_site,
    "unwh_richness_strata" = unwh_richness_strata,
    "Domain_est" = Domain_est,
    "species_diversity_site" = species_diversity_site
  )

  return(output)

}






