## Function to create a complete list of abundance per bleaching category by site and species, all years and regions

# Purpose:
# creates csv files with abundance per bleaching category


## Tag: data analysis


# outputs created in this file --------------
# ESA_NCRMP
# ESA_FRRP
# ESA_combined


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Viehman, Groves, Williams
# Last update: Jan 2024


##############################################################################################################################

#' Creates abundance per bleaching category dataframe for all regions and all years
#'
#'
#'
#'
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "case_when"
#' @export
#'
#'


NCRMP_make_bleaching_abundance <- function(){



  # Load NCRMP all regions, all years demo data

  dat <- NCRMP_demo_All_regions_years %>%
    # subset to species present, site data and bleaching columns of interest
    dplyr::filter(N == 1) %>%
    dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, STATION_NR, YEAR, MONTH, DAY, HABITAT_CD, STRAT,
                  RUGOSITY_CD, WTD_RUG, LAT_DEGREES, LON_DEGREES, MAPGRID_NR, SUB_REGION_NAME,
                  SUB_REGION_NR, ZONE_NAME, ZONE_NR, MPA_NAME, MPA_NR, ADMIN, PROT,
                  DEPTH_STRAT, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED, SPECIES_CD,
                  SPECIES_NAME, N, BLEACH_CONDITION) %>%
    dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, YEAR, MONTH, DAY, HABITAT_CD,
                    STRAT, RUGOSITY_CD, WTD_RUG, LAT_DEGREES, LON_DEGREES, MAPGRID_NR,
                    SUB_REGION_NAME, SUB_REGION_NR, ZONE_NAME, ZONE_NR, MPA_NAME, MPA_NR,
                    PROT, DEPTH_STRAT, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME, BLEACH_CONDITION) %>%
    dplyr::summarise(tot = sum(N)) %>%
    dplyr::mutate(BLEACH_CONDITION = as.character(BLEACH_CONDITION)) %>%
    dplyr::mutate(BLEACH_CONDITION = tidyr::replace_na(BLEACH_CONDITION, "NO_DATA")) %>%
    dplyr::mutate(BLEACH_CONDITION = replace(BLEACH_CONDITION, BLEACH_CONDITION == "N/A", "NO_DATA")) %>%
    # recode bleached definitions
    dplyr::mutate(BLEACH_CONDITION = case_when(BLEACH_CONDITION == "N" ~ "NO_BLEACHING",
                                               BLEACH_CONDITION == "T" ~ "TOTAL_BLEACHING",
                                               BLEACH_CONDITION == "P" & YEAR < 2018 ~ "PARTIAL_BLEACHING",
                                               BLEACH_CONDITION == "PB" & YEAR >= 2018 ~ "PARTIAL_BLEACHING",
                                               BLEACH_CONDITION == "P" & YEAR >= 2018 ~ "PALING",
                                               BLEACH_CONDITION == "B" ~ "BLEACHED",
                                               TRUE ~ BLEACH_CONDITION
                                               )) %>%
    tidyr::spread(., BLEACH_CONDITION, tot, fill = 0) %>%
    dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, YEAR, MONTH, DAY, HABITAT_CD,
                  STRAT, RUGOSITY_CD, WTD_RUG, LAT_DEGREES, LON_DEGREES, MAPGRID_NR,
                  SUB_REGION_NAME, SUB_REGION_NR, ZONE_NAME, ZONE_NR, MPA_NAME, MPA_NR,
                  PROT, DEPTH_STRAT, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME, BLEACHED,
                  TOTAL_BLEACHING, PARTIAL_BLEACHING, PALING, NO_BLEACHING, NO_DATA)


  NCRMP_bleaching_abundance_Allyears_Allregions <- dat

  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "NCRMP_bleaching_abundance_Allyears_Allregions" = NCRMP_bleaching_abundance_Allyears_Allregions)

  return(output)

}
