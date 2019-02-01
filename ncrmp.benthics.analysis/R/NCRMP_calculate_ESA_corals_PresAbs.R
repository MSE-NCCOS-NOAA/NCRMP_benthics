
#### Function to query # ESA coral species per region, year, strata.
# Also provides ESA coral species presence/absence by strata and region?



# Purpose:
# creates csv file with ESA species presence/absence and total number ESA species present for each region, year, strata.


## Tag: data analysis


# outputs created in this file --------------
# NCRMP_AllRegions_Years_ESA_PresAbs
# region_year


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Nov 2018


##############################################################################################################################

#' Creates colony density and colony size summary dataframes
#'
#'
#'
#'
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


# Specify AR dataset for Inverts/ESA corals as function inputs
NCRMP_calculate_ESA_corals_PresAbs <- function()
{

  # Load analysis ready data from package

  # SEFCRI
  tmp1 <- SEFCRI_2014_2stage_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  tmp2 <- SEFCRI_2016_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  # FLK
  tmp3 <- FLK_2014_2stage_inverts_ESAcorals %>%
    dplyr::mutate(YEAR = 2014,
                  ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  tmp4 <- FLK_2016_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  # Tortugas
  tmp5 <- TortugasMarq_2014_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
    dplyr::filter(SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans")

  tmp6 <- TortugasMarq_2016_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
    dplyr::filter(SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans")

  #Combine FL
  FL <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)

  # Carib / GOM
  # St. Thomas, St. John, & St. Croix

  tmp1 <- USVI_2013_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp2 <- USVI_2015_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp3 <- USVI_2017_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  # Puerto Rico
  tmp4 <- PRICO_2014_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp5 <- PRICO_2016_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                  YEAR = 2016)

  ## Flower Garden Banks National Marine Sanctuary (GOM)

  tmp6 <- FGBNMS_2013_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")

  tmp7 <- FGBNMS_2015_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")

  tmp8 <- FGBNMS_2018_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")

  #Combine Carib and GOM
  Carib_GOM <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) %>%
    # Remove METERS_COMPLETED as it is missing from the FL data
    dplyr::select(-METERS_COMPLETED)

  # Combine FL, Carib, and GOM

  dat <- rbind(FL, Carib_GOM)

  strat_totals <-  dat %>%
    # convert sampling years to analyses years for FL 2014/15 and PR2016/2017
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    dplyr::mutate(ANALYSES_YEAR = dplyr::case_when(REGION == "FLK" & YEAR == 2015 ~ 2014,
                                                   REGION == "PRICO" & YEAR == 2015 ~ 2014,
                                                   REGION == "PRICO" & YEAR == 2017 ~ 2016,
                                                   REGION == "SEFCRI" & YEAR == 2015 ~ 2014,
                                                   TRUE ~ as.numeric(YEAR))) %>%
    # convert A (Absence) to 0 and P (Presence) to 1
    dplyr::mutate_at(.vars = dplyr::vars ("A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                     .funs = dplyr::funs(ifelse(. == "A", 0,
                                                ifelse(. == "P", 1, NA_integer_))
                     )) %>%
    # sum ESA coral spp presence/absence by region, year, strat
    dplyr::group_by(REGION, ANALYSES_YEAR, ANALYSIS_STRATUM, STRAT) %>%
    dplyr::summarise_at(.vars = dplyr::vars ("A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                        sum, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    # convert strata-level sums to strata-level presence/absence (1/0) values
    dplyr::mutate_at(.vars = dplyr::vars ("A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                     .funs = dplyr::funs(ifelse(. > 0 , 1,
                                                ifelse(. == 0, 0, NA_integer_))
                     )) %>%
    # add a column that has total # of ESA corals
    dplyr::mutate(N_ESAcoralspp = rowSums(dplyr::select(., ids = "A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                                          na.rm = TRUE)) %>%
    # after examination, drop any areas where strat = NA
    dplyr::filter(ANALYSIS_STRATUM != "NA")

  # check levels
  region_year <- strat_totals %>%
    dplyr::select(REGION, ANALYSES_YEAR) %>%
    tidyr::unite(REGION_YEAR, REGION, ANALYSES_YEAR) %>%
    dplyr::distinct(REGION_YEAR)


  ################
  # Export
  ################

  # Create list to export

  output <- list("NCRMP_AllRegions_Years_ESA_PresAbs" = strat_totals,
                 "region_year" = region_year)

  return(output)


}



