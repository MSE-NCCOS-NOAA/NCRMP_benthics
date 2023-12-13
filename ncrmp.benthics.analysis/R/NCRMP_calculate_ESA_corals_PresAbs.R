
#### Function to query # ESA coral species per region, year, strata.
# Also provides ESA coral species presence/absence by strata and site



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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Jan 2023

##############################################################################################################################

#' Creates ESA presence/absence dataframes by strata and site
#'
#' Creates summaries of ESA coral presence/absence collected from LPI data at both
#' strata and site level for all years and regions.
#'
#'
#'
#'
#'
#' @return A list of dataframes including 1) strata level ESA coral presence/absence,
#' 2) site level ESA coral presence/absence, and 3) a check that all region/years are
#' included.
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

  tmp2.1 <- SEFCRI_2018_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  tmp2.2 <- SEFCRI_2020_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  tmp2.3 <- SEFCRI_2022_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  # FLK
  tmp3 <- FLK_2014_2stage_inverts_ESAcorals %>%
    dplyr::mutate(YEAR = 2014,
                  ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  tmp4 <- FLK_2016_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  tmp4.1 <- FLK_2018_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
    dplyr::select(-RUGOSITY_CD)

  tmp4.2 <- FLK_2020_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  tmp4.3 <- FLK_2022_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
  # UPDATE THE PROT (MIR sites initially labeled as PROT=2)
  grid_df <- FLK_2020_sample_frame@data
  new_prots <- grid_df %>% dplyr::select(MAPGRID_NR, PROT) %>% dplyr::rename("PROT_og" = PROT) %>% dplyr::mutate(MAPGRID_NR = as.numeric(MAPGRID_NR), PROT_og = as.numeric(PROT_og))
  tmp4.3 <- tmp4.3 %>% dplyr::left_join(., new_prots, by = c("MAPGRID_NR")) %>%
    # fix any that get left out manually, they fell outside the grid and JB fixed them
    dplyr::mutate(PROT_og = case_when(PRIMARY_SAMPLE_UNIT == 1006 ~ 0, PRIMARY_SAMPLE_UNIT == 1382 ~ 1, TRUE ~ PROT_og)) %>%
    dplyr::select(-PROT) %>%
    dplyr::rename("PROT" = PROT_og)

  # Tortugas
  tmp5 <- TortugasMarq_2014_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
    dplyr::filter(SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans")

  tmp6 <- TortugasMarq_2016_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
    dplyr::filter(SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans")

  tmp7 <- Tortugas_2018_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
    # Remove METERS_COMPLETED as it is missing from the other FL data
    dplyr::select(-METERS_COMPLETED)

  tmp7.1 <- Tortugas_2020_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
    # Remove METERS_COMPLETED as it is missing from the other FL data
    dplyr::select(-METERS_COMPLETED)

  tmp7.2 <- Tortugas_2022_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
    # Remove METERS_COMPLETED as it is missing from the other FL data
    dplyr::select(-METERS_COMPLETED)

  #Combine FL
  FL <- dplyr::bind_rows(tmp1, tmp2, tmp2.1, tmp2.2, tmp2.3, tmp3, tmp4, tmp4.1, tmp4.2, tmp4.3, tmp5, tmp6, tmp7, tmp7.1, tmp7.2 )  %>%
  # Change to factor - there are letters in the FGBNMS MAPGRID NRs
  dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))


  # Carib / GOM
  # St. Thomas, St. John, & St. Croix

  tmp1 <- USVI_2013_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp2 <- USVI_2015_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp3 <- USVI_2017_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp3.1 <- USVI_2019_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp3.2 <- USVI_2021_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  # Puerto Rico
  tmp4 <- PRICO_2014_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp5 <- PRICO_2016_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                  YEAR = 2016)

  tmp5.1 <- PRICO_2019_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  tmp5.2 <- PRICO_2021_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  ## Flower Garden Banks National Marine Sanctuary (GOM)

  tmp6 <- FGBNMS_2013_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")

  tmp7 <- FGBNMS_2015_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")

  tmp8 <- FGBNMS_2018_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")

  tmp8.1 <- FGBNMS_2022_inverts_ESAcorals %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")

  #Combine Carib and GOM
  tmp9 <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp3.1, tmp3.2, tmp4, tmp5, tmp5.1, tmp5.2, tmp8, tmp8.1) %>%
    # Remove METERS_COMPLETED as it is missing from the FL data
    dplyr::select(-METERS_COMPLETED) %>%
  # Combine and change to factor - there are letters in the FGBNMS MAPGRID NRs
  dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))

  Carib_GOM <- dplyr::bind_rows(tmp9, tmp6, tmp7)

  # Combine FL, Carib, and GOM

  dat <- dplyr::bind_rows(FL, Carib_GOM)

  site_totals <-  dat %>%
    # convert sampling years to analyses years for FL 2014/15 and PR2016/2017
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    dplyr::mutate(ANALYSES_YEAR = dplyr::case_when(REGION == "FLK" & YEAR == 2015 ~ 2014,
                                                   REGION == "FLK" & YEAR == 2021 ~ 2020,
                                                   REGION == "PRICO" & YEAR == 2015 ~ 2014,
                                                   REGION == "PRICO" & YEAR == 2017 ~ 2016,
                                                   REGION == "SEFCRI" & YEAR == 2015 ~ 2014,
                                                   REGION == "SEFCRI" & YEAR == 2021 ~ 2020,
                                                   REGION == "Tortugas" & YEAR == 2021 ~ 2020,
                                                   TRUE ~ as.numeric(YEAR))) %>%
    # convert A (Absence) to 0 and P (Presence) to 1
    dplyr::mutate(A_PALMATA = dplyr::case_when(A_PALMATA == "A" ~ 0, A_PALMATA == "PT" ~ 1, A_PALMATA == "PS" ~ 1, A_PALMATA == "P" ~ 1, TRUE ~ NA_real_),
                  A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == "A" ~ 0, A_CERVICORNIS == "PT" ~ 1, A_CERVICORNIS == "PS" ~ 1, A_CERVICORNIS == "P" ~ 1, TRUE ~ NA_real_),
                  D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == "A" ~ 0, D_CYLINDRUS == "PT" ~ 1, D_CYLINDRUS == "PS" ~ 1, D_CYLINDRUS == "P" ~ 1, TRUE ~ NA_real_),
                  M_FEROX = dplyr::case_when(M_FEROX == "A" ~ 0, M_FEROX == "PT" ~ 1, M_FEROX == "PS" ~ 1, M_FEROX == "P" ~ 1, TRUE ~ NA_real_),
                  O_ANNULARIS = dplyr::case_when(O_ANNULARIS == "A" ~ 0, O_ANNULARIS == "PT" ~ 1, O_ANNULARIS == "PS" ~ 1, O_ANNULARIS == "P" ~ 1, TRUE ~ NA_real_),
                  O_FRANKSI = dplyr::case_when(D_CYLINDRUS == "A" ~ 0, O_FRANKSI == "PT" ~ 1, O_FRANKSI == "PS" ~ 1, O_FRANKSI == "P" ~ 1, TRUE ~ NA_real_),
                  O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == "A" ~ 0, O_FAVEOLATA == "PT" ~ 1, O_FAVEOLATA == "PS" ~ 1, O_FAVEOLATA == "P" ~ 1, TRUE ~ NA_real_)) %>%

    # sum ESA coral spp presence/absence by region, year, strata, habitat
    # AS OF 9/23...this doesn't seem necessary according to BW...
    dplyr::group_by(REGION, MONTH, DAY, ANALYSES_YEAR, ANALYSIS_STRATUM, STRAT, HABITAT_CD, MAX_DEPTH, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES) %>%
    dplyr::summarise_at(.vars = dplyr::vars ("A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                        sum, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    # convert strata-level sums to strata-level presence/absence (1/0) values
    dplyr::mutate(A_PALMATA = dplyr::case_when(A_PALMATA == 0 ~ 0, A_PALMATA > 0 ~ 1, TRUE ~ NA_real_),
                  A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == 0 ~ 0, A_CERVICORNIS > 0 ~ 1, TRUE ~ NA_real_),
                  D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == 0 ~ 0, D_CYLINDRUS > 0 ~ 1, TRUE ~ NA_real_),
                  M_FEROX = dplyr::case_when(M_FEROX == 0 ~ 0, M_FEROX > 0 ~ 1, TRUE ~ NA_real_),
                  O_ANNULARIS = dplyr::case_when(O_ANNULARIS == 0 ~ 0, O_ANNULARIS > 0 ~ 1, TRUE ~ NA_real_),
                  O_FRANKSI = dplyr::case_when(O_FRANKSI == 0 ~ 0, O_FRANKSI > 0 ~ 1, TRUE ~ NA_real_),
                  O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == 0 ~ 0, O_FAVEOLATA > 0 ~ 1, TRUE ~ NA_real_)) %>%
    # add a column that has total # of ESA corals
    dplyr::mutate(N_ESAcoralspp = rowSums(dplyr::select(., ids = "A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                                          na.rm = TRUE)) %>%
    # after examination, drop any areas where strat = NA
    dplyr::filter(ANALYSIS_STRATUM != "NA")

  strat_totals <-  dat %>%
    # convert sampling years to analyses years for FL 2014/15 and PR2016/2017
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    dplyr::mutate(ANALYSES_YEAR = dplyr::case_when(REGION == "FLK" & YEAR == 2015 ~ 2014,
                                                   REGION == "FLK" & YEAR == 2021 ~ 2020,
                                                   REGION == "PRICO" & YEAR == 2015 ~ 2014,
                                                   REGION == "PRICO" & YEAR == 2017 ~ 2016,
                                                   REGION == "SEFCRI" & YEAR == 2015 ~ 2014,
                                                   REGION == "SEFCRI" & YEAR == 2021 ~ 2020,
                                                   REGION == "Tortugas" & YEAR == 2021 ~ 2020,
                                                   TRUE ~ as.numeric(YEAR))) %>%
    # convert A (Absence) to 0 and P (Presence) to 1
    dplyr::mutate(A_PALMATA = dplyr::case_when(A_PALMATA == "A" ~ 0, A_PALMATA == "PT" ~ 1, A_PALMATA == "PS" ~ 1, A_PALMATA == "P" ~ 1, TRUE ~ NA_real_),
                  A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == "A" ~ 0, A_CERVICORNIS == "PT" ~ 1, A_CERVICORNIS == "PS" ~ 1, A_CERVICORNIS == "P" ~ 1, TRUE ~ NA_real_),
                  D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == "A" ~ 0, D_CYLINDRUS == "PT" ~ 1, D_CYLINDRUS == "PS" ~ 1, D_CYLINDRUS == "P" ~ 1, TRUE ~ NA_real_),
                  M_FEROX = dplyr::case_when(M_FEROX == "A" ~ 0, M_FEROX == "PT" ~ 1, M_FEROX == "PS" ~ 1, M_FEROX == "P" ~ 1, TRUE ~ NA_real_),
                  O_ANNULARIS = dplyr::case_when(O_ANNULARIS == "A" ~ 0, O_ANNULARIS == "PT" ~ 1, O_ANNULARIS == "PS" ~ 1, O_ANNULARIS == "P" ~ 1, TRUE ~ NA_real_),
                  O_FRANKSI = dplyr::case_when(D_CYLINDRUS == "A" ~ 0, O_FRANKSI == "PT" ~ 1, O_FRANKSI == "PS" ~ 1, O_FRANKSI == "P" ~ 1, TRUE ~ NA_real_),
                  O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == "A" ~ 0, O_FAVEOLATA == "PT" ~ 1, O_FAVEOLATA == "PS" ~ 1, O_FAVEOLATA == "P" ~ 1, TRUE ~ NA_real_)) %>%

    # sum ESA coral spp presence/absence by region, year, strata, habitat
    dplyr::group_by(REGION, ANALYSES_YEAR, ANALYSIS_STRATUM, STRAT) %>%
    dplyr::summarise_at(.vars = dplyr::vars ("A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                        sum, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    # convert strata-level sums to strata-level presence/absence (1/0) values
    dplyr::mutate(A_PALMATA = dplyr::case_when(A_PALMATA == 0 ~ 0, A_PALMATA > 0 ~ 1, TRUE ~ NA_real_),
                  A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == 0 ~ 0, A_CERVICORNIS > 0 ~ 1, TRUE ~ NA_real_),
                  D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == 0 ~ 0, D_CYLINDRUS > 0 ~ 1, TRUE ~ NA_real_),
                  M_FEROX = dplyr::case_when(M_FEROX == 0 ~ 0, M_FEROX > 0 ~ 1, TRUE ~ NA_real_),
                  O_ANNULARIS = dplyr::case_when(O_ANNULARIS == 0 ~ 0, O_ANNULARIS > 0 ~ 1, TRUE ~ NA_real_),
                  O_FRANKSI = dplyr::case_when(O_FRANKSI == 0 ~ 0, O_FRANKSI > 0 ~ 1, TRUE ~ NA_real_),
                  O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == 0 ~ 0, O_FAVEOLATA > 0 ~ 1, TRUE ~ NA_real_)) %>%
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

  output <- list("NCRMP_AllRegions_Years_ESA_PresAbs_Strat" = strat_totals,
                 "NCRMP_AllRegions_Years_ESA_PresAbs_Site" = site_totals,
                 "region_year" = region_year)

  return(output)


}



