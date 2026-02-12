
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
# Last update: Jun 2025

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


NCRMP_calculate_ESA_corals_PresAbs <- function() {

  ####Change analysis strat Helper Function####
  change_analysis_strat <- function(data){
    data %>% dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
  }

  ####   SEFCRI   ####
  sefcri_datasets <- list(
    change_analysis_strat(SEFCRI_2014_2stage_inverts_ESAcorals),
    change_analysis_strat(SEFCRI_2016_inverts_ESAcorals),
    change_analysis_strat(SEFCRI_2018_inverts_ESAcorals),
    change_analysis_strat(SEFCRI_2020_inverts_ESAcorals),
    change_analysis_strat(SEFCRI_2022_inverts_ESAcorals),
    change_analysis_strat(SEFCRI_2024_inverts_ESAcorals)
  )

  ####  FLK   ####
  FLK_datasets <-list(
    FLK_2014_2stage_inverts_ESAcorals %>%  dplyr::mutate(YEAR = 2014, PROT = 0) %>% change_analysis_strat(),
    FLK_2016_inverts_ESAcorals %>% dplyr::mutate(PROT = 0) %>% change_analysis_strat(),
    FLK_2018_inverts_ESAcorals %>%  dplyr::mutate(PROT = 0) %>%  change_analysis_strat() %>% dplyr::select(-RUGOSITY_CD),
    FLK_2020_inverts_ESAcorals %>% dplyr::mutate(PROT = 0)%>% change_analysis_strat(),
    update_protection_status(
      (change_analysis_strat(FLK_2022_inverts_ESAcorals)), FLK_2020_sample_frame@data) %>%
                               dplyr::mutate(PROT = 0) ,
    FLK_2024_inverts_ESAcorals %>% dplyr::mutate(PROT = 0)%>% change_analysis_strat()
  )


  ####  Tortugas   ####
  Tort_datasets <- list(
    change_analysis_strat(TortugasMarq_2014_inverts_ESAcorals) %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans"),
    change_analysis_strat(TortugasMarq_2016_inverts_ESAcorals) %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans"),
    change_analysis_strat(Tortugas_2018_inverts_ESAcorals) %>%dplyr::select(-METERS_COMPLETED),
    change_analysis_strat(Tortugas_2020_inverts_ESAcorals) %>%dplyr::select(-METERS_COMPLETED),
    change_analysis_strat(Tortugas_2022_inverts_ESAcorals) %>%dplyr::select(-METERS_COMPLETED),
    change_analysis_strat(Tortugas_2024_inverts_ESAcorals) %>%dplyr::select(-METERS_COMPLETED)
  )

  ####Combine FL Datasets####
  FL <- dplyr::bind_rows(sefcri_datasets,FLK_datasets, Tort_datasets )  %>%
    # Change to factor - there are letters in the FGBNMS MAPGRID NRs
    dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))


  #####Mutate Analysis Strat####
  fix_strat <- function(data){
    data %>%dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))
  }


    ####Carib / fgb####
    # St. Thomas, St. John, & St. Croix
    usvi_datasets <- list(
      fix_strat(USVI_2013_inverts_ESAcorals),
      fix_strat(USVI_2015_inverts_ESAcorals),
      fix_strat(USVI_2017_inverts_ESAcorals),
      fix_strat(USVI_2019_inverts_ESAcorals),
      fix_strat(USVI_2021_inverts_ESAcorals),
      fix_strat(USVI_2023_inverts_ESAcorals),
      fix_strat(USVI_2025_inverts_ESAcorals)
    )

    #Puerto Rico
    prico_datasets <- list(
      fix_strat(PRICO_2014_inverts_ESAcorals),
      fix_strat(PRICO_2016_inverts_ESAcorals)%>%dplyr::mutate(YEAR = 2016),
      fix_strat(PRICO_2019_inverts_ESAcorals),
      fix_strat(PRICO_2021_inverts_ESAcorals),
      fix_strat(PRICO_2023_inverts_ESAcorals),
      fix_strat(PRICO_2025_inverts_ESAcorals)
    )

    ## Flower Garden Banks National Marine Sanctuary (fgb)
    fgb_datasets <- list(
      FGBNMS_2013_inverts_ESAcorals %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",MAPGRID_NR = as.factor(MAPGRID_NR)),
      FGBNMS_2015_inverts_ESAcorals %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",MAPGRID_NR = as.factor(MAPGRID_NR)),
      FGBNMS_2018_inverts_ESAcorals %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",MAPGRID_NR = as.factor(MAPGRID_NR)),
      FGBNMS_2022_inverts_ESAcorals %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",MAPGRID_NR = as.factor(MAPGRID_NR)),
      FGBNMS_2024_inverts_ESAcorals %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",MAPGRID_NR = as.factor(MAPGRID_NR))
    )


  #Combine Carib and FGB
    Carib_FGB <- dplyr::bind_rows(usvi_datasets, prico_datasets, fgb_datasets) %>%
    # Remove METERS_COMPLETED as it is missing from the FL data
    dplyr::select(-METERS_COMPLETED) %>%
    # Combine and change to factor - there are letters in the FGBNMS MAPGRID NRs
    dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))


  ####Combine FL, Carib, and FGB####

  dat <- dplyr::bind_rows(FL, Carib_FGB)

  ####Mutate Species Function###
  mutate_species <- function(data){
    data %>%dplyr::mutate(A_PALMATA = dplyr::case_when(A_PALMATA == 0 ~ 0, A_PALMATA > 0 ~ 1, TRUE ~ NA_real_),
                          A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == 0 ~ 0, A_CERVICORNIS > 0 ~ 1, TRUE ~ NA_real_),
                          D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == 0 ~ 0, D_CYLINDRUS > 0 ~ 1, TRUE ~ NA_real_),
                          M_FEROX = dplyr::case_when(M_FEROX == 0 ~ 0, M_FEROX > 0 ~ 1, TRUE ~ NA_real_),
                          O_ANNULARIS = dplyr::case_when(O_ANNULARIS == 0 ~ 0, O_ANNULARIS > 0 ~ 1, TRUE ~ NA_real_),
                          O_FRANKSI = dplyr::case_when(O_FRANKSI == 0 ~ 0, O_FRANKSI > 0 ~ 1, TRUE ~ NA_real_),
                          O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == 0 ~ 0, O_FAVEOLATA > 0 ~ 1, TRUE ~ NA_real_))
  }

  ####Mutate Years Function####
  mutate_years <- function(data){
    data %>%dplyr::mutate(ANALYSES_YEAR = dplyr::case_when(REGION == "FLK" & YEAR == 2015 ~ 2014,
                                                           REGION == "FLK" & YEAR == 2021 ~ 2020,
                                                           REGION == "PRICO" & YEAR == 2015 ~ 2014,
                                                           REGION == "PRICO" & YEAR == 2017 ~ 2016,
                                                           REGION == "SEFCRI" & YEAR == 2015 ~ 2014,
                                                           REGION == "SEFCRI" & YEAR == 2021 ~ 2020,
                                                           REGION == "Tortugas" & YEAR == 2021 ~ 2020,
                                                           TRUE ~ as.numeric(YEAR)))
  }

  ####Mutate Pres Abs Helper Function ####
  mutate_pres_abs <- function(data){
    data %>%    dplyr::mutate(A_PALMATA = dplyr::case_when(A_PALMATA == "A" ~ 0, A_PALMATA == "PT" ~ 1, A_PALMATA == "PS" ~ 1, A_PALMATA == "P" ~ 1, TRUE ~ NA_real_),
                              A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == "A" ~ 0, A_CERVICORNIS == "PT" ~ 1, A_CERVICORNIS == "PS" ~ 1, A_CERVICORNIS == "P" ~ 1, TRUE ~ NA_real_),
                              D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == "A" ~ 0, D_CYLINDRUS == "PT" ~ 1, D_CYLINDRUS == "PS" ~ 1, D_CYLINDRUS == "P" ~ 1, TRUE ~ NA_real_),
                              M_FEROX = dplyr::case_when(M_FEROX == "A" ~ 0, M_FEROX == "PT" ~ 1, M_FEROX == "PS" ~ 1, M_FEROX == "P" ~ 1, TRUE ~ NA_real_),
                              O_ANNULARIS = dplyr::case_when(O_ANNULARIS == "A" ~ 0, O_ANNULARIS == "PT" ~ 1, O_ANNULARIS == "PS" ~ 1, O_ANNULARIS == "P" ~ 1, TRUE ~ NA_real_),
                              O_FRANKSI = dplyr::case_when(D_CYLINDRUS == "A" ~ 0, O_FRANKSI == "PT" ~ 1, O_FRANKSI == "PS" ~ 1, O_FRANKSI == "P" ~ 1, TRUE ~ NA_real_),
                              O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == "A" ~ 0, O_FAVEOLATA == "PT" ~ 1, O_FAVEOLATA == "PS" ~ 1, O_FAVEOLATA == "P" ~ 1, TRUE ~ NA_real_))
  }

  ####Site Totals####
  site_totals <-  dat %>%
    # convert sampling years to analyses years for FL 2014/15 and PR2016/2017
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    mutate_years() %>%
    # convert A (Absence) to 0 and P (Presence) to 1
    mutate_pres_abs() %>%
    # sum ESA coral spp presence/absence by region, year, strata, habitat
    # AS OF 9/23...this doesn't seem necessary according to BW...
    dplyr::group_by(REGION, MONTH, DAY, ANALYSES_YEAR, ANALYSIS_STRATUM, STRAT, HABITAT_CD, MAX_DEPTH, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES) %>%
    dplyr::summarise_at(.vars = dplyr::vars ("A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                        sum, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    # convert strata-level sums to strata-level presence/absence (1/0) values
    mutate_species()%>%
    # add a column that has total # of ESA corals
    dplyr::mutate(N_ESAcoralspp = rowSums(dplyr::select(., ids = "A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                                          na.rm = TRUE)) %>%
    # after examination, drop any areas where strat = NA
    dplyr::filter(ANALYSIS_STRATUM != "NA")

  ####Strat Totals####
  strat_totals <-  dat %>%
    # convert sampling years to analyses years for FL 2014/15 and PR2016/2017
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    mutate_years() %>%
    # convert A (Absence) to 0 and P (Presence) to 1
    mutate_pres_abs() %>%

    # sum ESA coral spp presence/absence by region, year, strata, habitat
    dplyr::group_by(REGION, ANALYSES_YEAR, ANALYSIS_STRATUM, STRAT) %>%
    dplyr::summarise_at(.vars = dplyr::vars ("A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                        sum, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    # convert strata-level sums to strata-level presence/absence (1/0) values
    mutate_species()%>%
    # add a column that has total # of ESA corals
    dplyr::mutate(N_ESAcoralspp = rowSums(dplyr::select(., ids = "A_PALMATA", "A_CERVICORNIS", "D_CYLINDRUS", "M_FEROX", "O_ANNULARIS", "O_FRANKSI", "O_FAVEOLATA"),
                                          na.rm = TRUE)) %>%
    # after examination, drop any areas where strat = NA
    dplyr::filter(ANALYSIS_STRATUM != "NA")

  ######Check Levels#####
  region_year <- strat_totals %>%
    dplyr::select(REGION, ANALYSES_YEAR) %>%
    tidyr::unite(REGION_YEAR, REGION, ANALYSES_YEAR) %>%
    dplyr::distinct(REGION_YEAR)

  ####Export####
  output <- list("NCRMP_AllRegions_Years_ESA_PresAbs_Strat" = strat_totals,
                 "NCRMP_AllRegions_Years_ESA_PresAbs_Site" = site_totals,
                 "region_year" = region_year)

  return(output)
}
