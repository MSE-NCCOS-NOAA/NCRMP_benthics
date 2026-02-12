## Function to calculate diadema density

# Purpose:
# creates csv files with colony density and colony size structure.
# Creates figures with size distribution curves.


## Tag: data analysis


# outputs created in this file --------------
# diadema_density_site
# diadema_density_strata
# Domain est


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Mar 2025


##############################################################################################################################

#' Creates summary dataframes of Diadema density from NCRMP benthic assessment.
#' NCRMP utilizes a stratified random sampling design.
#' Regional estimates of Diadema density are weighted by the number of
#' grid cells of a stratum in the sample frame.
#'
#'
#'
#'
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "FGB".
#' @param project A string indicating the project: "NCRMP" or "MIR". Default is NCRMP.
#' @return A list dataframes including 1) Diadema density at each site,
#' 2) mean Diadema density by strata, and 3) weighted regional mean Diadema density,
#' all for a given region.
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_calculate_invert_density <- function(region, project = "NULL") {

  ####Load in Data####
  if(region == "SEFCRI"){
    dat_2stage <- SEFCRI_2014_2stage_inverts_ESAcorals
    dat_1stage <- dplyr::bind_rows(SEFCRI_2016_inverts_ESAcorals, SEFCRI_2018_inverts_ESAcorals, SEFCRI_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020), SEFCRI_2022_inverts_ESAcorals, SEFCRI_2024_inverts_ESAcorals)
  }

  #### Clean up  FLK ####
  if(region == "FLK"){
    if(project == "NCRMP" || project == "NULL"){

    dat_2stage <- FLK_2014_2stage_inverts_ESAcorals %>%
      dplyr::mutate(YEAR = 2014)

    #list of datasets besides 2022
    datasets <- list(FLK_2016_inverts_ESAcorals,  FLK_2018_inverts_ESAcorals, FLK_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020), FLK_2024_inverts_ESAcorals)
    #update 2022
    updated_2022 <-  update_protection_status(FLK_2022_inverts_ESAcorals, FLK_2020_sample_frame@data)
    #bind rows
    dat_1stage <- dplyr::bind_rows(datasets, updated_2022)

    ### UPDATE IN DEC. 2023!!
    # PROT is re-coded here to 0 for ALL sites as fish and benthics met 12/19/23
    # to determine that it is not appropraite to keep PROT in the analysis strat
    # in FLK because the data aren't allocated that way
    # only affects FLK data
    dat_1stage <- dat_1stage %>% dplyr::mutate(PROT = as.factor(0))
    dat_2stage <- dat_2stage %>% dplyr::mutate(PROT = as.factor(0))
    }

    if(project == "MIR"){
      dat_1stage <- MIR_2022_inverts_ESAcorals_DUMMY %>%
        dplyr::mutate(SURVEY = "MIR",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      REGION = "FLK",
                      SUB_REGION_NAME = MIR_zone)
    }
  } #end florida regions

  #### Clean up  Tortugas ####
  if(region == "Tortugas"){

    one_stage <- list(TortugasMarq_2014_inverts_ESAcorals,
                      TortugasMarq_2016_inverts_ESAcorals,
                      Tortugas_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020) %>%
                        dplyr::mutate(STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT))),
                      Tortugas_2022_inverts_ESAcorals,
                      Tortugas_2024_inverts_ESAcorals)

    dat_2stage <- Tortugas_2018_inverts_ESAcorals
    dat_1stage <- dplyr::bind_rows(one_stage)

  } #end tortugas

  #### Clean up   St. Thomas & St. John ####
  if(region == "STTSTJ"){
    #make a list of the datasets filtered to only st thomas st john
    datasets <- list(USVI_2013_inverts_ESAcorals %>% filter(REGION == "STTSTJ"),
                     USVI_2015_inverts_ESAcorals %>% filter(!is.na(DIADEMA_NUM))%>% filter(REGION == "STTSTJ"),
                     USVI_2017_inverts_ESAcorals%>% filter(REGION == "STTSTJ"),
                     USVI_2019_inverts_ESAcorals%>% filter(REGION == "STTSTJ"),
                     USVI_2021_inverts_ESAcorals%>% filter(REGION == "STTSTJ"),
                     USVI_2023_inverts_ESAcorals%>% filter(REGION == "STTSTJ"),
                     USVI_2025_inverts_ESAcorals%>% filter(REGION == "STTSTJ"))

    #Combine 1 stage or 2 stage data
    dat_1stage <-dplyr::bind_rows(datasets) %>%
      dplyr::mutate(DIADEMA_NUM = as.numeric(DIADEMA_NUM))
  } #end sttstj

  #### Clean up   St. Croix ####
  if(region == "STX"){
  # make a list of the datasets only including region = stx
    datasets <- list(
      USVI_2015_inverts_ESAcorals %>% filter(DIADEMA_NUM != "N/A", STRAT != "BDRK_DEEP", REGION == "STX"),
      USVI_2017_inverts_ESAcorals %>% filter(REGION == "STX"),
      USVI_2019_inverts_ESAcorals %>% filter(REGION == "STX"),
      USVI_2021_inverts_ESAcorals %>% filter(REGION == "STX"),
      USVI_2023_inverts_ESAcorals %>% filter(REGION == "STX"),
      USVI_2025_inverts_ESAcorals%>% filter(REGION == "STX")
    )
    dat_1stage <-dplyr::bind_rows(datasets)
  } #end stx

  #### Clean up   Puerto Rico ####
  if(region == "PRICO"){

    datasets <- list(PRICO_2014_inverts_ESAcorals,
                     PRICO_2016_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2016),
                     PRICO_2019_inverts_ESAcorals,
                     PRICO_2021_inverts_ESAcorals,
                     PRICO_2023_inverts_ESAcorals,
                     PRICO_2025_inverts_ESAcorals)

    #Combine data
    dat_1stage <- dplyr::bind_rows(datasets) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT)
  } #end PRICO

  #### Clean up   FGB ####
  if(region == "FGB"){

    #helper function for cleaning up FGB data
    mutate_FGB <- function(data){
      data %>%dplyr::mutate(SURVEY = "NCRMP",
                            ANALYSIS_STRATUM = "FGBNMS",
                            STRAT = "FGBNMS",
                            REGION = "FGB",
                            MAPGRID_NR = as.factor(MAPGRID_NR))

    }
     #list the data sets only including flower gardens
    #this calls the  helper function that cleans and mutates the datasets
    datasets <- list(mutate_FGB(FGBNMS_2013_inverts_ESAcorals),
                     mutate_FGB(FGBNMS_2015_inverts_ESAcorals),
                     mutate_FGB(FGBNMS_2018_inverts_ESAcorals),
                     mutate_FGB(FGBNMS_2022_inverts_ESAcorals),)

    #Combine data
    dat_1stage <- dplyr::bind_rows(datasets)

  } #end FGB


  ####Take the transect means (Account for SEFCRI/FLK 2014 2 stage data)####
  clean_data <- function(data) {
    # Remove Marquesas
    data %>% dplyr::filter(SUB_REGION_NAME != "Marquesas",
                           SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      # Change column class
      dplyr::mutate(YEAR = as.numeric(YEAR),
                    PROT = as.factor(PROT),
                    DIADEMA_NUM = as.numeric(as.character(DIADEMA_NUM)))
  }

  one_stage_diadema_dens <- function(data){
    data %>% dplyr::mutate(Diadema_dens = dplyr::case_when(REGION == "FLK" ~ DIADEMA_NUM/(15 * 2),
                                                           REGION == "Tortugas" ~ DIADEMA_NUM/(15 * 2),
                                                           REGION == "SEFCRI" ~ DIADEMA_NUM/(15 * 2),
                                                           REGION == "STTSTJ" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                           REGION == "STX" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                           REGION == "PRICO" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                           REGION == "FGBNMS" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                           TRUE ~ DIADEMA_NUM/(25 * 2) ))
  }

  ####calculate invert statistics ####
  #### Flordia Regions ####
  if(project == "NCRMP" && region == "SEFCRI" || project == "NCRMP" && region == "FLK" ||  project == "NCRMP" && region == "Tortugas") {

    dat1_1stage <- dat_1stage %>%
      clean_data() %>%
      # could expand here to include lobster and conch density
      # convert counts DIADEMA_NUM to density DIADEMA_DENS per site
      one_stage_diadema_dens() %>%
      # select columns
      dplyr::select(YEAR, MONTH, DAY, REGION, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                      MIN_DEPTH, MAX_DEPTH, STRAT, PROT, LOBSTER_NUM, CONCH_NUM, DIADEMA_NUM, Diadema_dens)

    dat1_2stage <- dat_2stage %>%
      clean_data() %>%
      # could expand here to include lobster and conch density
      # convert counts DIADEMA_NUM to density DIADEMA_DENS per transect
      dplyr::mutate(Diadema_dens = dplyr::case_when(REGION == "FLK" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "SEFCRI" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "Tortugas" ~ DIADEMA_NUM/(15 * 2),
                                                    TRUE ~ DIADEMA_NUM/(25 * 2) )) %>%
      # Calculate site density by taking the mean of 2 transects
      dplyr::group_by(YEAR, MONTH, DAY, REGION, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                      STRAT, PROT) %>%
      dplyr::summarise(MIN_DEPTH = mean(MIN_DEPTH),
                       MAX_DEPTH = mean(MAX_DEPTH),
                       LOBSTER_NUM = mean(LOBSTER_NUM, na.rm=T),
                       CONCH_NUM = mean(CONCH_NUM, na.rm=T),
                       DIADEMA_NUM = mean(DIADEMA_NUM, na.rm=T),
                       Diadema_dens = mean(Diadema_dens, na.rm=T))

    diadema_density_site <- dplyr::bind_rows(dat1_1stage, dat1_2stage)

    #### Non Florida Regions ####
  } else {
    diadema_density_site <- dat_1stage %>%
      filter(DIADEMA_NUM != "NA") %>%
      clean_data() %>%
      # could expand here to include lobster and conch density
      # convert counts DIADEMA_NUM to density DIADEMA_DENS per site
      one_stage_diadema_dens() %>%
      # drop columns
      dplyr::select(YEAR, MONTH, DAY, REGION, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                      MIN_DEPTH, MAX_DEPTH, STRAT, PROT, LOBSTER_NUM, CONCH_NUM, DIADEMA_NUM, Diadema_dens)

  }

  #### call function for weighting   ####
  tmp <- NCRMP_make_weighted_invert_density_data(inputdata = diadema_density_site, region, project)
  list2env(tmp, envir = .GlobalEnv)

  ####Export####
  output <- list(
    "diadema_density_site" = diadema_density_site,
    "invert_strata" = invert_strata,
    "Domain_est" = Domain_est)

  return(output)
}


