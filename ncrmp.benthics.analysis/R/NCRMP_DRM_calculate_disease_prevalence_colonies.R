## Function to calculate disease prevalence & bleaching prevalence for NCRMP, MIR, and NCRMP + DRM data (FL only) by calculating species then colony prevalence (%) at the site level,
## taking the mean of all sites within each strata, strata area weighting each strata and summing all strata means to reach the domain estimate.

# Purpose:
# creates csv files with disease/bleaching prevalence by region


## Tag: data analysis


# outputs created in this file --------------
# disease_prev_site
# disease_prev_strata
# Domain estimates

# CallS:
# analysis ready data

# output gets called by:
# NCRMP_DRM_calculate_dis_ble_prevalence_species_domain.R
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Feb 2023


##############################################################################################################################

#' disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data at the species/site, site, strata and domain levels
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP, MIR, or NCRMP and DRM combined
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_DRM_calculate_disease_prevalence_colonies <- function(project, region, species_filter = "NULL"){


  # Load data
  tmp <- load_NCRMP_DRM_demo_data(project = project,
                                  region = region,
                                  species_filter = species_filter)

  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  # Calulate site level disease prevalence

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    dat1_1stage <- dat_1stage %>%
      dplyr::filter(N == 1,
                    DISEASE != "N/A",
                    JUV == 0,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::filter(!(is.na(DISEASE))) %>%
       dplyr::mutate(PROT = as.factor(PROT),
                     LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                     LON_DEGREES = sprintf("%0.4f", LON_DEGREES),
                     PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1,
                                               DISEASE == "F" ~ 1,
                                               DISEASE == "S" ~ 1,TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_ble = sum(BLEACH),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100,
                       BLE_PREV = (Total_ble/Total_col)*100, .groups = "keep") %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)),
                    BLE_PREV = as.numeric(sprintf("%0.1f", BLE_PREV)))


    dis_species_1stage <- dat_1stage %>%
      dplyr::filter(N == 1,
                    DISEASE != "N/A",
                    JUV == 0,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::filter(!(is.na(DISEASE))) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES),
                    PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1,
                                               DISEASE == "F" ~ 1,
                                               DISEASE == "S" ~ 1,TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_ble = sum(BLEACH),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100,
                       BLE_PREV = (Total_ble/Total_col)*100, .groups = "keep") %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)),
                    BLE_PREV = as.numeric(sprintf("%0.1f", BLE_PREV)))


    dat1_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    DISEASE != "N/A",
                    JUV == 0,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES),
                    PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1,
                                               DISEASE == "F" ~ 1,
                                               DISEASE == "S" ~ 1,TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1,
                                              BLEACH_CONDITION == "PL" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_ble = sum(BLEACH),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100,
                       BLE_PREV = (Total_ble/Total_col)*100, .groups = "keep") %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = mean(Total_dis),
                       Total_ble = mean(Total_ble),
                       Total_col = mean(Total_col),
                       DIS_PREV = mean(DIS_PREV),
                       BLE_PREV = mean(BLE_PREV), .groups = "keep") %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)),
                    BLE_PREV = as.numeric(sprintf("%0.1f", BLE_PREV)))

    dis_species_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    DISEASE != "N/A",
                    JUV == 0,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES),
                    PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1,
                                               DISEASE == "F" ~ 1,
                                               DISEASE == "S" ~ 1,TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_ble = sum(BLEACH),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100,
                       BLE_PREV = (Total_ble/Total_col)*100, .groups = "keep") %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD) %>%
      dplyr::summarise(Total_dis = mean(Total_dis),
                       Total_ble = mean(Total_ble),
                       Total_col = mean(Total_col),
                       DIS_PREV = mean(DIS_PREV),
                       BLE_PREV = mean(BLE_PREV), .groups = "keep") %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)),
                    BLE_PREV = as.numeric(sprintf("%0.1f", BLE_PREV)))

    disease_prev_species <-dplyr::bind_rows(dis_species_1stage, dis_species_2stage)

    disease_prev_site <-dplyr::bind_rows(dat1_1stage, dat1_2stage)

  } else {

    disease_prev_site <- dat_1stage %>%
      dplyr::filter(N == 1,
                    #DISEASE != "N/A",
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1,
                                               DISEASE == "F" ~ 1,
                                               DISEASE == "S" ~ 1,TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_ble = sum(BLEACH),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100,
                       BLE_PREV = (Total_ble/Total_col)*100, .groups = "keep") %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)),
                    BLE_PREV = as.numeric(sprintf("%0.1f", BLE_PREV)))

    disease_prev_species <- dat_1stage %>%
      dplyr::filter(N == 1,
                    #DISEASE != "N/A",
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1,
                                               DISEASE == "F" ~ 1,
                                               DISEASE == "S" ~ 1,TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_ble = sum(BLEACH),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100,
                       BLE_PREV = (Total_ble/Total_col)*100, .groups = "keep") %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)),
                    BLE_PREV = as.numeric(sprintf("%0.1f", BLE_PREV)))

  }

  # Run through the weighting function

  tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = disease_prev_site, region, datatype = "disease")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])





  ################
  # Export
  ################

  # Create list to export
  output <- list(
    'dis_ble_prev_species' = disease_prev_species,
    "dis_ble_prev_site" = disease_prev_site,
    "dis_prev_strata" = dis_prev_strata,
    'ble_prev_strata' = ble_prev_strata,
    "Domain_est" = Domain_est)

  return(output)






}
