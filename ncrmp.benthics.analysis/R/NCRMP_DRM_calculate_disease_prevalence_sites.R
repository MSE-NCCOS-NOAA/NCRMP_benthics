## Function to calculate disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data (FL only) by sites prevalence (%; number of sites with disease present vs
## total number of sites) at the site level, strata and domain level

# Purpose:
# creates csv files with disease/bleaching prevalence by region


## Tag: data analysis


# outputs created in this file --------------
# disease_prev_site
# disease_prev_strata
# Domain estimates

# Calls:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Jan 2023


##############################################################################################################################

#' Creates species list, species richness and species diversity dataframes from NCRMP and DRM data
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_DRM_calculate_disease_prevalence_sites <- function(project, region, species_filter = "NULL"){

  tmp <- load_NCRMP_DRM_demo_data(project = project,
                                  region = region,
                                  species_filter = species_filter)

  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])



  # Calculate site level disease prevalence

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    dat_1stage <- dat_1stage %>% dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))
    dat_2stage <- dat_2stage %>% dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    dis_ble_prev_site <- dplyr::bind_rows(dat_1stage, dat_2stage) %>%
      dplyr::filter(N == 1,
                    STRAT != "N/A",
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    DATE = paste(MONTH, DAY, YEAR, sep = "/"),
                    PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1, TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE, na.rm = T),
                       Total_ble = sum(BLEACH, na.rm = T), .groups = "keep") %>%
      dplyr::mutate(dis_present = dplyr::case_when(Total_dis > 0 ~ 1, TRUE ~ 0),
                    ble_present = dplyr::case_when(Total_ble > 0 ~ 1, TRUE ~ 0)) %>%
      dplyr::select(-Total_dis, -Total_ble) %>%
    dplyr::ungroup()

  } else {

    dat_1stage <- dat_1stage %>% dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))


    dis_ble_prev_site <- dat_1stage %>%
      dplyr::filter(N == 1,
                    STRAT != "N/A",
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    DATE = paste(MONTH, DAY, YEAR, sep = "/"),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1,
                                               DISEASE == "F" ~ 1,
                                               DISEASE == "S" ~ 1,TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE, na.rm = T),
                       Total_ble = sum(BLEACH, na.rm = T), .groups = "keep") %>%
      dplyr::mutate(dis_present = dplyr::case_when(Total_dis > 0 ~ 1, TRUE ~ 0),
                    ble_present = dplyr::case_when(Total_ble > 0 ~ 1, TRUE ~ 0)) %>%
      dplyr::select(-Total_dis, -Total_ble) %>%
      dplyr::ungroup()
  }

  dis_ble_prev_strata <- dis_ble_prev_site %>%
    dplyr::group_by(REGION, YEAR, STRAT) %>%
    dplyr::summarise(dis_sites = sum(dis_present),
                     ble_sites = sum(ble_present),
                     total_sites = length(unique(PRIMARY_SAMPLE_UNIT)))

  dis_ble_prev_region <- dis_ble_prev_strata %>%
    dplyr::group_by(REGION, YEAR) %>%
    dplyr::summarise(dis_sites = sum(dis_sites),
                     ble_sites = sum(ble_sites),
                     total_sites = sum(total_sites),
                     dis_prev = (dis_sites/total_sites)*100,
                     ble_prev = (ble_sites/total_sites)*100)







  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "dis_ble_prev_site" = dis_ble_prev_site,
    "dis_ble_prev_strata" = dis_ble_prev_strata,
    "disease_prev_region" = dis_ble_prev_region)

  return(output)






}
