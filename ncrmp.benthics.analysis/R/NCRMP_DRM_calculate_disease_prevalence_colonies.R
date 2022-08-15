## Function to calculate disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data (FL only) by calculating species then colony prevalence (%) at the site level,
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
# Last update: Jul 2022


##############################################################################################################################

#' disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data at the species/site, site, strata and domain levels
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


NCRMP_DRM_calculate_disease_prevalence_colonies <- function(project, region, species_filter = "NULL"){


  # Load data
  # Florida

  if(project == "NCRMP_DRM"){

    if(region == "SEFCRI"){


      tmp1 <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      tmp2 <- SEFCRI_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp3 <- DRM_SEFCRI_2014_2021_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "DRM",
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                      STRAT = dplyr::case_when(STRAT == "NEAR2"~"NEAR1", TRUE ~ as.character(STRAT))) %>%
        dplyr::filter(STRAT != 'NA0')

      tmp4 <- SEFCRI_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp5 <- SEFCRI_2020_coral_demographics %>%
        dplyr::mutate(YEAR = 2020,
                      SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))



      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp2, tmp4, tmp5) %>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))

        dat_2stage <- dplyr::bind_rows(tmp1, tmp3) %>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))

      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp2, tmp4, tmp5) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter) %>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))

        dat_2stage <- dplyr::bind_rows(tmp1, tmp3) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter) %>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))
      }

    }

    if(region == "FLK"){

      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      YEAR = 2014)

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp3 <- dplyr::bind_rows(DRM_FLK_2014_2019_2stage_coral_demographics,
                               DRM_FLK_2020_2021_2stage_coral_demographics) %>%
        dplyr::mutate(SURVEY = "DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      REGION = ï..REGION) %>%
        dplyr::filter(!is.na(MAPGRID_NR),
                      !is.na(STRAT))

      tmp4 <- FLK_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp5 <- FLK_2020_coral_demographics %>%
        dplyr::mutate(YEAR = 2020,
                      SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


      if(species_filter == "FALSE" ||
         species_filter == "NULL") {

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4, tmp5) %>%
          dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                   "DIP CLIV" = "PSE CLIV",
                                                   "DIP STRI" = "PSE STRI",
                                                   'CLA ARBU' = "CLA ABRU"))

        dat_2stage <- dplyr::bind_rows(tmp3) %>%
          dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                   "DIP CLIV" = "PSE CLIV",
                                                   "DIP STRI" = "PSE STRI",
                                                   'CLA ARBU' = "CLA ABRU"))
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4) %>%
          dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                            "DIP CLIV" = "PSE CLIV",
                                            "DIP STRI" = "PSE STRI",
                                            'CLA ARBU' = "CLA ABRU"))%>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)

        dat_2stage <- dplyr::bind_rows(tmp3) %>%
          dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                            "DIP CLIV" = "PSE CLIV",
                                            "DIP STRI" = "PSE STRI",
                                            'CLA ARBU' = "CLA ABRU"))%>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)
      }
    }

    if(region == "Tortugas"){

      tmp1 <- TortugasMarq_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp2 <- TortugasMarq_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      tmp4 <- dplyr::bind_rows(DRM_Tort_2014_2019_2stage_coral_demographics %>% dplyr::mutate(REGION = ï..REGION,
                                                                                              SURVEY = "DRM"),
                               DRM_Tort_2020_2021_2stage_coral_demographics %>% dplyr::mutate(SURVEY = "NCRMP/DRM")) %>%

        dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
        dplyr::select(-ï..REGION)

      tmp5 <- Tortugas_2020_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                      STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT)))


      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2)

        dat_2stage <- dplyr::bind_rows(tmp3, tmp4, tmp5)
      }

      if(species_filter == "TRUE"){

        dat_1stage <- dplyr::bind_rows(tmp1, tmp2) %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

        dat_2stage <- dplyr::bind_rows(tmp3, tmp4, tmp5) %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)
      }
    }

  }

  if(project == "NCRMP"){

    if(region == "SEFCRI"){


      tmp1 <- SEFCRI_2014_2stage_coral_demographics

      tmp2 <- SEFCRI_2016_coral_demographics

      tmp3 <- SEFCRI_2018_coral_demographics

      tmp4 <- SEFCRI_2020_coral_demographics %>%
        dplyr::mutate(YEAR = 2020)

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        dat_2stage <- tmp1 %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))

        dat_1stage <- dplyr::bind_rows(tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))


      }

      if(species_filter == "TRUE"){
        dat_2stage <- tmp1 %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)%>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))

        dat_1stage <- dplyr::bind_rows(tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter) %>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))
      }
    }

    if(region == "FLK"){


      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      YEAR = 2014)

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp3 <- FLK_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp4 <- FLK_2020_coral_demographics %>%
        dplyr::mutate(YEAR = 2020,
                      SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                   "DIP CLIV" = "PSE CLIV",
                                                   "DIP STRI" = "PSE STRI",
                                                   'CLA ARBU' = "CLA ABRU"))
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4)%>%
          dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                   "DIP CLIV" = "PSE CLIV",
                                                   "DIP STRI" = "PSE STRI",
                                                   'CLA ARBU' = "CLA ABRU")) %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)

      }
    }

    if(region == "Tortugas"){


      tmp1 <- TortugasMarq_2014_coral_demographics

      tmp2 <- TortugasMarq_2016_coral_demographics

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp4 <- Tortugas_2020_coral_demographics %>%
        dplyr::mutate(STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT)),
                      SURVEY = "NCRMP/DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
        dplyr::mutate(YEAR = 2020)

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

        dat_2stage <- dplyr::bind_rows(tmp3)
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

        dat_2stage <- dplyr::bind_rows(tmp3)
          dplyr::filter(SPECIES_CD %in% Tort_filter)

      }
    }

    # Carib / GOM

    if(region == "STTSTJ"){

      tmp1 <- USVI_2013_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp2 <- USVI_2015_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp3 <- USVI_2017_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp4 <- USVI_2019_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp5 <- USVI_2021_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% STTSTJ_filter)
      }
    }

    if(region == "STX"){

      tmp1 <- USVI_2015_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      tmp2 <- USVI_2017_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      tmp3 <- USVI_2019_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      tmp4 <- USVI_2021_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% STX_filter)
      }

    }

    if(region == "PRICO"){

      tmp1 <- PRICO_2014_coral_demographics %>%
        dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp2 <- PRICO_2016_coral_demographics %>%
        dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      YEAR = 2016)

      tmp3 <- PRICO_2019_coral_demographics %>%
        dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp4 <- PRICO_2021_coral_demographics %>%
        dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP")

      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% PR_filter)

      }

    }

    if(region == "GOM"){

      tmp1 <- FGBNMS_2013_coral_demographics

      tmp2 <- FGBNMS_2015_coral_demographics

      tmp3 <- FGBNMS_2018_coral_demographics

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% GOM_filter)
      }

    }

  }

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
       dplyr::mutate(PROT = as.factor(PROT),
                     PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1, TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
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
      dplyr::mutate(PROT = as.factor(PROT),
                    PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1, TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD) %>%
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
                    PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1, TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1,
                                              BLEACH_CONDITION == "PL" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_ble = sum(BLEACH),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100,
                       BLE_PREV = (Total_ble/Total_col)*100, .groups = "keep") %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
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
                    PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1, TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_ble = sum(BLEACH),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100,
                       BLE_PREV = (Total_ble/Total_col)*100, .groups = "keep") %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD) %>%
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
                                               DISEASE == "P" ~ 1, TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
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
                                               DISEASE == "P" ~ 1, TRUE ~ 0),
                    BLEACH = dplyr::case_when(BLEACH_CONDITION == "N" ~ 0,
                                              BLEACH_CONDITION == "P" ~ 1,
                                              BLEACH_CONDITION == "B" ~ 1,
                                              BLEACH_CONDITION == "T" ~ 1,
                                              BLEACH_CONDITION == "PB" ~ 1, TRUE ~ 0)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD) %>%
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
