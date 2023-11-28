
## Function to load analysis ready NCRMP & DRM demo data
# Purpose:
# creates files with all demo programs/years combined


## Tag: data analysis


# outputs created in this file --------------
# dat_1stage
# dat_2stage



# CallS:
# analysis ready data

# output gets called by:
# NCRMP_DRM_calculate_colony_density
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Feb 2023


##############################################################################################################################

#' Creates combined demo dataframes
#'
#' Loads combined dataframes of coral demographics data from all years of a single region. Can be just NCRMP
#' data, or also NCRMP and DRM data combined.
#' In regions with two stage data (SEFCRI, FLK, Tortugas) a list of dataframes is produces, one
#' containing all the single stage data, one containing all the two stage data.
#' This function is called by nearly all of the functions that run summary calculations from coral demographic data.
#'
#'
#' @param project A string indicating the project, NCRMP, NCRMP and DRM combined, or MIR. Default is NCRMP.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param species_filter An optional string indicating the species to subset to, if desired. Format is the 7 digits species code (e.g. Acropora cervicornis would be ACR CERV).
#' @return A dataframe or list of dataframes containing all demographic data for specified region, and species, if selected. A list is produced for Florida regions, with one dataframe containing all single stage data and the other containing all two stage data.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "case_when"
#' @export
#'
#'


load_NCRMP_DRM_demo_data <- function(project = "NULL", region, species_filter = "NULL"){



  # Load data
  # Florida

  if(project == "MIR"){

    tmp1 <- MIR_FLK_2022_coral_demographics_DUMMY %>%
      dplyr::mutate(SURVEY = "MIR",
                    REGION = "FLK",
                    SUB_REGION_NAME = MIR_zone) %>%
      dplyr::filter(!is.na(MAPGRID_NR),
                    !is.na(MIR_zone))

    dat_1stage <- tmp1
  }

  if(project == "NCRMP_DRM"){

    if(region == "SEFCRI"){


      tmp1 <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      tmp2 <- SEFCRI_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- DRM_SEFCRI_2014_2022_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "DRM",
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                      STRAT = dplyr::case_when(STRAT == "NEAR2"~"NEAR1", TRUE ~ as.character(STRAT))) %>%
        dplyr::filter(STRAT != 'NA0')

      tmp4 <- SEFCRI_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp5 <- SEFCRI_2020_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      YEAR = 2020)

      tmp6 <- SEFCRI_2022_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")


      #Combine 1 stage or 2 stage data
      dat_1stage <- dplyr::bind_rows(tmp2, tmp4, tmp5, tmp6) %>%
        dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)),
                      SPECIES_CD = dplyr::case_when(SPECIES_CD == "MEAN JACK" ~ "MEA JACK",
                                             TRUE ~ SPECIES_CD),
                      SPECIES_NAME = dplyr::case_when(SPECIES_CD == "MEA JACK" ~ "Meandrina jacksoni",
                                               TRUE ~ SPECIES_NAME),
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      dat_2stage <- dplyr::bind_rows(tmp1, tmp3) %>%
        dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)),
                      SPECIES_CD = dplyr::case_when(SPECIES_CD == "DIP STRI" ~ "PSE STRI",
                                             TRUE ~ SPECIES_CD),
                      SPECIES_NAME = dplyr::case_when(SPECIES_CD == "PSE STRI" ~ "Pseudodiploria strigosa",
                                               TRUE ~ SPECIES_NAME))


      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

        dat_2stage <- dplyr::filter(SPECIES_CD %in% SEFCRI_filter)
      }

    }

    if(region == "FLK"){

      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      YEAR = 2014)

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- DRM_FLK_2014_2022_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "DRM") %>%
        dplyr::filter(!is.na(MAPGRID_NR),
                      !is.na(STRAT))

      tmp4 <- FLK_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp5 <- FLK_2020_coral_demographics %>%
        dplyr::mutate(YEAR = 2020,
                      SURVEY = "NCRMP")

      tmp6 <- FLK_2022_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")
      # UPDATE THE PROT (MIR sites initially labeled as PROT=2)
      grid_df <- FLK_2020_sample_frame@data
      new_prots <- grid_df %>% dplyr::select(MAPGRID_NR, PROT) %>% dplyr::rename("PROT_og" = PROT) %>% dplyr::mutate(MAPGRID_NR = as.numeric(MAPGRID_NR), PROT_og = as.numeric(PROT_og))
      tmp6 <- tmp6 %>% dplyr::left_join(., new_prots, by = c("MAPGRID_NR")) %>%
        # fix any that get left out manually, they fell outside the grid and JB fixed them
        dplyr::mutate(PROT_og = case_when(PRIMARY_SAMPLE_UNIT == 1006 ~ 0, PRIMARY_SAMPLE_UNIT == 1382 ~ 1, TRUE ~ PROT_og)) %>%
        dplyr::select(-PROT) %>%
        dplyr::rename("PROT" = PROT_og)


      dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4, tmp5, tmp6) %>%
        dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT)) %>%
        dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                 "DIP CLIV" = "PSE CLIV",
                                                 "DIP STRI" = "PSE STRI",
                                                 'CLA ARBU' = "CLA ABRU")) %>%
        dplyr::mutate(SPECIES_NAME = case_when(SPECIES_CD == "PSE CLIV" ~ "Pseudodiploria clivosa",
                                               SPECIES_CD == "PSE STRI" ~ "Pseudodiploria strigosa",
                                               SPECIES_CD == "CLA ABRU" ~ "Cladocora arbuscula",
                                               TRUE ~ as.character(SPECIES_NAME)))

      dat_2stage <- dplyr::bind_rows(tmp3) %>%
        dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                 "DIP CLIV" = "PSE CLIV",
                                                 "DIP STRI" = "PSE STRI",
                                                 'CLA ARBU' = "CLA ABRU")) %>%
        dplyr::mutate(SPECIES_NAME = case_when(SPECIES_CD == "PSE CLIV" ~ "Pseudodiploria clivosa",
                                               SPECIES_CD == "PSE STRI" ~ "Pseudodiploria strigosa",
                                               SPECIES_CD == "CLA ABRU" ~ "Cladocora arbuscula",
                                               TRUE ~ as.character(SPECIES_NAME)))


      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4, tmp5, tmp6) %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)

        dat_2stage <- dplyr::bind_rows(tmp3) %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)
      }
    }

    if(region == "Tortugas"){

      tmp1 <- TortugasMarq_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      tmp2 <- TortugasMarq_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP") %>%
        dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      tmp4 <- DRM_Tort_2014_2022_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "DRM",
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT)) %>%
        # 2020 NCRMP data overlaps some with DRM because NCRMP divers went on DRM cruise
        # and entered data in both data entry systems...
        # filter out the sites already in NCRMP data
        dplyr::filter(!(YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1490 & STATION_NR == 9290 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1562 & STATION_NR == 9508 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1564 & STATION_NR == 9362 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1570 & STATION_NR == 9345 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1571 & STATION_NR == 9366 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1572 & STATION_NR == 9515 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2166 & STATION_NR == 9297 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2172 & STATION_NR == 9668 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2187 & STATION_NR == 9375 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2188 & STATION_NR == 9376 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2209 & STATION_NR == 9514 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2211 & STATION_NR == 9333 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2215 & STATION_NR == 9344 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2219 & STATION_NR == 9339 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2221 & STATION_NR == 9520 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2224 & STATION_NR == 9289 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2227 & STATION_NR == 9343 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2229 & STATION_NR == 9577 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2231 & STATION_NR == 9492 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2241 & STATION_NR == 9340 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2243 & STATION_NR == 9335 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2255 & STATION_NR == 9365 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2264 & STATION_NR == 9586 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2289 & STATION_NR == 9591 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2300 & STATION_NR == 9334 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2307 & STATION_NR == 9341 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2309 & STATION_NR == 9485 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2312 & STATION_NR == 9589 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2315 & STATION_NR == 9320 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2317 & STATION_NR == 9481 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2319 & STATION_NR == 9288 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2329 & STATION_NR == 9374 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2340 & STATION_NR == 9511 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2512 & STATION_NR == 9576 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2571 & STATION_NR == 9360 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2606 & STATION_NR == 9579 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2646 & STATION_NR == 9578 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2698 & STATION_NR == 9330 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2716 & STATION_NR == 9315 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 3023 & STATION_NR == 9299))

      tmp5 <- Tortugas_2020_coral_demographics %>%
        dplyr::mutate(SURVEY = dplyr::case_when(MONTH == 8 ~ "NCRMP", MONTH == 9 ~ "DRM"), # assign DRM/NCRMP data to DRM so it can be combined with the second stage version of the data in the DRM cruise
                      STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT)),
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      tmp6 <- Tortugas_2022_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp6) %>%
          dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

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

  if(project == "NCRMP" || project == "NULL"){

    if(region == "SEFCRI"){

      dat_2stage <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP") %>%
        # fix some species names
        dplyr::mutate(SPECIES_CD = dplyr::case_when(SPECIES_CD == "DIP STRI" ~ "PSE STRI",
                                             TRUE ~ as.character(SPECIES_CD)),
                      SPECIES_NAME = dplyr::case_when(SPECIES_CD == "PSE STRI" ~ "Pseudodiploria strigosa",
                                                      TRUE ~ as.character(SPECIES_NAME)))

      dat_1stage <- dplyr::bind_rows(SEFCRI_2016_coral_demographics, SEFCRI_2018_coral_demographics, SEFCRI_2020_coral_demographics %>% dplyr::mutate(YEAR = 2020), SEFCRI_2022_coral_demographics) %>%
        dplyr::mutate(SURVEY = "NCRMP") %>%
        dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT))) %>%
        # fix some species names
        dplyr::mutate(SPECIES_CD = dplyr::case_when(SPECIES_CD == "MEAN JACK" ~ "MEA JACK",
                                                    TRUE ~ as.character(SPECIES_CD)),
                      SPECIES_NAME = dplyr::case_when(SPECIES_CD == "MEA JACK" ~ "Meandrina jacksoni",
                                                      TRUE ~ as.character(SPECIES_NAME)))

      if(species_filter == "TRUE"){
        dat_2stage <- dat_2stage %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

        dat_1stage <- dat_1stage %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)
      }
    }

    if(region == "FLK"){


      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(YEAR = 2014)

      tmp2 <- dplyr::bind_rows(FLK_2016_coral_demographics, FLK_2018_coral_demographics, FLK_2020_coral_demographics %>% dplyr::mutate(YEAR = 2020))

      tmp3 <- FLK_2022_coral_demographics
      # UPDATE THE PROT (MIR sites initially labeled as PROT=2)
      grid_df <- FLK_2020_sample_frame@data
      new_prots <- grid_df %>% dplyr::select(MAPGRID_NR, PROT) %>% dplyr::rename("PROT_og" = PROT) %>% dplyr::mutate(MAPGRID_NR = as.numeric(MAPGRID_NR), PROT_og = as.numeric(PROT_og))
      tmp3 <- tmp3 %>% dplyr::left_join(., new_prots, by = c("MAPGRID_NR")) %>%
        # fix any that get left out manually, they fell outside the grid and JB fixed them
        dplyr::mutate(PROT_og = case_when(PRIMARY_SAMPLE_UNIT == 1006 ~ 0, PRIMARY_SAMPLE_UNIT == 1382 ~ 1, TRUE ~ PROT_og)) %>%
        dplyr::select(-PROT) %>%
        dplyr::rename("PROT" = PROT_og)

      #Combine 1 stage or 2 stage data
      dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                 "DIP CLIV" = "PSE CLIV",
                                                 "DIP STRI" = "PSE STRI",
                                                 'CLA ARBU' = "CLA ABRU")) %>%
        dplyr::mutate(SPECIES_NAME = case_when(SPECIES_CD == "PSE CLIV" ~ "Pseudodiploria clivosa",
                                               SPECIES_CD == "PSE STRI" ~ "Pseudodiploria strigosa",
                                               SPECIES_CD == "CLA ABRU" ~ "Cladocora arbuscula",
                                               TRUE ~ as.character(SPECIES_NAME)))


      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dat_1stage %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)

      }
    }

    if(region == "Tortugas"){


      tmp1 <- TortugasMarq_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- TortugasMarq_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM")

      tmp4 <- Tortugas_2020_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM",
                      YEAR = 2020,
                      STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT)))

      tmp5 <- Tortugas_2022_coral_demographics


      dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4, tmp5)  %>%
        dplyr::mutate(across(where(is.character), stringr::str_trim)) %>%  # this may not be necessary
        dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      dat_2stage <- tmp3  %>%
        dplyr::mutate(across(where(is.character), stringr::str_trim)) %>%
        dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dat_1stage %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

        dat_2stage <- dat_2stage %>%
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
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
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
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% STX_filter)
      }

    }

    if(region == "PRICO"){

      tmp1 <- PRICO_2014_coral_demographics %>%
        dplyr::mutate(PROT = NA)

      tmp2 <- PRICO_2016_coral_demographics %>%
        dplyr::mutate(YEAR = 2016,
                      PROT = NA)

      tmp3 <- PRICO_2019_coral_demographics

      tmp4 <- PRICO_2021_coral_demographics

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

      tmp3 <- FGBNMS_2018_coral_demographics %>%
        dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))

      tmp4 <- FGBNMS_2022_coral_demographics %>%
        dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM")
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM") %>%
          dplyr::filter(SPECIES_CD %in% GOM_filter)
      }

    }

  }



  ################
  # Export
  ################

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    # Create list to export
    output <- list(
      "dat_1stage" = dat_1stage,
      "dat_2stage" = dat_2stage)

  } else {

    # Create list to export
    output <- list(
      "dat_1stage" = dat_1stage)



  }

  return(output)


}
