
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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Feb 2023


##############################################################################################################################

#' Creates combined demo dataframes
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP, NCRMP and DRM combined, or MIR. Default is NCRMP.
#' @param region A string indicating the region
#' @param species_filter A string indicating whether to filter to a subset of species
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


load_NCRMP_DRM_demo_data <- function(project = "NULL", region, species_filter = "NULL"){



  # Load data
  # Florida

  if(project == "MIR"){

    tmp1 <- MIR_FLK_2022_coral_demographics_DUMMY %>%
      dplyr::mutate(SURVEY = "MIR",
                    DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
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

      tmp3 <- DRM_SEFCRI_2014_2021_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "DRM",
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                      STRAT = dplyr::case_when(STRAT == "NEAR2"~"NEAR1", TRUE ~ as.character(STRAT))) %>%
        dplyr::filter(STRAT != 'NA0')

      tmp4 <- SEFCRI_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp5 <- SEFCRI_2020_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      YEAR = 2020)


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
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)%>%
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

      tmp6 <- FLK_2022_coral_demographics %>%
        dplyr::mutate(YEAR = 2022,
                      SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


      if(species_filter == "FALSE" ||
         species_filter == "NULL") {

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4, tmp5, tmp6) %>%
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
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4, tmp5, tmp6) %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)

        dat_2stage <- dplyr::bind_rows(tmp3) %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)
      }
    }

    if(region == "Tortugas"){

      tmp1 <- TortugasMarq_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- TortugasMarq_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM") %>%
        dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      tmp4 <- dplyr::bind_rows(DRM_Tort_2014_2019_2stage_coral_demographics %>% dplyr::mutate(REGION = ï..REGION,
                                                                                              SURVEY = "DRM"),
                               DRM_Tort_2020_2021_2stage_coral_demographics %>% dplyr::mutate(SURVEY = "NCRMP/DRM")) %>%

        dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT)) %>%
        dplyr::select(-ï..REGION)

      tmp5 <- Tortugas_2020_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM",
                      STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT)),
                      PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2) %>%
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

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        dat_2stage <- SEFCRI_2014_2stage_coral_demographics %>%
          dplyr::mutate(SURVEY = "NCRMP")

        dat_1stage <- dplyr::bind_rows(SEFCRI_2016_coral_demographics, SEFCRI_2018_coral_demographics, SEFCRI_2020_coral_demographics %>% dplyr::mutate(YEAR = 2020), SEFCRI_2022_coral_demographics_DUMMY) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))
      }

      if(species_filter == "TRUE"){
        dat_2stage <- SEFCRI_2014_2stage_coral_demographics %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

        dat_1stage <- dplyr::bind_rows(SEFCRI_2016_coral_demographics, SEFCRI_2018_coral_demographics, SEFCRI_2020_coral_demographics %>% dplyr::mutate(YEAR = 2020)) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)
      }
    }

    if(region == "FLK"){


      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(YEAR = 2014)

      tmp2 <- dplyr::bind_rows(FLK_2016_coral_demographics, FLK_2018_coral_demographics, FLK_2020_coral_demographics %>% dplyr::mutate(YEAR = 2020), FLK_2022_coral_demographics)


      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                   "DIP CLIV" = "PSE CLIV",
                                                   "DIP STRI" = "PSE STRI",
                                                   'CLA ARBU' = "CLA ABRU"))
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2)  %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                   "DIP CLIV" = "PSE CLIV",
                                                   "DIP STRI" = "PSE STRI",
                                                   'CLA ARBU' = "CLA ABRU")) %>%
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

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4)  %>%
          dplyr::mutate(across(where(is.character), stringr::str_trim)) # this may not be necessary

        dat_2stage <- tmp3  %>%
          dplyr::mutate(across(where(is.character), stringr::str_trim))
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4) %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

        dat_2stage <- tmp3 %>%
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
