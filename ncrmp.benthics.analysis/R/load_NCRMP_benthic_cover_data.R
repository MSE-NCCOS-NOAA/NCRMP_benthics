## Function to load analysis ready NCRMP benthic cover data
# Purpose:
# creates files with all cover years combined


## Tag: data analysis


# outputs created in this file --------------
# dat



# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_cover
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Nov 2023


##############################################################################################################################

#' Creates combined cover dataframes
#'
#' Loads combined dataframe of all years of benthic cover data from a single region.
#' Function is called by all other functions that calculate data summaries from the benthic cover data.
#'
#'
#'
#' @param project A string indicating the project: "NCRMP" or "MIR". Default is NCRMP.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @return A dataframe of combined benthic cover data from specified region across all sampled years.
#' @importFrom magrittr "%>%"
#' @export
#'
#'


load_NCRMP_benthic_cover_data <- function(project = "NULL", region){


  if(project == "MIR"){

    dat1 <- MIR_FLK_2022_benthic_cover_DUMMY %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::mutate(SURVEY = "MIR",
                    DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                    REGION = "FLK",
                    SUB_REGION_NAME = MIR_zone) %>%
      dplyr::filter(!is.na(MAPGRID_NR),
                    !is.na(MIR_zone))

    dat <- dat1

  }

  if(project == "NCRMP" || project == "NULL"){

    if(region == "SEFCRI"){

      dat1 <- SEFCRI_2014_2stage_benthic_cover %>%
        # KICK OUT STATION NOT SAMPLED
        dplyr::filter(!(PRIMARY_SAMPLE_UNIT == 3130 & STATION_NR == 1))

      dat2 <- SEFCRI_2016_benthic_cover

      dat3 <- SEFCRI_2018_benthic_cover

      dat4 <- SEFCRI_2020_benthic_cover  %>%
        dplyr::mutate(YEAR = 2020,
                      STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))

      dat5 <- SEFCRI_2022_benthic_cover

      dat <- dplyr::bind_rows(dat1, dat2, dat3, dat4, dat5) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        # update some old species codes
        dplyr::mutate(COVER_CAT_NAME = case_when(COVER_CAT_NAME == "Erythropodium caribaeorum" ~ "Encrusting gorgonian",
                                                 TRUE ~ COVER_CAT_NAME)) %>%
        dplyr::mutate(COVER_CAT_CD = case_when(COVER_CAT_CD == "ENCR GORG" ~ "GOR ENCR",
                                               COVER_CAT_CD == "POF SPE." ~ "SPO OTHE",
                                               COVER_CAT_CD == "ERY CARI" ~ "ERY CARY",
                                               TRUE ~ COVER_CAT_CD))


    }

    if(region == "FLK"){

      dat1 <- FLK_2014_2stage_benthic_cover %>%
        dplyr::mutate(YEAR = 2014)

      dat2 <- FLK_2016_benthic_cover %>% dplyr::filter(!(is.na(STRAT)))

      dat3 <- FLK_2018_benthic_cover

      dat4 <- FLK_2020_benthic_cover %>%
        dplyr::mutate(YEAR = 2020)

      dat5 <- FLK_2022_benthic_cover
      grid_df <- FLK_2020_sample_frame@data
      new_prots <- grid_df %>% dplyr::select(MAPGRID_NR, PROT) %>% dplyr::rename("PROT_og" = PROT) %>% dplyr::mutate(MAPGRID_NR = as.numeric(MAPGRID_NR), PROT_og = as.numeric(PROT_og))
      dat5 <- dat5 %>% dplyr::left_join(., new_prots, by = c("MAPGRID_NR")) %>%
        # fix any that get left out manually, they fell outside the grid and JB fixed them
        dplyr::mutate(PROT_og = case_when(PRIMARY_SAMPLE_UNIT == 1006 ~ 0, PRIMARY_SAMPLE_UNIT == 1382 ~ 1, TRUE ~ PROT_og)) %>%
        dplyr::select(-PROT) %>%
        dplyr::rename("PROT" = PROT_og)

      dat <- dplyr::bind_rows(dat1, dat2, dat3, dat4, dat5) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        # update some old species codes
        dplyr::mutate(COVER_CAT_NAME = case_when(COVER_CAT_CD == "MAD MIRA" ~ "Madracis auretenra", # species name updated
                                                 COVER_CAT_NAME == "Erythropodium caribaeorum" ~ "Encrusting gorgonian",
                                                 TRUE ~ COVER_CAT_NAME)) %>%
        dplyr::mutate(COVER_CAT_CD = case_when(COVER_CAT_CD == "MAD MIRA" ~ "MAD AURE", # species name updated
                                               COVER_CAT_CD == "ENCR GORG" ~ "GOR ENCR",
                                               COVER_CAT_CD == "POF SPE." ~ "SPO OTHE",
                                               COVER_CAT_CD == "ERY CARI" ~ "ERY CARY",
                                               TRUE ~ COVER_CAT_CD))

    }

    if(region == "Tortugas"){

      dat1 <- TortugasMarq_2014_benthic_cover

      dat2 <- TortugasMarq_2016_benthic_cover

      dat3 <- Tortugas_2018_benthic_cover

      dat4 <- Tortugas_2020_benthic_cover %>%
        dplyr::mutate(YEAR = 2020,
                      STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT)))

      dat5 <- Tortugas_2022_benthic_cover

      dat <- dplyr::bind_rows(dat1, dat2, dat3, dat4, dat5) %>%
        dplyr::filter(SUB_REGION_NAME != "Marquesas",
                      SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        # update some old species codes
        dplyr::mutate(COVER_CAT_NAME = case_when(COVER_CAT_NAME == "Erythropodium caribaeorum" ~ "Encrusting gorgonian",
                                                 COVER_CAT_NAME == "Diploria spp." ~ "Diploria spp",
                                                 TRUE ~ COVER_CAT_NAME)) %>%
        dplyr::mutate(COVER_CAT_CD = case_when(COVER_CAT_CD == "POF SPE." ~ "SPO OTHE",
                                               COVER_CAT_CD == "ERY CARI" ~ "ERY CARY",
                                               TRUE ~ COVER_CAT_CD))

    }

    # GOM / Carib

    if(region == "STTSTJ"){


      dat1 <- USVI_2013_benthic_cover %>%
        dplyr::filter(REGION == "STTSTJ")

      dat2 <- USVI_2015_benthic_cover %>%
        dplyr::filter(REGION == "STTSTJ")

      dat3 <- USVI_2017_benthic_cover %>%
        dplyr::filter(REGION == "STTSTJ")

      dat4 <- USVI_2019_benthic_cover %>%
        dplyr::filter(REGION == "STTSTJ")

      dat5 <- USVI_2021_benthic_cover %>%
        dplyr::filter(REGION == "STTSTJ")

      dat <- dplyr::bind_rows(dat1, dat2, dat3, dat4, dat5) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT)

    }

    if(region == "STX"){


      dat2 <- USVI_2015_benthic_cover %>%
        dplyr::filter(REGION == "STX")

      dat3 <- USVI_2017_benthic_cover %>%
        dplyr::filter(REGION == "STX")

      dat4 <- USVI_2019_benthic_cover %>%
        dplyr::filter(REGION == "STX")

      dat5 <- USVI_2021_benthic_cover %>%
        dplyr::filter(REGION == "STX")

      dat <- dplyr::bind_rows(dat2, dat3, dat4, dat5) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT)

    }

    if(region == "PRICO"){

      dat1 <- PRICO_2014_benthic_cover

      dat2 <- PRICO_2016_benthic_cover %>%
        dplyr::mutate(YEAR = 2016)

      dat3 <- PRICO_2019_benthic_cover

      dat4 <- PRICO_2021_benthic_cover

      dat <- dplyr::bind_rows(dat1, dat2, dat3, dat4) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT)

    }

    if(region == "GOM"){

      dat1 <- FGBNMS_2013_benthic_cover

      dat2 <- FGBNMS_2015_benthic_cover

      dat3 <- FGBNMS_2018_benthic_cover %>%
        dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))

      dat4 <- FGBNMS_2022_benthic_cover %>%
        dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))

      dat <- dplyr::bind_rows(dat1, dat2, dat3, dat4) %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")


    }

  }

  ################
  # Export
  ################

  return(dat)

}
