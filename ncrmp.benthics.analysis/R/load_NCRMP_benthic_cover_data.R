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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Aug 2022


##############################################################################################################################

#' Creates combined cover dataframes
#'
#'
#'
#'
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


load_NCRMP_benthic_cover_data <- function(region){



if(region == "SEFCRI"){

  dat1 <- SEFCRI_2014_2stage_benthic_cover

  dat2 <- SEFCRI_2016_benthic_cover

  dat3 <- SEFCRI_2018_benthic_cover

  dat4 <- SEFCRI_2020_benthic_cover  %>%
    dplyr::mutate(YEAR = 2020,
                  STRAT = dplyr::case_when(STRAT == "PTSH1"~"PTSH2", TRUE ~ as.character(STRAT)))

  dat <- dplyr::bind_rows(dat1, dat2, dat3, dat4) %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

}

if(region == "FLK"){

  dat1 <- FLK_2014_2stage_benthic_cover %>%
    dplyr::mutate(YEAR = 2014)

  dat2 <- FLK_2016_benthic_cover

  dat3 <- FLK_2018_benthic_cover

  dat4 <- FLK_2020_benthic_cover # will combine with 2022

  dat <- dplyr::bind_rows(dat1, dat2, dat3) %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

}

if(region == "Tortugas"){

  dat1 <- TortugasMarq_2014_benthic_cover

  dat2 <- TortugasMarq_2016_benthic_cover

  dat3 <- Tortugas_2018_benthic_cover

  dat4 <- Tortugas_2020_benthic_cover %>%
    dplyr::mutate(YEAR = 2020,
                  STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT)))

  dat <- dplyr::bind_rows(dat1, dat2, dat3, dat4) %>%
    dplyr::filter(SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

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

  dat3 <- FGBNMS_2018_benthic_cover

  dat <- dplyr::bind_rows(dat1, dat2, dat3) %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")


}

  ################
  # Export
  ################

  return(dat)

}
