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


# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams, Krampitz
# Last update: Jul 2025


##############################################################################################################################

#' Creates combined cover dataframes
#'
#' Loads combined dataframe of all years of benthic cover data from a single region.
#' Function is called by all other functions that calculate data summaries from the benthic cover data.
#'
#'
#'
#' @param project A string indicating the project: "NCRMP" or "MIR". Default is NCRMP.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "FGB".
#' @return A dataframe of combined benthic cover data from specified region across all sampled years.
#' @importFrom magrittr "%>%"
#' @export
#'
#'

load_NCRMP_benthic_cover_data <- function(project = "NULL", region) {

  ####Recode species####
  recode_and_clean_species <- function(data) {
    if ("COVER_CAT_NAME" %in% colnames(data)) {
      data <- data %>%
        dplyr::mutate(
          COVER_CAT_NAME = dplyr::case_when(
            COVER_CAT_NAME == "Erythropodium caribaeorum" ~ "Encrusting gorgonian",
            COVER_CAT_NAME == "Diploria spp." ~ "Diploria spp",
            COVER_CAT_NAME == "Cladocora abruscula" ~ "Cladocora arbuscula",
            COVER_CAT_CD == "MAD MIRA" ~ "Madracis auretenra",
            TRUE ~ COVER_CAT_NAME
          ),
          COVER_CAT_CD = dplyr::case_when(
            COVER_CAT_NAME == "Cladocora arbuscula" ~ "CLA ABRU",
            COVER_CAT_CD == "POF SPE." ~ "SPO OTHE",
            COVER_CAT_CD == "ERY CARI" ~ "ERY CARY",
            COVER_CAT_CD == "MAD MIRA" ~ "MAD AURE",
            COVER_CAT_CD == "ENCR GORG" ~ "GOR ENCR",
            TRUE ~ COVER_CAT_CD
          )
        )
    }
    return(data)
  }


  #### Data Processing Mission Iconic Reef ####
  if (project == "MIR") {

    dat <- MIR_FLK_2022_benthic_cover_DUMMY %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::mutate(
        SURVEY = "MIR",
        DATE = paste(MONTH, DAY, YEAR, sep = "/"),
        REGION = "FLK",
        SUB_REGION_NAME = MIR_zone
      ) %>%
      dplyr::filter(!is.na(MAPGRID_NR), !is.na(MIR_zone))
  } # end MIR


  #### Data Processing Southeast Florida NCRMP/Null Project ####
  if ((project == "NCRMP" || project == "NULL") && region == "SEFCRI") {

    any(is.na(SEFCRI_2014_2stage_benthic_cover$PRIMARY_SAMPLE_UNIT))
    any(is.na(SEFCRI_2014_2stage_benthic_cover$STATION_NR))

    datasets <- list(
      SEFCRI_2014_2stage_benthic_cover %>%
        # KICK OUT STATION NOT SAMPLED
        dplyr::filter(!(PRIMARY_SAMPLE_UNIT == 3130 & STATION_NR == 1)),
      SEFCRI_2016_benthic_cover,
      SEFCRI_2018_benthic_cover,
      SEFCRI_2020_benthic_cover %>%
        dplyr::mutate(
          YEAR = 2020,
          STRAT = dplyr::case_when(
            STRAT == "PTSH1" ~ "PTSH2",
            TRUE ~ as.character(STRAT)
          )
        ),
      SEFCRI_2022_benthic_cover,
      SEFCRI_2024_benthic_cover
    )

    dat <- dplyr::bind_rows(datasets) %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      recode_and_clean_species()
  } # end SEFCRI

  #### Data Processing Florida Keys NCRMP/Null Project ####
  if ((project == "NCRMP" || project == "NULL") && region == "FLK") {

    datasets <- list(
      FLK_2014_2stage_benthic_cover %>% dplyr::mutate(YEAR = 2014),
      FLK_2016_benthic_cover %>%
        dplyr::filter(!is.na(STRAT)) %>%
        dplyr::filter(!is.na(COVER_CAT_CD)),
      FLK_2018_benthic_cover,
      FLK_2020_benthic_cover %>% dplyr::mutate(YEAR = 2020),
      FLK_2022_benthic_cover %>% update_protection_status(grid_df = FLK_2020_sample_frame@data),
      FLK_2024_benthic_cover
    )

    dat <- dplyr::bind_rows(datasets) %>%
      recode_and_clean_species()  %>%
      dplyr::mutate(PROT = as.factor(0),
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
  }


  #### Data Processing Tortugas NCRMP/Null Project ####
  if ((project == "NCRMP" || project == "NULL") && region == "Tortugas") {

    datasets <- list(
      TortugasMarq_2014_benthic_cover,
      TortugasMarq_2016_benthic_cover,
      Tortugas_2018_benthic_cover,
      Tortugas_2020_benthic_cover %>%
        dplyr::mutate(
          YEAR = 2020,
          STRAT = dplyr::case_when(
            STRAT == "T08" & PROT == 2 ~ "T09",
            TRUE ~ as.character(STRAT)
          )
        ),
      Tortugas_2022_benthic_cover,
      Tortugas_2024_benthic_cover
    )


    dat <- dplyr::bind_rows(datasets) %>%
      dplyr::filter(
        SUB_REGION_NAME != "Marquesas",
        SUB_REGION_NAME != "Marquesas-Tortugas Trans"
      ) %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      recode_and_clean_species()
  }

  #### Data Processing St Thomas St John ####
  if (region == "STTSTJ") {

    datasets <- list(
      USVI_2013_benthic_cover,
      USVI_2015_benthic_cover,
      USVI_2017_benthic_cover,
      USVI_2019_benthic_cover,
      USVI_2021_benthic_cover,
      USVI_2023_benthic_cover,
      USVI_2025_benthic_cover
    )

    dat <- dplyr::bind_rows(datasets) %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT)
  } # end STTSTJ

  #### Data Processing St Croix ####
  if (region == "STX") {

    datasets <- list(
      USVI_2015_benthic_cover,
      USVI_2017_benthic_cover,
      USVI_2019_benthic_cover,
      USVI_2021_benthic_cover,
      USVI_2023_benthic_cover,
      USVI_2025_benthic_cover
    )

    dat <- dplyr::bind_rows(datasets) %>%
      dplyr::filter(REGION == "STX") %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT)
  } # end STX

  #### Data Processing Puerto Rico ####
  if (region == "PRICO") {

    datasets <- list(
      PRICO_2014_benthic_cover %>%
        recode_and_clean_species(),
      PRICO_2016_benthic_cover %>% dplyr::mutate(YEAR = 2016),
      PRICO_2019_benthic_cover,
      PRICO_2021_benthic_cover,
      PRICO_2023_benthic_cover,
      PRICO_2025_benthic_cover
    )

    dat <- dplyr::bind_rows(datasets) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT)
  } # end PRICO

  #### Data Processing Flower Gardens ####
  if (region == "FGB") {

    datasets <- list(
      FGBNMS_2013_benthic_cover,
      FGBNMS_2015_benthic_cover,
      FGBNMS_2018_benthic_cover %>% dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR)),
      FGBNMS_2022_benthic_cover %>%
        dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR)) %>%
        dplyr::mutate(MAPGRID_NR = paste("FGB", MAPGRID_NR, sep = "")),
      FGBNMS_2024_benthic_cover %>%
        dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR)) %>%
        dplyr::mutate(MAPGRID_NR = paste("FGB", MAPGRID_NR, sep = ""))
    )

    dat <- dplyr::bind_rows(datasets) %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")
  } # end FGB

  #### Export ####
  return(dat)
}
