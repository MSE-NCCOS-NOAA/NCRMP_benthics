
## Function to randomly subset 2 stage data for length frequency data

# Purpose:
# creates data objects of subsetted 2 stage data


## Tag: data analysis


# output gets called by:
# NCRMP_make_size_bins
#

# NCRMP Caribbean Benthic analytics team: Davis, Groves, Viehman, Williams
# Last update: Sep 2023



###############################################################################


#' Subset 2 stage data
#'
#' Subsets 2 stage data to single stage to provide data frames that can be used
#' for the size frequency data summaries. This function should NOT be called and run
#' but serves as base code for randomly subsetting 2 stage data into single stage
#' and saving it to the package.
#'
#' @param region A string indicating the region
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param year A numeric indicating the year of interest
#'
#' @return # A subsetted dataframe
#'



subset_2stage_data <- function(region, project, year){

  if(project == "NCRMP"){
    if(region == "SEFCRI" && year == 2014){

      dat <- SEFCRI_2014_2stage_coral_demographics

      tmp <- data.frame()

      set.seed(3)

      for(i in unique(dat$PRIMARY_SAMPLE_UNIT)){
        d <- dat %>% dplyr::filter(PRIMARY_SAMPLE_UNIT == i)

        d_unique <- d %>% dplyr::select(YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR) %>% dplyr::distinct(.)

        if(nrow(d_unique) == 1){
          d_new <- d_unique
        } else{

          set.seed(3)
          d_new <- sample_n(d_unique, size = 1)

        }
        tmp <- dplyr::bind_rows(tmp, d_new)
      }


      tmp2 <- tmp %>% dplyr::mutate(PSU_station = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "-"))

      SEFCRI_2014_1stage_coral_demographics <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(PSU_station = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "-")) %>%
        dplyr::filter(PSU_station %in% tmp2$PSU_station) %>%
        dplyr::select(-PSU_station)

      usethis::use_data(SEFCRI_2014_1stage_coral_demographics, overwrite = T)

    }



    ### Tortugas
    if(region == "Tortugas" && year == 2018){
      dat <- Tortugas_2018_coral_demographics

      tmp <- data.frame()

      set.seed(3)

      for(i in unique(dat$PRIMARY_SAMPLE_UNIT)){
        d <- dat %>% dplyr::filter(PRIMARY_SAMPLE_UNIT == i)

        d_unique <- d %>% dplyr::select(YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR) %>% dplyr::distinct(.)

        if(nrow(d_unique) == 1){
          d_new <- d_unique
        } else{

          set.seed(3)
          d_new <- sample_n(d_unique, size = 1)

        }
        tmp <- dplyr::bind_rows(tmp, d_new)
      }


      tmp2 <- tmp %>% dplyr::mutate(PSU_station = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "-"))

      Tortugas_2018_1stage_coral_demographics <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(PSU_station = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "-")) %>%
        dplyr::filter(PSU_station %in% tmp2$PSU_station) %>%
        dplyr::select(-PSU_station)

      usethis::use_data(Tortugas_2018_1stage_coral_demographics, overwrite = T)

    }
  }

}
