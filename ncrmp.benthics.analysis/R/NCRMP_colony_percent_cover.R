## Function to calculate coral cover by species

# Purpose:
# creates csv files with coral cover


## Tag: data analysis


# outputs created in this file --------------
#
#
#

# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams, Krampitz
# Last update: Jul 2025


##############################################################################################################################

#' Creates colony coral cover summary data frames
#'
#'
#' Calculates coral cover strata means and regional weighted means, by coral species. Also
#' produces a figure of percent cover by species from most recent year of data for the
#' selected region.
#'
#'
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "FGB".
#' @param ptitle A string indicating the plot title.
#' @param project A string indicating the project: "NCRMP" or "MIR".
#' @param file_path A string indicating the filepath for the figure output by this function.
#' @return A list of dataframes and a jpeg. Dataframes include strata mean cover by species and regional weighted mean cover by species.
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_colony_percent_cover <- function(region, ptitle, year, file_path, project = "NULL"){
  #one big change in this function is the addition of the year parameter, which allows for easier retreval of datasets and not having the manually change the code every year

  ####Helper Function that gets the Dataset ####

  #this doesn't require constant updating of the datasets
  get_dataset <- function(region, year, project) {
    if (region == "Tortugas"){ region = "Tort"}
    if (region == "FGB") { region = "FGBNMS"}
    year_short <- year %% 100

    string_version <- paste(project,  region, start_year, year_short, "percent_cover_species", sep = "_")
    get(string_version, envir = .GlobalEnv)
  }

  ####Get Dataset####

  #if the project isn't actually entered default to ncrmp
  if (project == "NULL"){ project = "NCRMP"}
  #call the helper function to get the dataset~
  sppcvr_dataset <- get_dataset(region = region, project = project, year = year)

  #### get weighted data   ####
  weighted_data <- NCRMP_make_weighted_species_coral_cover_data(region = region, sppcvr = sppcvr_dataset, project = project)
  list2env(weighted_data, envir = .GlobalEnv)

  #### Find Latest Sampling Year   ####
  latest_year <- max(region_means$YEAR, na.rm = TRUE)

  #### Create Plot   ####
  g1 <-  region_means %>%
    dplyr::ungroup() %>%
    # exclude occurrences of 0
    dplyr::filter(avCvr > 0,
                  YEAR >= latest_year) %>%
    ggplot(.,aes(x = reorder(SPECIES_NAME, avCvr),
               y = avCvr,
               fill = 'even')) +
    geom_bar(stat = "identity",
             fill = "deepskyblue4") +
    ggtitle(paste(ptitle, "Species Percent Cover", sep = " ")) +
    theme_light() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(face ="italic"),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(t = 1, r = 1, b = 1, l = 1), "mm"),
          plot.title = element_text(hjust = 0.5,
                                    size = 10,
                                    face = "bold")) +
    coord_flip() +
    guides(fill = "none")

  ggsave(filename = paste(region_means$REGION[1], "species_cover.jpeg", sep = "_"),
         path = file_path,
         plot = g1,
         width = 9.8,
         height = 6.5,
         dpi = 300,
         units = "in",
         device = "jpg")

  #### Export ####
  output <- list(
    "region_means" = region_means,
    "strata_means" = strata_means)

  return(output)
}

