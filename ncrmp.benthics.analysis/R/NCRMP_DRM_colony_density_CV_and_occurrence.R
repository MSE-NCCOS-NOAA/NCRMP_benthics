## Function to calculate colony density cv by species, site level occurrence, identify species with CV less than 20%, and plot back to back figure

# Purpose:
# creates csv files with colony density, occurrence and a plot of both.


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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams, Sturm
# Last update: Sep 2024


##############################################################################################################################

#' Creates colony density summary dataframes
#'
#' Calculates regional estimate of coral density and coefficient of variation (CV),
#' by species, for a given region. NCRMP utilizes a stratified random
#' sampling design. Regional estimates of density are weighted by the number of grid cells of a stratum
#' in the sample frame. Function calculates weighted strata means to produce
#' regional estimates for coral density data by species.
#' Also calculates occurrence of each species in each year.
#' Additionally, function produces a figure of species occurrence and CV by species
#' for a given region and year.
#'
#'
#'
#'
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "FGB".
#' @param ptitle A string indicating the plot title, usually the region.
#' @param year A numeric indicating the year of interest, which will be plotted.
#' @param path A string indicating the filepath for the figure.
#' @param project A string indicating the project, "NCRMP" or NCRMP and DRM combined ("NCRMP_DRM").
#' @return A list dataframes. First, a dataframe of regional weighted mean density, CV,
#' and occurrence, by species for a given region. Second, a dataframe of the same,
#' filtered to only species/years where CV is less than or equal to 20%.
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 "ggplot"
#' @export
#'
#'

NCRMP_DRM_colony_density_CV_and_occurrence <- function(region, ptitle, year, file_path = "NULL", species_filter = "NULL", project = "NULL"){

  #### Call Function to get Species ####
  #this is located in the .R folder of the package and might require updating if species we want change
  coral_species_by_region <- species_for_CV_and_occurrence(region)

  ####Helper Function that gets the Dataset ####
  #this doesn't require constant updating of the datasets
  get_dataset <- function(region, year, project) {

    #tort and fgb both have different "region names"  in the datasets so mutate here
    if (region == "Tortugas"){ region = "Tort"}
    if (region == "FGB") { region = "FGBNMS"}

    #get the last 2 num of the year
    year_short <- year %% 100

    string_version <- paste(project,  region, start_year, year_short, "density_species", sep = "_")
    get(string_version, envir = .GlobalEnv)
  }

  #if the project isn't actually entered default to ncrmp
  if (project == "NULL"){ project = "NCRMP"}
  #call the helper function to get the dataset~
  sppdens <- get_dataset(region = region, project = project, year = year)

  #### Call NCRMP_make_weighted_density_CV_data function to get region means ####
  region_means <- NCRMP_make_weighted_density_CV_data(region = region, sppdens = sppdens, project = project)

  ####filter species if user adds a species filter   ####
  if (species_filter == TRUE) {
    region_means <- region_means %>% dplyr::filter(SPECIES_CD %in% coral_species_by_region)
  }

  #### Make the plot ####
  plot_cv_occ <- function(region_means, set_occurrence, year, ptitle) {

    # list the esa species before the plot to make it easier to read the code
    esa_species <- c("Acropora cervicornis", "Acropora palmata",
                     "Dendrogyra cylindrus", "Orbicella annularis",
                     "Orbicella faveolata", "Orbicella franksi",   "Mycetophyllia ferox")

    #reduce the number of filtering
    year <- as.numeric(year)

    region_means <- region_means %>%
      filter(YEAR == year) %>%
      filter(!is.na(CV), CV < 1)

      g.mid <- region_means %>%
        dplyr::filter(occurrence > set_occurrence) %>%
        ggplot(aes(x = 1, y = reorder(SPECIES_CD, occurrence))) +
        #this makes sure that the esa species are bolded and italicized
        geom_text(aes(label = SPECIES_CD, fontface = ifelse(SPECIES_CD %in% esa_species, "bold.italic", "italic")), size = 3) +
        geom_segment(aes(x = 0.94, xend = 0.96, yend = SPECIES_CD)) +
        geom_segment(aes(x = 1.04, xend = 1.065, yend = SPECIES_CD)) +
        ggtitle(ptitle) +
        ylab(NULL) +
        scale_x_continuous(expand = c(0, 0), limits = c(0.94, 1.065)) +
        theme(axis.title = element_blank(),
              panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(color = NA),
              axis.ticks.x = element_line(color = NA),
              plot.margin = unit(c(t = 1, r = -1, b = 1, l = -1), "mm"),
              plot.title = element_text(hjust = 0.5))


    g1 <-  region_means %>%
      # exclude occurrences of 0
      #dplyr::filter(occurrence > 0.01) %>%
      dplyr::filter(occurrence > set_occurrence) %>%
      ggplot(., aes(x = reorder(SPECIES_CD, occurrence),
                 y = occurrence,
                 fill = 'even')) +
      geom_bar(stat = "identity",
               fill = "deepskyblue4") +
      ggtitle(paste("Species", "Occurrence", sep = " ")) +
      theme_light() +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(t = 1, r = -0.5, b = 1, l = 1), "mm"),
            plot.title = element_text(hjust = 0.5,
                                      size = 10,
                                      face = "bold")) +
      coord_flip() +
      scale_y_reverse(expand = c(NA,0)) +
      guides(fill = "none")

    g2 <- region_means %>%
      # exclude occurrences of 0
      dplyr::filter(occurrence > set_occurrence) %>%
      ggplot(data = .,  aes(x = reorder(SPECIES_CD, occurrence),
                 y = CV*100,
                 fill = 'even')) +
      xlab(NULL) +
      geom_bar(stat = "identity",
               fill = "deepskyblue4") +
      ggtitle("Coefficient of Variation (CV) of Density") +
      theme_light() +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(t = 1, r = 1, b = 1, l = -2), "mm"),
            plot.title = element_text(hjust = 0.5, size = 10,  face = "bold")) +
      coord_flip() +
      guides(fill = "none") +
      geom_hline(yintercept=20, linetype="dashed", color = "black")


    return(list(g.mid = g.mid, g1 = g1, g2 = g2))
  }


   #### Create the plot, but inputs to the function depend on region ####
    if (region == "STX" || region == "STTSTJ" || region == "PRICO" || region == "FGB") {
      resulting_plot_parts <- plot_cv_occ(region_means = region_means, set_occurrence = 0.01, year = year, ptitle = ptitle)
    } else {
      resulting_plot_parts <- plot_cv_occ(region_means, set_occurrence = 0.02, year = year,  ptitle = ptitle)
    }

  ####use cowplot to plot grid parts ####
  final_plot <- cowplot::plot_grid(
    resulting_plot_parts$g1,
    resulting_plot_parts$g.mid,
    resulting_plot_parts$g2,
    ncol = 3, rel_widths = c(3.5 / 9, 2 / 9, 3.5 / 9)
  )

  filename <- paste0(file_path, "/", region_means$REGION[1], "_Occ_v_CV.jpeg")

    #### use ggsave to save plots ####
    ggsave( filename = filename,
            plot = final_plot,
            width = 9.8, height = 6.5,  dpi = 300, units = "in",
             device = "jpg"
    )

  #### Make dataset only if CV < .2 ####
  region_means_cv20 <- region_means %>%
    dplyr::filter(CV <= .20)

  #### Export ####
  output <- list(
    "region_means" = region_means,
    "region_means_cv20" = region_means_cv20)

  return(output)
}




