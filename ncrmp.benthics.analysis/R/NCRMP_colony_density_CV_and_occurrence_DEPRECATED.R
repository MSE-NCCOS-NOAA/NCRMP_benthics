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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Dec 2023


##############################################################################################################################

#' Creates colony density summary dataframes
#'
#' DEPRECATED. Use [NCRMP_DRM_colony_density_CV_and_occurrence()] instead.
#'
#' Calculates regional weighted mean coral density and coefficient of variation (CV), by species,
#' from coral demographic data. NCRMP utilizes a stratified random
#' sampling design. Regional estimates of density are weighted by the number of grid cells of a stratum
#' in the sample frame. Function calculates strata means, weighted strata means,
#' and weighted regional estimates for various measures from coral demographic data.
#' Also calculates occurrence of each species in each year. Produces a figure
#' of occurrence and CV by species for specified year.
#' Utilizes [NCRMP_make_weighted_density_CV_data()] to calculate regional
#' mean density, CV, and occurrence.
#'
#'
#'
#' @param region A string indicating the region.  Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param ptitle A string indicating the plot title.
#' @param year A numeric indicating the year of interest. Only controls which data are plotted.
#' @param path A string indicating the filepath for the figure
#' @param project A sting indicating the project. Only option is "NCRMP".
#' @return A list of dataframes, including one with all regional weighted mean densities,
#' CV's, and occurrences per species per year, and another with the same but filtered
#' to species/years where CV <= 20%
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 "ggplot"
#' @export
#'
#'


NCRMP_colony_density_CV_and_occurrence <- function(region, ptitle, year, file_path = "NULL", species_filter = "NULL", project = "NULL"){


  .Deprecated("NCRMP_DRM_colony_density_CV_and_occurrence")
  #############
  # coral species used in allocation
  #############

  if (species_filter == TRUE) {
    if(region=="STX" || region=="STTSTJ") {

      coral_species <- c("Orbicella annularis", "Orbicella faveolata", "Orbicella franksi", "Acropora cervicornis", "Acropora palmata", "Dendrogyra cylindrus", "Mycetophyllia ferox", "Colpophyllia natans",
                         "Dichocoenia stokesii", "Diploria labyrinthiformis", "Eusmilia fastigiata", "Meandrina meandrites", "Pseudodiploria strigosa", "Pseudodiploria clivosa")
    }

    if(region=="PRICO") {

      coral_species <- c("Orbicella annularis", "Orbicella faveolata", "Orbicella franksi", "Acropora cervicornis", "Acropora palmata", "Dendrogyra cylindrus", "Mycetophyllia ferox", "Colpophyllia natans",
                         "Dichocoenia stokesii", "Diploria labyrinthiformis", "Eusmilia fastigiata", "Meandrina meandrites", "Pseudodiploria strigosa", "Pseudodiploria clivosa")
    }



    # FL- subset of species per region
    # these were the species used for 2018 allocation- modify if needed
    if(region=="FLK") {
      coral_species <- c("Colpophyllia natans", "Montastraea cavernosa", "Orbicella faveolata", "Porites astreoides", "Siderastrea siderea", "Solenastrea bournoni")
    }
    if(region=="Tortugas") {
      coral_species <- c("Colpophyllia natans", "Montastraea cavernosa", "Orbicella faveolata", "Porites astreoides", "Orbicella franksi", "Stephanocoenia intersepta")
    }
    if(region=="SEFCRI") {
      coral_species <- c("Acropora cervicornis", "Dichocoenia stokesii", "Montastraea cavernosa", "Porites astreoide", "Pseudodiploria strigosa", "Siderastrea siderea")
    }

  }

  if(region=="FLK" && project == "NCRMP") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = NCRMP_FLK_2014_22_density_species,
                                                        project = project)
  }

  if(region=="FLK" && project == "MIR") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = MIR_2022_density_species_DUMMY,
                                                        project = project)
  }

  if(region=="FLK" && project == "NCRMP_DRM") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = NCRMP_DRM_FLK_2014_22_density_species,
                                                        project = project)
  }


  if(region=="Tortugas") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = NCRMP_Tort_2014_22_density_species,
                                                        project = project)
  }

  if(region=="SEFCRI" & project == "NCRMP") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = NCRMP_SEFCRI_2014_22_density_species,
                                                        project = project)
  }

  if(region=="PRICO") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = NCRMP_PRICO_2014_21_density_species,
                                                        project = project)
  }

  if(region=="STTSTJ") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = NCRMP_STTSTJ_2013_21_density_species,
                                                        project = project)
  }

  if(region=="STX") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = NCRMP_STX_2015_21_density_species,
                                                        project = project)
  }


  if(region=="GOM") {

    region_means <- NCRMP_make_weighted_density_CV_data(region = region,
                                                        sppdens = NCRMP_FGBNMS_2013_22_density_species,
                                                        project = project)
  }


  if (species_filter == TRUE) {

    region_means <- region_means %>% dplyr::filter(SPECIES_CD %in% coral_species)

  }


  if(region=="STX" || region=="STTSTJ" || region == "PRICO" || region == "GOM") {

  # Create plot pieces to export
  g.mid <- region_means %>%
    # filter to year of interest
    dplyr::filter(YEAR == year) %>%
    # exclude occurrences of 0
    dplyr::filter(occurrence > 0.01) %>%
    # exclude CVs over 1
    dplyr::filter(CV < 1) %>%

    # plot
    ggplot(.,
           aes(x = 1,
               y = reorder(SPECIES_CD, occurrence)))+
    geom_text(aes(label = SPECIES_CD),
              size = 3,
              fontface = "italic") +
    geom_segment(aes(x = 0.94,
                     xend = 0.96,
                     yend = SPECIES_CD))+
    geom_segment(aes(x = 1.04,
                     xend = 1.065,
                     yend = SPECIES_CD))+
    ggtitle(ptitle) +
    ylab(NULL) +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0.94, 1.065))+
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
    # filter to year of interest
    dplyr::filter(YEAR == year) %>%
    # exclude occurrences of 0
    dplyr::filter(occurrence > 0.01) %>%
    # exclude CVs over 1
    dplyr::filter(CV < 1) %>%

    ggplot(.,
           aes(x = reorder(SPECIES_CD, occurrence),
               y = occurrence,
               fill = 'even')) +
    # geom_hline(yintercept = c(0.25, 0.5, 0.75),
    # colour = "light grey") +
    geom_bar(stat = "identity",
             fill = "deepskyblue4") +
    ggtitle(paste("Species", "occurrence", sep = " ")) +
    # scale_fill_manual(values = c( "#0a4595")) +
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
    # scale_y_reverse(breaks = c(0, 0.25, 0.5, 0.75)) +
    guides(fill = "none")


  g2 <- region_means %>%
    # filter to year of interest
    dplyr::filter(YEAR == year) %>%
    # exclude occurrences of 0
    dplyr::filter(occurrence > 0.01) %>%
    # exclude CVs over 1
    dplyr::filter(CV < 1) %>%
    ggplot(data = .,
           aes(x = reorder(SPECIES_CD, occurrence),
               y = CV*100,
               fill = 'even')) +
    xlab(NULL) +
    geom_bar(stat = "identity",
             fill = "deepskyblue4") +
    ggtitle("Coefficient of Variation (CV) of density") +
    theme_light() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(t = 1, r = 1, b = 1, l = -2), "mm"),
          plot.title = element_text(hjust = 0.5,
                                    size = 10,
                                    face = "bold")) +
    coord_flip() +
    guides(fill = "none") +
    geom_hline(yintercept=20, linetype="dashed", color = "black")
  # scale_fill_manual(values= c( "#58babb"))

  } else {

    # Create plot pieces to export
    g.mid <- region_means %>%
      # filter to year of interest
      dplyr::filter(YEAR == year) %>%
      # exclude occurrences of 0
      dplyr::filter(occurrence > 0.02) %>%
      # exclude CVs over 1
      dplyr::filter(CV < 1) %>%

      # plot
      ggplot(.,
             aes(x = 1,
                 y = reorder(SPECIES_CD, occurrence)))+
      geom_text(aes(label = SPECIES_CD),
                size = 4,
                fontface = "italic") +
      geom_segment(aes(x = 0.94,
                       xend = 0.96,
                       yend = SPECIES_CD))+
      geom_segment(aes(x = 1.04,
                       xend = 1.065,
                       yend = SPECIES_CD))+
      ggtitle(ptitle) +
      ylab(NULL) +
      scale_x_continuous(expand = c(0,0),
                         limits = c(0.94, 1.065))+
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
      # filter to year of interest
      dplyr::filter(YEAR == year) %>%
      # exclude occurrences of 0
      dplyr::filter(occurrence > 0.01) %>%
      # exclude CVs over 1
      dplyr::filter(CV < 1) %>%

      ggplot(.,
             aes(x = reorder(SPECIES_CD, occurrence),
                 y = occurrence,
                 fill = 'even')) +
      # geom_hline(yintercept = c(0.25, 0.5, 0.75),
      # colour = "light grey") +
      geom_bar(stat = "identity",
               fill = "deepskyblue4") +
      ggtitle(paste("Species", "occurrence", sep = " ")) +
      # scale_fill_manual(values = c( "#0a4595")) +
      theme_light() +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(t = 1, r = -0.5, b = 1, l = 1), "mm"),
            plot.title = element_text(hjust = 0.5,
                                      size = 12,
                                      face = "bold")) +
      coord_flip() +
      scale_y_reverse(expand = c(NA,0)) +
      # scale_y_reverse(breaks = c(0, 0.25, 0.5, 0.75)) +
      guides(fill = "none")


    g2 <- region_means %>%
      # filter to year of interest
      dplyr::filter(YEAR == year) %>%
      # exclude occurrences of 0
      dplyr::filter(occurrence > 0.02) %>%
      # exclude CVs over 1
      dplyr::filter(CV < 1) %>%
      ggplot(data = .,
             aes(x = reorder(SPECIES_CD, occurrence),
                 y = CV*100,
                 fill = 'even')) +
      xlab(NULL) +
      geom_bar(stat = "identity",
               fill = "deepskyblue4") +
      ggtitle("Coefficient of Variation (CV) of density") +
      theme_light() +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(t = 1, r = 1, b = 1, l = -2), "mm"),
            plot.title = element_text(hjust = 0.5,
                                      size = 12,
                                      face = "bold")) +
      coord_flip() +
      guides(fill = "none") +
      geom_hline(yintercept=20, linetype="dashed", color = "black")
    # scale_fill_manual(values= c( "#58babb"))


  }



  cowplot::plot_grid(g1,
                     g.mid,
                     g2,
                     ncol = 3
                     # rel_widths = c(1, 0.5)
  )

  ggsave(filename = paste(region_means$REGION[1], "Occ_v_CV.jpeg", sep = "_"),
         path = file_path,
         plot = gridExtra::grid.arrange(g1, g.mid, g2,
                                        ncol = 3,
                                        widths = c(3.5/9,2/9,3.5/9)),
         width = 9.8,
         height = 6.5,
         dpi = 300,
         units = "in",
         device = "jpg")


  region_means_cv20 <- region_means %>%
    dplyr::filter(CV <= .20)




  ################
  # Export
  ################


  # Create list to export
  output <- list(
    "region_means" = region_means,
    "region_means_cv20" = region_means_cv20)

  return(output)




}
