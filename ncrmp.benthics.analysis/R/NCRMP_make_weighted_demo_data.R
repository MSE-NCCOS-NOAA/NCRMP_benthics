## Function to calculate weighted coral density, species richness, mortality and disease prevalence by strata and protected area

# Purpose:
# support function to calculate weighted coral density, species richness, mortality and disease prevalence


## Tag: data analysis


# outputs created in this file --------------
# density_strata
# richness_strata
# mortality_strata
# dis_prev_strata
# size_strata
# and Domain estimates for each of the above metric

# Weighting scheme:
# STRAT + PROT
# STRAT (Carib/GOM)


# Calls:
# analysis ready data

# output gets called by:
# NCRMP_calculate_species_richness_diversity.R
# NCRMP_DRM_calculate_colony_density.R
# NCRMP_DRM_calculate_mortality.R
# NCRMP_DRM_calculate_disease_prevalence.R
# NCRMP_DRM_calculate_mean_colony_size

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: March 2022


##############################################################################################################################

#' Creates weighted demo data
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param inputdata A dataframe
#' @param region A string indicating the region
#' @param datatype A string indicating the datatype
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_demo_data <- function(project, inputdata, region, datatype, species_filter, species_data = NULL){

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")

  ntot <- load_NTOT(region = region,
                    inputdata = inputdata,
                    project = project)

  #### Calculate weighted species richness ####

  if(datatype == "richness"){

    if(region %in% FL) {

      # Calculate avspr, svar, n and std
      richness_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average richness
          avspr = mean(SPP_RICHNESS),
          # compute stratum variance
          svar = var(SPP_RICHNESS),
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      richness_est <- richness_est %>%
        # Merge ntot
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavspr = wh * avspr,
                      whvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0),
                      PROT = NA,
                      RUG_CD = NA)  %>%
        dplyr::ungroup()

    }

    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avspr, svar, n and std
      richness_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average richness
          avspr = mean(SPP_RICHNESS),
          # compute stratum variance
          svar = var(SPP_RICHNESS),
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      richness_est <- richness_est %>%
        # Merge ntot
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavspr = wh * avspr,
                      whvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format temporarily
                      PROT = NA,
                      RUG_CD = NA)  %>%
        dplyr::ungroup()


    }



    # Reformat output

    richness_strata <-  richness_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, n, avspr, Var, SE,) %>%
      dplyr::mutate(RUG_CD = as.factor(RUG_CD))

    ## Domain Estimates
    Domain_est <- richness_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avSpr = sum(whavspr, na.rm = T), # This accounts for strata with 0 species of interest present
                       Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                       SE=sqrt(Var),
                       n_sites = sum(n),
                       n_strat = length(unique(ANALYSIS_STRATUM)),
                       ngrtot = sum(NTOT) )  %>%
      dplyr::ungroup()






    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "richness_strata" = richness_strata,
      "Domain_est" = Domain_est)

    return(output)

  }

  #### Calculate weighted density ####

  if(datatype == "density"){

    if(species_filter == "FALSE" ||
       species_filter == "NULL"){

      if(region %in% FL) {

        # Calculate avdns, svar, n and std
        density_est <- inputdata %>%
          # group by analysis level strata
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
          dplyr::summarise(# compute average density
            avden = mean(DENSITY),
            # compute stratum variance
            svar = var(DENSITY),
            # calculate N
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/avden)*100)

        density_est <- density_est %>%
          # Merge ntot with coral_est_spp
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavden = wh * avden,
                        whvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0))  %>%
          dplyr::ungroup()

      }


      if(region %in% GOM |
         region %in% Carib) {

        # Calculate avdns, svar, n and std
        density_est <- inputdata %>%
          # group by analysis level strata
          dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
          dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
          dplyr::summarise(# compute average density
            avden = mean(DENSITY),
            # compute stratum variance
            svar = var(DENSITY),
            # calculate N
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/avden)*100)

        density_est <- density_est %>%
          # Merge ntot with coral_est_spp
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavden = wh * avden,
                        whvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0),
                        # Add the following to match FL format
                        PROT = NA,
                        RUG_CD = NA)  %>%
          dplyr::ungroup()

      }


      # Reformat output

      # strata_means
      density_strata <-  density_est %>%
        dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, n, avden, Var, SE, CV_perc)

      ## Domain Estimates
      # region/population means
      Domain_est <- density_est %>%
        dplyr::group_by(REGION, YEAR) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T), # This accounts for strata with 0 species of interest present
                         Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                         SE=sqrt(Var),
                         CV_perc=(SE/avDen)*100,
                         n_sites = sum(n),
                         n_strat = length(unique(ANALYSIS_STRATUM)),
                         ngrtot = sum(NTOT) )  %>%
        dplyr::ungroup()

      ################
      # Export
      ################

      # Create list to export
      output <- list(
        "density_strata" = density_strata,
        "Domain_est" = Domain_est)

      return(output)

    }

    if(species_filter == "TRUE"){

      if(region %in% FL) {

        # Calculate avdns, svar, n and std
        density_est <- inputdata %>%
          # group by analysis level strata
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
          dplyr::summarise(# compute average density
            avden = mean(DENSITY),
            # compute stratum variance
            svar = var(DENSITY),
            # calculate N
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/avden)*100)

        density_est <- density_est %>%
          # Merge ntot with coral_est_spp
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavden = wh * avden,
                        whvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0))  %>%
          dplyr::ungroup()

        # calculate species CVS
        strata_CV <- species_data %>%
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          group_by(YEAR, SPECIES_CD, ANALYSIS_STRATUM, STRAT, PROT) %>%
          summarize(mean=mean(DENSITY),
                    svar=var(DENSITY),
                    N_species=length(DENSITY)) %>% # sample variance of density in stratum
          mutate(svar=dplyr::case_when(svar==0 ~ 0.00000001, # replace zeros with very small number
                                       TRUE ~ svar)) %>%
          dplyr::left_join(., density_est %>% dplyr::select(YEAR, ANALYSIS_STRATUM, n)) %>%

          mutate(Var=svar/n, #variance of mean density in stratum
                 std = sqrt(svar), # std dev of density in stratum
                 SE=sqrt(Var), #SE of the mean density in stratum
                 CV_perc=(SE/mean)*100)

        # region/population means
        region_CV <- strata_CV %>%
          dplyr::full_join(., ntot) %>%
          dplyr::mutate(wh_mean=wh*mean, wh_var = wh^2*Var) %>%
          dplyr::group_by(YEAR, SPECIES_CD) %>%
          dplyr::summarize(mean=sum(wh_mean), Var=sum(wh_var, na.rm=TRUE),
                           SE=sqrt(Var), CV_perc=(SE/mean)*100, n= sum(N_species)) %>%
          dplyr::mutate(ANALYSIS_STRATUM="ALL_STRAT", STRAT="ALL_HABS", PROT="ALL_PROT") %>%  #add svar variable
          dplyr::select(YEAR, ANALYSIS_STRATUM, SPECIES_CD, n, mean, Var, SE, CV_perc, STRAT, PROT)


      }


      if(region %in% GOM |
         region %in% Carib) {

        # Calculate avdns, svar, n and std
        density_est <- inputdata %>%
          # group by analysis level strata
          dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
          dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
          dplyr::summarise(# compute average density
            avden = mean(DENSITY),
            # compute stratum variance
            svar = var(DENSITY),
            # calculate N
            n_sites = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/avden)*100)

        density_est <- density_est %>%
          # Merge ntot with coral_est_spp
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavden = wh * avden,
                        whvar = wh^2 * Var,
                        n_sites = tidyr::replace_na(n_sites, 0),
                        # Add the following to match FL format
                        PROT = NA,
                        RUG_CD = NA)  %>%
          dplyr::ungroup()

        # calculate species CVS
        strata_CV <- species_data %>%
          dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
          dplyr::group_by(YEAR, SPECIES_CD, ANALYSIS_STRATUM, STRAT) %>%
          dplyr::summarize(mean=mean(DENSITY),
                           svar=var(DENSITY),
                           n_colonies = sum(ABUNDANCE),
                           n_sites_present=length(DENSITY)) %>% # sample variance of density in stratum
          dplyr::mutate(svar=dplyr::case_when(svar==0 ~ 0.00000001, # replace zeros with very small number
                                              TRUE ~ svar)) %>%
          dplyr::left_join(., density_est %>% dplyr::select(YEAR, ANALYSIS_STRATUM, n_sites)) %>%

          dplyr::mutate(Var=svar/n_sites, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/mean)*100,
                        Occurrence = n_sites_present/n_sites)

        if(region %in% GOM) {

          # region/population means - FGB has no strata currently so this is equivalent to regional CVs
          region_CV <- strata_CV


        } else {

          Total_N <- density_est %>%
            dplyr::group_by(REGION) %>%
            dplyr::summarise(n_sites = sum(n_sites),
                             n_strat = length(unique(ANALYSIS_STRATUM)))  %>%
            dplyr::ungroup()

          # region/population means
          region_CV <- strata_CV %>%
            dplyr::full_join(., ntot, by=c("YEAR", "ANALYSIS_STRATUM")) %>%
            dplyr::mutate(wh_mean=wh*mean, wh_var = wh^2*Var) %>%
            dplyr::group_by(REGION, SPECIES_CD) %>%
            dplyr::summarize(avDen=sum(wh_mean), Var=sum(wh_var, na.rm=TRUE),
                             SE=sqrt(Var), CV_perc=(SE/avDen), n_colonies=sum(n_colonies), n_sites_present = sum(n_sites_present)) %>%
            dplyr::mutate(YEAR = "ALL_YEARS", ANALYSIS_STRATUM="ALL_STRAT", STRAT="ALL_HABS", PROT="ALL_PROT", n_sites = Total_N$n_sites,
                          Occurrence = n_sites_present/n_sites) %>%
            dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_CD, n_colonies, n_sites_present, n_sites, avDen, Var, SE, CV_perc, Occurrence, STRAT, PROT)



        }

        g.mid <- ggplot(region_CV, aes(x=1,y=reorder(SPECIES_CD, CV_perc)))+geom_text(aes(label=SPECIES_CD))+
          geom_segment(aes(x=0.94,xend=0.96,yend=SPECIES_CD))+
          geom_segment(aes(x=1.04,xend=1.065,yend=SPECIES_CD))+
          ggtitle(region_CV$REGION)+
          ylab(NULL)+
          scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
          theme(axis.title=element_blank(),
                panel.grid=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.background=element_blank(),
                axis.text.x=element_text(color=NA),
                axis.ticks.x=element_line(color=NA),
                plot.margin = unit(c(1,-1,1,-1), "mm"),
                plot.title = element_text(hjust = 0.5))

        g1 <- ggplot(data = region_CV, aes(x = reorder(SPECIES_CD, CV_perc), y = Occurrence, fill = 'even')) +
          geom_bar(stat = "identity") + ggtitle("Occurrence") +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.margin = unit(c(1,-1,1,0), "mm"),
                plot.title = element_text(hjust = 0.5)) +
          scale_y_reverse() + coord_flip() + guides(fill = FALSE) + scale_fill_manual(values= c( "#0a4595"))

        g2 <- ggplot(data = region_CV, aes(x = reorder(SPECIES_CD, CV_perc), y = CV_perc, fill = 'even')) +xlab(NULL)+
          geom_bar(stat = "identity") + ggtitle("Coefficient of Variation of mean density") +
          theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                plot.margin = unit(c(1,0,1,-1), "mm"), plot.title = element_text(hjust = 0.5)) +
          coord_flip() + guides(fill = FALSE) + scale_fill_manual(values= c( "#58babb"))


      }


      # Reformat output

      # strata_means
      density_strata <-  density_est %>%
        dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, n_sites, avden, Var, SE, CV_perc) %>%
        dplyr::mutate(RUG_CD = as.factor(RUG_CD))

      ## Domain Estimates
      # region/population means
      Domain_est <- density_est %>%
        dplyr::group_by(REGION, YEAR) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T), # This accounts for strata with 0 species of interest present
                         Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                         SE=sqrt(Var),
                         CV_perc=(SE/avDen)*100,
                         n_sites = sum(n_sites),
                         n_strat = length(unique(ANALYSIS_STRATUM)),
                         ngrtot = sum(NTOT) )  %>%
        dplyr::ungroup()

      ################
      # Export
      ################


      # Create list to export
      output <- list(
        "Species_regional_CV" = region_CV,
        "density_strata" = density_strata,
        "Domain_est" = Domain_est,
        'g1' = g1,
        'g2' = g2,
        'g.mid' = g.mid)

      return(output)


    }


  }


  #### Calculate mortality ####

  if(datatype == "mortality"){

    if(region %in% FL) {

      # Calculate avmort, svar, n and std
      mortality_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT, MORT_TYPE) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(
          # compute average mortality
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          n = length(unique(PRIMARY_SAMPLE_UNIT)), .groups = "keep") %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var), #SE of the mean density in stratum
                      CV_perc=(SE/avmort)*100)

      mortality_est <- mortality_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()

    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avdns, svar, n and std
      mortality_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, MORT_TYPE) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var), #SE of the mean density in stratum
                      CV_perc=(SE/avmort)*100)

      mortality_est <- mortality_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format
                      PROT = NA,
                      RUG_CD = NA)  %>%
        dplyr::ungroup()

    }


    # Reformat output

    # strata_means
    mortality_strata <-  mortality_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, n, avmort, Var, SE, CV_perc)

    ## Domain Estimates
    # region/population means
    Domain_est <- mortality_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avMort = sum(whavmort, na.rm = T), # This accounts for strata with 0 species of interest present
                       Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                       SE=sqrt(Var),
                       CV_perc=(SE/avMort)*100,
                       n_sites = sum(n),
                       n_strat = length(unique(ANALYSIS_STRATUM)),
                       ngrtot = sum(NTOT), .groups = "keep" )  %>%
      dplyr::ungroup()

    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "mortality_strata" = mortality_strata,
      "Domain_est" = Domain_est)

    return(output)
  }

  #### Calculate weighted diversity ####

  if(datatype == "diversity"){

    if(region %in% FL) {

      # Calculate avdiv, svar, n and std
      diversity_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(
          # compute average diversity
          avSimp = mean(Simpson),
          avInvSimp = mean(Inv_Simpson),
          avShannon = mean(Shannon),
          # compute stratum variance
          svar_Simp = var(Simpson),
          svar_InvSimp = var(Inv_Simpson),
          svar_Shan = var(Shannon),
          n = length(PRIMARY_SAMPLE_UNIT)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar_Simp = dplyr::case_when(svar_Simp == 0 ~ 0.00000001,
                                                   TRUE ~ svar_Simp),
                      svar_InvSimp = dplyr::case_when(svar_InvSimp == 0 ~ 0.00000001,
                                                      TRUE ~ svar_InvSimp),
                      svar_Shan = dplyr::case_when(svar_Shan == 0 ~ 0.00000001,
                                                   TRUE ~ svar_Shan)) %>%
        dplyr::mutate(Var_Simp =svar_Simp/n,
                      Var_InvSimp = svar_InvSimp,
                      Var_Shan = svar_Shan,
                      std_Simp = sqrt(svar_Simp),
                      std_InvSimp = sqrt(svar_InvSimp),
                      std_Shan = sqrt(svar_Shan),
                      SE_Simp = sqrt(Var_Simp),
                      SE_InvSimp = sqrt(Var_InvSimp),
                      SE_Shan = sqrt(Var_Shan))


      diversity_est <- diversity_est %>%
        # Merge ntot with diversity_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavSimp = wh * avSimp,
                      whavInvSimp = wh * avInvSimp,
                      whavShan = wh * avShannon,
                      whvar_Simp = wh^2 * Var_Simp,
                      whvar_InvSimp = wh^2 * Var_InvSimp,
                      whvar_Shan = wh^2 * Var_Shan,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avdiv, svar, n and std
      diversity_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(
          # compute average diversity
          avSimp = mean(Simpson),
          avInvSimp = mean(Inv_Simpson),
          avShannon = mean(Shannon),
          # compute stratum variance
          svar_Simp = var(Simpson),
          svar_InvSimp = var(Inv_Simpson),
          svar_Shan = var(Shannon),
          n = length(Simpson)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar_Simp = dplyr::case_when(svar_Simp == 0 ~ 0.00000001,
                                                   TRUE ~ svar_Simp),
                      svar_InvSimp = dplyr::case_when(svar_InvSimp == 0 ~ 0.00000001,
                                                      TRUE ~ svar_InvSimp),
                      svar_Shan = dplyr::case_when(svar_Shan == 0 ~ 0.00000001,
                                                   TRUE ~ svar_Shan)) %>%
        dplyr::mutate(Var_Simp =svar_Simp/n,
                      Var_InvSimp = svar_InvSimp,
                      Var_Shan = svar_Shan,
                      std_Simp = sqrt(svar_Simp),
                      std_InvSimp = sqrt(svar_InvSimp),
                      std_Shan = sqrt(svar_Shan),
                      SE_Simp = sqrt(Var_Simp),
                      SE_InvSimp = sqrt(Var_InvSimp),
                      SE_Shan = sqrt(Var_Shan))


      diversity_est <- diversity_est %>%
        # Merge ntot with diversity_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavSimp = wh * avSimp,
                      whavInvSimp = wh * avInvSimp,
                      whavShan = wh * avShannon,
                      whvar_Simp = wh^2 * Var_Simp,
                      whvar_InvSimp = wh^2 * Var_InvSimp,
                      whvar_Shan = wh^2 * Var_Shan,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format temporarily
                      PROT = NA,
                      RUG_CD = NA)  %>%
        dplyr::ungroup()


    }


    # Reformat output

    diversity_strata <-  diversity_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, n,
                    avSimp, avInvSimp, avShannon, Var_Simp, Var_InvSimp, Var_Shan,
                    SE_Simp, SE_InvSimp, SE_Shan) %>%
      dplyr::mutate(RUG_CD = as.factor(RUG_CD))

    ## Domain Estimates
    Domain_est <- diversity_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avSimp = sum(whavSimp),
                       avInvSimp = sum(whavInvSimp),
                       avShan = sum(whavShan),
                       Var_Simp = sum(whvar_Simp, na.rm = T),
                       Var_InvSimp = sum(whvar_InvSimp, na.rm = T),
                       Var_Shan = sum(whvar_Shan, na.rm = T),
                       SE_Simp = sqrt(Var_Simp),
                       SE_InvSimp = sqrt(Var_InvSimp),
                       SE_Shan = sqrt(Var_Shan),
                       n_sites = sum(n),
                       n_strat = length(unique(ANALYSIS_STRATUM)),
                       ngrtot = sum(NTOT), .groups = "keep" )  %>%
      dplyr::ungroup()




    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "diversity_strata" = diversity_strata,
      "Domain_est_div" = Domain_est)

    return(output)

  }

  if(datatype == "disease"){

    if(region %in% FL) {

      # Calculate avprev, svar, n and std
      disease_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avDprev = mean(DIS_PREV),
          avBprev = mean(BLE_PREV),
          # compute stratum variance
          svarD = var(DIS_PREV),
          svarB = var(BLE_PREV),
          # calculate N
          n_sites = length(PRIMARY_SAMPLE_UNIT)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svarD = dplyr::case_when(svarD == 0 ~ 0.00000001,
                                               TRUE ~ svarD)) %>%
        dplyr::mutate(stdD = sqrt(svarD))%>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svarB = dplyr::case_when(svarB == 0 ~ 0.00000001,
                                               TRUE ~ svarB)) %>%
        dplyr::mutate(stdB = sqrt(svarB))

      disease_est <- disease_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavDprev = wh * avDprev,
                      whavBprev = wh * avBprev,
                      whsvarD = wh^2 * svarD,
                      whsvarB = wh^2 * svarB,
                      whstdD = wh * stdD,
                      whstdB = wh * stdB,
                      n_sites = tidyr::replace_na(n_sites, 0))  %>%
        dplyr::ungroup()

    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avprev, svar, n and std
      disease_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avDprev = mean(DIS_PREV),
          avBprev = mean(BLE_PREV),
          # compute stratum variance
          svarD = var(DIS_PREV),
          svarB = var(BLE_PREV),
          # calculate N
          n_sites = length(PRIMARY_SAMPLE_UNIT)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svarD = dplyr::case_when(svarD == 0 ~ 0.00000001,
                                               TRUE ~ svarD)) %>%
        dplyr::mutate(stdD = sqrt(svarD))%>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svarB = dplyr::case_when(svarB == 0 ~ 0.00000001,
                                               TRUE ~ svarB)) %>%
        dplyr::mutate(stdB = sqrt(svarB))

      disease_est <- disease_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavDprev = wh * avDprev,
                      whavBprev = wh * avBprev,
                      whsvarD = wh^2 * svarD,
                      whsvarB = wh^2 * svarB,
                      whstdD = wh * stdD,
                      whstdB = wh * stdB,
                      n_sites = tidyr::replace_na(n_sites, 0))  %>%
        dplyr::ungroup()
    }


    # Reformat output

    disease_est <- disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT,  PROT, NTOT, ngrtot, wh, n_sites, avDprev, svarD, stdD, whavDprev, whsvarD, whstdD,
                    avBprev, svarB, stdB, whavBprev, whsvarB, whstdB)

    dis_prev_strata <-  disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT,  PROT, NTOT, ngrtot, wh, n_sites, avDprev, svarD, stdD)

    ble_prev_strata <-  disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, NTOT, ngrtot, wh, n_sites, avBprev, svarB, stdB)

    ## Domain Estimates
    Domain_est <- disease_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avDisPrev = sum(whavDprev, na.rm = T), # This accounts for strata with 0 species of interest present
                       avBlePrev = sum(whavBprev, na.rm = T),
                       VarD = sum(whsvarD, na.rm = T),
                       VarB = sum(whsvarB, na.rm = T),# This accounts for strata with N = 1
                       SE_D=sqrt(VarD),
                       SE_B=sqrt(VarB),
                       n_sites = sum(n_sites),
                       n_strat = length(unique(ANALYSIS_STRATUM)),
                       ngrtot = sum(NTOT) )  %>%
      dplyr::ungroup()


    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "dis_prev_strata" = dis_prev_strata,
      'ble_prev_strata' = ble_prev_strata,
      "Domain_est" = Domain_est)

    return(output)



  }

  if(datatype == "size"){

    if(region %in% FL) {

      # Calculate av, svar, n and std
      size_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avcm2 = mean(avg_cm2),
          avcm3 = mean(avg_cm3),
          # compute stratum variance
          svarcm2 = var(avg_cm2),
          svarcm3 = var(avg_cm3),
          # calculate N
          n_sites = length(PRIMARY_SAMPLE_UNIT)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svarcm2 = dplyr::case_when(svarcm2 == 0 ~ 0.00000001,
                                                 TRUE ~ svarcm2)) %>%
        dplyr::mutate(stdcm2 = sqrt(svarcm2))%>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svarcm3 = dplyr::case_when(svarcm3 == 0 ~ 0.00000001,
                                                 TRUE ~ svarcm3)) %>%
        dplyr::mutate(stdcm3 = sqrt(svarcm3))

      size_est <- size_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavcm2 = wh * avcm2,
                      whavcm3 = wh * avcm3,
                      whsvarcm2 = wh^2 * svarcm2,
                      whsvarcm3 = wh^2 * svarcm3,
                      whstdcm2 = wh * stdcm2,
                      whstdcm3 = wh * stdcm3,
                      n_sites = tidyr::replace_na(n_sites, 0))  %>%
        dplyr::ungroup()
    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate av, svar, n and std
      size_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avcm2 = mean(avg_cm2),
          avcm3 = mean(avg_cm3),
          # compute stratum variance
          svarcm2 = var(avg_cm2),
          svarcm3 = var(avg_cm3),
          # calculate N
          n_sites = length(PRIMARY_SAMPLE_UNIT)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svarcm2 = dplyr::case_when(svarcm2 == 0 ~ 0.00000001,
                                                 TRUE ~ svarcm2)) %>%
        dplyr::mutate(stdcm2 = sqrt(svarcm2))%>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svarcm3 = dplyr::case_when(svarcm3 == 0 ~ 0.00000001,
                                                 TRUE ~ svarcm3)) %>%
        dplyr::mutate(stdcm3 = sqrt(svarcm3))

      size_est <- size_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavcm2 = wh * avcm2,
                      whavcm3 = wh * avcm3,
                      whsvarcm2 = wh^2 * svarcm2,
                      whsvarcm3 = wh^2 * svarcm3,
                      whstdcm2 = wh * stdcm2,
                      whstdcm3 = wh * stdcm3,
                      n_sites = tidyr::replace_na(n_sites, 0))  %>%
        dplyr::ungroup()
    }


    # Reformat output

    size_est <- size_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT,  PROT, NTOT, ngrtot, wh, n_sites, avcm2, svarcm2, stdcm2, whavcm2, whsvarcm2, whstdcm2,
                    avcm3, svarcm3, stdcm3, whavcm3, whsvarcm3, whstdcm3)

    size_est_cm2_strata <-  size_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT,  PROT, NTOT, ngrtot, wh, n_sites, avcm2, svarcm2, stdcm2)

    size_est_cm3_strata <-  size_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, NTOT, ngrtot, wh, n_sites, avcm3, svarcm3, stdcm3)

    ## Domain Estimates
    Domain_est <- size_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avCm2 = sum(whavcm2, na.rm = T), # This accounts for strata with 0 species of interest present
                       avCm3 = sum(whavcm3, na.rm = T),
                       Var_cm2 = sum(whsvarcm2, na.rm = T),
                       Var_cm3 = sum(whsvarcm3, na.rm = T),# This accounts for strata with N = 1
                       SE_cm2=sqrt(Var_cm2),
                       SE_cm3=sqrt(Var_cm3),
                       n_sites = sum(n_sites),
                       n_strat = length(unique(ANALYSIS_STRATUM)),
                       ngrtot = sum(NTOT) )  %>%
      dplyr::ungroup()


    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "size_est_cm2_strata" = size_est_cm2_strata,
      'size_est_cm3_strata' = size_est_cm3_strata,
      "Domain_est" = Domain_est)

    return(output)



  }


}















