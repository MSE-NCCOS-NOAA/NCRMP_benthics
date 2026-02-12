
## Function to calculate weighted means for species specific size bins for NCRMP and DRM  data

# Purpose:
# creates csv files with weighted means for species specific size bins


## Tag: data analysis


# outputs created in this file --------------
# size_3d_demos
# length_demos
# size_estimates
# length_estimates
# size_domain_est
# length_domain_est
# length_freq_estimates
# length_freq_domain_est
# domain_mort_spp
# strat_mort

# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Davis, Groves, Viehman, Williams, Krampitz
# Last update: Jul 2025


##############################################################################################################################

#' Outputs the strata and domain estimates for 3D surface area and length
#'
#' Creates regional weighted densities at size (3D surface and length),
#' as well as regional weighted relative length frequencies from NCRMP coral
#' demographic data. NCRMP utilizes a stratified random
#' sampling design. Regional estimates are weighted by the number of
#' grid cells of a stratum in the sample frame. For coral size data, weighting
#' is done only by strata where species is present.
#'
#'
#'
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined ("NCRMP_DRM").
#' @param region A string indicating the region.  Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "FGB".
#' @param years A concatenation of numerics indicating the two years to compare. Must be NCRMP sampling years if project = "NCRMP".
#' @param size_bin_count A number indicating the desired number of bins for 3d surface area.
#' As function is currently set up, this is not used.
#' @param length_bin_count A number indicating the desired number of bins for length.
#' As function is currently set up, this is not used.
#' @param species_filter A concatenated string indicating whether to filter to a subset of species
#' @return A list of dataframes.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "case_when"
#' @export
#'
#'

NCRMP_make_size_bins <- function(region , project, years, size_bin_count = 10, length_bin_count = 10, species_filter = NULL) {

  ####Load Data###
  analyzed_species <-  species_filter

  #p - a constant for 3d surface area calculation
  p = 1.6

  #pull the demo data using the NCRMP function
  #outputs a list of two dfs: dat_1stage and dat_2stage
  demos <- load_NCRMP_DRM_demo_data(project = project, region = region)


  if(project == "NCRMP" && region %in% c("FLK", "PRICO", "STTSTJ", "STX", "FGB")){
    #These regions only have dat_1stage needed
    demos <- demos$dat_1stage %>%
      dplyr::filter(YEAR %in% years)
  }

  ####Prep Florida Region Data####

  if(project == "NCRMP_DRM" | region %in% c("SEFCRI", "Tortugas")){

    # with 2 stage data (NCRMP+DRM and SEFCRI 2014, Tortugas 2018 and 2020)
    # need to randomly select one transect to use

    if(region == "SEFCRI"){
      if(project == "NCRMP"){
        # 1 stage demo data
        tmp1 <- demos$dat_1stage
        # load subsetted version of 2 stage data
        tmp2 <- SEFCRI_2014_1stage_coral_demographics
        # combine with actual 1 stage data
        demos <- dplyr::bind_rows(tmp1, tmp2) %>%
          dplyr::filter(YEAR %in% years)
      }
      if(project == "NCRMP_DRM"){
        # 1 stage demo data
        tmp1 <- demos$dat_1stage %>% dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT))
        # load subsetted versions of 2 stage data
        tmp2 <- dplyr::bind_rows(SEFCRI_2014_1stage_coral_demographics %>% dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT)),
                                 DRM_SEFCRI_2014_2024_1stage_coral_demographics)
        # combine with actual 1 stage data
        demos <- dplyr::bind_rows(tmp1, tmp2) %>%
          dplyr::filter(YEAR %in% years)
      }
    }

    if(region == "Tortugas"){
      if(project == "NCRMP"){
        # 1 stage demo data
        tmp1 <- demos$dat_1stage
        # load subsetted versions of 2 stage data
        tmp2 <- Tortugas_2018_1stage_coral_demographics
        # combine with actual 1 stage data
        demos <- dplyr::bind_rows(tmp1, tmp2) %>%
          dplyr::filter(YEAR %in% years)
      }
      if(project == "NCRMP_DRM"){
        # 1 stage demo data
        tmp1 <- demos$dat_1stage
        # load subsetted versions of 2 stage data
        tmp2 <- dplyr::bind_rows(Tortugas_2018_1stage_coral_demographics, Tortugas_2020_coral_demographics) %>%
          dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT)) %>%
          # also add in the DRM data, but need to exclude tortugas 2021 because that is already in the NCRMP 2020 data
          dplyr::bind_rows(DRM_Tort_2014_2024_1stage_coral_demographics %>% dplyr::filter(YEAR != 2021))
        # combine with actual 1 stage data
        demos <- dplyr::bind_rows(tmp1, tmp2) %>%
          dplyr::filter(YEAR %in% years)
      }
    }
    if(region == "FLK"){
      # pull just ncrmp data from demos loaded data
      tmp1 <- dplyr::bind_rows(demos$dat_1stage %>% dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT)),
                               demos$dat_2stage) %>%
        dplyr::filter(SURVEY == "NCRMP")
      # pull DRM 'single stage' data (which is just the first transect in the dataset from each site)
      tmp2 <- DRM_FLK_2014_2024_1stage_coral_demographics %>% dplyr::mutate(PROT = as.factor(PROT))
      demos <- dplyr::bind_rows(tmp1, tmp2) %>%
        dplyr::filter(YEAR %in% years)
    }
  }


  if (!is.null(species_filter)) {
    demos <- demos %>%
      dplyr::filter(SPECIES_CD %in% analyzed_species)
  }

  ####Clean data####
  demos <- demos %>%
    dplyr::mutate(LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                  LON_DEGREES = sprintf("%0.4f", LON_DEGREES),
                  PROT = as.factor(PROT))



  ####Length Calculation####
  length_demos <- demos %>%
    #Year as factor (no calc needed as length = MAX_DIAMETER)
    dplyr::mutate(YEAR = as.factor(as.character(YEAR))) %>%
    #Filter out where MAX_DIAMETER does not exist
    #and where STRAT does not exist
    dplyr::filter(!is.na(MAX_DIAMETER), !is.na(STRAT), MAX_DIAMETER >= 4) %>%
    #Calculate the Ranges and the Bin Width by...
    #...grouping by the species (all years combined),
    dplyr::group_by(SPECIES_NAME) %>%
    #calculate max and min of length
    dplyr::mutate(max = max(MAX_DIAMETER),
                  min = 4) %>%
    # ROUND any diameters - there shouldn't be decimals in these, but sometimes they come up in the older data
    dplyr::mutate(MAX_DIAMETER = round(MAX_DIAMETER, digits = 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(  bin_num = dplyr::case_when(
                        MAX_DIAMETER <= 10 ~ 1, MAX_DIAMETER <= 15 ~ 2, MAX_DIAMETER <= 20 ~ 3,
                        MAX_DIAMETER <= 25 ~ 4, MAX_DIAMETER <= 30 ~ 5, MAX_DIAMETER <= 35 ~ 6,
                        MAX_DIAMETER <= 45 ~ 7, MAX_DIAMETER <= 65 ~ 8, MAX_DIAMETER <= 85 ~ 9,
                        MAX_DIAMETER <= 105 ~ 10, MAX_DIAMETER >=106 ~ 11, TRUE ~ NA_real_
                      ),
                      bin_name = dplyr::case_when(
                        bin_num == 1 ~ "4-10", bin_num == 2 ~ "11-15", bin_num == 3 ~ "16-20",
                        bin_num == 4 ~ "21-25", bin_num == 5 ~ "26-30", bin_num == 6 ~ "31-35",
                        bin_num == 7 ~ "36-45", bin_num == 8 ~ "46-65", bin_num == 9 ~ "66-85",
                        bin_num == 10 ~ "86-105", bin_num == 11 ~ "106+"
                       ))



  length_demos_raw <- length_demos


  length_demos <- length_demos %>%
    #summarize findings by bin count
    dplyr::group_by(SPECIES_NAME, SPECIES_CD, REGION, YEAR, PRIMARY_SAMPLE_UNIT,
                    STRAT, PROT, bin_num, bin_name, min) %>%
    dplyr::summarise(bin_tally = dplyr::n(), .groups = "keep") %>%
    dplyr::arrange(SPECIES_NAME, YEAR, PRIMARY_SAMPLE_UNIT, STRAT, PROT,
                   bin_num)


    # Define whether FL style stratification should be used
    use_FL_stratum <- region %in% c("FLK", "SEFCRI", "Tortugas")

    #### make analysis_stratum formatting####
    length_demos <- if (use_FL_stratum) {
      length_demos %>% dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
    } else {
      length_demos %>% dplyr::mutate(ANALYSIS_STRATUM = STRAT)
    }

    #### Length Estimates ####
    length_estimates <- length_demos %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
      dplyr::summarise(# compute average bin_tally
        avtally = mean(bin_tally),
        # compute stratum variance
        svar = var(bin_tally),
        # calculate N
        n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
        .groups = "keep") %>%
      dplyr::mutate(
        svar = dplyr::case_when(svar == 0 ~ 1e-8, TRUE ~ svar),
        Var = svar / n_sites,
        std = sqrt(svar),
        SE = sqrt(Var),
        CV_perc = (SE / avtally) * 100
      )

    #### Total Coral Counts ####
    tot_corals <- length_demos %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD) %>%
      dplyr::summarize(tot_corals = sum(bin_tally), .groups = "drop")

    ####  Relative Length Frequency ####
    length_freq_estimates <- length_demos %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
      # sum up the number of corals in each size bin in each strat
      dplyr::summarize(n_corals = sum(bin_tally)) %>%
      dplyr::ungroup() %>%
      # add total number of corals in each strat
      dplyr::left_join(., tot_corals, by = c("REGION", "YEAR", "ANALYSIS_STRATUM", "SPECIES_NAME", "SPECIES_CD")) %>%
      # calculate relative frequency (proportion) of corals in each size bin
      dplyr::mutate(length_freq = n_corals/tot_corals)

    #### Mortality Estimates ####
    length_demos_raw <- if (use_FL_stratum) {
      length_demos_raw %>% dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
    } else {
      length_demos_raw %>% dplyr::mutate(ANALYSIS_STRATUM = STRAT)
    }

    #### Avg Mort by Site ####
    avgmort_site <- length_demos_raw %>%
      dplyr::group_by(
        SPECIES_NAME, SPECIES_CD, REGION, YEAR, PRIMARY_SAMPLE_UNIT,
        STRAT, PROT, ANALYSIS_STRATUM, bin_num, bin_name
      ) %>%
      dplyr::summarize(
        avsitemort_old = mean(OLD_MORT / 100, na.rm = TRUE),
        avsitemort_rec = mean(RECENT_MORT / 100, na.rm = TRUE),
        .groups = "drop"
      )


    strat_mort <- avgmort_site %>%
      dplyr::mutate(
        ANALYSIS_STRATUM = if (!use_FL_stratum) STRAT else ANALYSIS_STRATUM,
        PROT = as.factor(PROT)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
      dplyr::summarize(
        avmort_old = mean(avsitemort_old, na.rm = TRUE),
        avmort_rec = mean(avsitemort_rec, na.rm = TRUE),
        svar_old = var(avsitemort_old, na.rm = TRUE),
        svar_rec = var(avsitemort_rec, na.rm = TRUE),
        n = dplyr::n_distinct(PRIMARY_SAMPLE_UNIT),
        .groups = "keep"
      ) %>%
      dplyr::mutate(svar_old = dplyr::case_when(svar_old == 0 ~ 0.00000001,
                                                TRUE ~ svar_old),
                    svar_rec = dplyr::case_when(svar_rec == 0 ~ 0.00000001,
                                                TRUE ~ svar_rec)) %>%
      dplyr::mutate(Var_old=svar_old/n, #variance of mean density in stratum
                    std_old = sqrt(svar_old), # std dev of density in stratum
                    SE_old=sqrt(Var_old), #SE of the mean density in stratum
                    CV_perc_old=(SE_old/avmort_old)*100,
                    Var_rec=svar_rec/n, #variance of mean density in stratum
                    std_rec = sqrt(svar_rec), # std dev of density in stratum
                    SE_rec=sqrt(Var_rec), #SE of the mean density in stratum
                    CV_perc_rec=(SE_rec/avmort_rec)*100)


    #### NTOT ####
    ntot <- load_NTOT(region = region, inputdata = demos, project = project) %>%
      dplyr::mutate(YEAR = as.factor(YEAR)) %>%
      dplyr::filter(YEAR %in% years) %>%
      dplyr::ungroup()

    # if its a flordia region make sure to add analysis strat to the ntot data
    if (!use_FL_stratum) {
      ntot <- ntot %>%
        dplyr::mutate(
          ANALYSIS_STRATUM = if (region == "FGB" & 2024 %in% years) ANALYSIS_STRATUM else STRAT
        )
    }


  #### Made Weighted Lenght Est ####
  length_estimates_wh <- length_estimates  %>%
    # Merge ntot with coral_est_spp
    dplyr::full_join(ntot) %>%
    # stratum estimates
    dplyr::mutate(whavtally = wh * avtally,
                  whsvar = wh^2 * svar,
                  whstd = wh * std,
                  n_sites = tidyr::replace_na(n_sites, 0))  %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(SPECIES_NAME))



  # For relative length frequencies, we need to re-weight ntot's because species aren't present in every strat
  # Strata are here re-weighted for each species,
  # based on the strata they are present in only
  length_freq_estimates_wh <- length_freq_estimates %>%
    # merge ntot with relative length frequency
    dplyr::full_join(ntot)


  ##### calculate NTOTs for each species, based on only strata they're present in ####
  ntot_spp <- length_freq_estimates_wh %>%
    dplyr::ungroup() %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD, PROT, NTOT, ngrtot, wh) %>%
    dplyr::distinct()  %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, SPECIES_CD) %>%
    dplyr::summarize(ngrtot_spp = sum(NTOT, na.rm = TRUE))


  # add new ntots, specific to species, to length frequency estimates and re weight
  length_freq_estimates <- length_freq_estimates_wh %>%
    dplyr::full_join(ntot_spp) %>%
    # calculate new species specific weights
    dplyr::mutate(wh_new = NTOT/ngrtot_spp) %>%
    # stratum estimates
    dplyr::mutate(wh_length_freq = wh_new * length_freq)



  #### species and size bin specific NTOT (for mortality estimates) ####
  ntot_spp_bin <- strat_mort %>%
    # bring in the new ntot
    dplyr::full_join(ntot)%>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD, bin_num, bin_name, NTOT, ngrtot, wh) %>%
    dplyr::distinct() %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarize(ngrtot_spp = sum(NTOT))


  ####Strat Mort####
  strat_mort_wh_spp <- strat_mort %>%
    # bring in the new ntot
    dplyr::full_join(ntot) %>%
    dplyr::full_join(., ntot_spp_bin, by = c("REGION", "YEAR", "SPECIES_NAME", "SPECIES_CD", "bin_num", "bin_name")) %>%
    dplyr::mutate(wh_new = NTOT/ngrtot_spp) %>%
    # stratum estimates
    dplyr::mutate(whavmort_old = wh_new * avmort_old,
                  whavmort_rec = wh_new * avmort_rec,
                  whvar_old = wh^2 * Var_old,
                  whvar_rec = wh^2 * Var_rec)



  ####Length Domain####
  length_domain_est <- length_estimates_wh %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarise(avtally = sum(whavtally, na.rm = T), # This accounts for strata with 0 species of interest present
                     Var_tally = sum(whsvar, na.rm = T),
                     SE_tally=sqrt(Var_tally),
                     n_sites = sum(n_sites),
                     n_strat = length(unique(ANALYSIS_STRATUM)),
                     ngrtot = sum(NTOT, na.rm = TRUE),
                     .groups = "keep")  %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SPECIES_CD, bin_num, YEAR)




  ##### Relative Length Freq Domain####
  length_freq_domain_est <- length_freq_estimates %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarize(length_freq_domain = sum(wh_length_freq, na.rm = T),
                     n_strat = length(unique(ANALYSIS_STRATUM))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SPECIES_CD, bin_num, YEAR)



  #### Mortality by bin Domain####
  domain_mort_spp <- strat_mort_wh_spp %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarize(oldmort_domain = sum(whavmort_old, na.rm = T),
                     recmort_domain = sum(whavmort_rec, na.rm = T),
                     n_strat = length(unique(ANALYSIS_STRATUM)))


  output <- list(
    length_demos = as.data.frame(length_demos),
    length_estimates = as.data.frame(length_estimates),
    length_domain_est = as.data.frame(length_domain_est),
    length_freq_estimates = as.data.frame(length_freq_estimates),
    length_freq_domain_est = as.data.frame(length_freq_domain_est),
    domain_mort_spp = as.data.frame(domain_mort_spp),
    strat_mort = as.data.frame(strat_mort),
    demos = demos)

  return(output)
}


