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

# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Davis, Groves, Viehman
# Last update: Jan 2023


##############################################################################################################################

#' Outputs the strata and domain estimates for 3D surface area and length
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param region A string indicating the region
#' @param years A string indicating the two years to compare. Must be NCRMP sampling years.
#' @param size_bin_count A number indicating the desired number of bins for 3d surface area
#' @param length_bin_count A number indicating the desired number of bins for length
#' @param species_filter A string indicating whether to filter to a subset of species
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


#Inputs: Region, Project (only NCRMP at this time), the years selected,
## and inputs for the 3D surface area bin count and Length bin count
NCRMP_make_size_bins <- function(region, project = "NCRMP", years,
                                 size_bin_count = 10, length_bin_count = 10,
                                 species_filter = NULL) {

  analyzed_species <- c(
    "ACR CERV", #A. cervicornis
    "ACR PALM", #A. palmata
    "ORB ANNU", #O. annularis
    "ORB FRAN", #O. franksi
    "ORB FAVE", #O. faveolata
    "MEA MEAN", #M. meandrites
    "DEN CYLI", #D. cylindrus
    "PSE STRI", #A. cervicornis
    "DIP LABI", #D. labyrinthiformis
    "COL NATA", #C. natans
    "SID SIDE", #S. siderea
    "POR ASTE", #P. astreoides
    "MON CAVE", #M. cavernosa
    "AGA AGAR", #A. agaricites
    "MAD AURE", #M. auretenra
    "STE INTE" #S. inercepta
  )



  #p - a constant for 3d surface area calculation
  p = 1.6

  #pull the demo data using the NCRMP function
  #outputs a list of two dfs: dat_1stage and dat_2stage
  demos <- load_NCRMP_DRM_demo_data(project = project, region = region)

  if (region == "SEFCRI" ) {
    #SEFCRI dat_1stage has one extra column for MEAN_RUG that is not needed
    demos <- dplyr::bind_rows(demos$dat_1stage, demos$dat_2stage) %>%
      #filter by year
      dplyr::filter(YEAR %in% years, !is.na(SPECIES_NAME))
  }

  if (region == "Tortugas") {
    #Tortugas requires dat_1stage and dat_2stage to be combined
    demos <- dplyr::bind_rows(demos$dat_1stage, demos$dat_2stage) %>%
      #filter by year
      dplyr::filter(YEAR %in% years)
  }

  if (region %in% c("FLK", "PRICO", "STTSTJ", "STX", "GOM")) {
    #These regions only have dat_1stage needed
    demos <- demos$dat_1stage %>%
      dplyr::filter(YEAR %in% years)
  }




  if (!is.null(species_filter)) {

    demos <- demos %>%
      dplyr::filter(SPECIES_CD %in% analyzed_species)
  }

  #3D Surface Area Calculation
  size_3d_demos <- demos %>%
    #calculate the 3d surface area
    dplyr::mutate(size_3d =
                    (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) +
                               ((MAX_DIAMETER/2)*(HEIGHT/2)) +
                               ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2) -
                    ((4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) +
                                ((MAX_DIAMETER/2)*(HEIGHT/2)) +
                                ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)*
                       (OLD_MORT+RECENT_MORT)/100),
                  YEAR = as.factor(as.character(YEAR))) %>%
    #Filter out where 3d surface area cannot be calculated
    dplyr::filter(!is.na(MAX_DIAMETER), !is.na(STRAT), !is.na(PERP_DIAMETER),
                  !is.na(size_3d)) %>%
    #Calculate the Ranges and the Bin Width by...
    #...grouping by the species (all years combined),
    dplyr::group_by(SPECIES_NAME) %>%
    #...calculating the number of bins as the lesser of...
    #... the user input size bin count OR
    #... the calculation from Rice's rule i.e 2 * cube root(# of obs)
    dplyr::mutate(n_bins = min(size_bin_count,floor((dplyr::n() ^(1/3)) * 2)),
                  #calculate max and min of size
                  max = max(size_3d),
                  min = min(size_3d),
                  #calculate bin_width = the bin range divided by n_bins
                  bin_width =
                    (max - min)/n_bins) %>%
    #ungroup
    dplyr::ungroup()%>%
    #mutate to calculate what bin each observation would fall under
    dplyr::mutate(
      bin_num = dplyr::if_else(
        #if size = max size...
        size_3d == max,
        #...then the bin number is equal to the number of bins
        n_bins,
        #otherwise it is equal to...
        #the difference between the size and the min size...
        #divided by the bin width...
        #rounded down to the nearest integer plus 1
        floor((size_3d - min)/bin_width) + 1),
      bin_name = paste(
        round(min + (bin_width * (bin_num-1)),2),
        "-",
        round(bin_width +
                min + (bin_width * (bin_num-1)),2)
      )
    ) %>%
    #summarize findings by bin count
    dplyr::group_by(SPECIES_NAME, SPECIES_CD, REGION, YEAR, PRIMARY_SAMPLE_UNIT,
                    STRAT, PROT, bin_num, bin_name, n_bins, bin_width, min) %>%
    dplyr::summarise(bin_tally = dplyr::n(), .groups = "keep") %>%
    dplyr::arrange(SPECIES_CD, YEAR, PRIMARY_SAMPLE_UNIT, STRAT, PROT,
                   bin_num)


  #Length Calculation
  length_demos <- demos %>%
    #Year as factor (no calc needed as length = MAX_DIAMETER)
    dplyr::mutate(YEAR = as.factor(as.character(YEAR))) %>%
    #Filter out where MAX_DIAMETER does not exist
    #and where STRAT does not exist
    dplyr::filter(!is.na(MAX_DIAMETER), !is.na(STRAT), MAX_DIAMETER >= 4) %>%
    #Calculate the Ranges and the Bin Width by...
    #...grouping by the species (all years combined),
    dplyr::group_by(SPECIES_NAME) %>%
    #...calculating the number of bins as the lesser of...
    #... the user input length bin count OR
    #... the calculation from Rice's rule i.e 2 * cube root(# of obs)
    dplyr::mutate(n_bins = min(length_bin_count,floor((dplyr::n() ^(1/3)) * 2)),
                  #calculate max and min of length
                  max = max(MAX_DIAMETER),
                  min = 4,
                  #calculate bin_width = the bin range divided by n_bins
                  bin_width =
                    (max - min)/n_bins,
                  # Convert bin width to muliples of 5 or 10
                  bin_width = dplyr::case_when(bin_width <= 5 ~ 5,
                                               bin_width > 5 && bin_width < 11 ~ 10,
                                               bin_width >= 11 ~ 20)) %>%
    #ungroup
    dplyr::ungroup()%>%
    #mutate to calculate what bin each observation would fall under
    dplyr::mutate(
      bin_num = dplyr::if_else(
        #if length = max length...
        MAX_DIAMETER == max,
        #...then the bin number is equal to the number of bins
        n_bins,
        #otherwise it is equal to...
        #the difference between the size and the min size...
        #divided by the bin width...
        #rounded down to the nearest integer plus 1
        floor((MAX_DIAMETER - min)/bin_width) + 1),
      bin_low = dplyr::if_else(
        bin_num == 1, 4, round(min + (bin_width * (bin_num-1))+2,2)),

      bin_high = round(bin_width +min + (bin_width * (bin_num-1))+1,2),
      bin_name = paste(bin_low, bin_high, sep = "-")) %>%
    #summarize findings by bin count
    dplyr::group_by(SPECIES_NAME, SPECIES_CD, REGION, YEAR, PRIMARY_SAMPLE_UNIT,
                    STRAT, PROT, bin_num, bin_name, n_bins, bin_width, min) %>%
    dplyr::summarise(bin_tally = dplyr::n(), .groups = "keep") %>%
    dplyr::arrange(SPECIES_NAME, YEAR, PRIMARY_SAMPLE_UNIT, STRAT, PROT,
                   bin_num)

  #CALCULATE ESTIMATES

  if (region %in% c("FLK", "SEFCRI", "Tortugas")) {

  #Estimates for 3D Surface Area
  size_estimates <- size_3d_demos %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT,
                                           sep = " ")) %>%
    dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME,
                    SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarise(# compute average bin_tally
      avtally = mean(bin_tally),
      # compute stratum variance
      svar = var(bin_tally),
      # calculate N
      n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
      .groups = "keep") %>%
    # convert 0 for stratum variance so that the sqrt is a small # but not a 0
    dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                          TRUE ~ svar)) %>%
    dplyr::mutate(Var=svar/n_sites, #variance of mean bin_tally in stratum
                  std = sqrt(svar), # std dev of bin_tally in stratum
                  SE=sqrt(Var), #SE of the mean bin_tally stratum
                  CV_perc=(SE/avtally)*100)

  #Estimates for Length
  length_estimates <- length_demos %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT,
                                           sep = " ")) %>%
    dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD,
                    bin_num, bin_name) %>%
    dplyr::summarise(# compute average bin_tally
      avtally = mean(bin_tally),
      # compute stratum variance
      svar = var(bin_tally),
      # calculate N
      n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
      .groups = "keep") %>%
    # convert 0 for stratum variance so that the sqrt is a small # but not a 0
    dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                          TRUE ~ svar)) %>%
    dplyr::mutate(Var=svar/n_sites, #variance of mean bin_tally in stratum
                  std = sqrt(svar), # std dev of bin_tally in stratum
                  SE=sqrt(Var), #SE of the mean bin_tally stratum
                  CV_perc=(SE/avtally)*100)


  # Estimates for RELATIVE Length Frequency (ADDED OCT 2022 by BW)
  # first sum up the total number of corals of each species, by strat
  tot_corals <- length_demos %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT,
                                           sep = " ")) %>%
    dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD) %>%
    dplyr::summarize(tot_corals = sum(bin_tally))

  length_freq_estimates <- length_demos %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT,
                                           sep = " ")) %>%
    dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD,
                    bin_num, bin_name) %>%
    # sum up the number of corals in each size bin in each strat
    dplyr::summarize(n_corals = sum(bin_tally)) %>%
    dplyr::ungroup() %>%
    # add total number of corals in each strat
    dplyr::left_join(., tot_corals, by = c("REGION", "YEAR", "ANALYSIS_STRATUM", "SPECIES_NAME", "SPECIES_CD")) %>%
    # calculate relative frequency (proportion) of corals in each size bin
    dplyr::mutate(length_freq = n_corals/tot_corals)

    ntot <- ncrmp.benthics.analysis::load_NTOT(region = region, inputdata = demos,
                                               project = project) %>%
      dplyr::mutate(YEAR = as.factor(YEAR)) %>%
      dplyr::filter(YEAR %in% years) %>%
      dplyr::ungroup()
  }

  if (region %in% c("PRICO", "STTSTJ", "STX", "GOM")) {


    #Estimates for 3D Surface Area
    size_estimates <- size_3d_demos %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME,
                      SPECIES_CD, bin_num, bin_name) %>%
      dplyr::summarise(# compute average bin_tally
        avtally = mean(bin_tally),
        # compute stratum variance
        svar = var(bin_tally),
        # calculate N
        n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
        .groups = "keep") %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n_sites, #variance of mean bin_tally in stratum
                    std = sqrt(svar), # std dev of bin_tally in stratum
                    SE=sqrt(Var), #SE of the mean bin_tally stratum
                    CV_perc=(SE/avtally)*100)

    #Estimates for Length
    length_estimates <- length_demos %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD,
                      bin_num, bin_name) %>%
      dplyr::summarise(# compute average bin_tally
        avtally = mean(bin_tally),
        # compute stratum variance
        svar = var(bin_tally),
        # calculate N
        n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
        .groups = "keep") %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n_sites, #variance of mean bin_tally in stratum
                    std = sqrt(svar), # std dev of bin_tally in stratum
                    SE=sqrt(Var), #SE of the mean bin_tally stratum
                    CV_perc=(SE/avtally)*100)


    # Estimates for RELATIVE Length Frequency (ADDED OCT 2022 by BW)
    # first sum up the total number of corals of each species, by strat
    tot_corals <- length_demos %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD) %>%
      dplyr::summarize(tot_corals = sum(bin_tally))

    length_freq_estimates <- length_demos %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD,
                      bin_num, bin_name) %>%
      # sum up the number of corals in each size bin in each strat
      dplyr::summarize(n_corals = sum(bin_tally)) %>%
      dplyr::ungroup() %>%
      # add total number of corals in each strat
      dplyr::left_join(., tot_corals, by = c("REGION", "YEAR", "ANALYSIS_STRATUM", "SPECIES_NAME", "SPECIES_CD")) %>%
      # calculate relative frequency (proportion) of corals in each size bin
      dplyr::mutate(length_freq = n_corals/tot_corals)
    #inputdata is dummy, unused if project != DRM
    ntot <- ncrmp.benthics.analysis::load_NTOT(region = region, inputdata = demos,
                                               project = project) %>%
      dplyr::mutate(YEAR = as.factor(YEAR)) %>%
      dplyr::filter(YEAR %in% years) %>%
      dplyr::ungroup()
  }


  # STRATUM SIZE ESTIMATES

  size_estimates_wh <- size_estimates  %>%
    # Merge ntot with coral_est_spp
    dplyr::full_join(ntot) %>%
    # stratum estimates
    dplyr::mutate(whavtally = wh * avtally,
                  whsvar = wh^2 * svar,
                  whstd = wh * std,
                  n_sites = tidyr::replace_na(n_sites, 0))  %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(SPECIES_NAME))

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

  # calculate NTOTs for each species, based on only strata they're present in
  ntot_spp <- length_freq_estimates_wh %>%
    dplyr::ungroup() %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD, PROT, NTOT, ngrtot, wh) %>%
    dplyr::distinct() %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, SPECIES_CD) %>%
    dplyr::summarize(ngrtot_spp = sum(NTOT))

  # add new ntots, specific to species, to length frequency estimates and re weight
  length_freq_estimates <- length_freq_estimates_wh %>%
    dplyr::full_join(ntot_spp, by = c("REGION", "YEAR", "SPECIES_NAME", "SPECIES_CD")) %>%
    # calculate new species specific weights
    dplyr::mutate(wh_new = NTOT/ngrtot_spp) %>%
    # stratum estimates
    dplyr::mutate(wh_length_freq = wh_new * length_freq)

  # Run checks
  # Check 1 - Sum of length frequencies in a strat for a species should be 1
   # check <- length_freq_estimates %>%
   # dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_NAME, SPECIES_CD) %>%
   # dplyr::summarize(sum_freq = sum(length_freq))
  # Check 2 - sum of weighted frequencies of a species should equal 1
   # check <- length_freq_estimates %>%
   #  dplyr::group_by(YEAR, SPECIES_NAME, SPECIES_CD) %>%
   #  dplyr::summarize(sum_wtfreq = sum(wh_length_freq))


  #End result: Return the list made up of strata est. and domain est.

  # Domain Estimates
  ##Size Domain
  size_domain_est <- size_estimates_wh %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarise(avtally = sum(whavtally, na.rm = T), # This accounts for strata with 0 species of interest present
                     Var_tally = sum(whsvar, na.rm = T),
                     SE_tally=sqrt(Var_tally),
                     n_sites = sum(n_sites),
                     n_strat = length(unique(ANALYSIS_STRATUM)),
                     ngrtot = sum(NTOT),
                     .groups = "keep")  %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SPECIES_CD, bin_num, YEAR)


  ##Length Domain
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


  ## Relative Length Freq Domain
  length_freq_domain_est <- length_freq_estimates %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarize(length_freq_domain = sum(wh_length_freq, na.rm = T),
                     n_strat = length(unique(ANALYSIS_STRATUM))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SPECIES_CD, bin_num, YEAR)



  output <- list(size_3d_demos = as.data.frame(size_3d_demos),
                 length_demos = as.data.frame(length_demos),
                 size_estimates = as.data.frame(size_estimates),
                 length_estimates = as.data.frame(length_estimates),
                 size_domain_est = as.data.frame(size_domain_est),
                 length_domain_est = as.data.frame(length_domain_est),
                 length_freq_estimates = as.data.frame(length_freq_estimates),
                 length_freq_domain_est = as.data.frame(length_freq_domain_est))

  return(output)
}

