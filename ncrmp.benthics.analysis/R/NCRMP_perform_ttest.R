
## Function to perform pair-wise, two-tailed t-tests on NCRMP domain estimates by year

# Purpose:
# create output with t-test results

## Tag: data analysis


# outputs created in this file --------------
#


# CallS:
# Subjurisdiction domain estimates

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams, Krampitz, Sturm
# Last update: Sept 2024


##############################################################################################################################

#' Performs t-test comparing estimates
#'
#' Function to perform t-test on various summaries of NCRMP data.
#' Typically, comparisons are made between yearly domain estimates (e.g. mean
#' coral cover by year) in a single region. But comparisons could be made between
#' inside/outside a protected area.
#'
#' @param dataframe A dataframe of yearly domain estimates for a single subjurisdiction.
#' @param metric1 The metric you are testing: density, hard coral cover, macroalgae cover, disease prevalence, bleaching prevalence.
#' @param metric2 The column name of the metric you are testing: avCvr, avDen, etc.
#' @param alpha The alpha level you want to test, ex. 0.05
#' @param test_type You can compare domain estimates between years or inside/ outside a protected area for a single year, enter "Years" or "PROT"
#' @param n_years You must specify the number of sampling years completed in the subjurisdiction if test_type = "Years", currently 4 for St. Thomas & St. John and 3 for all other regions.
#' @return A dataframe and test results as a list.
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_perform_ttest <- function(dataframe, metric1, metric2, alpha, n_years, return_dataframe) {

  in_range <- function(x, lower, upper) {
    range = c(lower, upper)
    stopifnot(length(range) == 2L)
    range[1] < x & x < range[2]
  }

  # Filter data based on cover_group
  if (metric1 == "coral_cover") {
    d <- dataframe %>% dplyr::filter(cover_group == "HARD CORALS")
  } else if (metric1 == "macroalgae_cover") {
    d <- dataframe %>% dplyr::filter(cover_group == "MACROALGAE")
  } else {
    d <- dataframe
  }

  # Check columns
  must_have <- c(metric2, "Var", "n_sites", "n_strat")
  for (i in must_have) {
    if (!i %in% colnames(d)) {
      stop(paste("Column", i, "not in dataframe"))
    }
  }

  # Compute statistics
  d$df       <- d$n_sites - d$n_strat
  d$t_value  <- abs(qt(alpha / 2, d$df))
  d$LCI      <- d[[metric2]] - (sqrt(d$Var) * d$t_value)
  d$UCI      <- d[[metric2]] + (sqrt(d$Var) * d$t_value)

  # Check if all years are included
  all_years <- unique(d$YEAR)
  if (length(all_years) < 2) {
    stop("Not enough years to perform t-tests")
  }

  # Perform pairwise comparisons
  comparisons <- combn(all_years, 2, simplify = FALSE)
  results <- list()

  for (comp in comparisons) {
    year1 <- comp[1]
    year2 <- comp[2]

    row1 <- d %>% dplyr::filter(YEAR == year1)
    row2 <- d %>% dplyr::filter(YEAR == year2)

    if (nrow(row1) == 0 || nrow(row2) == 0) {
      next
    }

    is_not_significant <- in_range(row1[[metric2]], row2$LCI, row2$UCI) && in_range(row2[[metric2]], row1$LCI, row1$UCI)

    result <- if (is_not_significant) {
      paste(metric2, year1, year2, "NOT significant at alpha =", alpha)
    } else {
      paste(metric2, year1, year2, "Significantly different p <", alpha)
    }

    results <- c(results, result)
  }

  if (return_dataframe) {
    return(d)
  } else {
    print(results)
  }
}

#Old version I could not get to work for 2023 data
# NCRMP_perform_ttest <- function(dataframe, metric1, metric2, alpha, test_type, n_years = "NULL", return_dataframe){
#
#
#   in_range <- function(x, lower, upper){
#     range = c(lower, upper)
#     stopifnot(length(range) == 2L)
#     range[1] < x & x < range[2]
#   }
#
#
#
#   # Function is expecting the following columns in the dataframe
#   must_have <- c(metric2, "Var", "n_sites", "n_strat")
#
#
#
#   if(metric1 == "coral_cover"){
#
#     d <- dataframe %>% dplyr::filter(cover_group == "HARD CORALS")
#
#   }
#
#   if(metric1 == "macroalgae_cover"){
#
#     d <- dataframe %>% dplyr::filter(cover_group == "MACROALGAE")
#
#   }
#
#   if(metric1 == "density"){
#
#     d <- dataframe
#
#   }
#
#   if(metric1 == "old_mortality"){
#
#     d <- dataframe
#
#   }
#
#   if(metric1 == "recent_mortality"){
#
#     d <- dataframe
#
#   }
#
#
#   if(metric1 == "colony_size"){
#
#     d <- dataframe %>% dplyr::select(-avCm2, -Var_cm2, -SE_cm2) %>%
#       dplyr::mutate(Var = Var_cm3)
#
#   }
#
#   # Check that dataframe includes must_have columns, stop with error if not
#   for (i in must_have) {
#     if (!i %in% colnames(d)) {
#       stop(paste("Column", i, "not in dataframe"))
#     }
#   }
#
#
#   # Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
#   d$df       <- d$n_sites - d$n_strat
#   d$t_value  <- abs(qt(alpha/2, d$df))
#   d$LCI      <- unlist(d[metric2]) - (sqrt(d$Var) * d$t_value)
#   d$UCI      <- unlist(d[metric2]) + (sqrt(d$Var) * d$t_value)
#
#
#   if(test_type == "PROT"){
#
#     t1 <- c(in_range(x = d[1,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI),
#             in_range(x = d[2,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#
#     return_test <- function(){
#       if(any(t1==T)) {
#         print(paste(metric2, "PROT", d[1,3], "vs.", d[2,3], "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, "PROT", d[1,3], "vs.", d[2,3], "Significantly different p <", alpha))
#       }
#     }
#
#   }
#
#   if(n_years > 4) {
#
#     # Check if metric_1 is within the confidence interval of metric_2 (return True/False) and
#     #       if metric_2 is within the confidence interval of metric_1 (return True/False)
#     t1 <- c(in_range(x = d[1,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI), in_range(x = d[2,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#     t2 <- c(in_range(x = d[1,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI), in_range(x = d[3,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#     t3 <- c(in_range(x = d[1,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#     t4 <- c(in_range(x = d[1,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI), in_range(x = d[5,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#     t5 <- c(in_range(x = d[1,][metric2], lower = d[6,]$LCI, upper = d[6,]$UCI), in_range(x = d[6,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#
#     t6 <- c(in_range(x = d[2,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI), in_range(x = d[3,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
#     t7 <- c(in_range(x = d[2,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
#     t8 <- c(in_range(x = d[2,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI), in_range(x = d[5,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
#     t9 <- c(in_range(x = d[2,][metric2], lower = d[6,]$LCI, upper = d[6,]$UCI), in_range(x = d[6,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
#
#     t10 <- c(in_range(x = d[3,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI))
#     t11<- c(in_range(x = d[3,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI), in_range(x = d[5,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI))
#     t12 <- c(in_range(x = d[3,][metric2], lower = d[6,]$LCI, upper = d[6,]$UCI), in_range(x = d[6,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI))
#
#     t13 <- c(in_range(x = d[4,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI), in_range(x = d[5,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI))
#     t14 <- c(in_range(x = d[4,][metric2], lower = d[6,]$LCI, upper = d[6,]$UCI), in_range(x = d[6,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI))
#
#     t15 <- c(in_range(x = d[5,][metric2], lower = d[6,]$LCI, upper = d[6,]$UCI), in_range(x = d[6,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI))
#
#     return_test <- function(){
#       if(any(t1==T)) {
#         print(paste(metric2, d[1,2], d[2,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[1,2], d[2,2], "Significantly different p <", alpha))
#       }
#       if(any(t2==T)) {
#         print(paste(metric2, d[1,2], d[3,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[1,2], d[3,2], "Significantly different p <", alpha))
#       }
#       if(any(t3==T)) {
#         print(paste(metric2, d[1,2], d[4,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[1,2], d[4,2], "Significantly different p <", alpha))
#       }
#       if(any(t4==T)) {
#         print(paste(metric2, d[1,2], d[5,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[1,2], d[5,2], "Significantly different p <", alpha))
#       }
#       if(any(t5==T)) {
#         print(paste(metric2, d[1,2], d[6,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[1,2], d[6,2], "Significantly different p <", alpha))
#       }
#
#
#       if(any(t6==T)) {
#         print(paste(metric2, d[2,2], d[3,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[2,2], d[3,2], "Significantly different p <", alpha))
#       }
#       if(any(t7==T)) {
#         print(paste(metric2, d[2,2], d[4,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[2,2], d[4,2], "Significantly different p <", alpha))
#       }
#       if(any(t8==T)) {
#         print(paste(metric2, d[2,2], d[5,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[2,2], d[5,2], "Significantly different p <", alpha))
#       }
#       if(any(t9==T)) {
#         print(paste(metric2, d[2,2], d[6,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[2,2], d[6,2], "Significantly different p <", alpha))
#       }
#
#       if(any(t10==T)) {
#         print(paste(metric2, d[3,2], d[4,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[3,2], d[4,2], "Significantly different p <", alpha))
#       }
#       if(any(t11==T)) {
#         print(paste(metric2, d[3,2], d[5,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[3,2], d[5,2], "Significantly different p <", alpha))
#       }
#       if(any(t12==T)) {
#         print(paste(metric2, d[3,2], d[6,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[3,2], d[6,2], "Significantly different p <", alpha))
#       }
#
#       if(any(t13==T)) {
#         print(paste(metric2, d[4,2], d[5,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[4,2], d[5,2], "Significantly different p <", alpha))
#       }
#       if(any(t14==T)) {
#         print(paste(metric2, d[4,2], d[6,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[4,2], d[6,2], "Significantly different p <", alpha))
#       }
#
#       if(any(t15==T)) {
#         print(paste(metric2, d[5,2], d[6,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[5,2], d[6,2], "Significantly different p <", alpha))
#       }
#     }
#
#   }else {
#
#     # Check if metric_1 is within the confidence interval of metric_2 (return True/False) and
#     #       if metric_2 is within the confidence interval of metric_1 (return True/False)
#     t1 <- c(in_range(x = d[1,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI), in_range(x = d[2,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#     t2 <- c(in_range(x = d[1,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI), in_range(x = d[3,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#     t3 <- c(in_range(x = d[1,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
#
#     t4 <- c(in_range(x = d[2,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI), in_range(x = d[3,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
#     t5 <- c(in_range(x = d[2,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
#
#     t6 <- c(in_range(x = d[3,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI))
#
#     return_test <- function(){
#       if(any(t1==T)) {
#         print(paste(metric2, d[1,2], d[2,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[1,2], d[2,2], "Significantly different p <", alpha))
#       }
#       if(any(t2==T)) {
#         print(paste(metric2, d[1,2], d[3,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[1,2], d[3,2], "Significantly different p <", alpha))
#       }
#       if(any(t3==T)) {
#         print(paste(metric2, d[1,2], d[4,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[1,2], d[4,2], "Significantly different p <", alpha))
#       }
#       if(any(t4==T)) {
#         print(paste(metric2, d[2,2], d[3,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[2,2], d[3,2], "Significantly different p <", alpha))
#       }
#       if(any(t5==T)) {
#         print(paste(metric2, d[2,2], d[4,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[2,2], d[4,2], "Significantly different p <", alpha))
#       }
#       if(any(t6==T)) {
#         print(paste(metric2, d[3,2], d[4,2],  "NOT significant at alpha = ", alpha))
#       } else {
#         print(paste(metric2, d[3,2], d[4,2], "Significantly different p <", alpha))
#       }
#
#
#     }
#
#   }
#
#
#   # Return dataframe (d) if return_datafram = T, otherwise return test output
#   ifelse(return_dataframe == FALSE ||
#            return_dataframe == "NULL", return_test(), return(d))
# }
