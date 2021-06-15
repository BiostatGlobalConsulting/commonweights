#' Common Weight Output
#'
#' Analyze multiple survey datasets: calculate changes in state weights over time, post-stratify to specified year(s), produce tables and plots
#' @param data Dataset to analyze. Should have one row per respondent and contain survey data from at least two years.
#' @param outcomevars Names of dichotomous outcome variable(s) coded 0/1/NA. Provide as a single variable name e.g. "dpt3_vx" or as multiple variables in a concatenated list, e.g. c("dpt3_vx", "mcv1_vx")
#' @param outcomenames Names of the outcomes in \code{outcomevars} to use in tables and plots; must be the same length as \code{outcomevars}. Optional, defaults to NULL. E.g. c("DPT3", "MCV1")
#' @param geovar Name of geography variable in \code{data}. Must have the same levels across all years in \code{years}.
#' @param yearvar Variable in dataset indicating survey year
#' @param weightvar Variable in dataset containing survey weights for each respondent
#' @param years Data years to analyze. Defaults to "all", which will analyze all years in \code{yearvar}. To analyze a subset of years, provide a vector of years to include, e.g. c(2000, 2005, 2010)
#' @param weightopt Which year(s) to use as common weights. "earliest" will calculate outcomes post-stratified to the earliest year in \code{years}. "previous" will calculate outcomes for each survey using the weights from the previous survey year. "custom" allows the user to select a year to use weights from.
#' @param weightyear Year to use for post-stratifying if \code{weightopt = "custom"}
#' @param age Age to filter by. Defaults to NULL. Provide as a single value of \code{agevar} to keep or provide as a pair of values for minimum and maximum ages, e.g. \code{age = c(12, 23)} will keep \code{agevar >= 12} and \code{<= 23}.
#' @param agevar Age variable in dataset; used if \code{age} is not NULL. Defaults to NA.
#' @param ci a logical value indicating whether confidence intervals should be calculated for outcomes.  Defaults to FALSE
#' @param cilevel Defaults to 0.95
#' @param clustvar Variable in the dataset identifying which cluster each respondent belongs to. Used in confidence interval calculations to take survey design into account. Provide if \code{ci = TRUE}. Defaults to NULL.
#' @param stratvar Variable in the dataset identifying which stratum each respondent belongs to. Used in confidence interval calculations to take survey design into account. Provide if \code{ci = TRUE}. Defaults to NULL.
#' @param countryname Character, name of the country (or other top-level grouping) that \code{data} is from
#' @param statelabels Character vector of labels corresponding to the levels of \code{geovar}. Defaults to NULL
#' @param palette Color palette for the levels of \code{geovar} in output plots. Defaults to NULL, in which case a default ggplot2 palette will be used.
#' @keywords survey
#' @importFrom magrittr %>%
#' @export
#' @examples
#' cwt_output()

cwt_output <- function(
  data,              # Name of the dataset to analyze
  outcomevars,       # dichotomous outcome variable(s)
  outcomenames = NULL, # names for outcomes (same length as outcomevars)
  geovar,            # geographic variable for weight calculation
  yearvar,           # variable storing information on year
  weightvar,         # variable storing weights for each respondent
  years = "all",     # Years to analyze (can be vector in c() or "all" which will loop through unique(data$yearvar))
  weightopt,         # "earliest", "previous", "custom"
  weightyear = NULL, # Year to take weights from if weightopt = "custom"

  # Optional arguments if filtering by age:
  age = NULL,        # Respondent age, for filtering
  agevar = NA,        # variable storing age in years

  # Optional arguments for calculating CI
  ci = FALSE,
  cilevel = 0.95,
  clustvar = NULL,
  stratvar = NULL,

  countryname,

  # For visualizations
  statelabels = NULL, # labels for geovar; use levels in dataset by default
  palette = NULL
){

  # Create a data object inside this function, equal to the dataset provided in the data argument
  analysis_data <- data

  ## Checks ----

  # Check 1: all required variables are present in the dataset
  variable_messages <- NULL

  if(!geovar %in% names(analysis_data)){
    variable_messages <- c(variable_messages,
                           paste0("Cannot find a variable called ", geovar,
                                  " in ", deparse(substitute(data)), ".\n"))
  }

  if(!yearvar %in% names(analysis_data)){
    variable_messages <- c(variable_messages,
                           paste0("Cannot find a variable called ", yearvar,
                                  " in ", deparse(substitute(data)), ".\n"))
  }

  if(!weightvar %in% names(analysis_data)){
    variable_messages <- c(variable_messages,
                           paste0("Cannot find a variable called ", weightvar,
                                  " in ", deparse(substitute(data)), ".\n"))
  }

  if(!is.na(agevar)){
    if(!agevar %in% names(analysis_data)){
      variable_messages <- c(variable_messages,
                             paste0("Cannot find a variable called ", agevar,
                                    " in ", deparse(substitute(data)), ".\n"))
    }}

  for(i in 1:length(outcomevars)){
    if(!outcomevars[i] %in% names(analysis_data)){
      variable_messages <- c(variable_messages,
                             paste0("Cannot find a variable called ", outcomevars[i],
                                    " in ", deparse(substitute(data)), ".\n"))
    }
  }

  if(!is.null(variable_messages)){
    stop(variable_messages)
  }

  ## Check 2: weightopt and weightyear
  if(weightopt == "custom" & is.null(weightyear)){
    stop("weightopt = 'custom' option selected, but no weight year provided.")
  }
  if(!is.null(weightyear) & weightopt != "custom"){
    stop(paste0('To weight to the year ', weightyear, ' you must select weightopt = "custom"'))
  }
  if(!is.null(weightyear)){
    checkyear <- get(yearvar, analysis_data)
    if(length(checkyear[checkyear == weightyear])==0){
      stop(paste0('Cannot calculate weights from year ', weightyear, ' because no data from that year is in ', deparse(substitute(data))))
    }
  }

  ## Check 3: if age is provided, agevar should be
  if(!is.null(age) & is.na(agevar)){
    stop("In order to filter by age, the variable containing age in the dataset must be provided in the agevar argument.")
  }
  if(!is.na(agevar) & is.null(age)){
    warning("An age variable (agevar) was provided, but no age to filter by was given in the age argument.")
  }

  ## Check 4: years
  if(any(years != "all")){ # if(years != "all")
    checkyear <- get(yearvar, analysis_data)
    yearmsgs <- NULL
    for(y in 1:length(years)){
      cy <- length(checkyear[checkyear==years[y]])
      if(cy==0){
        yearmsgs <- c(yearmsgs, paste0("No data from year ", years[y], " is present in ", deparse(substitute(data)), ".\n"))
      }
    }
    if(!is.null(yearmsgs)){stop(yearmsgs)}
  }

  ## Check 5: state labels
  if(!is.null(statelabels)){
    check5 <- analysis_data
    check5$yv <- get(yearvar, check5)

    if(any(years != "all")){
      check5 <- check5 %>%
        dplyr::filter(yv %in% years)}

    checkgeovar <- get(geovar, check5)

    if(length(unique(checkgeovar)) != length(statelabels)){
      stop(paste0("The number of levels in ", geovar, " does not match the number of levels provided in the statelabels argument."))}}

  ## Check 6: confidence interval variables
  if(ci == TRUE){
    ci_message <- NULL

    if(is.null(clustvar)){
      ci_message <- c(ci_message, "Cluster ID variable (clustvar) must be provided to calculate a confidence interval\n")
    }

    if(is.null(stratvar)){
      ci_message <- c(ci_message, "Stratum ID variable (stratvar) must be provided to calculate a confidence interval\n")
    }

    # Stop here if stratvar or clustvar is not specified at all
    if(!is.null(ci_message)){
      stop(ci_message)
    }

    if(!clustvar %in% names(analysis_data)){
      ci_message <- c(ci_message,
                      paste0("Cannot find a variable called ", clustvar, " in ",
                             deparse(substitute(data)),
                             ". Cluster ID variable required to calculate a confidence interval.\n"))
    }

    if(!stratvar %in% names(analysis_data)){
      ci_message <- c(ci_message,
                      paste0("Cannot find a variable called ", stratvar, " in ",
                             deparse(substitute(data)),
                             ". Stratum ID variable required to calculate a confidence interval.\n"))
    }

    # Stop here if stratvar or clustvar is not in the dataset
    if(!is.null(ci_message)){
      stop(ci_message)
    }
  }

  if(ci == TRUE){
    if(cilevel >= 1 | cilevel <= 0){
      stop("Confidence level must be between 0 and 1.")
    }
  }

  ## Check 7: outcome variable labels same length as outcomevars
  if(!is.null(outcomenames)){
    if(length(outcomenames) != length(outcomevars)){
      warning("Outcome name vector is not the same length as the outcome variables vector. Ignoring outcomenames in this run.")
      outcomenames <- NULL
    }
  }

  # If outcomenames not provided or not valid, use outcomevars as names
  if(is.null(outcomenames)){outcomenames <- outcomevars}

  ## Data processing ----

  # Pre-process data for weights
  weights_data <- data
  weights_data$fn_weight <- get(weightvar, weights_data)
  weights_data$State <- get(geovar, weights_data)
  weights_data$fn_year <- get(yearvar, weights_data)

  if(!is.na(agevar)){weights_data$fn_age <- get(agevar, weights_data)}

  # Filter data by age if user provided age/agevar
  if(!is.na(agevar) & !is.null(age)){

    if(length(age) == 1){
      weights_data <- dplyr::filter(weights_data,
                             fn_age %in% age)
    } else if(length(age) == 2){
      agemin <- min(age)
      agemax <- max(age)

      weights_data <- dplyr::filter(weights_data,
                             fn_age <= agemax, fn_age >= agemin)
    } else {
      stop("The age argument can have one or two values")
    }
  }

  # Create new weight, geo, and year variables using the values provided in the weightvar, geovar, and yearvar arguments to the function
  analysis_data$fn_weight <- get(weightvar, analysis_data)
  analysis_data$State <- get(geovar, analysis_data)
  analysis_data$fn_year <- get(yearvar, analysis_data)

  # If age is provided for filtering, get age variable
  if(!is.na(agevar)){analysis_data$fn_age <- get(agevar, analysis_data)}

  # Filter data by age if user provided age/agevar
  if(!is.na(agevar) & !is.null(age)){

    if(length(age) == 1){
      analysis_data <- dplyr::filter(analysis_data,
                                     fn_age %in% age)}

    if(length(age) == 2){
      agemin <- min(age)
      agemax <- max(age)

      analysis_data <- dplyr::filter(analysis_data,
                                     fn_age <= agemax, fn_age >= agemin)
    }
  }

  # Get stratum and cluster ID variables if CI to be calculated
  if(ci == TRUE){
    analysis_data$fn_clust <- get(clustvar, analysis_data)
    analysis_data$fn_strat <- get(stratvar, analysis_data)

  }

  # Save the user's current setting for the survey.lonely.psu option in the survey package, to be restored at the end of the function
  save_user_survey_option <- options("survey.lonely.psu")

  # inside function, set survey design handling of lone PSUs
  options(survey.lonely.psu="adjust")

  # Define years to do calculations for (ytc)
  if(length(years)==1){
    if(years == "all"){
      ytc <- unique(analysis_data$fn_year)
    } else {
      ytc <- years
    }
  } else {
    ytc <- years
  }

  if(weightopt == "earliest"){
    wtc <- rep(dplyr::first(ytc), length(ytc))
  } else if(weightopt == "previous"){
    if(length(ytc > 1)){
      wtc <- c(dplyr::first(ytc), ytc[1:length(ytc)-1])
    } else {
      wtc <- dplyr::first(ytc)
    }
  } else if(weightopt == "custom"){
    if(!is.null(weightyear)){
      wtc <- rep(weightyear, length(ytc))
    } else {
      ## This is redundant; check is done above
      stop("Weight option is 'custom', but weightyear argument was not provided.")
    }
  }

  ## Loop through years
  for(y in 1:length(ytc)){

    y_analysis_data <- dplyr::filter(analysis_data,
                                     fn_year == ytc[y])

    y_weights_data <- dplyr::filter(weights_data,
                                    fn_year == wtc[y])

    target_weights <- y_weights_data %>%
      dplyr::mutate(tot_weight = sum(fn_weight)) %>%
      dplyr::group_by(State) %>%
      dplyr::summarize(
        SUMWT = sum(fn_weight),
        tot_weight = dplyr::first(tot_weight)
      ) %>%
      dplyr::mutate(target = SUMWT/tot_weight) %>%
      dplyr::select(-tot_weight)

    # Create data frame shell to hold outcome output
    y_outcomes_df <- data.frame(
      Country = character(),
      OutcomeVar = character(),
      OutcomeName = character(),
      Outcome_Original = numeric(),
      CI_LB_Original = numeric(),
      CI_UB_Original = numeric(),
      Outcome_PS = numeric(),
      DataYear = numeric(),
      PSWeightYear = numeric(),
      stringsAsFactors = FALSE
    )

    ## Process each provided outcome variable
    for(i in 1:length(outcomevars)){

      loop_data <- y_analysis_data

      # Use outcome variable i
      loop_data$fn_outcome <- get(outcomevars[i], y_analysis_data)

      # Confidence interval: calculations for DF
      if(ci == TRUE){

        ## Are clusters unique across strata or only within strata?
        cs_vector <- paste0(loop_data$fn_strat, loop_data$fn_clust)

        ## If not unique across strata, create a new clustvar
        #  as a combination of stratum and cluster IDs
        if(length(unique(cs_vector)) > length(unique(loop_data$fn_clust))){
          loop_data <- loop_data %>%
            dplyr::mutate(
              orig_clustvar = fn_clust,
              fn_clust = paste0(fn_strat, fn_clust)
            )}

        # Calculate the number of clusters in this stratum
        nclust <- length(unique(loop_data$fn_clust))

        # Number of strata represented in this subset
        nstrat <- length(unique(loop_data$fn_strat))

      }

      # Create copy of loop_data for use in CI calculations
      loop_copy <- loop_data

      loop_data <- loop_data %>%
        dplyr::mutate(numerator = fn_outcome * fn_weight,
                      denominator = fn_weight) %>%
        dplyr::group_by(State) %>%
        dplyr::mutate(sumwt_zone = sum(fn_weight)) %>%
        dplyr::ungroup()

      # Confidence interval - Wilson CI calculations
      if(ci == TRUE){

        # Check: any missing values in strata or cluster?
        if(nrow(loop_data[is.na(loop_data$fn_strat),]) > 0 |
           nrow(loop_data[is.na(loop_data$fn_clust),]) > 0){

          p_lb <- NA
          p_ub <- NA

          warnmsg <- ""

          if(nrow(loop_data[is.na(loop_data$fn_strat),]) > 0){
            warnmsg <- c(warnmsg, paste0("Missing values in ", deparse(substitute(stratvar)), ".\n"))
          }

          if(nrow(loop_data[is.na(loop_data$fn_clust),]) > 0){
            warnmsg <- c(warnmsg, paste0("Missing values in ", deparse(substitute(clustvar)), ".\n"))
          }

          # Only print warning message if i = 1 (don't repeat warning for each outcome)
          if(i==1){
            warning(paste0(warnmsg, "Confidence intervals could not be calculated for year ", ytc[y], ". "))}
        } else {

          # Define a survey design object
          svy_design <- survey::svydesign(
            ids = ~fn_clust,
            weights = ~fn_weight,
            strata = ~fn_strat,
            data = loop_data
          )

          pcalc <- survey::svymean(~fn_outcome,
                                   design = svy_design,
                                   deff = "replace",
                                   df = survey::degf(svy_design),
                                   na.rm = TRUE)

          p_j <- sum(loop_data$numerator, na.rm=TRUE) / sum(loop_data$denominator, na.rm=TRUE) # same as coef(pcalc)

          se_j <- as.numeric(survey::SE(pcalc))

          if(se_j < 1e-15){
            neff_dp <- nrow(loop_data)
          } else {
            neff_dp <- p_j*(1-p_j)/(se_j^2)
          }

          # Wilson CI
          alpha <- 1 - cilevel
          adj_neff_j <- neff_dp * ((qnorm(1-alpha/2)/qt(1-alpha/2, df = nclust-nstrat))^2)

          zquant <- qnorm(p = (1 - alpha/2))

          numerator_lh <- p_j + zquant^2/(2*adj_neff_j)
          numerator_rh <- zquant * sqrt((p_j * (1-p_j))/adj_neff_j + zquant^2/((2*adj_neff_j)^2))
          denominator <- 1 + zquant^2/adj_neff_j

          p_lb <- ((numerator_lh - numerator_rh)/denominator) * 100
          p_ub <- ((numerator_lh + numerator_rh)/denominator) * 100}

      } else {
        p_lb <- NA
        p_ub <- NA
      }

      loop_data <- dplyr::left_join(loop_data, target_weights, by = "State") %>%
        dplyr::mutate(poststrwtzone = target * fn_weight/sumwt_zone,
                      numeratorzone = fn_outcome * poststrwtzone)

      outcome <- 100 * sum(loop_data$numerator, na.rm=TRUE) / sum(loop_data$denominator, na.rm=TRUE)
      outcomepost <- 100 * sum(loop_data$numeratorzone, na.rm=TRUE) / sum(loop_data$poststrwtzone, na.rm=TRUE)

      # Construct row i of data frame
      y_outcomes_df[i,1] <- countryname
      y_outcomes_df[i,2] <- as.character(outcomevars[i])
      y_outcomes_df[i,3] <- as.character(outcomenames[i])
      y_outcomes_df[i,4] <- outcome
      y_outcomes_df[i,5] <- p_lb
      y_outcomes_df[i,6] <- p_ub
      y_outcomes_df[i,7] <- outcomepost
      y_outcomes_df[i,8] <- ytc[y]
      y_outcomes_df[i,9] <- wtc[y]

      # Create outcome-by-state
      loopstate <- loop_data %>%
        dplyr::group_by(State) %>%
        dplyr::summarize(proportion = sum(numerator, na.rm = TRUE)/sum(denominator, na.rm = TRUE)) %>%
        dplyr::mutate(year = ytc[y]) %>%
        dplyr::select(State, year, proportion)

      if(i==1){
        y_outcomes_by_state <- loopstate
        names(y_outcomes_by_state) <- c("State", "Year", outcomevars[i])
      } else {
        savenames <- names(y_outcomes_by_state)
        y_outcomes_by_state$newprop <- loopstate$proportion
        names(y_outcomes_by_state) <- c(savenames, outcomevars[i])
      }


    } # End outcome loop

    if(y==1){
      weight_df <- y_analysis_data %>%
        dplyr::group_by(State) %>%
        dplyr::summarize(SUMWT = sum(fn_weight)) %>%
        dplyr::mutate(tot_weight = sum(y_analysis_data$fn_weight),
                      Ratio = SUMWT/tot_weight,
                      Year = ytc[y]) %>%
        dplyr::select(-SUMWT,-tot_weight)
    } else {
      tempwt <- y_analysis_data %>%
        dplyr::group_by(State) %>%
        dplyr::summarize(SUMWT = sum(fn_weight)) %>%
        dplyr::mutate(tot_weight = sum(y_analysis_data$fn_weight),
                      Ratio = SUMWT/tot_weight,
                      Year = ytc[y]) %>%
        dplyr::select(-SUMWT,-tot_weight)

      weight_df <- rbind(weight_df, tempwt)
    }

    if(y==1){
      outcomes_df <- y_outcomes_df
    } else {
      outcomes_df <- dplyr::bind_rows(outcomes_df, y_outcomes_df)
    }

    if(y==1){
      outcomes_by_state <- y_outcomes_by_state
    } else {
      outcomes_by_state <- rbind(outcomes_by_state, y_outcomes_by_state)
    }

  } # end year loop

  ## Tablemaker component
  ## Dependencies: tidyverse, openxlsx

  fndata <- outcomes_df
  years <- unique(fndata$DataYear)

  earlier_years <- years[1:length(years)-1]
  later_years <- years[2:length(years)]

  tc <- cbind(ytc,wtc)
  wt <- wtc[2:length(wtc)]

  outcomes <- unique(fndata$OutcomeVar)

  outlist <- list()
  outwb <- openxlsx::createWorkbook()

  for(i in 1:length(outcomes)){
    loopdata <- fndata %>%
      dplyr::filter(OutcomeVar == outcomes[i])

    looppivot <- loopdata %>%
      tidyr::pivot_longer(cols = c(Outcome_Original, Outcome_PS),
                          names_to = "Outcome",
                          values_to = "Value") %>%
      dplyr::mutate(PSWeightYear = ifelse(Outcome == "Outcome_Original", DataYear, PSWeightYear))

    plps_pairs <- data.frame(
      DataYear = later_years,
      PSWeightYear = wt
    )

    pkps_pairs <- data.frame(
      DataYear = earlier_years,
      PSWeightYear = wt
    )

    plps <- dplyr::left_join(plps_pairs, looppivot) %>%
      dplyr::select(DataYear, PSWeightYear, Value) %>%
      dplyr::mutate(Value = round(Value, 10)) %>%
      unique()

    pkps <- dplyr::left_join(pkps_pairs, looppivot) %>%
      dplyr::select(DataYear, PSWeightYear, Value) %>%
      dplyr::mutate(Value = round(Value, 10)) %>%
      unique()

    looptable <- data.frame(
      l_year = later_years,
      k_year = earlier_years,
      pl = loopdata[loopdata$DataYear %in% later_years,]$Outcome_Original,
      pk = loopdata[loopdata$DataYear %in% earlier_years,]$Outcome_Original,
      plps = plps$Value,
      pkps = pkps$Value
    ) %>%
      dplyr::mutate(
        Country = countryname,
        OutcomeVar = outcomes[i],
        OutcomeName = outcomenames[i],
        poststratyear = wtc[2:length(wtc)],
        pl_minus_pk = pl-pk,
        plps_minus_pkps = plps-pkps,
        rdsw = 100 * (pl_minus_pk - plps_minus_pkps)/pl_minus_pk,
        diff_in_diffs = plps_minus_pkps - pl_minus_pk
      ) %>%
      dplyr::select(Country, OutcomeVar, OutcomeName, everything())

    outlist[[paste0(countryname, "_", outcomes[i], "_table")]] <- looptable
    outlist[["excel"]]$poststratyear <- NULL

    openxlsx::addWorksheet(outwb, outcomes[i])
    openxlsx::writeData(outwb, sheet = i, looptable)
    openxlsx::setColWidths(outwb, i, cols = 1:length(looptable), "auto")
  }

  outlist[["excel"]] <- outwb

  ## Make visualizations ----

  # Weight plot

  weightplot <- ggplot2::ggplot(data = weight_df,
                                aes(x = Year, y = Ratio*100)) +
    ggplot2::geom_path(aes(group = as.factor(State), color = as.factor(State))) +
    # ggplot2::labs(x = "", y = "Estimated % of National Children Aged 12-23m") +
    ggplot2::labs(x = "", y = "Estimated % of Population") +
    ggplot2::ggtitle(paste0(countryname, " State Weights")) +
    {if(!is.null(statelabels)) ggplot2::scale_color_manual(values = pal, name = "State",
                                                           labels = statelabels)} +
    {if(is.null(statelabels)) ggplot2::scale_color_manual(values = pal, name = "State")} +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = ytc, labels = round(ytc))

  stateplots <- list()
  nationalplots <- list()
  combinedplots <- list()

  for(o in 1:length(outcomevars)){

    # State coverage over time (for each outcome)

    stateoutcomedata <- outcomes_by_state
    stateoutcomedata$plotoutcome <- get(outcomevars[o], stateoutcomedata)

    stateplot <- ggplot2::ggplot(data = stateoutcomedata,
                                 aes(x = Year, y = plotoutcome*100, group = State)) +
      ggplot2::geom_path(aes(group = as.factor(State), color = as.factor(State))) +
      ggplot2::labs(x = "", y = "% Coverage") +
      ggplot2::ggtitle(paste0(countryname, " Coverage by State: ", outcomenames[o])) +
      {if(!is.null(statelabels)) ggplot2::scale_color_manual(values = pal, name = "State",
                                                             labels = statelabels)} +
      {if(is.null(statelabels)) ggplot2::scale_color_manual(values = pal, name = "State")} +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(breaks = ytc, labels = round(ytc))

    stateplots[[paste0("state_plot_", outcomevars[o])]] <- stateplot

    # National outcome plot (original and PS)

    nationaloutcomedata <- outcomes_df %>%
      dplyr::filter(OutcomeVar == outcomevars[o]) %>%
      tidyr::pivot_longer(cols = c("Outcome_Original", "Outcome_PS"),
                          names_to = "Group",
                          values_to = "Proportion") %>%
      dplyr::mutate(Group = ifelse(Group == "Outcome_Original", "Original", "Post-Stratified"))

    nationalplot <- ggplot2::ggplot(data = nationaloutcomedata, aes(x = DataYear, y = Proportion)) +
      ggplot2::geom_path(aes(group = Group, color = Group)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "% Coverage", color = "") +
      ggplot2::ggtitle(paste0(countryname, " Coverage: ", outcomenames[o])) +
      ggplot2::scale_x_continuous(breaks = ytc, labels = round(ytc))

    nationalplots[[paste0("national_plot_", outcomevars[o])]] <- nationalplot

    combinedplot <- gridExtra::arrangeGrob(
      grobs = list(weightplot, stateplot, nationalplot),
      ncol = 1
    )

    combinedplots[[paste0("combined_plot_", outcomevars[o])]] <- combinedplot

  }

  if(is.null(statelabels)){
    null_sl <- TRUE
    statelabels <- NA
  } else {
    null_sl <- FALSE
  }

  outcomes_by_state <- outcomes_by_state %>%
    dplyr::mutate(Country = countryname,
                  StateName = rep(statelabels, length.out = nrow(outcomes_by_state))) %>%
    dplyr::select(Country, State, StateName, everything())

  if(null_sl == TRUE){
    outcomes_by_state <- outcomes_by_state %>%
      dplyr::select(-StateName)
  }

  # Restore user's original setting for survey.lonely.psu
  options(survey.lonely.psu = paste(save_user_survey_option))

  # Datasets, tables, and plots to return
  list(weightdata = weight_df,
       outcomes = outcomes_df,
       state_outcomes = outcomes_by_state,
       tables = outlist,
       weight_plot = weightplot,
       state_outcome_plots = stateplots,
       national_outcome_plots = nationalplots,
       combined_plots = combinedplots)

}
