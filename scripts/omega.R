
# - quantifying the importance of change ------------------------------------- #
# - omega -------------------------------------------------------------------- #

### note: this script
###       (a) processes the input data for omega calculation,
###       (b) calculates within variance, between variance and omega from MLMs,
###       (c) generates bootstrapped distributions for omega values.

# ---------------------------------------------------------------------------- #

# --- packages when the script is loaded
pacman::p_load(glmmTMB, lme4, performance, Matrix, tidyverse, parallel)

# --- function call
calculate_omega <- 
  
  function(
    
    # dataframe
    df,
    # outcome variable
    yname,
    # time variable
    tname,
    # person identifier
    pname,
    # bootstrap
    boot = FALSE,
    # number of boots
    nrep = 500,
    # parallel
    parallel = FALSE,
    # cores
    cores = 10,
    # center midpoint
    center = TRUE) {
    
    # ----------------------------------------------------- #
    # DATA PROCESSING                                       #
    # ----------------------------------------------------- #
    
    df <- df |>
      mutate(
        # get person identifier
        pid = {
          {
            pname
          }
        },
        # get time identifier
        time = {
          {
            tname
          }
        },
        # get outcome identifier
        y  = {
          {
            yname
          }
        }) |>
      
      # unique numeric identifier
      mutate(id = cur_group_id(), .by = "pid") |>
      
      # drop missing values
      select(id, time, y) |> drop_na() |>
      
      # drop respondents with only one observation
      mutate(n = n(), .by = "id") |> dplyr::filter(n != 1)
    
    # center at respondent midpoint
    if (center == TRUE) {
      df <- df |> 
        mutate(
          time = (time - (max(time) + min(time)) / 2),
          .by = "id")
    }

    # ----------------------------------------------------- #
    # MODELING                                              #
    # ----------------------------------------------------- #
    
    ## estimate LCAM
    model <- glmmTMB(y ~ time + (1 + time | id), data = df)
    
    ## prior specification
    prior_sd <- round(sd(df$y) * 10, 5) ## 10 times the observed y
    prior_sd <- paste("normal(0, ", prior_sd, ")", sep = "")
    
    ## impose priors
    priors <- data.frame(prior =
                           prior_sd,
                         class =
                           "theta",
                         coef = "")
    model <- update(model, priors = priors)
    
    ## model information
    var_corrs <- abs(attr(VarCorr(model)$cond$id, "correlation")[1, 2])
    converged <- performance::check_convergence(model)
    
    # ----------------------------------------------------- #
    # DECOMPOSITION                                         #
    # ----------------------------------------------------- #
    
    ## predictions
    df <- df |>
      mutate(
        ## midpoint prediction
        yhat_m = predict(model, df |> mutate(time = 0)),
        ## incorporating varying slopes
        yhat_s = predict(model))

    ## components
    vd <- 
      1 - sum((df$y - df$yhat_m)^2)/sum((df$y - mean(df$y))^2)
    vc <- 
      1 - sum((df$y - df$yhat_s)^2)/sum((df$y - mean(df$y))^2) - vd
    ve <- 
      1 - vd - vc
    omega <- vc/(vd + vc)
    
    if (boot == FALSE) {
      output <- 
        c(vd = vd,
          vc = vc,
          ve = ve,
          omega = omega,
          converged = converged,
          var_corrs = var_corrs)
      return(output)
    }
    
    # ----------------------------------------------------- #
    # BOOTSTRAPS                                            #
    # ----------------------------------------------------- #
    
    boot_function <- function(data) {
      
      ## boot dataframe
      boot_df <- data |>
        nest(.by = "id") |>
        slice_sample(n = length, replace = TRUE) |>
        mutate(id = cur_group_rows()) |> # new ids
        unnest("data")
      
      ## boot model
      boot_model <- glmmTMB(y ~ time + (1 + time | id), data = boot_df)
      boot_prior_sd <- round(sd(boot_df$y) * 10, 5)
      boot_prior_sd <- paste("normal(0, ", boot_prior_sd, ")", sep = "")
      boot_priors <- data.frame(prior =
                                  boot_prior_sd,
                                class =
                                  "theta",
                                coef = "")
      boot_model <- update(boot_model, priors = boot_priors)
      
      ## predictions
      boot_df <- boot_df |>
        mutate(
          ## midpoint prediction
          yhat_m = predict(boot_model, boot_df |> mutate(time = 0)),
          ## incorporating varying slopes
          yhat_s = predict(boot_model)
        )
      
      ## components
      boot_vd <-
        1 - sum((boot_df$y - boot_df$yhat_m) ^ 2) /
        sum((boot_df$y - mean(boot_df$y)) ^ 2)
      boot_vc <-
        1 - sum((boot_df$y - boot_df$yhat_s) ^ 2) /
        sum((boot_df$y - mean(boot_df$y)) ^ 2) - boot_vd
      boot_omega <- boot_vc / (boot_vd + boot_vc)
      
      ## return omega
      return(boot_omega)
    }
    
    if (parallel == FALSE) {
      
      length <- length(unique(df$id))
      bootstrap <- vector(mode = "numeric", length = nrep)
      # loop
      for (bloop in 1:nrep) {
        bootstrap[bloop] <- boot_function(data = df)
      }
    } else {
      
      length <- length(unique(df$id))
      bootstrap <- vector(mode = "numeric", length = nrep)
      # parallel
      bootstrap <- mclapply(rep(list(df), nrep), 
                            boot_function, mc.cores = cores)
      bootstrap <- unlist(bootstrap)
    }

    output <-
      c(
        vd = vd,
        vc = vc,
        ve = ve,
        omega = omega,
        converged = converged,
        var_corrs = var_corrs,
        boots = list(bootstrap)
      )
    return(output)
    
  }

# ---------------------------------------------------------------------------- #
