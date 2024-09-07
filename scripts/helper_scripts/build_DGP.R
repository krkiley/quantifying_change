
# - quantifying the importance of change ------------------------------------- #
# - generate data ------------------------------------------------------------ #

### note: this script
###       (a) simulates panel data values using a set of parameters,
###       (b) exports a dataframe with individual ID, time, and observed `y`.

# ---------------------------------------------------------------------------- #

# --- packages when the script is loaded
require(tidyverse)

# --- function call
generate_data <-
  
  function(
    
    ## the number of N
    n = 1000,
    ## the number of T
    t = 3,
    
    ## v(d) at midpoint
    vd = 1,
    ## measurement error shock
    ve = 1,
    ## the rate of change
    rate = 0.25,
    ## strength of change
    strength = 1,
    
    ## balance in slopes
    balance = 1,
    ## reliability score
    reliable = 0.8) {

    # ----------------------------------------------------- #
    # BUILDING BLOCKS                                       #
    # ----------------------------------------------------- #
    
    # --- part 1: generate actors
    if (rate == 0) {
      u <- 
        tibble(
          ## actors
          pid = c(1:n),
          ## central tendency
          u = rnorm(n = n, mean = 0, sd = vd)
        ) |> 
        mutate(
          ## changer status
          changer = 0
        )
    } else {
      u <-
        tibble(
          ## actors
          pid = c(1:n),
          ## central tendency
          u = rnorm(n = n, mean = 0, sd = vd)
        ) |> 
        mutate(
          ## changer status
          changer = sample(0:1, 
                           n(), 
                           replace = TRUE, 
                           prob = c(1 - rate, rate))
        )
    }
    
    # --- part 2: generate the longitudinal data setup
    
    ## 2.1: build sequences
    sequence <- function(N) {
      if (N %% 2 == 1) {
        # if N is odd, 0 will be the absolute center
        half_n <- (N - 1) / 2
        seq(from = -half_n,
            to = half_n,
            length.out = N)
      } else {
        # if N is even, we'll bias the waves towards positive
        half_n <- N / 2
        seq(from = -(half_n - 1),
            to = half_n,
            length.out = N)
      }
    }
    
    ## 2.2: build the window
    span_window <- sequence(t)
    
    ## 2.3: generate pid x time grid
    data <- expand_grid(u, waves = span_window)
    
    # ----------------------------------------------------- #
    # RESPONSE CONSTRUCTION                                 #
    # ----------------------------------------------------- #

    # --- part 3: generate the outcomes
    
    ## 3.1: add change scores
    data <- data |>
      ## information about directions
      left_join(u |> select(pid) |>
                  mutate(
                    upper =
                      rbinom(n = n, size = 1, prob = balance),
                    upper =
                      ifelse(upper == 1, 1, -1)
                  ), by = "pid") |>
      ## add a growth term ranging from -0.5 to 0.5
      mutate(
        growth =
          ((waves - min(waves)) / (max(waves) - min(waves)))
        -
          0.5
      ) |>
      ## add slopes to each observation time
      mutate(u = u + strength * growth * changer * upper) |>
      ## clean-up the columns
      select(-upper, -growth)
    
    ## 3.2: generate the realized y scores
    data <- data |>
      mutate(y =
               ## true scores
               (u * sqrt(reliable))
             +
               ## error
               rnorm(n = n * t, mean = 0, sd = ve) 
             * 
               sqrt((1 - reliable)))

    # --- part 4: organize and spit out the data
    
    data <- data |>
      dplyr::select(pid,
                    t = waves,
                    y,
                    changer)
    
    return(data)
    
  }

# ---------------------------------------------------------------------------- #
