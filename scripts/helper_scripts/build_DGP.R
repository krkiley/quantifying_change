
# - quantifying change ------------------------------------------------------- #
# - DGPs --------------------------------------------------------------------- #

### note: this script
###       (a) simulates panel data values using a set of parameters,
###       (b) exports a dataframe with individual ID, time, and observed `y`.

# ---------------------------------------------------------------------------- #

# --- packages when the script is loaded
pacman::p_load(tidyverse)

# --- function call
generate_data <-
  
  function(
    
    ## the number of N
    n = 1000,
    ## the number of T
    t = 3,
    ## the rate of change
    rate = 0.25,
    ## strength of change
    strength = 1,
    ## change balance
    balance = 0.5,
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
          u = rnorm(n = n, mean = 0, sd = 1)
        )
    } else {
      u <-
        tibble(
          ## actors
          pid = c(1:n),
          ## central tendency
          u = rnorm(n = n, mean = 0, sd = 1)
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
    
    ## 2.1: build the window
    span_window <- seq(
      ## first t
      from = 0,
      ## final t                 
      to =   1,
      ## the number of ts
      length.out = t)
    
    ## 2.2: generate pid x time grid
    data <- expand_grid(u, t = span_window)
    
    # ----------------------------------------------------- #
    # RESPONSE CONSTRUCTION                                 #
    # ----------------------------------------------------- #
    
    # --- part 3: generate the observed scores
    
    ## 3.1: add change scores
    data <- data |> 
      left_join(u |> select(pid) |>
                  mutate(upper = 
                           rbinom(n = n, size = 1, prob = balance)) |>
                  mutate(upper = ifelse(upper == 1, 1, -1)),
                by = "pid") |>
      mutate(u = u + strength * t * changer * upper) |>
      select(-upper)
    
    ## 3.2: extract the new error variance
    new_error <- sd(data$u)
    
    ## 3.3: generate the realized y scores
    data <- data |>
      mutate(y =
               ## true scores
               (u * sqrt(reliable))
             +
               ## error
               rnorm(n = n, mean = 0, sd = new_error) 
             * 
               sqrt((1 - reliable)))
    
    # --- part 4: organize and spit out the data
    
    data <- data |>
      dplyr::select(pid,
                    t,
                    y)
    
    return(data)
    
  }

# ---------------------------------------------------------------------------- #
