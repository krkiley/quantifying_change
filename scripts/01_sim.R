
# - quantifying change ------------------------------------------------------- #
# - simulations -------------------------------------------------------------- #

### note: this script
###       (a) generates dataframes with varying DGPs,
###       (b) calculates omega values across different parameter combinations,
###       (c) draws average omega values under different specifications.

# ---------------------------------------------------------------------------- #

rm(list = ls())
pacman::p_load(
  purrr,
  furrr, 
  hrbrthemes,
  conflicted,
  tidyverse)
conflicts_prefer(dplyr::select())
conflicts_prefer(dplyr::filter())

# --- helpers
my_oka <-
  c("#eebd64",
    "#8b3b36",
    "#35455d")
theme_set(theme_ipsum_rc())
`%nin%` = Negate(`%in%`)

# --- scripts
suppressMessages(
  source("./scripts/helper_scripts/build_DGP.R"))
suppressMessages(
  source("./scripts/helper_scripts/omega.R"))

# --- parallel sessions
plan(multisession); 
set.seed(11235) # set seed for simulation protocol

# ---------------------------------------------------------------------------- #
# PART 1: BUILD DATAFRAMES --------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- PARAMETER SPACE

d <-
  expand_grid(
    # varying rates of change
    p_rate = c(.10,
               .25,
               .50),
    # varying strengths of change
    p_strength = c(1, 
                   2,
                   4),
    # varying balance of change
    p_balance = c( 0,
                  .5),
    # 500 simulation runs per condition
    sim = c(1:500)
  )

# --- DATA GENERATION

d <- d |>
  mutate(
    data =
      furrr::future_pmap(
        ## mapping list
        .l = list(p_rate, p_strength, p_balance),
        ## refer to list based on index
        .f = ~ generate_data(
          n = 500,
          reliable = 0.9,
          t = 3,
          rate = ..1,
          strength = ..2,
          balance = ..3
        ),
        ## for reproducibility
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
      )
  )

# ---------------------------------------------------------------------------- #
# PART 2: CALCULATE OMEGAS --------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- CALCULATION

d <- d |>
  mutate(omega = future_map(
    .x = data,
    .f = ~ calculate_omega(
      df = .,
      yname = y,
      tname = t,
      pname = pid,
      boot = FALSE
    ),
    .progress = TRUE
  ))

# --- PRETTIFY

d <- d |>
  dplyr::select(-data) |>
  unnest_wider(omega) |>
  # turn attributes into factors
  mutate_at(.vars = c("p_rate",
                      "p_strength",
                      "p_balance"),
            ~ as.factor(.)) |>
  mutate(
    p_rate = factor(
      p_rate, 
      labels = c("Rate: 10%",
                 "Rate: 25%",
                 "Rate: 50%")),
    p_strength = factor(
      p_strength, 
      labels = c("d = 1",
                 "d = 2",
                 "d = 4")),
    p_balance = factor(
      p_balance,
      labels = c("One-Direction",
                 "50%-50% Split")))

saveRDS(d, "./data/output/simulations.rds")

# ---------------------------------------------------------------------------- #
# PART 3: DISTRIBUTIONS ------------------------------------------------------ #
# ---------------------------------------------------------------------------- #

png(
  "./figures/simulation.png",
  w = 8,
  h = 4,
  units = "in",
  res = 500
)
d |>
  ## summarize simulations
  summarize(
    mean = mean(omega),
    sd = sd(omega),
    .by = c("p_rate", "p_strength", "p_balance")
  ) |>
  ## simulation variation
  mutate(ymin = mean - 1.96*sd, ymax = mean + 1.96*sd) |>
  mutate(ymin = ifelse(ymin < 0, 0, ymin)) |>
  ## plot
  ggplot(aes(
    x = p_rate,
    y = mean,
    ymin = ymin,
    ymax = ymax,
    col = p_strength
  )) +
  geom_pointrange(linewidth = 0.5, size = 0.5) +
  scale_color_manual(values = my_oka[1:3]) +
  labs(x = "Rate of Change",
       y = "Omega",
       col = "Standardized Difference",
       title = "") +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_ipsum_rc(
    axis_title_size = 12,
    grid = "YyXx",
    plot_margin = margin(0, 10, 10, 10)
  ) +
  theme(legend.position = "top") +
  facet_grid(~p_balance)
dev.off()

# ---------------------------------------------------------------------------- #
