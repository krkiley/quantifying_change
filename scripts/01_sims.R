
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
    "#35455d",
    "#a2c2a4")
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
# PART 1: CASE STUDY --------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- STEP 1: PARAMETER SPACE

d1 <-
  expand_grid(
    # v(c)
    p_vc = c(0.5, 1, 2),
    # rate
    p_cr = c(0, .25, .50, .75, 1),
    # 100 simulation runs per condition
    sim = c(1:100)
  )

# --- STEP 2: DATA GENERATION

d1 <- d1 |>
  mutate(
    data =
      furrr::future_pmap(
        ## mapping list
        .l = list(p_vc, p_cr),
        ## refer to list based on index
        .f = ~ generate_data(
          n = 1000,
          t = 10,
          vd = 1,
          ve = 1,
          rate = ..2,
          strength = ..1,
          balance = 1,
          reliable = 0.5
        ), 
        ## for reproducibility
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
      )
  )

# --- STEP 3: OMEGA CALCULATION

d1 <- d1 |>
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

# --- STEP 4: DRAW THE RESULTS

png(
  "./figures/simulation01.png",
  w = 8,
  h = 4,
  units = "in",
  res = 500
)
d1 |>
  ## unburden the datafile
  dplyr::select(-data) |> unnest_wider(omega) |>
  ## OK, now prettify
  mutate_at(.vars = c("p_vc",
                      "p_cr"),
            ~ as.factor(.)) |>
  mutate(
    p_vc = factor(p_vc, labels = c("0.5", "1", "2")),
    p_cr = factor(p_cr, labels = c("Rate: 0%",
                                   "Rate: 25%",
                                   "Rate: 50%",
                                   "Rate: 75%",
                                   "Rate: 100%"))) |>
  ## summarize the simulation results
  summarize(
    mean = mean(omega), sd = sd(omega),
    .by = c("p_vc", "p_cr")) |>
  mutate(ymin = mean - 1.96*sd, ymax = mean + 1.96*sd) |>
  mutate(ymin = ifelse(ymin < 0, 0, ymin)) |>
  ## start plotting the stuff
  ggplot(aes(
    x = p_cr,
    y = mean,
    ymin = ymin,
    ymax = ymax,
    col = p_vc
  )) +
  geom_pointrange(linewidth = 0.5, size = 0.5) +
  scale_color_manual(values = my_oka[1:4]) +
  labs(x = "Rate of Change",
       y = "Omega",
       col = "Standardized Difference",
       title = "") +
  theme_ipsum_rc(
    axis_title_size = 12,
    grid = "YyXx",
    plot_margin = margin(0, 10, 10, 10)
  ) +
  theme(legend.position = "top")
dev.off()

# ---------------------------------------------------------------------------- #
