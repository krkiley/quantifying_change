
# - quantifying change ------------------------------------------------------- #
# - college example ---------------------------------------------------------- #

### note: this script
###       (a) prepares 8 political items from the General Social Survey,
###       (b) calculates omega values across college and no-college subgroups,
###       (c) compares the differences of omega values in booted samples.

# ---------------------------------------------------------------------------- #

rm(list = ls())
pacman::p_load(
  purrr,
  furrr, 
  hrbrthemes,
  conflicted,
  gssr,
  tidyverse)
conflicts_prefer(dplyr::select())
conflicts_prefer(dplyr::filter())

# --- helpers
my_oka <-
  c("#eebd64",
    "#8b3b36",
    "#35455d",
    "#144525")
theme_set(theme_ipsum_rc())
`%nin%` = Negate(`%in%`)

# --- scripts
suppressMessages(
  source("./scripts/helper_scripts/omega.R"))

# --- data
data("gss_panel06_long")
data("gss_panel08_long")
data("gss_panel10_long")

# --- parallel sessions
plan(multisession); 
set.seed(11235) # set seed for simulation protocol
options(future.globals.maxSize=5000*1024^2)

# ---------------------------------------------------------------------------- #
# PART 1: DATA PREPS --------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- get the data
gss <- bind_rows(
  gss_panel06_long |>
    select(
      firstid,
      wave,
      age,
      degree,
      partyid,
      polviews,
      eqwlth,
      helpsick,
      helpnot,
      helpblk,
      helppoor,
      affrmact
    ) |>
    mutate(pid = paste("06", firstid, sep = "-")),
  gss_panel08_long |>
    select(
      firstid,
      wave,
      age,
      degree,
      partyid,
      polviews,
      eqwlth,
      helpsick,
      helpnot,
      helpblk,
      helppoor,
      affrmact
    ) |>
    mutate(pid = paste("08", firstid, sep = "-")),
  gss_panel10_long |>
    select(
      firstid,
      wave,
      age,
      degree,
      partyid,
      polviews,
      eqwlth,
      helpsick,
      helpnot,
      helpblk,
      helppoor,
      affrmact
    ) |>
    mutate(pid = paste("10", firstid, sep = "-"))
  ) |>
  ## drop variable labels
  haven::zap_labels()

# --- wrangle
gss <- gss |>
  ## recode the party id
  mutate(partyid = ifelse(partyid == 7, 
                          NA_real_, 
                          partyid)) |>
  ## generate first-wave college indicator
  left_join(gss |>
              slice(1, .by = "pid") |>
              mutate(ba = ifelse(degree >= 3, 1, 0)) |>
              select(pid, ba),
            by = "pid") |>
  ## go long with the questions
  select(-degree, -firstid) |>
  pivot_longer(
    cols = -c(pid, wave, age, ba),
    names_to = "question",
    values_to = "y") |>
  drop_na() |>
  nest(.by = c("question", "ba")) ## nest by item and college status

# ---------------------------------------------------------------------------- #
# PART 2: CALCULATE OMEGAS --------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- omegas
gss <- gss |>
  mutate(omega = future_map(
    .x = data,
    .f = purrr::possibly(
      ~ calculate_omega(
        df = .,
        yname = y,
        tname = wave,
        pname = pid,
        boot = TRUE,
        nrep = 500
      )
    ),
    ## for reproducibility
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
  ))

# --- prettify
gss <- gss |>
  ## drop data
  dplyr::select(-data) |>
  ## unnest
  unnest_wider(omega) |> 
  ## boot values
  mutate(omega_mean = 
           map_dbl(.x = boots, .f = ~ mean(.)),
         omega_lo =
           map_dbl(.x = boots, .f = ~quantile(., 0.025)),
         omega_hi =
           map_dbl(.x = boots, .f = ~quantile(., 0.975)))
         
saveRDS(gss, "./data/output/colleges.rds")

# ---------------------------------------------------------------------------- #
# PART 3: DRAW OMEGAS -------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

png("./figures/colleges.png",
    w = 9,
    h = 9,
    units = "in",
    res = 500)
gss |>
  left_join(read_csv("./misc/var_labels.csv"),
            by = c("question" = "variable")) |>
  mutate(label = str_to_title(label)) |> 
  mutate(ba = factor(ba,
                     levels = c(0, 1),
                     labels = c("No College",
                                "College"))) |>
  unnest(boots) |> 
  ggplot(aes(x = boots, y = after_stat(scaled), fill = ba)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ label, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c(my_oka[1], my_oka[3])) +
  labs(x = "Omega", y = "Density", fill = "") +
  theme(legend.position = "top")
dev.off()

# ---------------------------------------------------------------------------- #
