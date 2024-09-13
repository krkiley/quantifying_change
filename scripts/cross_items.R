
# - quantifying the importance of change ------------------------------------- #
# - omega calculations ------------------------------------------------------- #

### note: this script
###       (a) calculates omega values across survey items,
###       (b) draws the relative shares of intrapersonal/interpersonal culture,
###       (c) exports V(D), V(C) and Omega as a LaTeX table.

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
    "#144525")
theme_set(theme_ipsum_rc())
`%nin%` = Negate(`%in%`)

# --- scripts
suppressMessages(
  source("./scripts/omega.R"))

# --- data
d <- readRDS("./data/data.rds") # nested survey x item panels

# --- parallel sessions
plan(multisession)
options(future.globals.maxSize=5000*1024^2)

# ---------------------------------------------------------------------------- #
# PART 1: CALCULATE OMEGAS --------------------------------------------------- #
# ---------------------------------------------------------------------------- #

d <- d |>
  mutate(omega = future_map(
    .x = data,
    .f = purrr::possibly(
      ~ calculate_omega(
        df = .,
        yname = y,
        tname = wave,
        pname = pid,
        boot = FALSE
      )
    ),
    ## for reproducibility
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
  ))

# ---------------------------------------------------------------------------- #
# PART 2: PRETTIFY AND EXPORT ------------------------------------------------ #
# ---------------------------------------------------------------------------- #

d <- d |>
  ## drop data
  dplyr::select(-data) |>
  ## unnest
  unnest_wider(omega) |>
  ## add variable labels
  left_join(read_csv("./misc/var_labels.csv"),
            by = c("surveys", "variable"))

saveRDS(d, "./data/output/omegas.rds")

# ---------------------------------------------------------------------------- #
# PART 3: DRAW OMEGAS -------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

png("./figures/omegas.png",
    w = 7,
    h = 9,
    units = "in",
    res = 500)
d |>
  mutate(leftover = 1 - omega) |>
  mutate(order = omega) |>
  pivot_longer(
    cols = c(omega, leftover),
    names_to = "component",
    values_to = "values"
  ) |>
  select(surveys, variable, component, order, values) |>
  ggplot(aes(
    x = values,
    y = tidytext::reorder_within(variable, order, surveys),
    fill = component
  )) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(
    . ~ surveys,
    scales = "free_y",
    ncol = 1,
    strip.position = "left",
    labeller = as_labeller(
      c(
        bhps = "BHPS",
        psid = "PSID",
        shp = "SHP",
        soep = "SOEP",
        ukhls = "UKHLS",
        gss = "GSS",
        hilda = "HILDA"
      )
    )
  ) +
  labs(title = "",
       x = "Proportion of Systematic Variance",
       y = "Survey Items",
       fill = "") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_ipsum_rc(
    axis_title_size = 12,
    plot_margin = margin(10, 20, 20, 20)
  ) +
  theme(
    legend.position = "top",
    axis.text.y  = element_blank(),
    strip.text   = element_text(),
    axis.ticks.y = element_blank(),
    strip.text.y = element_text(hjust = 1)
  ) +
  scale_fill_manual(
    values = c(my_oka[1], my_oka[3]),
    labels = c("Interpersonal ", "Intrapersonal")
  )
dev.off()

# ---------------------------------------------------------------------------- #
# PART 4: EXPORT OMEGAS ------------------------------------------------------ #
# ---------------------------------------------------------------------------- #

print(
  xtable::xtable(
    d |>
      select(
        Survey = surveys,
        Item = variable,
        Label = label,
        VD = vd,
        VC = vc,
        Omega = omega
      ),
    caption = "Table X"
  ),
  include.rownames = FALSE,
  file = "./tables/omegas.txt",
  tabular.environment = "longtable",
  floating = FALSE
)

# ---------------------------------------------------------------------------- #
