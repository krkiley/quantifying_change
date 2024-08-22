
library(haven)
library(tidyverse)
library(lme4)

#Load in GSS data
g6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")
g8 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta")
g10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")


gss <- bind_rows(g6 %>%
                   select(id_1, degree_1, age_1, wtpannr123,
                          partyid_1, partyid_2, partyid_3,
                          polviews_1, polviews_2, polviews_3,
                          eqwlth_1, eqwlth_2, eqwlth_3,
                          helpsick_1, helpsick_2, helpsick_3,
                          helpnot_1, helpnot_2, helpnot_3,
                          helpblk_1, helpblk_2, helpblk_3,
                          helppoor_1, helppoor_2, helppoor_3,
                          affrmact_1, affrmact_2, affrmact_3,
                          confed_1, confed_2, confed_3,
                          finalter_1, finalter_2, finalter_3) %>%
                   mutate(id_1 = paste("06", id_1, sep = "-")),
                 g8 %>%
                   select(id_1, degree_1, age_1, wtpannr123,
                          partyid_1, partyid_2, partyid_3,
                          polviews_1, polviews_2, polviews_3,
                          eqwlth_1, eqwlth_2, eqwlth_3,
                          helpsick_1, helpsick_2, helpsick_3,
                          helpnot_1, helpnot_2, helpnot_3,
                          helpblk_1, helpblk_2, helpblk_3,
                          helppoor_1, helppoor_2, helppoor_3,
                          affrmact_1, affrmact_2, affrmact_3,
                          confed_1, confed_2, confed_3,
                          finalter_1, finalter_2, finalter_3)%>%
                   mutate(id_1 = paste("08", id_1, sep = "-")),
                 g10 %>%
                   select(id_1, degree_1, age_1, WTPANNR123,
                          partyid_1, partyid_2, partyid_3,
                          polviews_1, polviews_2, polviews_3,
                          eqwlth_1, eqwlth_2, eqwlth_3,
                          helpsick_1, helpsick_2, helpsick_3,
                          helpnot_1, helpnot_2, helpnot_3,
                          helpblk_1, helpblk_2, helpblk_3,
                          helppoor_1, helppoor_2, helppoor_3,
                          affrmact_1, affrmact_2, affrmact_3,
                          confed_1, confed_2, confed_3,
                          finalter_1, finalter_2, finalter_3)%>%
                   mutate(id_1 = paste("10", id_1, sep = "-"),
                          wtpannr123 = WTPANNR123) %>%
                   select(-WTPANNR123))


gssb <- gss %>%
  zap_labels() %>%
  mutate(across(c(partyid_1, partyid_2, partyid_3),
                ~ifelse(.x == 7, NA, .x)),
         ba = ifelse(degree_1 >= 3, 1, 0)) %>%
  pivot_longer(partyid_1:finalter_3) %>%
  separate(name, into = c("question", "t")) %>%
  mutate(t = as.numeric(t)-2)

questions <- unique(gssb$question)
results <- vector(mode = "list", length = length(questions))
for (i in 1:length(questions)) {
  q <- questions[i]
  
  #Get a vector of unique IDs that could be
  #bootstrapped
  #Filter to only people with at least one
  #response on that question
  gssc <- gssb %>% filter(question == q) %>%
    group_by(id_1) %>% filter(sum(!is.na(value))==3) %>%
    ungroup()
  g_ids <- unique(gssc$id_1)
  
  #Remove missing responses
  gssa <- gssc %>% filter(!is.na(value))
  
  #Estimate multi-level model
  m_gss <- lmer(value ~ t + (1 + t|id_1), data = gssa)
  predict.data <- gssa %>% mutate(t = 0)
  
  ## Predict values using the time midpoint
  gssa$yhat_m <- predict(m_gss, predict.data)
  
  ## Predict allow for individual slopes
  gssa$yhat_s <- predict(m_gss)
  
  # Calculate components of Omega
  vd <- 1 - sum((gssa$value - gssa$yhat_m)^2)/sum((gssa$value - mean(gssa$value))^2)
  vf <- 1 - sum((gssa$value - gssa$yhat_s)^2)/sum((gssa$value - mean(gssa$value))^2)
  vc <- 1 - sum((gssa$value - gssa$yhat_s)^2)/sum((gssa$value - mean(gssa$value))^2) - vd
  omega <- vc/(vd + vc)
  gamma <- vf/vd
  
  
  #Separate data to calculate separate Omega for ba and hs 
  gss_ba <- gssa %>% filter(ba == 1)
  gss_hs <- gssa %>% filter(ba == 0)
  
  #Calculate omega components for ba holder
  vd_ba <- 1 - sum((gss_ba$value - gss_ba$yhat_m)^2)/
    sum((gss_ba$value - mean(gss_ba$value))^2)
  vc_ba <- 1 - sum((gss_ba$value - gss_ba$yhat_s)^2)/
    sum((gss_ba$value - mean(gss_ba$value))^2) - vd_ba
  omega_ba <- vc_ba/(vd_ba + vc_ba)
  
  #calcualte omega components for hs only
  vd_hs <- 1 - sum((gss_hs$value - gss_hs$yhat_m)^2)/
    sum((gss_hs$value - mean(gss_hs$value))^2)
  vc_hs <- 1 - sum((gss_hs$value - gss_hs$yhat_s)^2)/
    sum((gss_hs$value - mean(gss_hs$value))^2) - vd_hs
  omega_hs <- vc_hs/(vd_hs + vc_hs)
  
  observed_vals <- 
    data.frame(group = rep(c("full", "ba", "hs"), each = 3),
               quantity = rep(c("vd", "vc", "omega"), 3),
               value = c(vd, vc, omega, 
                         vd_ba, vc_ba, omega_ba,
                         vd_hs, vc_hs, omega_hs),
               question = q)
  
  ## Bootstrap Confidence Intervals
  bootstrap <- vector(mode = "list", length = 100)
  for (j in 1:100) {
    ids <- sample(g_ids, length(g_ids), replace = TRUE)
    boot_df <- data.frame(id = rep(ids, each = 3),
                          new_id = rep(1:length(g_ids), each = 3),
                          t = rep(-1:1, length(g_ids)))
    boot_df <- left_join(boot_df, gssc, by = c("id" = "id_1", "t"="t")) %>%
      filter(!is.na(value))
    
    m_b <- lmer(value ~ t + (1+t|new_id), data = boot_df)
    
    predict.data <- boot_df %>% mutate(t = 0)
    
    ## Predict values using the time midpoint
    boot_df$yhat_m <- predict(m_b, predict.data)
    
    ## Predict allow for individual slopes
    boot_df$yhat_s <- predict(m_b)
    
    #Calculate full value
    # Calculate components of Omega
    vd_full_boot <- 1 - sum((boot_df$value - boot_df$yhat_m)^2)/
      sum((boot_df$value - mean(boot_df$value))^2)
    vc_full_boot <- 1 - sum((boot_df$value - boot_df$yhat_s)^2)/
      sum((boot_df$value - mean(boot_df$value))^2) - vd_full_boot
    omega_full_boot <- vc_full_boot/(vd_full_boot + vc_full_boot)
    
    #Separate data to calculate separate Omega for ba and hs 
    boot_ba <- boot_df %>% filter(ba == 1)
    boot_hs <- boot_df %>% filter(ba == 0)
    
    #Calculate omega components for ba holder
    vd_ba_boot <- 1 - sum((boot_ba$value - boot_ba$yhat_m)^2)/
      sum((boot_ba$value - mean(boot_ba$value))^2)
    vc_ba_boot <- 1 - sum((boot_ba$value - boot_ba$yhat_s)^2)/
      sum((boot_ba$value - mean(boot_ba$value))^2) - vd_ba_boot
    omega_ba_boot <- vc_ba_boot/(vd_ba_boot + vc_ba_boot)
    
    #calculate omega components for hs only
    vd_hs_boot <- 1 - sum((boot_hs$value - boot_hs$yhat_m)^2)/
      sum((boot_hs$value - mean(boot_hs$value))^2)
    vc_hs_boot <- 1 - sum((boot_hs$value - boot_hs$yhat_s)^2)/
      sum((boot_hs$value - mean(boot_hs$value))^2) - vd_hs_boot
    omega_hs_boot <- vc_hs_boot/(vd_hs_boot + vc_hs_boot)
    
    bootstrap[[j]] <- data_frame(vd_full = vd_full_boot, vc_full = vc_full_boot, omega_full = omega_full_boot, 
                                 vd_ba = vd_ba_boot, vc_ba = vc_ba_boot, omega_ba = omega_ba_boot,
                                 vd_hs = vd_hs_boot, vc_hs = vc_hs_boot, omega_hs = omega_hs_boot, 
                                 j = j)
    
    
    print(j)
  }
  
  
  bs_est <- bind_rows(bootstrap) %>%
    pivot_longer(vd_full:omega_hs) %>%
    separate(name, into = c("quantity", "group")) %>%
    group_by(quantity, group) %>%
    summarise(ci.low = quantile(value, .025), ci.high = quantile(value, .975)) 
  results[[i]] <- left_join(observed_vals, bs_est, by = c("group"="group",
                                          "quantity"="quantity"))
  
}



bind_rows(results) %>%
  #filter(question %in% c("confed", "finalter")) %>%
  filter(quantity == "vc") %>%
  ggplot(aes(x = value, y = group)) + 
  geom_bar(stat = "identity", fill = "pink") + 
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), width = .5) + 
  facet_wrap(~question) + 
  theme_bw() + 
  labs(title = "Vc")

bind_rows(results) %>%
  #filter(question %in% c("confed", "finalter")) %>%
  filter(quantity == "vd") %>%
  ggplot(aes(x = value, y = group)) + 
  geom_bar(stat = "identity", fill = "lightgreen") + 
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), width = .5) + 
  facet_wrap(~question) + 
  theme_bw() + 
  labs(title = "Vd")


bind_rows(results) %>%
  #filter(question %in% c("confed", "finalter")) %>%
  filter(quantity == "omega") %>%
  ggplot(aes(x = value, y = group)) + 
  geom_bar(stat = "identity", fill = "lightblue") + 
  geom_errorbar(aes(xmin = ci.low, xmax = ci.high), width = .5) + 
  facet_wrap(~question) + 
  theme_bw() + 
  labs(title = "Omega")

