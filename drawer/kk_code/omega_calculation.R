

###################################
####### Using Real Data ###########
###################################

#Load in GSS data
g6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")
g8 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta")
g10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")

#Getting partyid variable
gss <- bind_rows(g6 %>%
                   select(id_1, degree_1, partyid_1, partyid_2, partyid_3) %>%
                   mutate(id_1 = paste("06", id_1, sep = "-")),
                 g8 %>%
                   select(id_1, degree_1, partyid_1, partyid_2, partyid_3)%>%
                   mutate(id_1 = paste("08", id_1, sep = "-")),
                 g10 %>%
                   select(id_1, degree_1, partyid_1, partyid_2, partyid_3)%>%
                   mutate(id_1 = paste("10", id_1, sep = "-")))

#Clean the GSS Data
gssb <- gss %>%
  select(id_1, degree_1, partyid_1, partyid_2, partyid_3) %>%
  mutate(across(c(partyid_1, partyid_2, partyid_3),
                ~ifelse(.x == 7, NA, .x)),
         degree_1 = ifelse(degree_1 >= 3, 1, 0)) %>%
  pivot_longer(partyid_1:partyid_3) %>%
  separate(name, into = c("question", "t")) %>%
  mutate(t = as.numeric(t)-2)

#Get a vector of unique IDs that could be
#bootstrapped
g_ids <- unique(gssb$id_1)

gssa <- gssb %>% filter(!is.na(value))

#Estimate multi-level model
m_gss <- lmer(value ~ t + (1 + t|id_1), data = gssa)
predict.data <- gssa %>% mutate(t = 0)

## Predict values using the time midpoint
gssa$yhat_m <- predict(m_gss, predict.data)

## Predict allow for individual slopes
gssa$yhat_s <- predict(m_gss)

#alcualte components of Omega
vd <- 1 - sum((gssa$value - gssa$yhat_m)^2)/sum((gssa$value - mean(gssa$value))^2)
vc <- 1 - sum((gssa$value - gssa$yhat_s)^2)/sum((gssa$value - mean(gssa$value))^2) - vd
omega_original <- vc/(vd + vc)

#Separate data to calculate separate Omega for ba and hs 
gss_ba <- gssa %>% filter(degree_1 == 1)
gss_hs <- gssa %>% filter(degree_1 == 0)

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

observed_vals <- data.frame(group = c("b", "h"),
                            o_omega = c(omega_ba, omega_hs))

# Bootstrap the calculation of Omega
bootstrap <- vector(mode = "list", length = 500)
for (i in 1:500) {
  ids <- sample(g_ids, length(g_ids), replace = TRUE)
  boot_df <- data.frame(id = rep(ids, each = 3),
                        new_id = rep(1:length(g_ids), each = 3),
                        t = rep(-1:1, length(g_ids)))
  boot_df <- left_join(boot_df, gssb, by = c("id" = "id_1", "t"="t")) %>%
    filter(!is.na(value))
  
  m_b <- lmer(value ~ t + (1+t|new_id), data = boot_df)
  
  predict.data <- boot_df %>% mutate(t = 0)
  
  ## Predict values using the time midpoint
  boot_df$yhat_m <- predict(m_b, predict.data)
  
  ## Predict allow for individual slopes
  boot_df$yhat_s <- predict(m_b)
  
  #Separate data to calculate separate Omega for ba and hs 
  boot_ba <- boot_df %>% filter(degree_1 == 1)
  boot_hs <- boot_df %>% filter(degree_1 == 0)
  
  #Calculate omega components for ba holder
  vd_ba_boot <- 1 - sum((boot_ba$value - boot_ba$yhat_m)^2)/
    sum((boot_ba$value - mean(boot_ba$value))^2)
  vc_ba_boot <- 1 - sum((boot_ba$value - boot_ba$yhat_s)^2)/
    sum((boot_ba$value - mean(boot_ba$value))^2) - vd_ba_boot
  omega_ba_boot <- vc_ba_boot/(vd_ba_boot + vc_ba_boot)
  
  #calcualte omega components for hs only
  vd_hs_boot <- 1 - sum((boot_hs$value - boot_hs$yhat_m)^2)/
    sum((boot_hs$value - mean(boot_hs$value))^2)
  vc_hs_boot <- 1 - sum((boot_hs$value - boot_hs$yhat_s)^2)/
    sum((boot_hs$value - mean(boot_hs$value))^2) - vd_hs_boot
  omega_hs_boot <- vc_hs_boot/(vd_hs_boot + vc_hs_boot)
  
  bootstrap[[i]] <- data_frame(vd_b = vd_ba_boot, vc_b = vc_ba_boot, omega_b = omega_ba_boot,
                               vd_h = vd_hs_boot, vc_h = vc_hs_boot, omega_h = omega_hs_boot, 
                               i = i)
  print(i)
}

bs <- bind_rows(bootstrap) %>%
  pivot_longer(-i) %>%
  separate(name, into = c("stat", "group")) %>%
  spread(stat, value)

bs %>%
  ggplot(aes(x = omega)) + 
  geom_histogram(color = "gray", fill = "gray") + 
  geom_vline(data = observed_vals, aes(xintercept = o_omega),
             linetype = 2) + 
  facet_wrap(~group) + 
  theme_bw()