#reading in the NPP data from the different cruises.

q1_NPP <- read_delim("1- Data and metadata/Estimates/AeN_Q1_NPP.csv")
q2_NPP <- read_delim("1- Data and metadata/Estimates/AeN_Q2_NPP.csv")
q3_NPP <- read_delim("1- Data and metadata/Estimates/AeN_Q3_NPP.csv")
q4_NPP <- read_delim("1- Data and metadata/Estimates/AeN_Q4_NPP.csv")

#combining into one dataset.

NPP <- bind_rows(q1_NPP,q2_NPP,q3_NPP,q4_NPP)  
NPP <- NPP %>% 
  mutate(
  cruise = c(rep("Q1",22),rep("Q2",14),rep("Q3",28),rep("Q4",18)),
  depth_m = plyr:: round_any(depth_m, 10)) %>% 
  slice(-40)

#joining the ctd, ice, nutrient and photopigment data.
ice <- read_delim("1- Data and metadata/AeN_season_ice_conc.csv")
ctd <- read_delim("1- Data and metadata/CTD/AeN_season_CTD_pstations.csv")
chla <- read_delim("1- Data and metadata/Photopigments/AeN_season_photopigments.csv")
chla <- chla %>% 
  slice(-c(8,251)) %>%
  mutate(
  depth_m = plyr::round_any(depth_m,10)) %>% 
  drop_na()
nut <- read_delim("1- Data and metadata/Nutrients/AeN_season_nutrients.csv")
nut <- nut %>% 
  filter(depth_m<100) %>% 
  drop_na()

NPP <- left_join(NPP,ctd, by=c("cruise", "station", "depth_m"))
NPP <- left_join(NPP,ice, by=c("cruise", "station"))
NPP <- left_join(NPP,chla, by=c("cruise", "station", "depth_m")) 
NPP <- left_join(NPP,nut, by=c("cruise", "station", "depth_m"))
NPP_df <- NPP %>% 
  slice(-77) %>% 
  mutate(
    NPP_ugC_L_h = replace(NPP_ugC_L_h, which(NPP_ugC_L_h<0),0), 
    NPP_ugC_L_d = NPP_ugC_L_h*24,
    NPP_chla = NPP_ugC_L_d/chla_ug_L,
  )
#let's save it for now and figure out why it adds rows
write_xlsx(NPP, "1- Data and metadata/Estimates/AeN_season_primary.xlsx")

###### data exploration #####

#####vertical profiles####
NPP_df %>%
  filter(cruise =="Q1") %>% 
  ggplot(aes(x= depth_m, y=NPP_chla, color = station)) + 
  geom_point(size=4) +
  geom_line() + 
  scale_x_reverse() + 
  coord_flip() + 
  ylim(c(0,75)) + 
  theme_bw() + 
  scale_color_brewer(palette = "Paired")
  
NPP_df %>%
  filter(cruise =="Q2") %>% 
  ggplot(aes(x= depth_m, y=NPP_chla, color = station)) + 
  geom_point(size=4) +
  geom_line() + 
  scale_x_reverse() + 
  coord_flip() + 
  ylim(c(0,75)) + 
  theme_bw() + 
  scale_color_brewer(palette = "Paired")

NPP_df %>%
  filter(cruise =="Q3") %>% 
  ggplot(aes(x= depth_m, y=NPP_chla, color = station)) + 
  geom_point(size=4) +
  geom_line() + 
  scale_x_reverse() + 
  coord_flip() +
  ylim(c(0,75)) + 
  theme_bw() + 
  scale_color_brewer(palette = "Paired")

NPP_df %>%
  filter(cruise =="Q4") %>% 
  ggplot(aes(x= depth_m, y=NPP_chla, color = station)) + 
  geom_point(size=4) +
  geom_line() + 
  scale_x_reverse() + 
  coord_flip() + 
  ylim(c(0,75)) + 
  theme_bw() + 
  scale_color_brewer(palette = "Paired")


####pairwise correlations.####
#NPP and chlorophyll
NPP %>% 
  ggplot(aes(x = chla_ug_L, y = NPP_ugC_L_h,color=cruise)) + 
  geom_point(size=4) + 
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L*d)") +
  xlab("Chlorophyll a concentration (ug/L)") + 
  xlim(c(-0.1,2)) + 
  theme_bw() + 
  theme_bw() + 
  stat_smooth(method = "glm",
              formula = y ~ x,
              geom = "smooth",
              se=F)
#NPP_chla and NO3
NPP_df %>% 
  ggplot(aes(x = NO3_umol_kg, y = NPP_chla,color=cruise)) + 
  geom_point(size=4) + 
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L*d)") +
  xlab("Chlorophyll a concentration (ug/L)") + 
  ylim(0,75) + 
  theme_bw() + 
  theme_bw() + 
  stat_smooth(method = "glm",
              formula = y ~ x,
              geom = "smooth",
              se=F)

#NPP_chla and ice
NPP_df %>% 
  filter(cruise =="Q3") %>% 
  ggplot(aes(x = ice_conc, y = NPP_chla,color=cruise)) + 
  geom_point(size=4) + 
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L*d)") +
  xlab("Chlorophyll a concentration (ug/L)") + 
  ylim(0,75) + 
  theme_bw() + 
  theme_bw() + 
  stat_smooth(method = "glm",
              formula = y ~ x,
              geom = "smooth",
              se=F)
#boxplots.
NPP_df %>%
  filter(cruise == "Q1") %>% 
  ggplot(aes(x = as.factor(station), y = NPP_chla, color = station)) + 
  geom_jitter() +
  stat_summary(fun = "mean", fun.args = list(mult=1), 
    size = 1, position = position_dodge(0.8)
  )+
  scale_color_brewer(palette= "Paired") + 
  ylab("Biomass-specific Net Primary Production (µgC/µgChla-1 d-1)") +
  xlab("station") + 
  theme_bw() + 
  ylim(c(0,40))
NPP_df %>%
  filter(cruise == "Q2") %>% 
  ggplot(aes(x = as.factor(station), y = NPP_ugC_L_d, color = station)) + 
  geom_jitter() +
  stat_summary(fun = "mean", fun.args = list(mult=1), 
               size = 1, position = position_dodge(0.8)
  )+
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L-1d-1)") +
  xlab("station") + 
  theme_bw() + 
  ylim(c(0,40))

NPP_df %>%
  filter(cruise == "Q3") %>% 
  ggplot(aes(x = as.factor(station), y = NPP_ugC_L_d, color = station)) + 
  geom_jitter() +
  stat_summary(fun = "mean", fun.args = list(mult=1), 
               size = 1, position = position_dodge(0.8)
  )+
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L-1d-1)") +
  xlab("station") + 
  theme_bw() + 
  ylim(c(0,40))

NPP_df %>%
  filter(cruise == "Q4") %>% 
  ggplot(aes(x = as.factor(station), y = NPP_chla, color = station)) + 
  geom_jitter() +
  stat_summary(fun = "mean", fun.args = list(mult=1), 
               size = 1, position = position_dodge(0.8)
  )+
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L-1d-1)") +
  xlab("station") + 
  theme_bw() + 
  ylim(c(0,40))

NPP_df %>%
  filter(cruise == c("Q4","Q1")) %>% 
  ggplot(aes(x = as.factor(station), y = NPP_ugC_L_d, color = cruise)) + 
  geom_jitter() +
  stat_summary(fun = "mean", fun.args = list(mult=1), 
               size = 1, position = position_dodge(0.8)
  )+
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L-1d-1)") +
  xlab("station") + 
  theme_bw() + 
  ylim(c(0,40))

NPP_df %>%
  ggplot(aes(x = as.factor(station), y = NPP_ugC_L_d, color = cruise)) + 
  geom_jitter() +
  stat_summary(fun = "mean", fun.args = list(mult=1), 
               size = 1, position = position_dodge(0.8)
  )+
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L-1d-1)") +
  xlab("station") + 
  theme_bw() + 
  ylim(c(0,40))

#jitter plot by water masses, color by cruises.
NPP_df %>%
  drop_na(water_mass) %>% 
  ggplot(aes(x = as.factor(water_mass), y = NPP_ugC_L_d, color = cruise)) + 
  geom_jitter() +
  stat_summary(fun = "mean", fun.args = list(mult=1), 
               size = 1, position = position_dodge(0.8)
  )+
  scale_color_brewer(palette= "Paired") + 
  ylab("Net Primary Production (ugC/L-1d-1)") +
  xlab("station") + 
  theme_bw() + 
  ylim(c(0,40))

NPP_df %>%
  drop_na(water_mass) %>% 
  ggplot(aes(x = as.factor(water_mass), y = NPP_ugC_L_d, fill = cruise, color = cruise)) + 
  geom_boxplot() + 
  scale_color_brewer(palette= "Paired") +
  scale_fill_brewer(palette = "Paired") + 
  ylab("Net Primary Production (ugC/L-1d-1)") +
  xlab("Water mass") + 
  theme_bw() + 
  ylim(c(0,18))



NPP_df %>%
  drop_na(water_mass) %>% 
  dplyr::mutate(
    season = recode(cruise,
                    "Q1" = "March",
                    "Q2" = "May",
                    "Q3" = "August",
                    "Q4" = "December"
                    )) %>% 
  ggplot(aes(x = as.factor(cruise), y = NPP_chla, fill = water_mass, color = water_mass)) + 
  geom_boxplot() + 
  scale_color_brewer(palette= "Paired") +
  scale_fill_brewer(palette = "Paired") + 
  ylab(bquote(Chlorophyll-normalised~Net~Primary~Production~(µgC~"*"~µgChla^-1~"*"~d^-1))) +
  xlab("Month") + 
  theme_bw() + 
  ylim(c(0,18))

NPP_df %>%
  drop_na(water_mass) %>% 
  dplyr::mutate(
    season = recode(cruise,
                    "Q1" = "March",
                    "Q2" = "May",
                    "Q3" = "August",
                    "Q4" = "December"
    )) %>% 
  ggplot(aes(x = as.factor(cruise), y = NPP_chla, fill = station, color = station)) + 
  geom_boxplot() + 
  scale_color_brewer(palette= "Paired") +
  scale_fill_brewer(palette = "Paired") + 
  ylab("Chlorophyll-normalised Net Primary Production (µgC*µgChla-1*d-1)") +
  xlab("Month") + 
  theme_bw() + 
  ylim(c(0,18))

#### multivariate analysis ####
#checking for correlations within the explanatory variables.
#performing the multiple regression/RDA.
rda_df <- NPP_df %>% 
  select(c(cruise,station,depth_m,asal, ctemp,water_mass,ice_conc,chla_ug_L,NO3_umol_kg,PO4_umol_kg,Si_umol_kg,NPP_ugC_L_d,NPP_chla)) %>% 
  drop_na()

rda_df %>% 
  select(cruise,asal,ctemp,ice_conc,NO3_umol_kg,Si_umol_kg) %>% 
  scale(center=T,scale=T) %>%
  as.data.frame()-> rda_exp
rda_df %>% 
  select(NPP_chla) %>% 
  scale(center=T,scale=T)-> rda_resp
rownames(rda_resp) <- paste(rda_df$cruise, rda_df$station, rda_df$depth_m)

rda <- rda(rda_resp ~ .,data= rda_exp, scale=T, na.action= na.exclude)
summary(rda)

#multiple linear regression.

#overall.
mlr_all <- lm(formula =  rda_resp~ asal + ctemp + ice_conc + NO3_umol_kg + Si_umol_kg, data = rda_exp)
summary(mlr_all)
#by cruise.
exp_q1 <- rda_exp %>% 
  slice(c(1:22))
resp_q1 <- rda_resp[1:22] 
mlr_q1 <- lm(formula =resp_q1~ asal + ctemp + ice_conc + NO3_umol_kg + Si_umol_kg, data = exp_q1)
summary(mlr_q1)

exp_q2 <- rda_exp %>% 
  slice(c(23:33))
resp_q2 <- rda_resp[23:33] 
mlr_q2 <- lm(formula =resp_q2~ asal + ctemp + ice_conc + NO3_umol_kg + Si_umol_kg, data = exp_q2)
summary(mlr_q2)

exp_q3 <- rda_exp %>% 
  slice(c(34:62))
resp_q3 <- rda_resp[34:62] 
mlr_q3 <- lm(formula =resp_q3~ asal + ctemp + ice_conc + NO3_umol_kg + Si_umol_kg, data = exp_q3)
summary(mlr_q3)

exp_q4 <- rda_exp %>% 
  slice(c(63:77))
resp_q4 <- rda_resp[63:77] 
mlr_q4 <- lm(formula =resp_q4~ asal + ctemp + ice_conc + NO3_umol_kg + Si_umol_kg, data = exp_q4)
summary(mlr_q4)

q1_coeff <- as_tibble(summary(mlr_q1)$coefficients)
q1_coeff %>% 
  mutate(
    variable = c("Intercept",
                 "Salinity",
                 "Temperature",
                 "Ice concentration",
                 "Nitrate",
                 "Silicic acid")
  ) %>% 
  filter(variable != "Intercept") %>%  
  ggplot(aes(variable, Estimate)) + 
  geom_point(size = 3) + 
  geom_hline(yintercept=0, linetype = "dashed") + 
  coord_flip() + 
  ylim(c(-0.0015,0.0015)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  xlab("") + 
  ylab("Standardised regression coefficient")

q2_coeff <- as_tibble(summary(mlr_q2)$coefficients)

q2_coeff %>% 
  mutate(
    variable = c("Intercept",
                 "Salinity",
                 "Temperature",
                 "Ice concentration",
                 "Nitrate",
                 "Silicic acid")
  ) %>% 
  filter(variable != "Intercept") %>%  
  ggplot(aes(variable, Estimate)) + 
  geom_point(size = 3) + 
  geom_hline(yintercept=0, linetype = "dashed") + 
  coord_flip() + 
  ylim(c(-32,32)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  xlab("") + 
  ylab("Standardised regression coefficient")

q3_coeff <- as_tibble(summary(mlr_q3)$coefficients)
q3_coeff %>% 
  mutate(
    variable = c("Intercept",
                 "Salinity",
                 "Temperature",
                 "Ice concentration",
                 "Nitrate",
                 "Silicic acid")
  ) %>% 
  filter(variable != "Intercept") %>%  
  ggplot(aes(variable, Estimate)) + 
  geom_point(size = 3) + 
  geom_hline(yintercept=0, linetype = "dashed") + 
  coord_flip() + 
  ylim(c(-0.4,0.4)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  xlab("") + 
  ylab("Standardised regression coefficient")


q4_coeff <- as_tibble(summary(mlr_q4)$coefficients)
q4_coeff %>% 
  mutate(
    variable = c("Intercept",
                 "Salinity",
                 "Temperature",
                 "Ice concentration",
                 "Nitrate",
                 "Silicic acid")
  ) %>% 
  filter(variable != "Intercept") %>%  
  ggplot(aes(variable, Estimate)) + 
  geom_point(size = 3) + 
  geom_hline(yintercept=0, linetype = "dashed") + 
  coord_flip() + 
  ylim(c(-1.5,1.5)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  xlab("") + 
  ylab("Standardised regression coefficient")


