#manipulated data for PycnoHeat
#by Nikita Sridhar


#temp ###########################################################################

#adding trial and treatment from metadata

#metadata for emergent effects only
metadata_EF <- metadata %>%
  filter(Experiment == "Emergent effects")

exp_temp_clean <- hobo_temp %>%
  mutate(tank = as.factor(tank)) %>% #needed as factor later for model
  left_join(metadata_EF, join_by("tank" == "tank",
                                "date" >= "date_start",
                                "date" <= "date_end")) %>% #i think NA from before/after trial
  drop_na(date_start) 

#since can't track each ramp tank --> exp tank, need to compare ramp set's
#temp treatment w the avg actual temp for that overall temp treatment in exp tanks
#for the given trial.
exp_temp_avg <- exp_temp_clean %>%
  group_by(`Ramping set`,Trial, Temp_treatment, tank) %>%
  summarise(avg_exp_temp = mean(temp_c, na.rm = TRUE),
            sd_exp_temp = sd(temp_c, na.rm = TRUE)) %>%
  rename(temp_treatment = Temp_treatment,
         ramping_set = `Ramping set`)


#calculating ramping daily deviations from target temp
ramp_temp_dev <- ramp_temp_clean %>%
  left_join(exp_temp_avg, by = c("ramping_set","temp_treatment")) %>%
  mutate(Date = as_date(Date),
         day = as.numeric(Date - min(Date)) + 1) %>%
  
  group_by(Date, ramping_set, ramping_tank_id, Trial, temp_treatment) %>%
  summarise(daily_temp = mean(temp_c, na.rm = TRUE), .groups = "drop") %>%

  arrange(ramping_set, ramping_tank_id, Date) %>%
  group_by(ramping_set, ramping_tank_id) %>%
  mutate(daily_temp_change = daily_temp - lag(daily_temp)) %>%
  ungroup() %>%
  
  group_by(Trial, temp_treatment) %>%
  summarise(trial_ramp_temp_dev = mean(abs(daily_temp_change), na.rm = TRUE), .groups = "drop") %>%
  rename(Temp_treatment = temp_treatment)

ggplot(ramp_temp_dev,
       aes(x = Temp_treatment,
           y = trial_ramp_temp_dev,
           group = Trial,
           color = factor(Trial))) +
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  labs(
    title = "Ramp temperature deviation by trial and treatment",
    x = "Temperature treatment",
    y = "Mean absolute daily temperature change (°C)",
    color = "Trial"
  ) +
  scale_x_discrete(limits = c("T1", "T2", "T3")) +
  theme_bw()


#kelp ###########################################################################

#one row per every time pt for temp measurement
kelp_time <- exp_temp_clean %>%
 
   full_join(kelp, join_by("tank" == "Tank",
                          "Trial" == "Trial",
                          "Pred_treatment" == "Pred_treatment",
                          "Temp_treatment" == "Temp_treatment"),
            relationship = "many-to-many") %>%
 
   left_join(ramp_temp_dev, by = c("Trial","Temp_treatment")) %>%
  
  #some summary stats for kelp
  mutate(weight_diff = Kelp_weight_before_g - Kelp_weight_after_g,
         #changing neg values to 0
         weight_diff = case_when(weight_diff <= 0 ~ 0.00001, .default = weight_diff),
         Trial = as.factor(Trial),
         Pred_treatment = as.factor(Pred_treatment),
         Temp_treatment = factor(Temp_treatment, levels = c("T1", "T2", "T3")),
         date = as_date(date_time)) %>%
  
  select(date_time, date, temp_c, Trial, tank,"Ramping set", Temp_treatment, 
         Pred_treatment, unique_trtmt, Replicate, Kelp_ID, Kelp_weight_before_g, 
         Kelp_weight_after_g, weight_diff, trial_ramp_temp_dev) 
  
 # %>% filter(temp_c < 20) #keep?


#one row per kelp blade
kelp_blade <- kelp_time %>% 
  group_by(Trial, tank,`Ramping set`, Temp_treatment, Pred_treatment, 
           unique_trtmt, Replicate, Kelp_ID) %>%
  summarise(mean_temp_c = mean(temp_c, na.rm = TRUE),
            sd_temp_c = sd(temp_c, na.rm = TRUE),
            weight_diff = mean(weight_diff, na.rm = TRUE), #this was repeated per every 30s of temp measurement previously
            weight_before = mean(Kelp_weight_before_g),
            trial_ramp_temp_dev = mean(trial_ramp_temp_dev)) %>%
  ungroup()


#one row per tank during a trial (kelp blades within tank are summed)
kelp_tank <- kelp_blade %>%
  group_by(Trial, tank,`Ramping set`, Temp_treatment, Pred_treatment, 
           unique_trtmt, Replicate) %>%
  summarise(mean_temp_c = mean(mean_temp_c, na.rm = TRUE), #previously same temp was repeated per blade
            sd_temp_c = mean(sd_temp_c, na.rm = TRUE),
            sum_weight_diff = sum(weight_diff, na.rm = TRUE),
            sd_weight_diff = sd(weight_diff, na.rm = TRUE),
            sum_weight_before = sum(weight_before, na.rm = TRUE),
            sd_weight_before = sd(weight_before, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sum_weight_diff = case_when(sum_weight_diff <= 0 ~ 0.00001, .default = sum_weight_diff)) 



