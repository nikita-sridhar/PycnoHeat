#manipulated data for PycnoHeat
#by Nikita Sridhar


#temp ###########################################################################

#adding trial and treatment from metadata

#metadata for emergent effects only
metadata_EF <- metadata %>%
  filter(Experiment == "Emergent effects")

exp_temp_hobo_clean <- hobo_temp %>%
  mutate(tank = as.factor(tank)) %>% #needed as factor later for model
  left_join(metadata_EF, join_by("tank" == "tank",
                                "date" >= "date_start",
                                "date" <= "date_end")) %>% #i think NA from before/after trial
  drop_na(date_start) 

#since can't track each ramp tank --> exp tank, need to compare ramp set's
#temp treatment w the avg actual temp for that overall temp treatment in exp tanks
#for the given trial.
exp_temp_avg <- exp_temp_hobo_clean %>%
  group_by(`Ramping set`,Trial, Temp_treatment) %>%
  summarise(avg_exp_temp = mean(temp_c, na.rm = TRUE),
            sd_exp_temp = sd(temp_c, na.rm = TRUE)) %>%
  rename(temp_treatment = Temp_treatment,
         ramping_set = `Ramping set`)


#adding what the avg temp treatment ended up being for the ramp set:
#first, need to add trial approximates
ramp_temp_w_exp <- ramp_temp_clean %>%
  mutate(target_temp = ifelse((parse_number(ramping_set) >= 1), parse_number(ramping_set), NA))
  left_join(exp_temp_avg %>% 
              select(temp_treatment,ramping_set,avg_exp_temp, sd_exp_temp), 
            by = c("temp_treatment", "ramping_set"))
#note: something is off w the target temp (t1 target shouldn't be t2 - double check this code from cleaning is right)

#kelp ###########################################################################

#adding kelp data to temp  - repeated kelp values per temp to keep temp distribution intact (or do I need only 1 temp value per blade)
kelp_wtemp <- exp_temp_hobo_clean %>%
  full_join(kelp, join_by("tank" == "Tank",
                          "Trial" == "Trial",
                          "Pred_treatment" == "Pred_treatment",
                          "Temp_treatment" == "Temp_treatment"),
            relationship = "many-to-many") %>%
  #some summary stats for kelp
  mutate(weight_diff = Kelp_weight_before_g - Kelp_weight_after_g,
         #changing neg values to 0
         weight_diff = case_when(weight_diff <= 0 ~ 0.00001, .default = weight_diff),
         Trial = as.factor(Trial),
         Pred_treatment = as.factor(Pred_treatment)) %>%
  group_by(Trial,tank) %>%
  mutate(avg_weight_diff = mean(weight_diff),
         sd_weight_diff = sd(weight_diff)) %>%
  ungroup()


