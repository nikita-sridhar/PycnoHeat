#continuous temp effect on kelp grazing

temp <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18vD4OGGKN3MY4Yx75_gLunwMmuHXkH9oBcuUNzi12KM/edit?gid=22340292#gid=22340292", "Experimental tanks") 

metadata_EF <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hYKwyw7xpImw0UkSa-QuuZrkBy2YU88_AIKomJshBQA/edit?gid=0#gid=0", "Trial info") %>% filter(Experiment == "Emergent effects")


avg_trial_temp <- temp %>% 
  pivot_longer(cols = c("Tank 1 temp (C)", "Tank 2 temp (C)","Tank 3 temp (C)",
                        "Tank 4 temp (C)","Tank 5 temp (C)","Tank 6 temp (C)",
                        "Tank 7 temp (C)","Tank 8 temp (C)","Tank 9 temp (C)"),
               names_to = "Tank",
               values_to = "Temp" ) %>%
  mutate(Tank = as.double(substr(Tank, 6,6) )) %>%
  rename(Trial = EF_Trial) %>%
  select(Date, Trial, Time,Tank,Temp) %>%  
  filter(!is.na(Trial)) %>%
  group_by(Trial, Tank) %>%
  summarise(mean_temp = mean(Temp, na.rm = TRUE))


kelp_wtemp <- left_join(kelp, avg_trial_temp, by = c("Tank", "Trial")) %>%
  mutate(pcnt_grazed = ((Kelp_weight_before_g - Kelp_weight_after_g)/ 
                          Kelp_weight_before_g)*100,
         weight_diff = Kelp_weight_before_g - Kelp_weight_after_g) 

kelp_wtemp <- unique(kelp_wtemp)

#PLOT
# temps per trial
ggplot(data = kelp_wtemp,
       aes(x = Trial, y = mean_temp, color = Temp_treatment)) +
  geom_point() 
#mean temp per trial
ggplot(data = kelp_wtemp,
       aes(x = Trial, y = mean_temp, color = Temp_treatment)) +
  geom_point()


#kelp grazed w/ or w/o star at diff mean temps
ggplot(data = kelp_wtemp, aes(x = mean_temp, y = pcnt_grazed, color = Pred_treatment) ) +
  geom_point() +
  geom_abline() +
  theme_classic() +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Pred treatment") 

ggplot(data = kelp_wtemp, aes(x = mean_temp, y = pcnt_grazed) ) +
  stat_summary(fun = mean, geom = "point", position = "dodge") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Pred treatment") + 
  facet_wrap(~ Pred_treatment)


#model

#distribution
ggplot(kelp_wtemp, aes(x = weight_diff)) +
  geom_density() +
  facet_wrap(vars(Pred_treatment,Temp_treatment))

model <- lm(weight_diff ~ 
              
              Pred_treatment + Temp_treatment + 
              Pred_treatment*Temp_treatment,  
            
            data = kelp_wtemp)

summary(model)

#need to do this by the ramping temp also 
ramping_temps <

