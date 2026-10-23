#analysis using ysi temps for experimental tanks instead of hobo


#temp ###########################################################################

#reshaping and adding metadata

exp_temp_ysi_clean <- exp_temp_ysi %>%
  
  #changing from wide to long format
  pivot_longer(cols = c("Tank 1 temp (C)", "Tank 2 temp (C)","Tank 3 temp (C)",
                        "Tank 4 temp (C)","Tank 5 temp (C)","Tank 6 temp (C)",
                        "Tank 7 temp (C)","Tank 8 temp (C)","Tank 9 temp (C)"),
               names_to = "tank",
               values_to = "temp_c" ) %>%
  mutate(tank = as.double(substr(tank, 6,6) ), #cleaning up so just says "Tank x"
         tank = as.factor(tank)) %>% #needed as factor later for model
  select(EF_Trial, Date, Time, date_time, tank,temp_c) %>%  
  filter(!is.na(EF_Trial))  

#adding metadata. also, I think EF_trial is wrong - matching up tank and date
#from ysi data bc that for sure is right with the metadata to get which 
#experiment was in which tank and which trial 
#%>%left_join(metadata_EF, by = join_by("Tank" == "Tank",
#  "Date" >= "Date start",
#  "Date" <= "Date end")) #investigate why this adds more rows







#finding average temp (over all time pts where temp was taken) per tank per trial
avg_per_trial_temp_ysi <- exp_temp_ysi_clean %>% 
  rename(Trial = EF_Trial) %>% #don't think u should do this bc EF trial could be dif to movement trial num
  
  #finding mean/sd temp over all time pts where temp measured
  group_by(Trial, tank) %>%
  summarise(mean_temp = mean(temp_c, na.rm = TRUE),
            sd_temp = sd(temp_c, na.rm = TRUE)) %>%
  #adding some columns from metadata to temp dataset 
  left_join((metadata_EF%>%select(tank,Trial,Experiment, Pred_treatment,Temp_treatment, unique_trtmt)), 
            by = c("tank", "Trial")) %>%
  #filtering for only emergent effects experiment 
  filter(Experiment == "Emergent effects") %>%
  select(-Experiment)

#avg temp across trials (i.e. avg temp for T1 treatment across trials)
avg_across_trial_temp_ysi <-  avg_per_trial_temp_ysi %>% #using this instead of avg_per-trial_temp bc it has metadata
  group_by(unique_trtmt) %>%
  mutate(exp_mean_temp = mean(mean_temp, na.rm=TRUE), ,
         exp_sd_temp = sd(mean_temp,na.rm=TRUE)) %>%
  #doing this to get data to scale of per unique treatment
  select(-tank, -Trial,-mean_temp,-sd_temp) %>%
  distinct()



#kelp ###########################################################################

#adding temp data to kelp -  per trial
kelp_wtemp <- kelp%>%
  rename(tank= Tank) %>%
  left_join(avg_per_trial_temp_ysi,
            by = c("tank", "Trial","Pred_treatment","Temp_treatment")) %>%
  #remember: this introduced exp_mean_temp var, which is avg T1 temp across all trials of the exp
  left_join(avg_across_trial_temp_ysi, by = c("Pred_treatment","Temp_treatment","unique_trtmt")) %>%
  #some summary stats for kelp
  mutate(pcnt_grazed = ((Kelp_weight_before_g - Kelp_weight_after_g)/ 
                          Kelp_weight_before_g)*100,
         weight_diff = Kelp_weight_before_g - Kelp_weight_after_g,
         #changing neg values to 0
         weight_diff = case_when(weight_diff <= 0 ~ 0, .default = weight_diff)) 

#EXPLORATORY PLOTS #############################################################
#temps per trial
#trial 6 ribbon not showing bc only one temp recording for the trial

ggplot(data = kelp_wtemp,
       aes(x = Trial, y = mean_temp, color = Temp_treatment)) +
  geom_point() +
  geom_errorbar(aes(x = Trial, ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp),
                size = 0.4, width = 0.2, alpha = 0.2) +
  geom_line()+
  labs(title = "Mean temperature per trial") +
  ylab("Mean temperature (C)") +
  labs(color = "Temperature treatment") +
  theme_bw()

theme_bw() +
  theme(legend.title = element_text(size = 9))

ggplot(data = kelp_wtemp,
       aes(x = Trial, y = mean_temp, color = Temp_treatment)) +
  geom_point() +
  geom_smooth(method=lm)+
  labs(title = "Mean temp (across all time pts temp measured) per trial",
       caption = "regression line based on linear model") +
  theme_bw()

#temps across whole exp
ggplot(data = kelp_wtemp,
       aes(x = Temp_treatment, y = exp_mean_temp, color = Pred_treatment)) +
  geom_point() +
  geom_errorbar(aes(x = Temp_treatment, ymin = exp_mean_temp-exp_sd_temp, 
                    ymax = exp_mean_temp+exp_sd_temp), size = 0.4, width = 0.2) +
  labs(title = "Mean temp (across all trials) per temperature treatment",
       caption = "Error bars are exp mean temp +/- 1 SD") +
  theme_bw()

#kelp grazed w/ or w/o star at diff mean temps
ggplot(data = kelp_wtemp, aes(x = mean_temp, y = weight_diff, color = Pred_treatment) ) +
  geom_point() +
  geom_smooth(method = lm, aes(color = Pred_treatment)) +
  theme_bw() +
  ylab("Kelp weight diff (g)") +
  labs(title = "Raw difference in kelp weight with/without predator across temperatures",
       caption = " all trials, raw data") 

#histograms
ggplot(kelp_wtemp, aes(x=weight_diff)) +
  geom_histogram() +
  facet_wrap(vars(unique_trtmt))

#MODEL #########################################################################
#model - kelp blade is level of replication
mod <- glmmTMB::glmmTMB(weight_diff ~ 
                          
                          unique_trtmt  + 
                          
                          (1| Trial/Tank),  
                        
                        data = kelp_wtemp,
                        
                        family = gaussian(link = "identity"))

#model summary table
sjPlot::tab_model(mod)
Anova(mod)

#emmeans and contrasts
EMM <- emmeans(mod, ~ unique_trtmt)

contrast_weights <- list(
  "a) No star T1 vs. Star T1" = c(1,0,0,-1,0,0),
  "b) No star T2 vs. Star T2" = c(0,1,0,0,-1,0),
  "c) No star T3 vs. Star T3" = c(0,0,1,0,0,-1))

#contrast table
contrast(EMM, contrast_weights) %>%
  as.data.frame() %>%
  flextable() %>%
  colformat_double(j = c(2:6), digits = 4) %>%
  autofit()


#joining temp to emmeans for plots
EMM_wtemp_across_trial <- as.data.frame(EMM) %>%
  left_join(avg_across_trial_temp, by = "unique_trtmt")


ggplot(EMM_wtemp_across_trial, aes(x = exp_mean_temp, y = emmean, 
                                   colour = Pred_treatment)) +
  geom_point() +
  geom_text(aes(label = Temp_treatment), vjust = 1.5, size = 4) +
  geom_line() +
  geom_ribbon(aes(x = exp_mean_temp, ymin = lower.CL, ymax = upper.CL, fill = Pred_treatment), 
              alpha = 0.3, linetype = 0) + 
  theme_bw() +
  labs(title = "Least sq mean of kelp grazed by urchins with/without a star at 
       different temperatures",
       
       caption = "Least sq means from a glmm with unique treatment as 
       fixed effect (i.e. Star_T1, NoStar_T1, etc), and tank nested within trial
       as the random effect. Least square means plotted against mean temperature
       in the unique treatment combo across trials (aka mean temperature for Star_T1
       across all trials with confidence interval")



#response = weight diff
#fixed effect: only interaction term
#random effect: tank nested within trial
#run contrasts between comparisons based on hypothesis
#plot the difference between star and no star from least sq with temperature 
#try adding a bunch of other co-variates including continuous temp data and ramping history


########################################################
#analysis 2 (after talking to Marm) ##############

#this time, trying to run model without discrete temp treatment,
#instead with continuous temperature data. and, using a gamma distribution.

kelp_wtemp_forgamma <- kelp_wtemp %>%
  mutate(weight_diff = ifelse(weight_diff <= 0, 0.0000001,weight_diff))

#model - kelp blade is level of replication
mod1 <- glmmTMB::glmmTMB(weight_diff ~ 
                           
                           Pred_treatment*mean_temp  + 
                           
                           (1| Trial/Tank),  
                         
                         data = kelp_wtemp_forgamma,
                         
                         family = Gamma(link = "log"))

#model summary table
sjPlot::tab_model(mod1)
Anova(mod1)

#-------------------------------------------------------------------------------

#instead of including all kelp blades, finding mean of kelp weight diff per tank

kelp_wtemp_summary <- kelp_wtemp %>%
  group_by(Trial, Tank, Pred_treatment, Temp_treatment) %>%
  summarise(tank_mean_kelp_diff = mean(weight_diff),
            tank_sd_kelp_diff = sd(weight_diff),
            tank_mean_temp = mean(mean_temp),#this was already at tank scale (repeated values)
            tank_sd_temp = mean(sd_temp),#this was already at tank scale (repeated values)
            exp_mean_temp = mean(exp_mean_temp),#this was already at tank scale (repeated values)
            exp_sd_temp = mean(exp_sd_temp)) %>%  #this was already at tank scale (repeated values)
  mutate(tank_mean_kelp_diff = ifelse(tank_mean_kelp_diff <= 0, 0.000001, tank_mean_kelp_diff))

#kelp grazed w/ or w/o star at diff mean temps
ggplot(data = kelp_wtemp_summary, aes(x = tank_mean_temp, y = tank_mean_kelp_diff, color = Pred_treatment) ) +
  geom_point() +
  geom_smooth(method = lm, aes(color = Pred_treatment)) +
  theme_bw() +
  ylab("Kelp weight diff (g)") +
  labs(title = "Mean difference in kelp weight across temperatures") 

#histograms - this is looking kind of normal now
ggplot(kelp_wtemp_summary, aes(x=tank_mean_kelp_diff)) +
  geom_histogram() #+
#facet_wrap(vars(Pred_treatment))

#trying model again, this time with mean kelp per tank as response variable
mod2 <- glmmTMB::glmmTMB(tank_mean_kelp_diff ~ 
                           
                           Pred_treatment*tank_mean_temp  + tank_sd_temp  +
                           
                           (1| Trial),  
                         
                         data = kelp_wtemp_summary,
                         
                         family = gaussian(link = "identity"))

#model summary table
sjPlot::tab_model(mod2)
Anova(mod2)

EMM2 <- emmeans(mod2, ~ Pred_treatment*tank_mean_temp  + tank_sd_temp)
contrast(EMM2)

#slope for continous var contrast
emt <- emtrends(mod2, pairwise ~ Pred_treatment, var = "tank_mean_temp")
pairs(emt)

#plotting predicted values of model

plot_model(mod2, type = "pred", terms = c("tank_mean_temp","Pred_treatment")) +
  theme_bw() +
  labs(title = "Predicted values of mean difference in kelp weight (g)",
       color = "Predator treatment") +
  xlab("Mean tank temperature (C)") +
  ylab("Mean difference in kelp weight (g)") 





write.csv(kelp_wtemp, "data/processed/kelp_wtemp.csv")
