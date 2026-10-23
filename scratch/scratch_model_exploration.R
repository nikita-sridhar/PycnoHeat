ggplot(kelp_tank, aes(x = sum_weight_diff)) +
  geom_histogram() #normal

kelp_tank_formod <- kelp_tank %>%
  mutate(Pred_treatment = factor(Pred_treatment),
         Temp_treatment = factor(Temp_treatment, levels = c("T1", "T2", "T3")),
         sum_weight_diff = ifelse(sum_weight_diff == 0, 0.0001, sum_weight_diff))


mod_gaussian_continuous_ramp <- glmmTMB::glmmTMB(weight_diff ~ 
                                                   mean_temp_c + Pred_treatment + mean_temp_c*Pred_treatment +
                                                   mean_weight_before + mean_temp_c*mean_weight_before +
                                                   mean_temp_c*Pred_treatment*mean_weight_before + trial_ramp_temp_dev +
                                                   (1 | Trial) + (1 | tank),
                                                 data = kelp_blade_formod,
                                                 family = gaussian(link = "log"))


mod_gaussian_continuous <- glmmTMB::glmmTMB(weight_diff ~ 
                                              mean_temp_c + Pred_treatment + mean_temp_c*Pred_treatment +
                                              mean_weight_before + mean_temp_c*mean_weight_before +
                                              mean_temp_c*Pred_treatment*mean_weight_before +
                                              (1 | Trial) + (1 | tank),
                                            data = kelp_blade_formod,
                                            family = gaussian(link = "log"))

#temp as categorical not continuous here
mod_poisson <- glmmTMB::glmmTMB(weight_diff ~ 
                                  Temp_treatment + Pred_treatment + Temp_treatment*Pred_treatment +
                                  mean_weight_before + Temp_treatment*mean_weight_before +
                                  Temp_treatment*Pred_treatment*mean_weight_before +
                                  (1 | Trial) + (1 | tank),
                                data = kelp_blade_formod,
                                family = poisson(link = "log"))

#this is the one according to Pete
mod_gaussian <- glmmTMB::glmmTMB(weight_diff ~ 
                                   Temp_treatment + Pred_treatment + Temp_treatment*Pred_treatment +
                                   mean_weight_before + Temp_treatment*mean_weight_before +
                                   Temp_treatment*Pred_treatment*mean_weight_before +
                                   (1 | Trial) + (1 | tank),
                                 data = kelp_blade_formod,
                                 family = gaussian(link = "log"))

#simplifying above model
mod_gaussian2 <- glmmTMB::glmmTMB(weight_diff ~ 
                                    Temp_treatment + Pred_treatment + Temp_treatment*Pred_treatment +
                                    mean_weight_before + Temp_treatment*mean_weight_before  +
                                    (1 | Trial) + (1 | tank),
                                  data = kelp_blade_formod,
                                  family = gaussian(link = "log"))

mod_tweedie <- glmmTMB::glmmTMB(weight_diff ~ 
                                  Temp_treatment + Pred_treatment + Temp_treatment*Pred_treatment +
                                  mean_weight_before + Temp_treatment*mean_weight_before  +
                                  (1 | Trial) + (1 | tank),
                                data = kelp_blade_formod,
                                family = glmmTMB::tweedie(link = "log"))



#w sum of kelp in tank
mod_gaussian_tank <- glmmTMB::glmmTMB(sum_weight_diff ~ 
                                        Temp_treatment + Pred_treatment + Temp_treatment*Pred_treatment +
                                        sum_weight_before + Temp_treatment*sum_weight_before +
                                        Temp_treatment*Pred_treatment*sum_weight_before +
                                        (1 | Trial) + (1 | tank),
                                      data = kelp_tank_formod,
                                      family = gaussian(link = "log"))

mod_gaussian_tank_simple <- glmmTMB::glmmTMB(sum_weight_diff ~ 
                                               Temp_treatment + Pred_treatment + Temp_treatment*Pred_treatment +
                                               (1 | Trial) + (1 | tank),
                                             data = kelp_tank_formod,
                                             family = gaussian(link = "log"))


mod_tweedie_tank <- glmmTMB::glmmTMB(sum_weight_diff ~ 
                                       Temp_treatment + Pred_treatment + Temp_treatment*Pred_treatment +
                                       sum_weight_before + Temp_treatment*sum_weight_before +
                                       Temp_treatment*Pred_treatment*sum_weight_before +
                                       (1 | Trial) + (1 | tank),
                                     data = kelp_tank_formod,
                                     family = glmmTMB::tweedie(link = "log"))

mod_tweedie_tank_simple <- glmmTMB::glmmTMB(
  sum_weight_diff ~ Temp_treatment + Pred_treatment +  Temp_treatment*Pred_treatment +sum_weight_before +
    (1 | Trial) + (1 | tank),
  data = kelp_tank_formod,
  family = glmmTMB::tweedie(link = "log")
)

############################
#10/22

gaussian_tank_continuous <- glmmTMB::glmmTMB(sum_weight_diff ~ 
                                               mean_temp_c + Pred_treatment + mean_temp_c*Pred_treatment +
                                               sd_temp_c + sd_temp_c*Pred_treatment +
                                               sum_weight_before + Pred_treatment*sum_weight_before +
                                               (1 | Trial),
                                             data = kelp_tank_formod,
                                             family = gaussian(link = "log"))
