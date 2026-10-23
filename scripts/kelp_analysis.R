library(mgcv)
library(gratia)
library(ggeffects)
library(DHARMa)




#EXPLORATORY PLOTS #############################################################

#temp values over trials
ggplot(data = kelp_wtemp,
       aes(x = factor(Trial), y = temp_c, color = Temp_treatment)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Mean temp (across all time pts temp measured) per trial",
       caption = "regression line based on linear model") +
  theme_bw()


#kelp by temp trial avg (scale of kelp blade in a trial)
ggplot(data = kelp_blade,
       aes(x = mean_temp_c, y = weight_diff, color = Pred_treatment)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  facet_wrap(~Temp_treatment, scales = "free")


#histogram of reponse variable
ggplot(kelp_blade, aes(x = weight_diff)) +
  geom_histogram() +
  ggtitle("Histogram for kelp weight diff")


ggplot(kelp_tank, aes(x = sum_weight_diff)) +
  geom_histogram() +
  ggtitle("Histogram for sum of kelp weight diff in a tank")





#MODEL #########################################################################


# sum of kelp in tank -----------------------------------------------------

#no
sum_gaussian_mod1 <- glmmTMB::glmmTMB(sum_weight_diff ~ 
                                               mean_temp_c + Pred_treatment + mean_temp_c*Pred_treatment +
                                               sd_temp_c + sd_temp_c*Pred_treatment +
                                               sum_weight_before + Pred_treatment*sum_weight_before +
                                               (1 | Trial),
                                             data = kelp_tank,
                                             family = gaussian(link = "log"))
Anova(sum_gaussian_mod1)


#yes
sum_gaussian_mod2 <- glmmTMB::glmmTMB(sum_weight_diff ~ 
                                        Temp_treatment + Pred_treatment + Temp_treatment*Pred_treatment +
                                        sd_temp_c + sd_temp_c*Pred_treatment +
                                        sum_weight_before + Pred_treatment*sum_weight_before +
                                        (1 | Trial),
                                      data = kelp_tank,
                                      family = gaussian(link = "log"))
Anova(sum_gaussian_mod2)

#NOTE: mod2 has higher chisq than mod1. And I don't think the distribution can rlly be anything but gaussian. Even in most
#simplified version, pred x temp not significant.



# kelp blade unit as response ---------------------------------------------

#yes but aic so high
poisson_mod <- glmmTMB::glmmTMB(weight_diff ~ 
                                  Temp_treatment*Pred_treatment*weight_before + sd_temp_c +
                                  (1 | Trial/tank),
                                data = kelp_blade,
                                family = poisson(link = "log"))
Anova(poisson_mod)
AIC(poisson_mod)
sim_res <- simulateResiduals(fittedModel = poisson_mod)
plot(sim_res)


#NOTE: not sure which of the two above are better, pretty similar. maybe first one.


#no
gaussian_mod <- glmmTMB::glmmTMB(weight_diff ~ 
                                   Temp_treatment*Pred_treatment*weight_before + sd_temp_c +
                                   (1 | Trial/tank),
                                 data = kelp_blade,
                                 family = gaussian(link = "log"))
Anova(gaussian_mod)
AIC(gaussian_mod)

#YES (in maybe a concerning way) - but also AIC is NA???
gaussian_mod2 <- glmmTMB::glmmTMB(weight_diff ~ 
                                    mean_temp_c*Pred_treatment*weight_before*sd_temp_c*Temp_treatment +
                                    (1 | Trial/tank),
                                  data = kelp_blade,
                                  family = gaussian(link = "log"))
Anova(gaussian_mod2)
AIC(gaussian_mod2)


tweedie_mod <- glmmTMB::glmmTMB(weight_diff ~ 
                                  mean_temp_c*Pred_treatment*weight_before*sd_temp_c*Temp_treatment  +
                                  (1 | Trial/tank),
                                data = kelp_blade,
                                family = glmmTMB::tweedie(link = "log"))
Anova(tweedie_mod)
AIC(tweedie_mod)

#low aic
tweedie_mod2 <- glmmTMB::glmmTMB(weight_diff ~ 
                                   Temp_treatment*Pred_treatment*weight_before + sd_temp_c +
                                  (1 | Trial/tank),
                                data = kelp_blade,
                                family = glmmTMB::tweedie(link = "log"))
Anova(tweedie_mod)
AIC(tweedie_mod)

gamma_mod <-  glmmTMB::glmmTMB(weight_diff ~ 
                                 Temp_treatment*Pred_treatment*weight_before + sd_temp_c  +
                                  (1 | Trial/tank),
                                data = kelp_blade,
                                family = Gamma(link = "log"))

Anova(gamma_mod)
AIC(gamma_mod)
sim_res2 <- simulateResiduals(fittedModel = gamma_mod)
plot(sim_res2)



gamma_mod2 <-  glmmTMB::glmmTMB(weight_diff ~ 
                                  mean_temp_c*Pred_treatment*weight_before*sd_temp_c*Temp_treatment  +
                                  
                                 (1 | Trial/tank),
                               data = kelp_blade,
                               family = Gamma(link = "log"))

Anova(gamma_mod2)
AIC(gamma_mod2)

#---------------------------------------------------

predictions_poisson <- ggpredict(poisson_mod,
                              terms = c("Temp_treatment", "Pred_treatment"))

ggplot(predictions_poisson,
       aes(x = x, y = predicted,
           color = group, group = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3,
             position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.2),
    width = 0.15
  ) +
  labs(
    x = "Temperature treatment",
    y = "Estimated kelp weight difference",
    color = "Predator treatment"
  ) +
  theme_bw()


#find sum of kelp eaten per tank


#-------------------------------------------------------------------------------

EMM <- emmeans(mod, ~ temp_c*Pred_treatment  + Trial)
contrast(EMM)

#slope for continous var contrast
emt <- emtrends(mod, pairwise ~ Pred_treatment, var = "temp_c")
summary(emt)
plot(emt)

#plotting predicted values of model
plot_model(mod, type = "pred", terms = c("temp_c","Pred_treatment")) +
  theme_bw() +
  labs(title = "Predicted values of mean difference in kelp weight (g)",
       color = "Predator treatment") +
  xlab("Tank temperature (C)") +
  ylab("Mean difference in kelp weight (g)") 

plot_model(mod, type = "slope", terms = c("temp_c","Pred_treatment")) +
  theme_bw()  


#-------------------------------------------------------------------------------

#adding on ramping history as a co-variate





