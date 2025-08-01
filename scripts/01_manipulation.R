#manipulated data for PycnoHeat
#by Nikita Sridhar
#8/1/25

#kelp
kelp_processed <- kelp %>%
  mutate(pcnt_grazed = ((Kelp_weight_before_g - Kelp_weight_after_g)/ 
                          Kelp_weight_before_g)*100) 
