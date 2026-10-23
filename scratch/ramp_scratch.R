ramp_temp_dev <- ramp_temp_clean %>%
  left_join(exp_temp_avg, by = c("ramping_set","temp_treatment")) %>%
  mutate(Date = as_date(Date)) %>%
  
  arrange(Trial, ramping_tank_id, Date) %>%
  group_by(Trial, ramping_tank_id) %>%
  
  mutate(day = as.numeric(Date - min(Date)) + 1, 
         
         inflow_temp = first(temp_c),
         
         ramp_length = if (any(abs(temp_c - avg_exp_temp) <= 0.5, na.rm = TRUE)) {
           min(day[abs(temp_c - avg_exp_temp) <= 0.5])} 
         else {max(day, na.rm = TRUE)},
         
         ramping_rate = if_else(
           ramp_length > 1,
           (avg_exp_temp - inflow_temp) / (ramp_length - 1),
           0),
         
         target_temp = pmin(
           inflow_temp + (day - 1) * ramping_rate,
           avg_exp_temp
         ),
         
         daily_temp_change = temp_c - lag(temp_c),
         
         rapid_increase = daily_temp_change > 1,
         
         temp_deviation = if_else(
           day >= ramp_length,
           NA_real_,
           temp_c - target_temp)) %>%
  
  ungroup()



ramp_temp_dev %>%
  group_by(Trial, temp_treatment) %>%
  summarise(
    mean_deviation = mean(temp_deviation, na.rm = TRUE),
    mean_abs_deviation = mean(abs(temp_deviation), na.rm = TRUE),
    max_abs_deviation = max(abs(temp_deviation), na.rm = TRUE),
    .groups = "drop"
  )

ramp_temp_dev %>%
  ggplot(aes(x = Date)) +
  geom_point(
    aes(y = temp_c, color = factor(Trial)),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = target_temp),
    color = "grey",
    linewidth = 0.5
  ) +
  facet_wrap(~ temp_treatment, scales = "free_y") +
  labs(
    x = "Date",
    y = "Temperature (°C)",
    color = "Trial",
    title = "Actual vs. Expected Ramping Temperatures",
    subtitle = "Solid lines = actual; dashed lines = expected"
  ) +
  theme_bw()



ramp_temp_dev %>%
  ggplot(aes(x = day, y = temp_deviation, color = factor(Trial))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ temp_treatment) +
  labs(
    x = "Day of ramping",
    y = "Temperature deviation (°C)",
    color = "Trial",
    title = "Deviation from Expected Ramping Temperature",
    subtitle = "Positive = warmer than expected; negative = cooler than expected"
  ) +
  theme_bw()

ramp_temp_dev %>%
  group_by(Trial, temp_treatment, ramping_tank_id) %>%
  summarise(
    ramp_days = max(day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = factor(Trial),
    y = ramp_days,
    fill = temp_treatment
  )) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    x = "Trial",
    y = "Days of ramping",
    fill = "Temperature treatment",
    title = "Ramping Duration by Trial and Treatment"
  ) +
  scale_y_continuous(breaks = scales::breaks_width(1)) +
  theme_bw()


#summary - don't trust temp_dev just yet need to investigate earlier code more
ramp_summary <- ramp_temp_dev %>%
  group_by(Trial, temp_treatment) %>%
  summarise(trtmt_ramp_dev = mean(abs(daily_temp_change), na.rm = TRUE)) %>%
  ungroup() %>%
  
  #stupid
  rename(Temp_treatment = temp_treatment)
