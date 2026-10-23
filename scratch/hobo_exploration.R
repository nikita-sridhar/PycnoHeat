#run 00_cleaning and 01_manipulation
#trying to match up hobo ids with tank numbers
library(ggpmisc)

#loading HOBO data ------------------------------------------------------------
hobo_file_paths <- list.files(path = here("data","raw","hobo"),
                              pattern = "\\.csv$",
                              full.names = TRUE)


hobo_gmt <-read_csv(hobo_file_paths,skip=1,col_select = c(2, 3), id = "hobo_id") %>%
  rename("date_time" = "Date Time (GMT-08:00)",
         "temp_f" = "Temp (°F )") %>%
  mutate(temp_c = (temp_f  - 32)/1.8) %>%
  #adding id numbers to diff hobos (naming system is currently arbitrary)
  group_by(hobo_id) %>%
  mutate(hobo_id = cur_group_id()) %>%
  ungroup() 

hobo_utc <- hobo_gmt %>%
  mutate(date_time = mdy_hms(date_time))
  
hobo_pst <- hobo_utc %>%
  with_tz(date_time, tzone = "America/Los_Angeles")


#plotting time series of temp from ysi and hobo to try and match up hobo id to tank number

#putting ysi and hobo in same df so can overlay plots
ysi_wide <- exp_temp_ysi_clean %>%
  pivot_wider(names_from = tank,
              values_from = temp_c,
              names_prefix = "tank_") %>%
  mutate(source = "ysi",
         date_time = as.POSIXct(date_time)) %>%
  select(-EF_Trial,-Date,-Time)

hobo_wide <- hobo_pst %>%
  pivot_wider(names_from = hobo_id,
              values_from = temp_c,
              names_prefix = "hobo") %>%
  mutate(source = "hobo",
         date_time = as.POSIXct(date_time)) %>%
  select(-temp_f)

all_temp <- merge(ysi_wide, hobo_wide, by = c("date_time","source"), all = TRUE) %>%
  group_by(date_time,source) %>%
  summarise(across(everything(), ~mean(.,na.rm=TRUE))) %>%
  ungroup()



#plotting ysi temp
ggplot(exp_temp_ysi_clean, aes(x = date_time, y = Temp_c, color = EF_Trial)) +
  geom_point() +
  geom_smooth()+
  theme_bw() +
  facet_wrap(~Tank) 


#plotting hobo temp
ggplot(hobo, aes(x = date_time, y = temp_c)) +
  geom_point(size = 0.5) +
  geom_smooth()+
  theme_bw() +
  facet_wrap(~hobo_id)

#filtering for certain dates
start_datetime <- as.POSIXct('2025-08-07 11:00:00')
end_datetime <- as.POSIXct('2025-08-24 11:00:00')

all_temp_filter <- all_temp %>% ungroup() %>%
  filter(date_time >= start_datetime & date_time <= end_datetime) 
         

#T3

ggplot(all_temp_filter) +
  
  geom_point(aes(x = date_time, y = tank_3), color = "black", size = 3) + 
  geom_point(aes(x = date_time, y = hobo5),color = "blue", size = 0.8) +
  geom_point(aes(x = date_time, y = hobo3),color = "magenta", size = 0.8) +
  geom_point(aes(x = date_time, y = hobo4),color = "purple", size = 0.8) +
  theme_bw()

#T2

ggplot(all_temp_filter) +
  
  geom_point(aes(x = date_time, y = tank_6), color = "black", size = 2) + 
  geom_point(aes(x = date_time, y = hobo1), color = "orange", size = 0.8) +
  geom_point(aes(x = date_time, y = hobo2),color = "green", size = 0.8) +
  geom_point(aes(x = date_time, y = hobo8),color = "red", size = 0.8) +
  theme_bw()

#T1

ggplot(all_temp_filter) +
  
  geom_point(aes(x = date_time, y = tank_9), color = "black", size = 2) + 
  geom_point(aes(x = date_time, y = hobo7),color = "orange", size = 0.8) +
  geom_point(aes(x = date_time, y = hobo6),color = "maroon", size = 0.8) +
  theme_bw()

  
ggplot(all_temp_filter) +
  
  geom_point(aes(x = date_time, y = tank_9), color = "black", size = 3) + 
  geom_point(aes(x = date_time, y = hobo1), color = "yellow") +
  geom_point(aes(x = date_time, y = hobo2),color = "green") +
  geom_point(aes(x = date_time, y = hobo3),color = "magenta") +
  geom_point(aes(x = date_time, y = hobo4),color = "purple") +
  geom_point(aes(x = date_time, y = hobo5),color = "blue") +
  geom_point(aes(x = date_time, y = hobo6),color = "maroon") +
  geom_point(aes(x = date_time, y = hobo7),color = "orange") +
  geom_point(aes(x = date_time, y = hobo8),color = "red") +
  theme_bw() 
  
  
#mean and sd for tank and hobo data

all_temp_summary <- all_temp %>%
  pivot_longer(cols = 3:19,
               names_to = "id",
               values_to = "temp_c") %>%
  group_by(id) %>%
  summarise(mean = mean(temp_c, na.rm=TRUE),
            sd = sd(temp_c, na.rm = TRUE),
            min = min(temp_c, na.rm=TRUE),
            max = max(temp_c, na.rm = TRUE),
            median = median(temp_c, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(temp_treatment = case_when(id %in% c("tank_1","tank_2","tank_3", "hobo5","hobo3","hobo4") ~ "T3",
                                    id %in% c("tank_4","tank_5","tank_6","hobo1","hobo2","hobo8") ~ "T2",
                                    id %in% c("tank_7","tank_8","tank_9","hobo7","hobo6") ~ "T1"))


#after this exploration (visually and comparing summary stats, deciding on hobo/tank matchups as follows)
#and only adding ysi from tank 9 (no hobo here)

tank9ysi <- exp_temp_ysi_clean%>%filter(tank==9)%>%select(-EF_Trial,-Date,-Time)

hobo_temp <- hobo_pst %>%
  mutate(tank = case_when(hobo_id == 5 ~ 1,
                          hobo_id == 3 ~ 2,
                          hobo_id == 4 ~ 3,
                          hobo_id == 1 ~ 4,
                          hobo_id == 2 ~5,
                          hobo_id == 8 ~ 6,
                          hobo_id == 6 ~ 7,
                          hobo_id == 7 ~ 8)) %>% 
  select(-hobo_id,-temp_f) %>%

#adding ysi measurements from tank 9 (no hobo in this tank)

rbind(tank9ysi) %>%
  mutate(date = as_date(date_time))

