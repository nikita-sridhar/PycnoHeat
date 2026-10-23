#Data cleaning script for PycnoHeat (expeirment run in Sitka summer of 2025)
#By Nikita Sridhar
#8/1/25


#metadata - loading and cleaning------------------------------------------------
metadata <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hYKwyw7xpImw0UkSa-QuuZrkBy2YU88_AIKomJshBQA/edit?gid=0#gid=0", 
                                              "Trial info") %>% 
  rename("Pred_treatment" = "Pred treatment (for EF)") %>%
  rename("Temp_treatment" = "Temp Treatment") %>%
  rename(time_start = "Time start") %>%
  rename(time_end = "Time end") %>%
  rename(date_start = "Date start") %>%
  rename(date_end = "Date end") %>%
  mutate(tank = as.factor(Tank),
         Pred_treatment = ifelse(Pred_treatment == "No Star", "No_star",Pred_treatment),
         unique_trtmt = paste(Pred_treatment,Temp_treatment,sep = "_"),
         time_end = ifelse(is.na(time_end),"9:30:00 AM", time_end),
         date_start = ymd(date_start, tz = "America/Los_Angeles"),
         date_end = ymd(date_end, tz = "America/Los_Angeles"),
         date_time_start = ymd_hms(paste(date_start, time_start), tz = "America/Los_Angeles"),
         date_time_end = ymd_hms(paste(date_end, time_end)), tz = "America/Los_Angeles") %>%
  select(-Tank)

#kelp - loading and cleaning----------------------------------------------------
kelp <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hYKwyw7xpImw0UkSa-QuuZrkBy2YU88_AIKomJshBQA/edit?gid=1832909041#gid=1832909041",
                                  "Kelp weight") %>%
  mutate(Tank = as.factor(Tank)) %>%
  select(Trial, Kelp_ID,Tank,Pred_treatment,Temp_treatment,Kelp_weight_before_g,
         Kelp_weight_after_g)

#ysi exp
exp_temp_ysi <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18vD4OGGKN3MY4Yx75_gLunwMmuHXkH9oBcuUNzi12KM/edit?gid=22340292#gid=22340292", 
                                          "Experimental tanks") %>%
  mutate(Time = ifelse(is.na(Time),"9:30:00 AM", Time),
         date_time = ymd_hms(paste(Date, Time), tz = "America/Los_Angeles"))

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



#adding YSI ramping temperatures to the dataset ------------------------------------
ramp_temp_ysi <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18vD4OGGKN3MY4Yx75_gLunwMmuHXkH9oBcuUNzi12KM/edit?gid=22340292#gid=22340292", 
                                       "Ramping tanks")

ramp_temp_clean <- ramp_temp_ysi %>%
  select(-"Observer", -"Time", -contains("DO"), -"Notes") %>%
  pivot_longer(cols = 4:15, #wide to long
               names_to = "tank_temp_id",
               values_to = "temp_c") %>%
  separate(col = tank_temp_id,#cleaning up tank and temp treatment cols
           into = c("ramping_tank_id","temp_treatment"),
           sep = "T") %>%
  mutate(ramping_tank_id = as.numeric(substring(ramping_tank_id,2,)),
         temp_treatment = paste0("T",(substring(temp_treatment,1,1))),
         ramping_set = str_sub(if_else(ramping_tank_id <= 6, `R1-6 set`, `R7-12 set`), 1, 1),
         target_temp = str_extract(if_else(ramping_tank_id <= 6, 
                                           `R1-6 set`, 
                                           `R7-12 set`),
                                   "(?<=\\().*?(?=\\))")) %>%
 select(-`R1-6 set`,-`R7-12 set`) %>%
 filter(!is.na(temp_c))

#adding HOBO temp data ---------------------------------------------------------
#note: hobos were only in experimental tanks

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

#and only adding ysi from tank 9 (no hobo here)
tank9ysi <- exp_temp_ysi_clean%>%filter(tank==9)%>%select(-EF_Trial,-Date,-Time)

#to see how I matched HOBO and exp tanks, see hobo_exploration.R in scratch folder
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



rm(exp_temp_ysi)
rm(hobo_gmt)
rm(hobo_pst)
rm(hobo_utc)
rm(tank9ysi)
rm(hobo_file_paths)
