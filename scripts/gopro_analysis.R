library(sf)
library(sftrack)
library(nngeo)

track_test <- as_sftrack(as.data.frame(googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1J_vQ2ykeYcq-YdtOcHePyMV0fkG0YEz30Q8FoMFT2sQ/edit?gid=82257187#gid=82257187", 
                                         "TEST")))

plot(track_test, main = "Original")
summary_sftrack(track_test)

nn_results <-  st_nn(track_test$x, track_test$y, k = 4, returnDist = TRUE)

#at every hour, what is the average distance between 

filter_time = as.POSIXct("1899-12-30 12:30:00")

track_test_filter <- track_test %>%
  filter(time == filter_time)
