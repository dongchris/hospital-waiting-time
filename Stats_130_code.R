# library declarations
library("plyr")
library("stringr")
library("lubridate")

# import data
# make sure that both data sets are properly named
event_report <- read.csv("2013 event report.csv", na.strings = "",
                         sep=";", dec=",", stringsAsFactors=FALSE,)
svh_census <- read.csv("SVHCENSUS.csv", stringsAsFactors=FALSE)
# convert column names to lower caess for easier programming
names(event_report) <- tolower(names(event_report))
names(svh_census) <- tolower(names(svh_census))

# FIN_nbr is unique to visits.
# this is the id for each patient's visit
# we define the waiting time to be from
# when the patient arrives at the hospital to when he is attended by a doctor
# arr_time is included in every event record
# so the events we are interested in is MSEI
# also there are some orders cancelled
# so we would like to final_event_status to be complete
event_interest_index <- which(event_report$event_name == "MSEI" & 
                                event_report$final_event_status == "Complete")
event_interest <- event_report[event_interest_index, ]
# we notice that there are multiple request for doctors for a same visit
# we assume that the patient get attended when the doctor responds to the first
# request
# therefore we get any duplicates after the first encounter
fin_duplicates_index <- duplicated(event_interest$fin_nbr)
event_interest <- event_interest[!fin_duplicates_index, ]
# now there are no duplicates
anyDuplicated(event_interest$fin_nbr)
# define the arrival date time
arrive_date_time <- str_c(event_interest$arr_time, event_interest$arr_date, 
                          sep = " ")
arrive_date_time <- as.POSIXct(strptime(arrive_date_time, "%R %m/%d/%y"))
# Check NA
anyNA(arrive_date_time)
# also define start date time
start_date_time <- str_c(event_interest$start_time, event_interest$start_date, 
                         sep = " ")
start_date_time <- as.POSIXct(strptime(start_date_time, "%R %m/%d/%y"))
# check NA
anyNA(arrive_date_time)
# add arrival date time to dataset
event_interest$arr_date_time <- arrive_date_time
# add start_date_time to dataset
event_interest$start_date_time <- start_date_time
# compute waiting time
wait_time <- as.numeric(difftime(event_interest$start_date_time, 
                                 event_interest$arr_date_time, units = "mins"))
# check NA
anyNA(wait_time)
# add wait time to dataset
event_interest$wait_time <- wait_time
# date_hour_event
date_hour_event <- format(event_interest$arr_date_time, format = "%H %m/%d/%y")
event_interest$date_hour <- date_hour_event
# date_hour_census
# check NA in census date hour
anyNA(svh_census$datetime)
# there is duplicate in svh_census
cencus_dup_index <- which(duplicated(svh_census$datetime, fromLast = T))
# take a look
svh_census[svh_census$datetime == svh_census$datetime[cencus_dup_index], ]
# we should preserve the latest update
svh_census <- svh_census[-cencus_dup_index, ]
date_time_census <- as.POSIXct(strptime(svh_census$datetime, 
                                        format = "%Y-%m-%d %R:%S"))
# date_hour_census
date_hour_census <- format(date_time_census, format = "%H %m/%d/%y")
svh_census$date_hour <- date_hour_census

# merge two datasets
project_data <- join(event_interest, svh_census, "date_hour", "left")
# check NA in count
anyNA(project_data$count)
# take a look
names(project_data)
# determine variables to delete
variables_delete <- c("arr_date", "arr_time", "start_time", "start_date", 
                      "date_hour", "datetime", "dow", "hour", "day")
variables_delete_index <- which(names(project_data) %in% variables_delete)
project_data <- project_data[, -variables_delete_index]
# get date of birth
dob <- as.Date(project_data$dob, format = "%m/%d/%y")
yob <- as.numeric(format(dob, format = "%Y"))
# some years before 1968 are incorrectly imported as in 21st century
# correct those years
# first look for the latest year in the dataset
current_year <- max(year(project_data$start_date_time))
wrong_year_index <- which(yob > current_year)
# decrement incorrect years by 100 years
yob[wrong_year_index] <- yob[wrong_year_index] - 100
# age
age <- year(project_data$arr_date_time) - yob
# plug year to data
project_data$age <- age

# some pecularities of the dataset
# some waiting times were suspiciously long
head(sort(project_data$wait_time, decreasing = T), n = 100L)
# we take a look at those record in which the waiting time is more than 10 hours
project_data[which(project_data$wait_time >= 600), 
             c("arr_date_time", "start_date_time", "wait_time")]
# we cannot be sure about the exceptionally long waiting time was due to
#   recording error or something else, so we are to keep those records
# but we are almost sure that the observation with waiting time more than 2 months
#  is due to recording error, so we delete this observation
record_del_index <- which(project_data$wait_time >= 60 * 24 * 60)
project_data <- project_data[-record_del_index, ]
# some waiting time are negative
negative_time_index <- which(project_data$wait_time < 0)
project_data$wait_time[negative_time_index]
# we believe that these were data errors and we reverse the sign
project_data$wait_time[negative_time_index] <- 
  -project_data$wait_time[negative_time_index]

# add a indicator variable for acuity 2-Very Urgent for Part b
project_data$very_urgent <- NA
project_data$very_urgent[complete.cases(project_data$acuity)] <- 0
very_urgent_index <- which(project_data$acuity == "2-Very Urgent")
project_data$very_urgent[very_urgent_index] <- 1
table(project_data$very_urgent, useNA = 'ifany')

# save the data
saveRDS(object = project_data, file = "project_data.RDS")
