library(openxlsx)
library(tidyverse)

dat = read.xlsx('J:/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Multiyear data/WatercraftInspectionData_AllYears_Selected_Columns.xlsx')

# Summarize to year, month, station, high risk, mussel fouled

# Convert date / datetime columns
dat_c = dat |> 
  dplyr::mutate(dplyr::across(c(Start_Time,End_Time,TimeOfInspection), openxlsx::convertToDateTime))

recent_date = max(dat_c$TimeOfInspection, na.rm=T)
oldest_date = min(dat_c$TimeOfInspection, na.rm=T)

print(paste0("The current range of inspection data is from ",oldest_date," to ",recent_date))

dat_summarized = dat_c |> 
  mutate(Year = as.numeric(Year)) |> 
  mutate(month = lubridate::month(TimeOfInspection),
         day_of_week = lubridate::wday(TimeOfInspection, label = TRUE)) |> 
  count(Year,
        month,
        day_of_week,
        station = Station,
        mussels = MusselsFound_Ind,
        high_risk = High_Risk_AIS_Ind,
        prev_wb = Previous_Waterbody_1_Name,
        prev_city = Previous_Major_City,
        dest_wb = Destination_Waterbody_1_Name,
        dest_city = Destination_Major_City,
        commhauled = Commercially_Hauled_Ind,
        Non_Motorized_Counter,
        Simple_Counter,
        Complex_Counter,
        Very_Complex_Counter,
        decon_perf = Decontamination_Performed_Ind,
        quarantine_issued = Quarantine_Period_Issued_Ind) |> 
  tidyr::as_tibble()

# Identify which stations are permanent and which are roving.
perm_stats = c("Golden","Radium","Olsen","Yahk",
                       "Osoyoos","Mt. Robson",
                       "Dawson Creek")

# I'm assuming that all stations that are not permanent stations are 
# roving stations; but to be safe, I'm just labelling them as not permanent.

dat_summarized = dat_summarized |> 
  dplyr::mutate(station_type = ifelse(station %in% perm_stats,"permanent","not permanent"))

# Write to qs file.
qs::qsave(dat_summarized, 'www/insp_dat_summarized.qs')
  