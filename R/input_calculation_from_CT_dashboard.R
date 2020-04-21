Input_generation<-function(total_covid_inpatient,
                           total_inpatient,
                           total_covid_icu,
                           total_icu,
                           total_beds,
                           total_beds_icu,
                           ...
){
  # occupancy
  total_covid_floor = total_covid_inpatient- total_covid_icu;
  
  total_noncovid_icu = total_icu- total_covid_icu;
  total_noncovid_floor = total_inpatient - total_icu - total_covid_floor
  
  
  
  # capacity
  total_floor_beds = total_beds - total_beds_icu
  
  total_covid_beds_icu = total_beds_icu - total_noncovid_icu;
  total_covid_beds_floor = total_floor_beds - total_noncovid_floor;
  
  # percentages
  covid_icu_percentage = total_covid_icu / total_covid_beds_icu
  covid_floor_percentage = total_covid_floor / total_covid_beds_floor
  
  return(list(
    total_covid_beds_icu=total_covid_beds_icu,
    total_covid_beds_floor= total_covid_beds_floor,
    covid_icu_percentage=covid_icu_percentage,
    covid_floor_percentage= covid_floor_percentage
  ))
  
}
  
# Input_generation(total_covid_inpatient=1926,
#                  total_inpatient = 5333,
#                  total_covid_icu=533,
#                  total_icu = 948,
#                  total_beds = 7968,
#                  total_beds_icu = 1551)