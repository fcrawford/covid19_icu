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





####################

server_inputs <- function(input, output, session) {
  
  
  output$outputs <- renderText({
    outputs <- Input_generation(total_covid_inpatient=input$total_covid_inpatient,
                                 total_inpatient = input$total_inpatient,
                                 total_covid_icu=input$total_covid_icu,
                                 total_icu = input$total_icu,
                                 total_beds = input$total_beds,
                                 total_beds_icu = input$total_beds_icu) 
    paste("<h4> <b>Initial ICU capacity for COVID19+ patients (number of beds):<br></b> ", 
          outputs$total_covid_beds_icu, 
          "<br><br><b>Initial floor capacity for COVID19+ patients (number of beds):<br></b>  ",
          outputs$total_covid_beds_floor, 
          "<br><br><b>% of initial ICU capacity for COVID19+ patients occupied at time 0:<br></b> ", 
          outputs$covid_icu_percentage, 
          "%<br><br><b>% of initial floor capacity for COVID19+ patients occupied at time 0:<br></b>", 
          outputs$covid_floor_percentage,
          " %</h4>", 
          sep="")  
  })
  
}

####################


generate_ui_inputs <- function() {
  
  fluidPage(theme=shinytheme("simplex"),
            titlePanel("COVID-19 Hospital Capacity Model Input Calculation"),
            sidebarLayout(
              sidebarPanel(
                tabsetPanel(
                  tabPanel("Inputs from dashboard", fluid=TRUE,
                           numericInput("total_beds", "Total inpatient beds (number of beds) ",  min=0, max=10000, step=1, value=8000),
                           numericInput("total_beds_icu", "Total ICU beds (number of beds) ",  min=0, max=3000, step=1, value=2000),
                          numericInput("total_inpatient", "Total inpatient census ",  min=0, max=10000, step=1, value=4000),
                          numericInput("total_icu", "Total ICU census ",  min=0, max=3000, step=1, value=1500),
                          numericInput("total_covid_inpatient", "Total COVID+ inpatient census",  min=0, max=10000, step=1, value=1000),
                          numericInput("total_covid_icu", "Total COVID+ ICU census ",  min=0, max=3000, step=1, value=500)
                  )
                  
                )
              )
            ,
            mainPanel(
              tableOutput("outputs")
              
              
              
              
            )
            )
  )
  
}

#' @export
runApp_inputs <- function() { 

  shinyApp(ui = generate_ui_inputs, server = server_inputs)
}