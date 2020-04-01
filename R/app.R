


####################

server <- function(input, output, session) {

    params = yaml.load_file( system.file("content/parameter_values.yaml", package='covid19icu') )

    observe({
        updateSliderInput(session, "floorcapramp", max=input$time)
        updateSliderInput(session, "icucapramp", max=input$time)

        # check if numeric inputs are NA
        if(is.na(input$floorcap)) { 
          updateNumericInput(session, "floorcap", value=params$L)
         } 
        if(is.na(input$icucap)) { 
          updateNumericInput(session, "icucap", value=params$M)
        }




        if(is.na(input$floorcaptarget)) { 
          updateNumericInput(session, "floorcaptarget", value=params$L)
         } 

        if(!is.na(input$floorcaptarget) && !is.na(input$floorcap) && input$floorcaptarget<input$floorcap) {
          updateNumericInput(session, "floorcaptarget", value=input$floorcap)
        }


        if(is.na(input$icucaptarget)) { 
          updateNumericInput(session, "icucaptarget", value=params$M)
        } 

       if(!is.na(input$icucaptarget) && !is.na(input$icucap) && input$icucaptarget<input$icucap) { 
          updateNumericInput(session, "icucaptarget", value=input$icucap)
        }
        
      })


  output$hospitalPlot <- renderPlot({
    # put slider control values here as arguments
    plots<- plot_hospital(initial_report=input$initrep,
                  final_report=input$finalrep,
                  L=ifelse(is.na(input$floorcap),params$L,input$floorcap),
                  M=ifelse(is.na(input$icucap),params$M,input$icucap),
                  distribution=input$distrib,
                  t= input$time,
                  chi_C=1/input$avgicudischargetime,
                  chi_L=1/input$avgfloordischargetime,
                  growth_rate=log(2)/(input$doubling_time),
            			mu_C1 = input$ICUdeath_young,
            			mu_C2 = input$ICUdeath_medium,
            			mu_C3 = input$ICUdeath_old,
            			rampslope = input$rampslope,
            			Cinit = input$Cinit,
            			Finit = input$Finit,
            			Lfinal=ifelse(is.na(input$floorcaptarget),params$L,input$floorcaptarget),
            			Lramp=input$floorcapramp,
            			Mfinal=ifelse(is.na(input$icucaptarget),params$M,input$icucaptarget),
            			Mramp=input$icucapramp,
                  doprotocols=input$doprotocols
			
			)
    



    plot_grid(plots[[1]], plots[[2]],plots[[3]],plots[[4]], nrow=2, ncol=2, labels=c('A', 'B', 'C', 'D'), align="hv")

  })
  
  output$hospitalTable <- renderTable({
    
    text = text_hospital(initial_report=input$initrep,
                  final_report=input$finalrep,
                  L=ifelse(is.na(input$floorcap),params$L,input$floorcap),
                  M=ifelse(is.na(input$icucap),params$M,input$icucap),
                  distribution=input$distrib,
                  t= input$time,
                  chi_C=1/input$avgicudischargetime,
                  chi_L=1/input$avgfloordischargetime,
                  growth_rate=log(2)/(input$doubling_time),
                  mu_C1 = input$ICUdeath_young,
                  mu_C2 = input$ICUdeath_medium,
                  mu_C3 = input$ICUdeath_old,
                  rampslope = input$rampslope,
                  Cinit = input$Cinit,
                  Finit = input$Finit,
            			Lfinal=ifelse(is.na(input$floorcaptarget),params$L,input$floorcaptarget),
                  Lramp=input$floorcapramp,
            			Mfinal=ifelse(is.na(input$icucaptarget),params$M,input$icucaptarget),
                  Mramp=input$icucapramp,
                  doprotocols=input$doprotocols)
  })

  output$keypoints <- renderText({
    dat = text_hospital(initial_report=input$initrep,
                  final_report=input$finalrep,
                  L=ifelse(is.na(input$floorcap),params$L,input$floorcap),
                  M=ifelse(is.na(input$icucap),params$M,input$icucap),
                  distribution=input$distrib,
                  t= input$time,
                  chi_C=1/input$avgicudischargetime,
                  chi_L=1/input$avgfloordischargetime,
                  growth_rate=log(2)/(input$doubling_time),
                  mu_C1 = input$ICUdeath_young,
                  mu_C2 = input$ICUdeath_medium,
                  mu_C3 = input$ICUdeath_old,
                  rampslope = input$rampslope,
                  Cinit = input$Cinit,
                  Finit = input$Finit,
            			Lfinal=ifelse(is.na(input$floorcaptarget),params$L,input$floorcaptarget),
                  Lramp=input$floorcapramp,
            			Mfinal=ifelse(is.na(input$icucaptarget),params$M,input$icucaptarget),
                  Mramp=input$icucapramp,
                  doprotocols=input$doprotocols)

    rownames(dat) = dat$Variable
    
    dat$Value = as.character(dat$Value)
    
    if (dat["Days to floor overflow","Value"] == "No shortage"){
      dat["Days to floor overflow","Value"] = "a time beyond the simulation"
    } else {dat["Days to floor overflow","Value"] = paste(dat["Days to floor overflow","Value"], " days")}
    
    if (dat["Days to ICU overflow","Value"] == "No shortage"){
      dat["Days to ICU overflow","Value"] = "a time beyond the simulation"
    } else {dat["Days to ICU overflow","Value"] = paste(dat["Days to ICU overflow","Value"], " days")}

    paste("<h4> </br> <b> Key points: </b> Under the specified capacities and expansion strategy, 
          the model predicts that <b> ICU beds will reach capacity at ", 
          dat["Days to ICU overflow","Value"], 
          "</b>, and <b> floor beds at ",
          dat["Days to floor overflow","Value"], 
          "</b>. The model predicts <b>", 
          dat["Total deaths","Value"], 
          " deaths </b> and a hospital <b> case-fatality rate of ",
          dat["Case fatality ratio","Value"], 
          " </b>. </h4>", sep="")

  })


}

####################


generate_ui <- function(params) {

fluidPage(theme=shinytheme("simplex"),
 titlePanel("COVID-19 Hospital Capacity Model"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Scenario", fluid=TRUE,
          includeMarkdown(system.file("content/instructions.md", package='covid19icu')),
          h4("Scenario:"),
          sliderInput("time", "Time Horizon (days)",     min=1, max=params$T_Max, step=1, value=params$T),
          radioButtons("distrib",                     "Infection curve",
                       c("Exponential"="exponential",
                         "Linear"="ramp",
                         "Saturated"="logistic",
                         "Flat"="uniform"),
                       inline=TRUE,
                       selected="exponential"),
          sliderInput("initrep", "Initial cases per day", min=1, max=params$I_initmax, value=params$I_init),
          conditionalPanel(
            condition = "input.distrib=='logistic'",
            sliderInput("finalrep", "Peak number of cases", min=1, max=params$I_finalmax, value=params$I_final)
            ),
	conditionalPanel(
            condition = "input.distrib=='ramp'",
            sliderInput("rampslope", "Rate of increase in new cases per day", min=params$rampslope_min, max=params$rampslope_max, value=params$rampslope, step = params$rampslope_step)
            ),
          conditionalPanel(
            condition = "input.distrib == 'exponential'",
            sliderInput("doubling_time", "Doubling time (days)", min=params$doublingtime_min, max=params$doublingtime_max, value=params$doublingtime, step=0.1)
            ),

        ),
        tabPanel("Capacity", fluid=TRUE,
		      includeMarkdown(system.file("content/capacity.md", package='covid19icu')),

          	

		numericInput("icucap", "Initial ICU capacity (number of beds)",  min=0, max=params$M_Max, step=1, value=params$M),
		numericInput("floorcap", "Initial floor capacity (number of beds)", min=0, max=params$L_Max, step=1, value=params$L),

		sliderInput("Cinit", "% of ICU capacity occupied at time 0",     min=0, max=100, value=params$M_occupied),
		sliderInput("Finit", "% of floor capacity occupied at time 0",     min=0, max=100, value=params$L_occupied)),

        tabPanel("Strategy", fluid=TRUE,
          includeMarkdown(system.file("content/protocols.md", package='covid19icu')),
          radioButtons("doprotocols", "Capacity expansion strategy",
                       c("Off"=0, "On"=1),
                       inline=TRUE,
                       selected=0),
          conditionalPanel(
            condition = "input.doprotocols==1",
            numericInput("icucaptarget",  "Target ICU capacity", min=0, max=params$M_Max, step=1, value=params$M),

            sliderInput("icucapramp",  "ICU capacity scale-up (days)", min=0, max=30, value=c(params$icucapramp1,params$icucapramp2)),
            numericInput("floorcaptarget",  "Target floor capacity", min=0, max=params$L_Max, step=1, value=params$L),

            sliderInput("floorcapramp",  "Floor capacity scale-up (days)", min=0, max=30, value=c(params$floorcapramp1,params$floorcapramp2))
          )),
          
        tabPanel("Parameters", fluid=TRUE,
          includeMarkdown(system.file("content/parameters.md", package='covid19icu')),
          sliderInput("avgfloordischargetime", "Average time on floor", min=0, max=params$maxfloordischargetime, value=params$avgfloordischargetime),
          sliderInput("avgicudischargetime", "Average time in ICU",     min=0, max=params$maxicudischargetime, value=params$avgicudischargetime),
		sliderInput("ICUdeath_young", "Death rate in ICU (<18 years)",     min=0, max=1, value=params$mu_C1),
		sliderInput("ICUdeath_medium", "Death rate in ICU (18-64 years)",     min=0, max=1, value=params$mu_C2),
		sliderInput("ICUdeath_old", "Death rate in ICU (65+ years)",     min=0, max=1, value=params$mu_C3),
        )),width=4),
    mainPanel(
    tabsetPanel(
       tabPanel("Plots", fluid=TRUE,
         plotOutput("hospitalPlot",height="700px")
       ),
       tabPanel("Summary", fluid=TRUE,
			includeMarkdown(system.file("content/summary.md", package='covid19icu')),
                tableOutput("hospitalTable")
                ),
    tabPanel("Inputs", fluid=TRUE,
             includeMarkdown(system.file("content/inputs.md", package='covid19icu'))
    ),
    tabPanel("Outputs", fluid=TRUE,
             includeMarkdown(system.file("content/outputs.md", package='covid19icu'))
    ),
	    tabPanel("About", fluid=TRUE,
       includeMarkdown(system.file("content/queue_graphic.md", package='covid19icu'))
       )
    )
  )),
  htmlOutput("keypoints"),

  hr(),
  includeMarkdown(system.file("content/footer.md", package='covid19icu'))
)
}

#' @export
runApp <- function() { 
  params = yaml.load_file( system.file("content/parameter_values.yaml", package='covid19icu') )
  shinyApp(ui = generate_ui(params), server = server)
}
