
source("plot_hospital.R")
source("queueinputs.R")
source("queuemodel-3-23.R")

library(shinythemes)


####################

server <- function(input, output) {
  output$hospitalPlot <- renderPlot({
    # put slider control values here as arguments
    plots<- plot_hospital(initial_report=input$initrep,
                  final_report=input$finalrep,
                  L=input$floorcap,
                  M=input$icucap,
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
			Finit = input$Finit)


    plot_grid(plots[[1]], plots[[2]],plots[[3]],plots[[4]], nrow=2, ncol=2, labels=c('A', 'B', 'C', 'D'), align="hv")

  })

}

####################

ui <- fluidPage(theme=shinytheme("simplex"),
 titlePanel("COVID-19 ICU dynamics"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Main", fluid=TRUE,
          includeMarkdown("content/instructions.md"),
          h4("Scenario:"),
          sliderInput("time", "Time Horizon (days)",     min=1, max=60, value=30),
          radioButtons("distrib",                     "Infection curve",
                       c("Exponential"="exponential",
                         "Linear"="ramp",
                         "Saturated"="logistic",
                         "Flat"="uniform"),
                       inline=TRUE,
                       selected="exponential"),
          sliderInput("initrep", "Initial cases per day", min=1, max=1e3, value=50),
          conditionalPanel(
            condition = "input.distrib=='geometric'||input.distrib=='logistic'",
            sliderInput("finalrep", "Peak number of cases", min=1, max=3000, value=1000)
            ),
	conditionalPanel(
            condition = "input.distrib=='ramp'",
            sliderInput("rampslope", "Rate of increase in new cases per day", min=0, max=5, value=1.2, step = .1)
            ),
          conditionalPanel(
            condition = "input.distrib == 'exponential'",
            sliderInput("doubling_time", "Doubling time (days)", min=3, max=28, value=14)
            ),

        ),
        tabPanel("Capacity", fluid=TRUE,
		      includeMarkdown("content/capacity.md"),
          sliderInput("floorcap", "Floor capacity", min=0, max=15000, value=100),
          sliderInput("icucap", "ICU capacity",     min=0, max=3000, value=50),
		sliderInput("Cinit", "% of ICU capacity occupied at time 0",     min=0, max=100, value=12),
		sliderInput("Finit", "% of floor capacity occupied at time 0",     min=0, max=100, value=56)),
        tabPanel("Parameters", fluid=TRUE,
          includeMarkdown("content/parameters.md"),
          sliderInput("avgfloordischargetime", "Average time on floor", min=0, max=25, value=7),
          sliderInput("avgicudischargetime", "Average time in ICU",     min=0, max=25, value=10),
		sliderInput("ICUdeath_young", "Death rate in ICU (<18 years)",     min=0, max=1, value=.1),
		sliderInput("ICUdeath_medium", "Death rate in ICU (18-64 years)",     min=0, max=1, value=.1),
		sliderInput("ICUdeath_old", "Death rate in ICU (65+ years)",     min=0, max=1, value=.1),
        )),width=3),
    mainPanel(
    tabsetPanel(
       tabPanel("Plots", fluid=TRUE,
         plotOutput("hospitalPlot",height="700px")
       ), 
    tabPanel("About", fluid=TRUE,
       # CHANGE THIS
       includeMarkdown("content/instructions.md")
       )
    )
  )),
  hr(),
  includeMarkdown("content/footer.md")
)

shinyApp(ui = ui, server = server)
