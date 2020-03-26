
source("plot_hospital.R")
source("queueinputs.R")
source("queuemodel-3-23.R")


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
                  growth_rate=log(input$growth_rate))
    
    
    plot_grid(plots[[1]], plots[[2]],plots[[3]],plots[[4]], nrow=2, ncol=2, labels=c('A', 'B', 'C', 'D'), align="hv")
    
  })
}

####################

ui <- fluidPage(
 titlePanel("COVID-19 ICU dynamics"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Main", fluid=TRUE,
          includeMarkdown("content/instructions.md"),
          h4("Scenario:"),
          sliderInput("initrep", "Initial cases per day", min=1, max=1e3, value=1e4),
          sliderInput("finalrep", "Expected cases per day at time horizon", min=1, max=1e3, value=1e4),
          sliderInput("growth_rate", "Growth rate (exponential)", min=1.00, max=1.1, value=1.02),
          sliderInput("time", "Time Horizon",     min=30, max=120, value=60),
          radioButtons("distrib", 
                       "Infection curve",
                       c("Exponential"="exponential",
                         "Ramp"="ramp",
                         "Logistic"="logistic",
                         "Geometric"="geometric",
                         "Uniform"="uniform"),
                       inline=TRUE,
                       selected="exponential"),
		
          h4("Capacity:"),
		includeMarkdown("content/capacity.md"),
          sliderInput("floorcap", "Floor capacity", min=0, max=2500, value=1781),
          sliderInput("icucap", "ICU capacity",     min=0, max=500, value=352)),
          # put more sliderInputs here! 
        tabPanel("Parameters", fluid=TRUE,
          includeMarkdown("content/parameters.md"),
          sliderInput("avgfloordischargetime", "Average time on floor", min=0, max=25, value=7),
          sliderInput("avgicudischargetime", "Average time in ICU",     min=0, max=25, value=10),
        ),
        tabPanel("About", fluid=TRUE,
          includeMarkdown("content/about.md")
        )),width=3),
    mainPanel(plotOutput("hospitalPlot",height="700px"))
  ),
  hr(),
  includeMarkdown("content/footer.md")
)

shinyApp(ui = ui, server = server)


