
source("plot_hospital.R")
source("queueinputs.R")
source("queuemodel-3-23.R")


####################

server <- function(input, output) {
  output$hospitalPlot <- renderPlot({
    # put slider control values here as arguments
    plot_hospital(initial_report=input$initrep, 
                  final_report=input$finalrep,
                  L=input$floorcap, 
                  M=input$icucap,
                  distribution=input$distrib,
                  t= input$time)
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
          sliderInput("initrep", "Initial report", min=1, max=5e3, value=1e5),
          sliderInput("finalrep", "Final report", min=1, max=5e3, value=1e5),
          sliderInput("floorcap", "Floor capacity", min=0, max=2500, value=1781),
          sliderInput("icucap", "ICU capacity",     min=0, max=500, value=352),
          sliderInput("time", "Days",     min=30, max=120, value=60),
          # put more sliderInputs here! 
          radioButtons("distrib", 
                       "Infection curve",
                       c("Ramp"="ramp",
                         "Logistic"="logistic",
                         "Geometric"="geometric",
                         "Uniform"="uniform"),
                       inline=TRUE,
                       selected="ramp")
        ),
        tabPanel("Parameters", fluid=TRUE,
          includeMarkdown("content/parameters.md")
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


