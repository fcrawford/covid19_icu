
source("plot_hospital.R")
source("queueinputs.R")
source("queuemodel-3-23.R")


####################

server <- function(input, output) {
  output$hospitalPlot <- renderPlot({
    # put slider control values here as arguments
    plot_hospital(initial_report=input$initrep, 
                  L=input$floorcap, 
                  M=input$icucap)
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
          sliderInput("initrep", "Initial report", min=5e2, max=5e3, value=1e3),
          sliderInput("floorcap", "Floor capacity", min=1, max=500, value=100),
          sliderInput("icucap", "ICU capacity",     min=1, max=500, value=100)
          # put more sliderInputs here! 
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


