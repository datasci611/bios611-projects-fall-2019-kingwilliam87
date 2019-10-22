#app!
source("helper_functions.R",local=environment())
library(rsconnect)
deployApp()

ui <- fluidPage(
  titlePanel("Services Provided, 2000-2019"),
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("var", "Service:",
                   choices =
                     c("Clothing"=1,
                       "Diapers"=2,
                       "Food Pounds"=3,
                       "Hygiene Kits"=4,
                       "School Kits"=5
                     ))),
  mainPanel(
    plotOutput("plot1", brush = brushOpts(id = "plot1_brush"))
    )
  )
)

server <- function(input,output) {
  output$plot1 <- renderPlot({
     varplot(input$var)
  })
  
}
  
shinyApp(ui, server)

