library(shiny)
library(markdown)

# UI ----
ui <- fluidPage(
  navbarPage("<title>__navbarPage__",
             tabPanel(title="<tab_1>",
                      sidebarLayout(
                        sidebarPanel("__sidebarPanel__",
                                     radioButtons(inputId = "plotType", 
                                                  label   = "<radioButton_label>",
                                                  choices = c("Scatter" = "p", 
                                                              "Line"    = "l"))),
                        mainPanel("__mainPanel__",
                                  plotOutput(outputId = "plot", 
                                             width    = "100%", 
                                             height   = "400px", 
                                             click    = NULL, 
                                             dblclick = NULL, 
                                             hover    = NULL, 
                                             brush    = NULL, 
                                             inline   = F)
                        ))),
             tabPanel(title="<tab_2>",
                      verbatimTextOutput(outputId = "summary"), 
                      #value = title, 
                      icon = NULL),
             navbarMenu(title = "<more_tabs>",
                        #menuName = title,
                        tabPanel(title="<tab_3>",
                                 DT::dataTableOutput(outputId = "table")),
                        tabPanel(title="<tab_4>",
                                 fluidRow(column(6,
                                                 includeMarkdown("about.md"), 
                                                 offset = 0),
                                          column(3,
                                                 img(class="img-polaroid",
                                                       src=paste0("http://upload.wikimedia.org/",
                                                                  "wikipedia/commons/9/92/",
                                                                  "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                                 tags$small("Source: Photographed at the Bay State Antique ",
                                                            "Automobile Club's July 10, 2005 show at the ",
                                                            "Endicott Estate in Dedham, MA by ",
                                                            a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                                              "User:Sfoskett")), 
                                                 offset = 0)
                                 )
                        )
             )
  )
)

# SERVER----
server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}



# APP ----
shinyApp(ui = ui, server = server)
