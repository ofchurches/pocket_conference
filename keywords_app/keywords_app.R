library(ggplot2)

load("pd.Rda")
load("pd_tf.Rda")

ui <- fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("interview", "Select interview:",
                  c( 
                    "Cognitive decline" = "Cognitive decline.txt",
                    "Risks" = "risks.txt", 
                    "Sleep IQ" = "sleep IQ.txt", 
                    "Stimulation" = "stimulation.txt"
                  ), 
                  selectize = FALSE)
      
    ),
    
    mainPanel(
      plotOutput("pd_plot"), 
      plotOutput("pd_tf_plot")
    )
  )
)

server <- function(input, output) {
  
  pd_data <- reactive({
    filter(pd, doc_id == input$interview)
  })
  
  pd_tf_data <- reactive({
    filter(pd_tf, doc_id == input$interview)
  })
  
  output$pd_plot <- renderPlot({
    ggplot(data = pd_data(), aes(x = order, y = n, fill = doc_id)) + 
      geom_bar(show.legend = FALSE, stat = "identity") + 
      #facet_wrap(~ doc_id, scales = "free")  +
      # Add categories to axis
      # scale_x_continuous(
      #   breaks = pd$order,
      #   labels = pd$word,
      #   expand = c(0,0)
      # ) + 
      coord_flip() + 
      labs(y = "Number of times the word was used", x = "", title = "Most frequently used words in each interview")
  })
  
  output$pd_tf_plot <- renderPlot({
    ggplot(data = pd_tf_data(), aes(x = order, y = tf_idf, fill = doc_id)) + 
      geom_bar(show.legend = FALSE, stat = "identity") + 
      #facet_wrap(~ doc_id, scales = "free")  +
      # Add categories to axis
      # scale_x_continuous(
      #   breaks = pd_tf$order,
      #   labels = pd_tf$word,
      #   expand = c(0,0)
      # ) + 
      coord_flip() + 
      labs(y = "tf-idf of words within each review", x = "", title = "Highest tf-idf words in each review")
  })
  
}

shinyApp(ui = ui, server = server)