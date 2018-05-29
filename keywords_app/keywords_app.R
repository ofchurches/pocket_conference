library(ggplot2)
library(dplyr)

load("pd.Rda")
load("pd_tf.Rda")
load("pd_bi.Rda")
load("pd_tf_bi.Rda")

ui <- fluidPage(
  
  titlePanel("Interview keywords"),
  
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
    
    # Output: Data file ----
    tabsetPanel(type = "tabs", 
                tabPanel("Words",plotOutput("pd_plot"), plotOutput("pd_tf_plot")),
                tabPanel("Bigrams",plotOutput("pd_plot_bi"), plotOutput("pd_tf_plot_bi"))
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
  
  pd_bi_data <- reactive({
    filter(pd_bi, doc_id == input$interview)
  })
  
  pd_tf_bi_data <- reactive({
    filter(pd_tf_bi, doc_id == input$interview)
  })
  
  output$pd_plot <- renderPlot({
    ggplot(data = pd_data(), aes(x = reorder(word, order), y = n)) + 
      geom_bar(show.legend = FALSE, stat = "identity", fill = "aquamarine2") + 
      #facet_wrap(~ doc_id, scales = "free")  +
      # Add categories to axis
      # scale_x_continuous(
      #   breaks = pd$order,
      #   labels = pd$word,
      #   expand = c(0,0)
      # ) + 
      coord_flip() + 
      labs(y = "Number of times the word was used in this interview", x = "", title = "Most frequently used words in each interview")
  })
  
  output$pd_tf_plot <- renderPlot({
    ggplot(data = pd_tf_data(), aes(x = reorder(word, order), y = tf_idf)) + 
      geom_bar(show.legend = FALSE, stat = "identity", fill = "aquamarine2") + 
      #facet_wrap(~ doc_id, scales = "free")  +
      # Add categories to axis
      # scale_x_continuous(
      #   breaks = pd_tf$order,
      #   labels = pd_tf$word,
      #   expand = c(0,0)
      # ) + 
      coord_flip() + 
      labs(y = "tf-idf of words within this interview", x = "", title = "Highest tf-idf words in each review")
  })
  
  output$pd_plot_bi <- renderPlot({
    ggplot(data = pd_bi_data(), aes(x = reorder(word, order), y = n)) + 
      geom_bar(show.legend = FALSE, stat = "identity", fill = "darkslategrey") + 
      #facet_wrap(~ doc_id, scales = "free")  +
      # Add categories to axis
      # scale_x_continuous(
      #   breaks = pd$order,
      #   labels = pd$word,
      #   expand = c(0,0)
      # ) + 
      coord_flip() + 
      labs(y = "Number of times the bigram was used in this interview", x = "", title = "Most frequently used words in each interview")
  })
  
  output$pd_tf_plot_bi <- renderPlot({
    ggplot(data = pd_tf_bi_data(), aes(x = reorder(word, order), y = tf_idf)) + 
      geom_bar(show.legend = FALSE, stat = "identity", fill = "darkslategrey") + 
      #facet_wrap(~ doc_id, scales = "free")  +
      # Add categories to axis
      # scale_x_continuous(
      #   breaks = pd_tf$order,
      #   labels = pd_tf$word,
      #   expand = c(0,0)
      # ) + 
      coord_flip() + 
      labs(y = "tf-idf of words within this interview", x = "", title = "Highest tf-idf bigrams in each review")
  })
  
}

shinyApp(ui = ui, server = server)