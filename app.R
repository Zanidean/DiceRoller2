library(shiny)
library(tibble)
library(ggplot2)
library(glue)

ui <- function(request) {fluidPage(
  
  titlePanel("Dice Rolling Simulator"),
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                  .js-irs-0 .irs-bar {background: #CF2A26}")),
  
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, 
                  .js-irs-1 .irs-bar {background: #31B995}")),
  
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, 
                  .js-irs-2 .irs-bar {background: #FDB813}")),
  
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, 
                  .js-irs-3 .irs-bar {background: #58595B}")),
  
  tags$style(".irs-bar-edge {
    height: 8px;
    top: 25px;
    width: 14px;
    border: 1px solid #fff;
    border-right: 0;
    background: #fff;
    border-radius: 16px 0 0 16px;
    -moz-border-radius: 16px 0 0 16px;
}"),
  
  tags$style(".irs-grid-pol {
    opacity: 0.5;
    background: #DBDCDE;}"),
  
  tags$style(".irs-grid-pol.small {
    opacity: 0.5;
    background: #fff;}"),
  
  
  tags$style(".irs-bar {
    height: 8px;
    top: 25px;
    width: 14px;
    border: 1px solid #fff;
    border-right: 0;
    background: #fff;
    border-radius: 16px 0 0 16px;
    -moz-border-radius: 16px 0 0 16px;
}"),
  
  tags$style(".well {background-color:#fff;}"),
  
  tags$style("a {color: #000}"),
  
  sidebarLayout(
    sidebarPanel(width = 3, 
      helpText(HTML("Imagine a world in which you need to illustrate the central limit
               theorem.<br>
               You are given as many dice, with as many sides,
               and as many rolls as you want (within reason!).")),
      hr(),
      sliderInput("die","Number of die:", min = 1, max = 800, value = 2),
      sliderInput("side","Number of sides per die:", min = 1, max = 800, value = 6),
      sliderInput("rolls","Number of rolls:", min = 1, max = 800, value = 30),
      hr(),
      sliderInput("binwidth","Number of Breaks:", min = 5, max = 100, value = 10),
      checkboxInput("densitycurve", "Show Density Curve", value = F),
      #colourInput("barcol", "Bar Colour:", value = "#7ECBB5"),
      #HTML("<a href=\"http://rserver-test.mhc.ab.ca:3838/\">Click to go home</a>"),
      bookmarkButton("Save your place")
      ),
    
    mainPanel(
      plotOutput("distPlot", width = "100%", height = 700),
      textOutput("mean"), textOutput("sd"), textOutput("max"), textOutput("min")
    )
    )
  )}

server <- function(input, output, session) {
 
  p <- reactive({
    
    die <- input$die
    side <- input$side
    rolls <- input$rolls
    p <- c()
    for(i in 1:rolls){
      roll <- function(side, die){
        hand <- c() 
        j <- 0
        repeat{
          number <- round(runif(1, min = 1, max = side), 0)
          hand <- c(number, hand)
          j <- j+1
          if(j == die){break}
        }
        hand_total <- sum(hand)
        return(hand_total)
      }
      q <- roll(side, die)
      p <- c(p, q)
    }
    p
  })

  
  output$distPlot <- renderPlot({
    distplot = ggplot(as_tibble(p()), aes(x=value)) +
      geom_histogram(aes(y=..density..), colour="white", fill="#7ECBB5", bins = input$binwidth) +

      labs(x = "Sum of Dice", y = "Frequency") + 
      ggtitle("Histogram of Dice Rolls",
              glue("Simulation of {input$rolls} rolls of {input$die} die with {input$side} sides each.")) + 
      theme_minimal() + 
      theme(axis.text.x = element_text(size = 16, family = "Arial"),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 17, face = "bold", family = "Arial"),
            plot.title = element_text(size = 24, family = "Arial"),
            plot.subtitle = element_text(size = 20, family = "Arial"))
    
    if(input$densitycurve == T){
      distplot + geom_density(alpha=.5, na.rm = T, fill = "#D1D3D4", color = "white")
    } else {
      distplot
    }


  }, height = 700)
  # 
  # 
  output$mean <- renderText(
    paste0("Mean = ", round(mean(p(), na.rm = T), 2)))
  output$sd <- renderText(
    paste0("Standard Deviation = ", round(sd(p(), na.rm = T), 2)))
  output$min <- renderText(
    paste0("Min. Value = ", round(min(p(), na.rm = T), 0)))
  output$max <- renderText(
    paste0("Max. Value = ", round(max(p(), na.rm = T), 0)))


}

shinyApp(ui = ui, server = server, enableBookmarking = "url")