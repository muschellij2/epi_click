library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)

N = 200
data = data.frame(x = runif(N, min = 0, max = 10),
                  y = rnorm(N),
                  region = factor(sample(1:5, N, replace = TRUE)),
                  stringsAsFactors = FALSE)
data = arrange(data, region, x, y)

g = ggplot(data, aes(x = x, y = y, colour = region)) + geom_line()
g

ui = fluidPage(
  titlePanel("Hey"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p("Click the button to begin cropping the image")
    ),
    mainPanel(
      plotOutput("plot1", 
                 # brush = brushOpts(
                 #   id = "plot1_brush",
                 #   resetOnNew = TRUE
                 # ),
                 click = "plot1_click",
                 dblclick = "plot1_dblclick"
                 
                 )
    )
  )
)


server = function(input, output, session) {
  
  v <- reactiveValues(
    imgclick.x = NULL,
    imgclick.y = NULL,
    dbl.x = NULL,
    dbl.y = NULL
  )
  
  
  ### Keep track of click locations if tracing paw or tumor 
  observeEvent(input$plot1_click, {
    # Keep track of number of clicks for line drawing
    v$imgclick.x <- c(v$imgclick.x, input$plot1_click$x)
    v$imgclick.y <- c(v$imgclick.y, input$plot1_click$y)
  })

  
  make_data = reactive({
    df = data.frame(x = v$imgclick.x,
                    y = v$imgclick.y,
                    stringsAsFactors = FALSE)
    df
  })
  
  observeEvent(input$plot1_dblclick, {
    print(input$plot1_dblclick)
    rm.x = input$plot1_dblclick$x
    rm.y = input$plot1_dblclick$y
    dist = (v$imgclick.x - rm.x) ^ 2 + (v$imgclick.y - rm.y) ^ 2
    rm_point = which.min(dist)
    v$imgclick.x = v$imgclick.x[-rm_point]
    v$imgclick.y = v$imgclick.y[-rm_point]
    
  })

  ### Original Image
  output$plot1 <- renderPlot({

    df = make_data()    
    # print(df)
    if (nrow(df) > 0) {
      df$region = factor(1, levels = levels(data$region))
      df = arrange(df, region, x, y)
      print("in there")
      print("df is")
      print(df)
      gg = g + 
        geom_point(data = df, 
                          aes(x = x, y = y, colour = region)) +
        geom_line(data = df, 
                   aes(x = x, y = y, colour = region))        
    } else {
      gg = g
    }
    gg
  })
  
}

shinyApp(ui = ui, server = server)
