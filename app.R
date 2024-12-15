library(shiny)

# Define UI for application that optimizes and graphs a quadratic function
ui <- fluidPage(
  
  # Application title
  titlePanel("Métodos de Optimización: Mínimos y Máximos de una Función Cuadrática"),
  
  # Sidebar with slider inputs for coefficients a, b, c
  sidebarLayout(
    sidebarPanel(
      sliderInput("a", "Coeficiente a (curvatura):", 
                  min = -10, max = 10, value = 1, step = 0.1),
      sliderInput("b", "Coeficiente b (pendiente):", 
                  min = -10, max = 10, value = 0, step = 0.1),
      sliderInput("c", "Coeficiente c (intercepto):", 
                  min = -10, max = 10, value = 0, step = 0.1)
    ),
    
    # Show the plot and results
    mainPanel(
      plotOutput("quadPlot"),        # Gráfico de la función
      verbatimTextOutput("result")   # Texto con el vértice (mínimo/máximo)
    )
  )
)

# Define server logic to optimize and graph a quadratic function
server <- function(input, output) {
  
  # Calcular el vértice de la función cuadrática
  vertex <- reactive({
    a <- input$a
    b <- input$b
    c <- input$c
    
    if (a == 0) {
      return("El coeficiente 'a' no puede ser cero (no es una parábola).")
    }
    
    x_vertex <- -b / (2 * a)
    y_vertex <- a * x_vertex^2 + b * x_vertex + c
    type <- ifelse(a > 0, "mínimo", "máximo")
    
    list(x = x_vertex, y = y_vertex, type = type)
  })
  
  # Renderizar el gráfico de la función cuadrática
  output$quadPlot <- renderPlot({
    a <- input$a
    b <- input$b
    c <- input$c
    
    x <- seq(-10, 10, length.out = 500)
    y <- a * x^2 + b * x + c
    
    plot(x, y, type = "l", col = "blue", lwd = 2,
         xlab = "x", ylab = "f(x)", 
         main = "Gráfica de la Función Cuadrática")
    
    # Agregar el vértice al gráfico
    vert <- vertex()
    if (!is.character(vert)) {
      points(vert$x, vert$y, col = "red", pch = 19, cex = 1.5)
      text(vert$x, vert$y, labels = paste0("(", round(vert$x, 2), ", ", round(vert$y, 2), ")"),
           pos = 4, col = "red")
    }
  })
  
  # Mostrar el resultado del vértice
  output$result <- renderText({
    vert <- vertex()
    if (is.character(vert)) {
      return(vert)
    } else {
      paste("El punto", vert$type, "de la función es:",
            sprintf("(x = %.2f, y = %.2f)", vert$x, vert$y))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
