library(shiny)
library(mathjaxr)
library(tidyverse)
ui <- fluidPage(
  titlePanel("Solow-Modell"),
  withMathJax(helpText(
    "Cobb-Douglas-Produktionsfunktion: \n $$AK^\\alpha+L^{1-\\alpha}$$"
  )),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "k", label = "Maximum von k", value = 0, min = 0),
      sliderInput(
        inputId="alpha", label="alpha", value = 0.5, min = 0.01, max = 0.99
        ),
      sliderInput(
        inputId = "a", label = "A", value = 1, min = 0.01, max = 9.99
        ),
      sliderInput(
        inputId = "s", label = "s", value = 0.3, min = 0.01, max = 0.99
        ),
      sliderInput(
        inputId = "delta", label = "delta", value = 0.3, min = 0.01, max = 0.99
        ),
      sliderInput(
        inputId = "n", label = "n", value = 0.3, min = 0.01, max = 0.99
        ),
      sliderInput(
        inputId = "g", label = "g", value = 0.3, min = 0.01, max = 0.99
        ),
      tags$head(tags$style(
        type = 'text/css',
        'form.well { max-height: 600px; overflow-y: auto; }'
      ))
    ),
    mainPanel(
      plotOutput('graph', click = "plot_click"),
      fluidRow(
        column(width = 6,h4("steady state"), tableOutput('table')),
        column(
          width = 6, h4("click values"), verbatimTextOutput("click_info")
          ),
      ),
      tableOutput("info")
    )
  )
)

server <- function(input, output){
  dat <- reactive({
    max_k <-
      (input$a / (input$delta + input$n + input$g))**(1 / (1 - input$alpha))
    k <- seq(0, ifelse(input$k == 0, max_k*2, input$k), 0.01)
    y <- input$a * k ** input$alpha
    sy <- input$s * y
    dk <- (input$delta + input$n + input$g) * k
    k_star <-
      (
        (input$s * input$a) / (input$delta + input$n + input$g)
      ) ** (1 / (1 - input$alpha))
    y_star <- input$a*(k_star ** input$alpha)
    data_table <- data.frame(k, y, sy, dk, k_star, y_star)
    data_table
  })
  output$graph <- renderPlot({
    bgc <- ifelse(input$alpha == input$s, "#FFD700", "white")
    ggplot(data = dat(), aes(x = k)) +
      geom_line(aes(y = y, colour = "y"), size = 1) +
      geom_line(aes(y = sy, colour = "sy"), size = 1) +
      geom_line(aes(y = dk, colour = "(d+n+g)k")) +
      geom_segment(
        aes(x = k_star, y = 0, xend = k_star, yend = y_star),
        linetype = "dashed", colour = "black"
        ) +
      geom_segment(
        aes(x = 0, y = y_star, xend = k_star, yend = y_star),
        linetype = "dashed", colour = "black"
      ) +
      geom_point(aes(x=k_star, y=y_star), size=2) +
      theme(plot.background = element_rect(fill = bgc))
  })
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    cx <- as.numeric(input$plot_click["x"])
    cy <- input$a * (cx ** (1/(1 - input$alpha)))
    cs <- input$s * cy
    cd <- (input$delta + input$g + input$n) * cx
    data.frame(
      "k" = cx,
      "y" = cy,
      "sy" = cs,
      "depr" = cd
    )
  })
  output$table <- renderTable({
    tmp <- dat()
    equil <- data.frame(
      "k" = tmp$k_star,
      "y" = tmp$y_star,
      "c" = tmp$y_star * (1 - input$s),
      "s" = tmp$y_star * input$s
      )[1, ]
  })
}
shinyApp(ui = ui, server = server)

