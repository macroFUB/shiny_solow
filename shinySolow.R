library(shiny)
library(mathjaxr)
library(tidyverse)
ui <- fluidPage(
  titlePanel("Solow-Modell"),
  withMathJax(helpText(
    "Cobb-Douglas-Produktionsfunktion:\n $$K^\\alpha+(L\\times E)^{1-\\alpha}$$"
  )),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId="k", label="Maximum von k", value=0, min=0),
      conditionalPanel(
        condition="input.fixFunc == 0",
        sliderInput(
          inputId="alpha", label="alpha", value=0.5, min=0.1, max=0.9,
          step=0.1
        ),
        sliderInput(
          inputId="s", label="s", value=0.3, min=0.1, max=0.9,
          step=0.1
        ),
        sliderInput(
          inputId="delta", label="delta", value=0.3, min=0.05,
          max=0.95, step=0.05
        ),
        sliderInput(
          inputId="n", label="n", value=0.3, min=0.05, max=0.95,
          step=0.05
        ),
        sliderInput(
          inputId="g", label="g", value=0.3, min=0.05, max=0.95,
          step=0.05
        ),
        actionButton("fixFunc", "aktuelle Parameter fixieren")
      ),
      conditionalPanel(
        condition="input.fixFunc == 1",
        uiOutput("slider_alpha"),
        uiOutput("slider_s"),
        uiOutput("slider_delta"),
        uiOutput("slider_n"),
        uiOutput("slider_g")
      ),
    tags$i("Um die Fixierung zurueckzusetzen, laden Sie die Seite bitte neu."),
    tags$head(tags$style(
      type='text/css',
      'form.well { max-height: 600px; overflow-y: auto; }'
    ))
  ),
    mainPanel(
      conditionalPanel(
        condition="input.fixFunc == 0",
        plotOutput('graph', click="plot_click"),
        fluidRow(
          column(width=6, h4("steady state"), tableOutput('table')),
          column(
            width=6, h4("click values"), verbatimTextOutput("click_info")
          )
        ),
        tableOutput("info")
      ),
      conditionalPanel(
        condition="input.fixFunc == 1",
        plotOutput('graph2', click="plot_click2"),
        fluidRow(
          column(width=6, h4("steady state"), tableOutput('table2')),
          column(
            width=6, h4("click values"), verbatimTextOutput("click_info2")
          )
        ),
        tableOutput("info2")
      )
    )
  )
)

server <- function(input, output){
  dat <- reactive({
    max_k <-
      (1 / (input$delta + input$n + input$g)) ** (1 / (1 - input$alpha))
    k <- seq(0, ifelse(input$k == 0, max_k*2, input$k), .1)
    y <- k ** input$alpha
    sy <- input$s * y
    dk <- (input$delta + input$n + input$g) * k
    k_star <-
      (
        (input$s) / (input$delta + input$n + input$g)
      ) ** (1 / (1 - input$alpha))
    y_star <- (k_star ** input$alpha)
    data_table <- data.frame(k, y, sy, dk, k_star, y_star)
    data_table
  })
  dat2 <- reactive({
    max_k <-
      (1 / (input$delta + input$n + input$g)) ** (1 / (1 - input$alpha))
    k <- seq(0, ifelse(input$k == 0, max_k*2, input$k), .1)
    y <- k ** input$alpha
    sy <- input$s * y
    dk <- (input$delta + input$n + input$g) * k
    k_star <-
      (
        (input$s) / (input$delta + input$n + input$g)
      ) ** (1 / (1 - input$alpha))
    y_star <- (k_star ** input$alpha)
    y_2 <- k ** input$alpha_2
    sy_2 <- input$s_2 * y_2
    dk_2 <- (input$delta_2 + input$n_2 + input$g_2) * k
    k_star_2 <-
      (
        (input$s_2) / (input$delta_2 + input$n_2 + input$g_2)
      ) ** (1 / (1 - input$alpha_2))
    y_star_2 <- (k_star_2 ** input$alpha_2)
    data_table_2 <- data.frame(
      k, y, sy, dk, k_star, y_star,
      y_2, sy_2, dk_2, k_star_2, y_star_2
      )
    data_table_2
  })
  output$graph <- renderPlot({
    bgc <- ifelse(input$alpha == input$s, "#FFD700", "white")
    ggplot(data=dat(), aes(x=k)) +
      geom_line(aes(y=y, colour="y"), size=1) +
      geom_line(aes(y=sy, colour="sy"), size=1) +
      geom_line(aes(y=dk, colour="(d+n+g)k")) +
      geom_segment(
        aes(x=k_star, y=0, xend=k_star, yend=y_star),
        linetype="dashed", colour="black"
      ) +
      geom_segment(
        aes(x=0, y=y_star, xend=k_star, yend=y_star),
        linetype="dashed", colour="black"
      ) +
      geom_point(aes(x=k_star, y=y_star), size=2) +
      theme(plot.background=element_rect(fill=bgc))
  })
  output$graph2 <- renderPlot({
    bgc <- ifelse(input$alpha_2 == input$s_2, "#FFD700", "white")
    ggplot(data=dat2(), aes(x=k)) +
      geom_line(aes(y=y, colour="y"), linetype="dotted", size=1) +
      geom_line(aes(y=sy, colour="sy"), linetype="dotted", size=1) +
      geom_line(aes(y=dk, colour="(d+n+g)k"), linetype="dotted", size=1) +
      geom_segment(
        aes(x=k_star, y=0, xend=k_star, yend=y_star),
        linetype="dashed", colour="black"
      ) +
      geom_segment(
        aes(x=0, y=y_star, xend=k_star, yend=y_star),
        linetype="dashed", colour="black"
      ) +
      geom_point(aes(x=k_star, y=y_star), size=2) +
      theme(plot.background=element_rect(fill=bgc)) +
      geom_line(aes(y=y_2, colour="y_2"), size=1) +
      geom_line(aes(y=sy_2, colour="sy_2"), size=1) +
      geom_line(aes(y=dk_2, colour="(d+n+g)k_2"), size=1) +
      geom_segment(
        aes(x=k_star_2, y=0, xend=k_star_2, yend=y_star_2),
        linetype="dashed", colour="red"
      ) +
      geom_segment(
        aes(x=0, y=y_star_2, xend=k_star_2, yend=y_star_2),
        linetype="dashed", colour="red"
      ) +
      geom_point(aes(x=k_star_2, y=y_star_2), size=2, colour="red")
  })
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    cx <- as.numeric(input$plot_click["x"])
    cy <- (cx ** (1/(1 - input$alpha)))
    cs <- input$s * cy
    cd <- (input$delta + input$g + input$n) * cx
    data.frame(
      "k"=cx,
      "y"=cy,
      "sy"=cs,
      "depr"=cd
    )
  })
  output$click_info2 <- renderPrint({
    cat("input$plot_click2:\n")
    cx_2 <- as.numeric(input$plot_click2["x"])
    cy_2 <- (cx_2 ** (1/(1 - input$alpha_2)))
    cs_2 <- input$s_2 * cy_2
    cd_2 <- (input$delta_2 + input$g_2 + input$n_2) * cx_2
    data.frame(
      "k"=cx_2,
      "y"=cy_2,
      "sy"=cs_2,
      "depr"=cd_2
    )
  })
  output$table <- renderTable({
    tmp <- dat()
    equil <- data.frame(
      "k"=tmp$k_star,
      "y"=tmp$y_star,
      "c"=tmp$y_star * (1 - input$s),
      "s"=tmp$y_star * input$s
    )[1, ]
  })
  output$table2 <- renderTable({
    tmp <- dat2()
    equil <-data.frame(
      cbind(
        tmp$k_star_2,                        # new k
        tmp$k_star,                          # old k
        tmp$k_star_2 - tmp$k_star            # delta
      )[1, ],
      cbind(
        tmp$y_star_2,                        # new y
        tmp$y_star,                          # old y
        tmp$y_star_2 - tmp$y_star            # delta
      )[1, ],
      cbind(
        tmp$y_star_2 * (1 - input$s_2),      # new c
        tmp$y_star * (1 - input$s),          # old c
        tmp$y_star_2 * (1 - input$s_2) - tmp$y_star * (1 - input$s)
                                             # delta
      )[1, ],
      cbind(
        tmp$y_star_2 * input$s_2,            # new s
        tmp$y_star * input$s,                # old s
        tmp$y_star_2 * input$s_2 - tmp$y_star * input$s
                                             # delta
      )[1, ]
    )
    equil <- t(equil)
    equil <- data.frame(
      "var"=c("k*", "y*", "c*", "y*"),
      "new"=equil[, 1],
      "old"=equil[, 2],
      "delta"=equil[, 3]
    )
  })
  output$slider_alpha <- renderUI(
    sliderInput(
      inputId="alpha_2", label="alpha", value=input$alpha, min=0.1, max=0.9,
      step=0.1
    )
  )
  output$slider_s <- renderUI(
    sliderInput(
      inputId="s_2", label="s", value=input$s, min=0.1, max=0.9, step=0.1
    )
  )
  output$slider_delta <- renderUI(
    sliderInput(
      inputId="delta_2", label="delta", value=input$delta, min=0.05,
      max=0.95, step=0.05
    )
  )
  output$slider_n <- renderUI(
    sliderInput(
      inputId="n_2", label="n", value=input$n, min=0.05, max=0.95, step=0.05
    )
  )
  output$slider_g <- renderUI(
    sliderInput(
      inputId="g_2", label="g", value=0.3, min=0.05, max=0.95, step=0.05
    )
  )
}
shinyApp(ui=ui, server=server)
