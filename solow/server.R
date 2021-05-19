library(shiny)
library(tidyverse)

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
      theme(
        plot.background=element_rect(fill=bgc), legend.title=element_blank()
        )
  })
  output$graph2 <- renderPlot({
    bgc <- ifelse(input$alpha_2 == input$s_2, "#FFD700", "white")
    big_plot <- ggplot(data=dat2(), aes(x=k)) +
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
      geom_segment(
        aes(x=k_star_2, y=0, xend=k_star_2, yend=y_star_2),
        linetype="dashed", colour="red"
      ) +
      geom_segment(
        aes(x=0, y=y_star_2, xend=k_star_2, yend=y_star_2),
        linetype="dashed", colour="red"
      ) +
      geom_point(aes(x=k_star_2, y=y_star_2), size=2, colour="red") +
      theme(legend.title = element_blank())
    if (all(dat2()$y == dat2()$y_2)) {
      big_plot <- big_plot +
        geom_line(aes(y=y, colour="y"), size=1)
    }
    if (all(dat2()$sy == dat2()$sy_2)) {
      big_plot <- big_plot +
        geom_line(aes(y=sy, colour="sy"), size=1)
    }
   if (all(dat2()$dk == dat2()$dk_2)) {
     big_plot <- big_plot +
       geom_line(aes(y=dk, colour="dk"), size=1)
    }
    if (!all(dat2()$y == dat2()$y_2)) {
      big_plot <- big_plot +
        geom_line(
          data=dat2(), aes(x=k, y=y_2, colour="y", linetype="new"), size=1
          ) +
        geom_line(aes(y=y, colour="y", linetype="old"), size=0.7)
    }
    if (!all(dat2()$sy == dat2()$sy_2)) {
      big_plot <- big_plot +
        geom_line(aes(y=sy_2, colour="sy", linetype="new"), size=1) +
        geom_line(aes(y=sy, colour="sy", linetype="old"), size=0.7)
    }
    if (!all(dat2()$dk == dat2()$dk_2)) {
      big_plot <- big_plot +
        geom_line(aes(y=dk_2, colour="dk", linetype="new"),  size=1) +
        geom_line(aes(y=dk, colour="dk", linetype="old"), size=0.7)
    }
    big_plot
  })
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    cx <- as.numeric(input$plot_click["x"])
    cy <- cx ** input$alpha
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
    cy_2 <- cx_2 ** input$alpha_2
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
      "var"=c("k*", "y*", "c*", "sy*"),
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
