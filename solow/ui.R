library(shiny)
library(mathjaxr)

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
