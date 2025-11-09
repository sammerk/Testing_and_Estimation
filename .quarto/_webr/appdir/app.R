library(bslib)
library(shiny)
library(ggplot2)
library(tibble)
library(dplyr)
library(munsell)
library(shinyjs)

ui <- page_fluid(
  theme = bs_theme(
    "bg-primary" = "#1bbc9d50",
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = "#267326",
    secondary = "#267326",
    "input-border-color" = "#267326"
  ),
  h5(""),
    layout_column_wrap(
  card(
      card_header(class = "bg-primary", "Punkthypothesen"),
      card_body(
        sliderInput(
          "theta1",
          "Hypothese 1: Anteil Pro G9",
          min = 0,
          max = 1,
          value = .4,
          step = .1
        ),
        sliderInput(
          "theta2",
          "Hypothese 2: Anteil Pro G9",
          min = 0,
          max = 1,
          value = .6,
          step = .1
        )
      )
    ),
    card(
      card_header(class = "bg-primary", "Daten"),
      card_body(
        numericInput(
          "prog9",
          "n₁ = Befürwortung G9",
          min = 0,
          value = 10,
          step = 1
        ),
        numericInput(
          "prog8",
          "n₂ = Befürwortung G8",
          min = 0,
          value = 5,
          step = 1
        )
      )
    )
  ),
  card(
    card_header("Likelihoods und Bayes Factor", class = "bg-primary"),
    #card_body(shinycssloaders::withSpinner(
    plotOutput("plot"),
    #  color = "#267326"
    #))
  )
)


server <- function(input, output, session) {
  # n <- 30 # N()
  # obs <- 20 input$prog9
  # theta1 <- .5 # input$theta1
  # theta2 <- .7 # input$theta2
  # wkeit1 <- choose(n, obs)*theta1^obs*(1-theta1)^(n-obs) # wkeit1()
  # wkeit2 <- choose(n, obs)*theta2^obs*(1-theta2)^(n-obs) # wkeit2()

  ### custom reactive values #####################################################
  N <- reactive({
    input$prog8 + input$prog9
  })

  wkeit1 <-
    reactive({
      choose(N(), input$prog9) *
        input$theta1^input$prog9 *
        (1 - input$theta1)^(N() - input$prog9)
    })

  wkeit2 <-
    reactive({
      choose(N(), input$prog9) *
        input$theta2^input$prog9 *
        (1 - input$theta2)^(N() - input$prog9)
    })

  ### create data ################################################################
  data <- reactive({
    return(
      rbind(
        tibble(
          k = 1:N(),
          p = choose(N(), k) * input$theta1^k * (1 - input$theta1)^(N() - k),
          theta = as.character(input$theta1)
        ),
        tibble(
          k = 1:N(),
          p = choose(N(), k) * input$theta2^k * (1 - input$theta2)^(N() - k),
          theta = as.character(input$theta2)
        )
      ) %>%
        mutate(obs_eq_k = k == N()) %>%
        as_tibble()
    )
  })

  ### plot #######################################################################
  output$plot <- renderPlot({
    ggplot() +
      # add whole binomial distributions with alpha
      geom_segment(
        data = data() %>%
          filter(theta == input$theta1),
        aes(x = k - .1, xend = k - .1, y = 0, yend = p),
        color = "#26732650",
        linewidth = 2
      ) +
      geom_segment(
        data = data() %>%
          filter(theta == input$theta2),
        aes(x = k + .1, xend = k + .1, y = 0, yend = p),
        color = "#d77d0050",
        linewidth = 2
      ) +
      # add selected binomial distributions without alpha
      geom_segment(
        data = data() %>%
          filter(theta == input$theta1 & k == input$prog9),
        aes(x = k - .1, xend = k - .1, y = 0, yend = p, color = "#267326"),
        linewidth = 2
      ) +
      geom_segment(
        data = data() %>%
          filter(theta == input$theta2 & k == input$prog9),
        aes(x = k + .1, xend = k + .1, y = 0, yend = p, color = "#d77d00"),
        linewidth = 2
      ) +
      theme_minimal() +
      geom_text(
        data = tibble(
          x = input$prog9,
          y = -.005,
          text = paste(
            "BF =",
            formatC(wkeit1() / wkeit2(), format = "e", digits = 2)
          )
        ),
        aes(x, y, label = text)
      ) +
      xlab("Anzahl") +
      ylab("Wahrscheinlichkeit") +
      ggtitle("Berechnung des Bayes-Faktors", "bei Punkthypothesen") +
      scale_color_identity(
        name = "Likelihood",
        breaks = c("#bc991b", "#bc1b9a"),
        labels = c("Hyp. 1", "Hyp 2."),
        guide = "legend"
      )
  })

  ### debug ######################################################################
  output$debug <- renderPrint({
    data()
  })
}

shinyApp(ui = ui, server = server)
