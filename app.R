library(bslib)
library(shiny)
library(tidyverse)
library(shinyjs)
library(bayesplay)

ui <- page_fluid(
  theme = bs_theme(
   "bg-dark" = "#1bbc9d50",
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = "#1bbc9d",
    secondary = "#1bbc9d",
    "input-border-color" = "#1bbc9d"
  ),
  h5(""),
  layout_column_wrap(
    card(card_header(class = "bg-dark", "Hypothese 1"),
      layout_column_wrap(
         card(
           sliderInput(
             "prior_mu1",
             "Mittelwert Hypothese 1",
             min = 0,
             max = 1,
             value = .42,
             step = .01
           ),
           sliderInput(
             "prior_phi1",
             "Präzision Hypothese 1",
             min = 2,
             max = 100,
             value = 13,
             step = 1
           )),
         card(
           plotOutput("prior1", height = "200px")
         )
         )),
   
    card(card_header(class = "bg-dark", "Hypothese 2"),
      layout_column_wrap(
         card(
          sliderInput(
             "prior_mu2",
             "Mittelwert Hypothese 2",
             min = 0,
             max = 1,
             value = .65,
             step = .01
           ),
           sliderInput(
             "prior_phi2",
             "Präzision Hypothese 2",
             min = 2,
             max = 100,
             value = 13,
             step = 1
           )),
           card(plotOutput("prior2", height = "200px"))
         ))), 
 layout_column_wrap(
  card(card_header(class = "bg-dark", "Daten"),
     numericInput(
                "prog9",
                "n₁ = Befürwortung G9",
                min = 0,
                value = 5,
                step = 1),
     numericInput(
                "prog8",
                "n₂ = Befürwortung G8",
                min = 0,
                value = 12,
                step = 1)
           ), 
  card(card_header("Likelihoods und Bayes Factor", class = "bg-dark"),
       card_body(shinycssloaders::withSpinner(plotOutput("plot"), color = "#1bbc9d")
))
))



server <- function(input, output, session) {

################################################################################
### Plot of Priors                                                           ###
################################################################################


### custom functions ###########################################################
# muphi_to_shapes 
muphi_to_shapes <- function(mu, phi) {
  shape1 <- mu * phi
  shape2 <- (1 - mu) * phi
  return(list(shape1 = shape1, shape2 = shape2))
}

### aux variables ##############################################################
# convert prior parameterization

prior_shapes1 <- reactive({
  muphi_to_shapes(input$prior_mu1, input$prior_phi1)
})

prior_shapes2 <- reactive({
  muphi_to_shapes(input$prior_mu2, input$prior_phi2)
})

### Plot Prior 1 ###############################################################
output$prior1 <- renderPlot({

  p <- seq(0,1, length=1000)

  plot(
    p,
    dbeta(p,
          prior_shapes1()$shape1,
          prior_shapes1()$shape2),
    type = 'l',
    col = "#bc1b9a",
    ylab = "W'keitsdichte",
    xlab = "Anteil G9-Befürworter:innen",
    frame.plot = F
    )
}
)

### Plot Prior 2 ###############################################################
output$prior2 <- renderPlot({

  p <- seq(0,1, length=1000)

  plot(
    p,
    dbeta(p,
          prior_shapes2()$shape1,
          prior_shapes2()$shape2),
    type = 'l',
    col = "#bc991b",
    ylab = "W'keitsdichte",
    xlab = "Anteil G9-Befürworter:innen",
    frame.plot = F
    )
}
)


################################################################################
### Plot of Likelihoods                                                      ###
################################################################################

### custom reactive values #####################################################
N <- reactive({input$prog8 + input$prog9})


### compute the bf from marginal likelihoods ##################################
wkeit1 <- reactive({
  integral(likelihood(family = "binomial", 
                      successes = input$prog9, 
                      trials = input$prog9 + input$prog8)*
              prior(family = "beta", 
                  alpha = prior_shapes1()$shape1, 
                  beta = prior_shapes1()$shape2))
})

wkeit2 <- reactive({ 
  integral(likelihood(family = "binomial", 
                      successes = input$prog9, 
                      trials = input$prog9 + input$prog8)*
              prior(family = "beta", 
                  alpha = prior_shapes2()$shape1, 
                  beta = prior_shapes2()$shape2))
})

### create data for the two marginal likelihood distributions ####################
data <- reactive({

data_h1 <- tibble(k = 1:N(),
                  hyp = "Hypothese 1")
for(i in 1:N()){
data_h1$p[i] <- integral(likelihood(family = "binomial", 
                         successes = data_h1$k[i], 
                         trials = input$prog9 + input$prog8)*
                   prior(family = "beta", 
                         alpha = prior_shapes1()$shape1, 
                         beta = prior_shapes1()$shape2))
}

data_h1 <- data_h1 %>% 
  mutate(obs_eq_k = k == N()) 


data_h2 <- tibble(k = 1:N(),
                  hyp = "Hypothese 2")
for(i in 1:N()){
data_h2$p[i] <- integral(likelihood(family = "binomial", 
                         successes = data_h2$k[i], 
                         trials = input$prog9 + input$prog8)*
                   prior(family = "beta", 
                         alpha = prior_shapes2()$shape1, 
                         beta = prior_shapes2()$shape2))
}

data_h2 <- data_h2 %>% 
  mutate(obs_eq_k = k == N()) 



return(full_join(data_h1, data_h2))

})

### plot #######################################################################
output$plot <- renderPlot({

 ggplot() +
    # add whole binomial distributions with alpha
    geom_segment(data = data() %>% 
                          filter(hyp == "Hypothese 1"), 
                 aes(x = k - .1, xend = k -.1, y = 0, yend = p), 
                 color = "#bc1b9a50") +
    geom_segment(data = data() %>% 
                     filter(hyp == "Hypothese 2"), 
                 aes(x = k + .1, xend = k +.1, y = 0, yend = p), 
                 color = "#bc991b60") +
    # add selected binomial distributions without alpha
    geom_segment(data = data() %>% 
                     filter(hyp == "Hypothese 1" & k == input$prog9), 
                 aes(x = k - .1, xend = k -.1, y = 0, yend = p, color = "#bc1b9a")) +
    geom_segment(data = data() %>% 
                     filter(hyp == "Hypothese 2" & k == input$prog9), 
                 aes(x = k + .1, xend = k +.1, y = 0, yend = p, color = "#bc991b")) +
    theme_minimal() +
    geom_text(data = tibble(x = input$prog9, y = -.005, 
                            text = paste("BF =", 
                                         formatC(wkeit1()/wkeit2(), 
                                                 format = "e", 
                                                 digits = 2))),
              aes(x, y, label = text)) +
    xlab("Anzahl") + ylab("Wahrscheinlichkeit") +
    ggtitle("Berechnung des Bayes-Faktors", "bei Punkthypothesen") +
    scale_color_identity(name = "Marginal Likelihood",
                         breaks = c("#bc1b9a", "#bc991b"),
                         labels = c("Hyp. 1", "Hyp 2."),
                         guide = "legend") +
    theme(legend.position = "bottom")
})

### debug ######################################################################
output$debug <- renderPrint({
 data()
})

}

shinyApp(ui = ui, server = server)