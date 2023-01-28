library(tidyverse)
library(shiny)
library(plotly)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    fluidRow(
        column(12,
            plotOutput(outputId = "distPlot")
        )

    ),

    fluidRow(
        br(), 
        column(12, align = "center",
            sliderInput(inputId = "range",
                        label = "Number of bins:",
                        min = 1,
                        max = 1000,
                        value = c(200,500))
        )
    )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

    set.seed(1)

    artificial_data <- data.frame(x1 = seq(1, 1000)) %>%
        mutate(x2 = x1 + rnorm(1000, 0, 1), 
                y = (x1 + x2 + runif(1000, 0, 500))/4)

    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot

    output$distPlot <- renderPlot({

        lm <- lm(y ~ x1 + x2, data = slice(artificial_data, input$range[1]:input$range[2]))
        #lm

        grid <- expand_grid(x1 = seq(0, 1000, by = 200), x2 = seq(0, 1000, by = 200)) %>% 
            mutate(preds = predict(lm, newdata = data.frame(x1, x2))) %>%
            pivot_wider(names_from = x1, values_from = preds) %>% 
            select(-1) %>%  
            as.matrix()

        fig <- plot_ly(artificial_data, x = ~artificial_data$x1, y = ~artificial_data$x2, z = ~artificial_data$y, alpha = 0.25)
        fig <- fig %>% add_markers()
        fig <- fig %>% layout(scene = list(xaxis = list(title = 'x1'),
                            yaxis = list(title = 'x2'),
                            zaxis = list(title = 'y', nticks = 5, range = c(0,1000))))

        fig <- add_trace(p = fig,
                        z = grid,
                        x = seq(0, 1000, by = 200),
                        y = seq(0, 1000, by = 200),
                        type = "surface")
        fig
        })

}

shinyApp(ui = ui, server = server)