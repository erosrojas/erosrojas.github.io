library(tidyverse)
library(shiny)
library(plotly)
library(broom)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #FFFFFF;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
    )),
    sidebarLayout(
        sidebarPanel(id = "sidebar",
            helpText("Choose a subset of the data to display (range refers to the indices of the rows)."),
            sliderInput(inputId = "range",
                label = "Range of input",
                min = 1,
                max = 1000,
                value = c(400,600), 
                animate = animationOptions(interval = 50, loop = TRUE)),
            tableOutput(outputId = "coefficients")
        ),
        mainPanel(
            # h3(strong(), align = "center"),
            plotlyOutput(outputId = "distPlot", 
                width = "100%", 
                height = "600px")
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

    set.seed(1)

    artificial_data <- data.frame(x1 = seq(1, 1000)) %>%
        mutate(x2 = x1 + rnorm(1000, 0, 1), 
                y = (x1 + x2 + runif(1000, 0, 100))/4)

    # artificial_data %>%
    #     cor()

        dataInput <- reactive({ 
        plot_ly(artificial_data, x = ~artificial_data$x1, y = ~artificial_data$x2, z = ~artificial_data$y, alpha = 0.25) %>%
            add_markers() %>%
            layout(scene = list(xaxis = list(title = 'x1', range = c(0,1000)),
                yaxis = list(title = 'x2', range = c(0,1000)),
                zaxis = list(title = 'y', range = c(0,1000)), 
                aspectmode = 'cube'), 
                title = "\n Collinear features with regression plane overlaid (panning enabled)")
        
        })
    # add colour, and deploy
    output$distPlot <- renderPlotly({

        lm <- lm(y ~ x1 + x2, data = slice(artificial_data, input$range[1]:input$range[2]))

        grid <- expand_grid(x1 = seq(0, 1000, by = 200), x2 = seq(0, 1000, by = 200)) %>% 
            mutate(preds = predict(lm, newdata = data.frame(x1, x2))) %>%
            pivot_wider(names_from = x1, values_from = preds) %>% 
            select(-1) %>%  
            as.matrix()

        fig <- add_trace(p = dataInput(),
                        z = grid,
                        x = seq(0, 1000, by = 200),
                        y = seq(0, 1000, by = 200),
                        type = "surface")
        fig
    })

    output$coefficients <- renderTable({
        lm <- tidy(lm(y ~ x1 + x2, data = slice(artificial_data, input$range[1]:input$range[2]))) %>%
            select(term, estimate) %>%
            rename(Coefficient = term, Magnitude = estimate)
        lm
    })

}

shinyApp(ui = ui, server = server)