library(shiny)
library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)

titles <- gutenberg_works() %>%
    select(title, gutenberg_id) %>%
    filter(gutenberg_id >= 5)

afinn <- read_csv("afinn.csv")
    
    ui <- fluidPage(
        titlePanel("Tracing Sentiments in Texts"),
        sidebarLayout(
            sidebarPanel(
                selectizeInput("text", "Text:",
                               choices = titles$title,
                               selected = "The Scarlet Letter")
            ),
            mainPanel(
                plotOutput("plot")
            )
        )
    )
    
    server <- function(input, output) {
        
        output$plot = renderPlot({
            titles %>%
                filter(title == input$text) %>%
                select(gutenberg_id) %>%
                as.numeric() %>%
                gutenberg_download() %>%
                mutate(text = str_to_lower(text, locale = "en")) %>%
                unnest_tokens(output = word, input = text, token = "words") %>%
                inner_join(afinn, by = "word") %>%
                mutate(value = case_when(value == 1 ~ 1,
                                         value == 2 ~ 2,
                                         value == -1 ~ -1,
                                         value == -2 ~ -2,
                                         value %in% 3:5 ~ 3,
                                         value %in% -3:-5 ~ -3)) %>%
                select(value) %>%
                mutate(sign = sign(value),
                       x_coord = cumsum(sign),
                       y_coord = cumsum(value)) %>%
                mutate(x_coord = cumsum(case_when(value == 1 ~ .78,
                                                  value == 2 ~ .97,
                                                  value == 3 ~ .43,
                                                  value == -1 ~ -.78,
                                                  value == -2 ~ -.97,
                                                  value == -3 ~ -.43)),
                       y_coord = cumsum(case_when(value == 1 ~ .62,
                                                  value == 2 ~ -.22,
                                                  value == 3 ~ -.9,
                                                  value == -1 ~ .62,
                                                  value == -2 ~ -.22,
                                                  value == -3 ~ -.9))) %>%
                ggplot(mapping = aes(x = x_coord, y = y_coord)) +
                geom_path(color = "white") +
                geom_point(mapping = aes(x = 0, y = 0, color = "red")) +
                theme(plot.title = element_text(hjust = 0.5, 
                                                family = "Minerva Modern", 
                                                size = 10, 
                                                color = "#36454f"),
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(), 
                      axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_rect(fill = "#36454f",
                                                      colour = "#36454f",
                                                      size = 0.5, linetype = "solid"))
        }, height = 500, width = 600)
    }
    
    shinyApp(ui = ui, server = server)