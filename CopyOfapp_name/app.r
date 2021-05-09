library(shiny)
library(tidyverse)
library(shinythemes)
library(gutenbergr)
library(tidytext)
library(gutenbergr)
library(rvest)
library(httr)
require(cowplot)

colors <- tibble(Name = "White,Yellow,Blue,Red,Green,Black,Brown,Azure,Ivory,Teal,Silver,Purple,Navy,Gray,Orange,Maroon,Charcoal,Aquamarine,Coral,Fuchsia,Wheat,Lime,Grey,Crimson,Khaki,Pink,Magenta,Olden,Plum,Olive,Cyan")

colors <- colors %>%
  mutate(Name = str_to_lower(Name, locale = "en"))

colors <- colors_scrape %>%
  select(Name) 

colors_all <- paste0("\\b(", colors, ")\\b")

gutenberg_works <- gutenberg_works(languages = "en", only_text = TRUE) %>%
  select(title, gutenberg_id) %>%
  drop_na()

# User interface
ui <- fluidPage(
  titlePanel(title = "Generating Art"),
  sidebarLayout(
    sidebarPanel(selectizeInput(inputId = "selected_work",
                                label = "Title of Text",
                                choices = gutenberg_works$title,
                                multiple = FALSE),
                 submitButton("Update")
               ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Art", plotOutput("art")),
                  tabPanel("Reference", p("The data were collected and made freely available by the U.S. Census Bureau and DC GIS. Further, the data are comprised of observations of human population, human population density, tree population, and tree population density, from each of Washington DC’s 179 Census Tracts."),
                           tags$p("The data were obtained from the", tags$code("DCTreePops"), "library in R."),
                           p("This shinyApp was made by Amrita Sawhney and Maxwell VanLandschoot. On git as amsahwney and maxvanlan, respectively."))
                 )
              )
        )
)


server <- function(input, output){
  

output$art <- renderPlot({ 
  text_num <- gutenberg_works %>%
    filter(title == input$selected_work) %>% 
    select(gutenberg_id)
  
  observe()
  text <- gutenberg_download(text_num) %>%
    mutate(text = str_to_lower(text, locale = "en"))
  
  afinn <- get_sentiments("afinn")
  
  text_afinn <- text %>%
    unnest_tokens(output = word, input = text, token = "words") %>%
    count(word, sort = TRUE) %>%
    inner_join(afinn, by = "word") %>% 
    mutate(prob = n/sum(n)) %>%
    arrange(desc(n))
  
  text_neg <- text_afinn %>%
    filter(value < 0) %>%
    arrange(desc(n))
  
  text_pos <- text_afinn %>%
    filter(value > 0) %>%
    arrange(desc(n))
  
  senti_raw <- sum(text_afinn$n*text_afinn$value)/sum(text_afinn$n) * 10000
  
  senti_pos <- sum(text_pos$n*text_pos$value)/sum(text_pos$n)
  
  senti_neg <- sum(text_neg$n*text_neg$value)/sum(text_neg$n)
  
  senti <- abs(round(senti_raw))
  
  set.seed(senti)
  
  text_grammed <- text %>%
    unnest_tokens(output = word, input = text,
                  token = "ngrams", n = 3)
  
  colors_parsed <- text_grammed %>%
    mutate(Name = str_extract(word, pattern = colors_all)) %>%
    drop_na() %>%
    select(Name)
  
  colors_counted <- colors_parsed %>%
    group_by(Name) %>%
    count() %>%
    ungroup() %>%
    mutate(prob = n/sum(n)) %>%
    arrange(desc(n))
  
  colors_final <- left_join(colors_counted, colors_scrape, by = "Name") %>%
    select(Name, `Hex(RGB)`, n, prob) %>%
    rename(Hex = `Hex(RGB)`)
  
  color_main <- colors_final %>%
    filter(row_number() == 1) %>%
    select(Hex) %>%
    as.character()
  
  color_background <- colors_final %>%
    filter(row_number() == 2) %>%
    select(Hex) %>%
    as.character()
  
  formula <- list(
    x = quote(runif(1, -1, 1) * x_i^2 - senti_neg * sin(y_i^2)),
    y = quote(runif(1, -1, 1) * y_i^2 - senti_pos * cos(x_i^2))
  )
  
  dat <- seq(from = -pi, to = pi, by = 0.01) %>%
    expand.grid(x_i = ., y_i = .) %>%
    mutate(!!!formula)
  
  title <- sample_n(text_afinn, 3, replace = TRUE, weight = prob) %>%
    select(word)
  
  title <- title %>%
    summarise(word = paste0(word, collapse = " ")) %>%
    as.character(expression())
  
  ggplot(dat, aes(x = x, y = y)) +
                               geom_point(alpha = 0.1, size = 0, shape = 20, color = color_main) +
                               theme_void() +
                               coord_fixed() +
                               coord_polar() +
                               theme(
                                 panel.background = element_rect(fill = color_background, color = color_background),
                                 plot.background = element_rect(fill = color_background, color = color_background))
  })

}

shinyApp(ui = ui, server = server)
