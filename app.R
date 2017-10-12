#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse, warn.conflicts = F)
require(tidytext)
require(ggplot2)
require(ggthemes)

newBible   = readRDS("newBible.rda")
unnested   = readRDS("unnested.rda")
# unnestedKjb = subset(unnested, version == "kjb") 
stop_words = readRDS("stop_words.rda") %>% as.tibble()

versions   = c('kjb',  'asv', 'drb',  'erv',  'wbt',  'web',  'ylt',  'akjv', 'wnt')

newBible = dplyr::select(newBible,  order, dplyr::everything())

bookName = unique(newBible$bookName)


myStops = rbind(
  rbind(
    rbind(
      rbind(
        c("thy", "fred"),
        c("thee", "fred")),
      c("ye", "fred")),
    c("hath", "fred"),
    c("thou", "fred")
    )
  )

colnames(myStops) = c("word", "lexicon")
stop_words=rbind(stop_words, myStops)

unnested = unnested %>%
  dplyr::anti_join(stop_words)





library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("New Bible"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        actionButton("goButton", "Go!"),
        numericInput("nFreqMin",
                     "Minimum Word Frequency",
                     value = 1, 
                     min = 1, 
                     max = Inf),
        numericInput("nFreqMax",
                     "Maximum Word Frequency",
                     value = 100000, 
                     min = 1, 
                     max = Inf),
        selectizeInput("versions",
                       "Versions",
                       versions,
                       multiple = TRUE,
                       selected = c("kjb")),
        selectizeInput("books",
                       "Books",
                       bookName,
                       multiple = TRUE,
                       selected = c("Genesis")),
        sliderInput("bookNumber",
                    "Book Number:",
                    min = 1,
                    max = 3,
                    value = c(1:3)),
        sliderInput("chapter",
                    "Chapter:",
                    min = 1,
                    max = 150,
                    value = c(1:10)),
         sliderInput("verse",
                     "Verse:",
                     min = 1,
                     max = 176,
                     value = c(1:10))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Plot",
                   plotOutput("biblePlot")
          ),
          tabPanel(title = "Data",
                   tableOutput("distPlot")
          )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  bibleOutput = reactive({
    # Take a dependency on input$goButton
    input$goButton
    
    bibleOutput = isolate({newBible %>% 
      dplyr::filter(version %in% input$versions) %>%
      dplyr::filter(bookName %in% input$books) %>% 
      dplyr::filter(bookNumber >= min(input$bookNumber)) %>% 
      dplyr::filter(bookNumber <= max(input$bookNumber)) %>% 
      dplyr::filter(chapter >= min(input$chapter)) %>%
      dplyr::filter(chapter <= max(input$chapter)) %>%
      dplyr::filter(verse >= min(input$verse)) %>% 
      dplyr::filter(verse <= max(input$verse))})
    bibleOutput
  })
  
   output$distPlot <- renderTable({
     input$goButton
     
     bibleOutput()
     
   })
   
   
   output$biblePlot <- renderPlot({

     newBible$text = as.character(newBible$text)

     # Take a dependency on input$goButton
     input$goButton

     # if (input$goButton == 0)
     #   return()
     unnested = bibleOutput() 
     unnested$text = as.character(unnested$text)
     unnested = unnested %>%
       unnest_tokens(word, text)
     
     
     myStops = rbind(rbind(rbind(c("thy", "fred"),
                                 c("thee", "fred")),
                           c("ye", "fred")),
                     c("hath", "fred"))
     colnames(myStops) = c("word", "lexicon")
     stop_words=rbind(stop_words, myStops)
     
     unnested = unnested %>%
       anti_join(stop_words)
     
     
      gg = isolate({
        unnested   %>% 
          dplyr::filter(version %in% input$versions) %>%
          dplyr::filter(bookName %in% input$books) %>%
          dplyr::filter(bookNumber >= min(input$bookNumber)) %>%
          dplyr::filter(bookNumber <= max(input$bookNumber)) %>%
          dplyr::filter(chapter >= min(input$chapter)) %>%
          dplyr::filter(chapter <= max(input$chapter)) %>%
          dplyr::filter(verse >= min(input$verse)) %>%
          dplyr::filter(verse <= max(input$verse))%>%
          dplyr::count(word, sort = TRUE) %>%
          dplyr::filter(n > input$nFreqMin) %>%
          dplyr::filter(n < input$nFreqMax) %>%
          dplyr::mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip()  +
          theme_solarized(light = TRUE) +
          ggtitle("Unique Word Frequency") +
          xlab("Frequency") +
          ylab("Count") +
          theme(axis.text.y = element_text(angle = 40, hjust = 1),
                text = element_text(size = 20))
      })
      gg
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

