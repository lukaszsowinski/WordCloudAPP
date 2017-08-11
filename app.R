#SHINY APP WORD CLOUD

library(tm)
library(wordcloud)
library(memoise)
library(stringr)
library(reshape2)

suppressPackageStartupMessages(library(dplyr))
#dlibrary(ggplot2)
library(xlsx)
#install.packages("stringr")

rm(list=ls())


textframe <- read.xlsx("C:/Users/Lukasz Sowinski/Desktop/WordCloudQCmapping/WordMappingInfo(AutoRecovered).xlsx", 1)

            schooldata <- list (
                "Num" = textframe$Num[!is.na(textframe$Num)],
                "SchoolName" = C(textframe$SchoolName),
                "Path" = c(
                            "C:/Users/Lukasz Sowinski/Desktop/WordCloudQCmapping/QueensCollegeSociology.txt",
                            "C:/Users/Lukasz Sowinski/Desktop/WordCloudQCmapping/LehmanCollegeSociology.txt",
                            "C:/Users/Lukasz Sowinski/Desktop/WordCloudQCmapping/DenisonUniversityDataAnalytics.txt", 
                            "C:/Users/Lukasz Sowinski/Desktop/WordCloudQCmapping/StJohnsUniversityDataAnalytics.txt",
                            "C:/Users/Lukasz Sowinski/Desktop/WordCloudQCmapping/RutgersUniversity.txt"
                           ),
                "Students" = textframe$Students[!is.na(textframe$Students)],
                "Tuition" = textframe$Tuition[!is.na(textframe$Tuition)],
                "Program" = textframe$Program[!is.na(textframe$Program)]
            )
            


ui <- fluidPage(
    # Application title
    titlePanel("Word Cloud"),
    
    sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
            selectInput("selection", "Choose a Program:",
                        choices =  schooldata$Program),  
           # actionButton("update", "Change"),
        sliderInput("range", "# of Students in Program", min=100, max=80000, value = c(30000, 40000), step= 100),
                    hr(),
                    hr(),
                    hr(),
        textInput("rm_word", "Remove a Word", value = "", placeholder = "type in a word to remove")
                    
        ),
        
        # Show Word Cloud
        
        mainPanel(
            textOutput("rm_word"),
            hr(),
            plotOutput("wordcloud"),
            tableOutput("hist")
            
        )
    )
)

server <- function(input, output, server) 
    {
    
    createmaster <- reactive({
        
        mastertext.1= 0
        temptext = 0
        
        for(i in 1:max(schooldata$Num))
                {
                    if (schooldata$Program[i] == input$selection & schooldata$Students[i] > input$range[1] & schooldata$Students < input$range[2])
                    {
                        
                        if (temptext == 0)
                        {
                            temptext <- schooldata$Path[i]
                            temptext.1 <- readLines(temptext)
                            mastertext.1 = temptext.1
                        }
                        
                        temptext <- schooldata$Path[i]
                        temptext.1 <- readLines(temptext)
                        mastertext.1 <- rbind(mastertext.1, temptext.1)
                        
                    }
                    
                }
                
                               master.Corpus <- Corpus(VectorSource(mastertext.1))
                master.clean <- text.cleaning(master.Corpus)
                master.dtm <- TermDocumentMatrix(master.clean)
                master.m <- as.matrix(master.dtm)
                master.v <- sort(rowSums(master.m),decreasing=TRUE)
                master.d <- data.frame(word=names(master.v),freq=master.v)
        return(master.d)
    })
    
    text.cleaning <- function(text){
        
    # wordlist <- renderText({strsplit(input$rm_word, ", "))
    
        text <- tm_map(text, content_transformer(tolower))
        text <- tm_map(text, removeNumbers)
        text <- tm_map(text, removeWords, stopwords("english")) 
        #remove special words
        text <- tm_map(text, removeWords, c("classroom", "login", "and"))
        text <- tm_map(text, removeWords, input$rm_word)
        #text <- tm_map(text, removeWords, wordlist)
        text <- tm_map(text, removePunctuation)
        text <- tm_map(text, stripWhitespace)
    return(text)
    }
    
    output$wordcloud <- renderPlot({
        
       wordcloud(words = createmaster()$word, freq = createmaster()$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.10, 
                colors=brewer.pal(8, "Dark2")) 
                
    })
    
    output$hist <- renderTable({
        head(createmaster(), 20)
        
    })
   
    output$rm_word <- renderText({
        print(input$rm_word)
   })
    
    }

shinyApp(ui = ui, server = server)
