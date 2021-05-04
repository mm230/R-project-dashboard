###################################  Projet Final ##############################
# Tableau de bord de suivi du COVID-19 au Sénégal avec Shinydashboard
# 
# Projet de groupe
#
########################### # Importation des librairies #######################
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr) 
library(DT)

######################### Chargement de la base ################################
db <- read_excel("BaseCovidSN.xlsx", sheet = 1)
View(db)
#
################################ Interface UI ##################################
ui <- dashboardPage(
  dashboardHeader( title = "Covid-19 au Sénégal "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cas guéris", tabName = "gueris", icon = icon("dashboard")),
      menuItem("Cas positifs", tabName = "positifs", icon = icon("dashboard")),
      menuItem("Cas de décès", tabName = "deces", icon = icon("dashboard")),
      menuItem("Evolution des cas", tabName = "evolution", icon = icon("dashboard")),
      menuItem("Corrélation entre cas", tabName = "courbe", icon = icon("dashboard")),
      menuItem("Statistiques", tabName = "statistique", icon = icon("poll")),
      menuItem("Lecture des données", tabName = "donnees", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "statistique",
        fluidRow(
          valueBox(sum(db$`Cas positifs`), "Nombres de cas postifs", icon = icon("userr", lib = "glyphicon")),
          valueBox(sum(db$Deces), "Nombres de Décès", icon = icon("Death", lib = "glyphicon",color = "red"),
          valueBox(sum(db$`Cas gueris`), "Nombres de guéris", icon = icon("thumbs-up", lib = "glyphicon"), color = "green")
        )
      ),
      tabItem(
        tabName = "positifs",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Evolution des cas positifs",
              footer = "Evolution des cas positifs",
              status = "info",
              width = 12,
              solidHeader = TRUE,
              
              plotOutput("cas_positifs")
              
            )
          )
          
        )    
      ),
      tabItem(
        tabName = "gueris",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Courbe d'évolutions des cas guéris",
              footer = "Courbe d'évolutions des cas guéris",
              status = "info",
              width = 12,
              solidHeader = TRUE,
              
              plotOutput("cas_gueris")
              
            )
          )
          
        )    
      ),
      tabItem(
        tabName = "deces",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Courbe d'évolutions des cas de décès",
              footer = "Courbe d'évolutions des cas de décès",
              status = "info",
              width = 12,
              solidHeader = TRUE,
              
              plotOutput("cas_deces")
              
            )
          )
          
        )    
      ),
      tabItem(
        tabName = "evolution",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Cas : importés,contacts suivis et transmissions communautaires",
              footer = "Cas : importés,contacts suivis et transmissions communautaires",
              status = "info",
              width = 12,
              solidHeader = TRUE,
              
              plotOutput("evolution")
              
            )
          )
          
        )    
      ),
      tabItem(
        tabName = "courbe",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Corrélation entre cas testés et cas guéris",
              footer = "Corrélation entre cas testés et cas guéris",
              status = "info",
              width = 12,
              solidHeader = TRUE,
              
              plotOutput("courbe")
              
            )
          )
          
        )    
      ),
      tabItem(
        tabName = "donnees",
           dataTableOutput("donnees")
             
      )
      
      
    )
  )
  
  
)

)

#################################### Server ####################################
server <- function(input, output) {
  
  
  # Graphiques
  
  # Courbe cas positifs
  output$cas_positifs <- renderPlot({
    ggplot(db, aes(Date, `Cas positifs`)) + geom_line(color = "blue", size = 1)
  })
  
  # Courbe ca guéris
  output$cas_gueris <- renderPlot({
    ggplot(db, aes(Date, `Cas gueris`)) + geom_line(color = "green", size = 1)
  })
  
  # Courbes décès
  output$cas_deces <- renderPlot({
    ggplot(db, aes(Date, `Deces`)) + geom_line(color = "red", size = 1)
  })
 
  # Courbes cas: 
   output$evolution <- renderPlot({
    ggplot(db, aes(Date)) + geom_line(aes(y=`Cas importes`), color = "magenta", size = 1) + 
      geom_line(aes(y=`Cas contact`), linetype = "dotted", color = "black", size = 1) +
      geom_line(aes(y=`Cas communautaires`), linetype = "dashed", color = "blue", size = 1)
  })
  
  output$courbe <- renderPlot({
    ggplot(db, aes(x = `Cas testes`, y= `Cas gueris`)) +
      geom_point(shape=23, fill = "black", color= "black", size=3)
  })
  
  #Affichage du tableau de données dans l'interface
  output$donnees = renderDT(db, options = list(lengthChange = FALSE))
  
}

################################ ShinyApp ##################################

shinyApp(ui, server)
