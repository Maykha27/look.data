library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(DT)
library(shinythemes)
library(plotly)
library(ggplot2)


ui <- fluidPage(
  shinythemes::themeSelector(),
  dashboardPage(
  dashboardHeader(
    title = h2("Look.data"),
    dropdownMenu(type='notifications',
                 notificationItem(text='Github de Khady',href="https://github.com/Maykha27/portifilo")
    ),
    dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Total Users",
                                 message = "Users during this month"
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2023-03-02"
                               )
                  ),
    # dropdownMenu(type = "notifications",
    #              notificationItem(
    #                text = "5 new users today",
    #                icon("users")
    #              ),
    #              notificationItem(
    #                text = "12 items delivered",
    #                icon("truck"),
    #                status = "success"
    #              ),
    #              notificationItem(
    #                text = "Server load at 86%",
    #                icon = icon("exclamation-triangle"),
    #                status = "warning"
    #              )
    # ),
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Documentation"
                 ),
                 taskItem(value = 1, color = "aqua",
                          "Project X"
                 ),
                 taskItem(value = 5, color = "yellow",
                          "Server deployment"
                 ),
                 taskItem(value = 80, color = "red",
                          "Overall project"
                 )
    )
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Paramètre", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll"))
    )
  ),
  dashboardBody(
    tags$img(src="https://lh3.googleusercontent.com/AXNILcXeKldQ1r5gs2-zl1oYIctKUl0Zq0r9jzYLcq_zQ8aEa6Ax9ppCsEldUm8Yb8M354Dx4pM3n5gz2Sj_l9bER38umIJL9Y_qVZpgIguydL9FhKMU9Q48yS0nDdgpUwQSy5dCpo0o3-qN_aQZQSc", width = "25%", height = "150%"),

    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #660099;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #660099;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #660099;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #97DFC6;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #97DFC6;
                              color: #000000;
                              }
                              '))),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h1("Lecture des données"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              h3("Parametres"),
              # Input: Checkbox if file has header
              radioButtons(inputId = "header",
                           label = "Header",
                           choices = c("Yes" = TRUE,
                                       "No" = FALSE),
                           selected = TRUE, inline=T),
              # Input: Select separator ----
              radioButtons(inputId = "sep",
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "t"),
                           selected = "t", inline=T),
              # Input: Select quotes ----
              radioButtons(inputId = "quote",
                           label= "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = "", inline=T),
              h3("File preview"),
              dataTableOutput(outputId = "preview")
      ),
      tabItem(tabName = "visualization",
              h1("Exploration du tableau"),
              sidebarLayout(position="left",
                sidebarPanel(
                  selectInput('plot_type',
                              'choisis le type de graphique',
                              choices = c('Histogram', 'Scatter Plot', "Diagramme à Bande",
                                          "Courbe", "Pie Chart")),
                  selectInput('var', 'choisie une variable :', choices = ""),
                  conditionalPanel(
                    condition = "input.plot_type == 'Scatter Plot'",
                    selectInput('var2', 'choisis une 2e variable :', choices = "")
                  ),
                  conditionalPanel(
                    condition = "input.plot_type == 'Diagramme à Bande'",
                    selectInput('var3', 'choisis une 2e variable :', choices = ""),
                    selectInput(
                      inputId = "couleurs",
                      label = "Choisir une couleur",
                      # Soit j'utilise une liste de couleurs comme ci-dessous
                      # choices = c("red", "dark green", "blue", "violet", "yellow"),
                      # Soit j'utilise une fonction
                      choices = colours(),
                      selected = "darkgreen"
                    )),
                    conditionalPanel(
                      condition = "input.plot_type == 'Courbe'",
                      selectInput('var4', 'choisis une 2e variable :', choices = "")
                  ),
                  conditionalPanel(
                    condition = "input.plot_type == 'Pie Chart'",
                    selectInput('var5', 'choisis la variable de Groupe :', choices = "")),

                  sliderInput("bins",
                              "Number of bins:",
                              min = 1,
                              max = 5000,
                              value = 30)
                  ),

                mainPanel(
                  tabsetPanel(
                    tabPanel('Data', DTOutput('khady'),
                             downloadButton('save_data', 'save to csv')),
                    tabPanel('statistiques', verbatimTextOutput('summary')),
                    tabPanel('Graphique',
                             conditionalPanel(
                               condition = "input.plot_type == 'Histogram'",
                               plotOutput('hist')),

                             conditionalPanel(
                               condition = "input.plot_type == 'Scatter Plot'",
                               plotOutput('nuage')),

                             conditionalPanel(
                               condition = "input.plot_type == 'Diagramme à Bande'",
                               plotlyOutput(outputId = "D_Bande")),

                    conditionalPanel(
                      condition = "input.plot_type == 'Courbe'",
                      plotlyOutput(outputId = "D_Line")),

                    conditionalPanel(
                      condition = "input.plot_type == 'Pie Chart'",
                      plotOutput(outputId = "Cam"))
                  )
                  )
                ))
      )
    )
  )
))

