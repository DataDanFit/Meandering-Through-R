library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(rpart)
library(rpart.plot)

# User Interface ----------------------------------------------------------


ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Iris Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Analytics", tabName = "analytics", icon = icon("stats", lib = "glyphicon")),
            menuItem("Information", tabName = "information", icon = icon("info"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "analytics",
                fluidRow(
                    column(width = 8, 
                           box(title = "Plot",
                               width = "100%",
                               tags$div(style = "display:inline-block; margin-right:1em;width:25%;", 
                                   selectInput("selected_x", "X-Axis:",
                                            c("Sepal Length" = "Sepal.Length",
                                              "Sepal Width" = "Sepal.Width",
                                              "Petal Length" = "Petal.Length",
                                              "Petal Width" = "Petal.Width"),
                                            selected = c("Sepal.Length")
                                   )
                               ),
                               tags$div(style = "display:inline-block;width:25%;", 
                                selectInput("selected_y", "Y-Axis:",
                                            c("Sepal Length" = "Sepal.Length",
                                              "Sepal Width" = "Sepal.Width",
                                              "Petal Length" = "Petal.Length",
                                              "Petal Width" = "Petal.Width"),
                                            selected = c("Sepal.Width")
                                )
                               ),
                            plotlyOutput("plot", height = 600, width = "100%")
                           ),
                           box(title = "Averages",
                               width = "100%",
                               dataTableOutput("aggregated")
                           )
                    ),
                    column(width = 4,
                           box(title = "Predict Species",
                               width = "100%",
                               sliderInput("sepal_length", label = "Sepal Length", min = 0, max = 10, value = 5, step = 0.1, width = "75%"),
                               sliderInput("sepal_width", label = "Sepal Width", min = 0, max = 5, value = 2.5, step = 0.1, width = "75%"),
                               sliderInput("petal_length", label = "Petal Length", min = 0, max = 10, value = 5, step = 0.1, width = "75%"),
                               sliderInput("petal_width", label = "Petal Width", min = 0, max = 5, value = 2.5, step = 0.1, width = "75%"),
                               tabsetPanel(type = "tabs",
                                           tabPanel("Result",
                                                    tags$br(),
                                                    tags$div(style = "font-size: 1.5em;", textOutput("predicted_species_text")),
                                                    uiOutput("predicted_species_image")
                                                    ),
                                           tabPanel("Model", tags$br(), plotOutput("decision_tree"))
                               )
                               
                           )
                    )
                )
            ),
            tabItem(tabName = "information",
                    fluidRow(
                        column(width = 4,
                               box(title = "Description",
                                   width = "100%",
                                   tags$p(
                                       "Iris is a genus of 260–300 species of flowering plants with showy flowers.
                            It takes its name from the Greek word for a rainbow, which is also the name for the Greek goddess of the rainbow, Iris.
                            Some authors state that the name refers to the wide variety of flower colors found among the many species.
                            As well as being the scientific name, iris is also widely used as a common name for all Iris species, as well as some belonging to other closely related genera. It is a popular garden flower."
                                   ),
                                   tags$a(href = "https://en.wikipedia.org/wiki/Iris_flower_data_set", "Source: Wikipedia")
                               ),
                               box(title = "Images",
                                   width = "100%", 
                                   tabsetPanel(
                                       tabPanel(
                                           tags$h5("Setosa"),
                                           tags$img(src = "IRIS_SETOSA.jpg", class = "responsive")
                                       ),
                                       tabPanel(
                                           tags$h5("Versicolor"),
                                           tags$img(src = "IRIS_VERSICOLOR.jpg", class = "responsive")
                                       ),
                                       tabPanel(
                                           tags$h5("Virginica"),
                                           tags$img(src = "IRIS_VIRGINICA.jpg", class = "responsive")
                                       )
                                   )
                               )
                        ),
                        
                        column(width = 6,
                               box(title = "Dataset",
                                   width = "100%",
                                   tags$p(
                                       "The Iris flower data set or Fisher's Iris data set is a multivariate data set introduced by the British statistician and biologist Ronald Fisher in his 1936 paper 'The use of multiple measurements in taxonomic problems as an example of linear discriminant analysis.'
                         Two of the three species were collected in the Gaspé Peninsula 'all from the same pasture, and picked on the same day and measured at the same time by the same person with the same apparatus'.
                         The data set consists of 50 samples from each of three species of Iris (Iris setosa, Iris virginica and Iris versicolor).
                         Four features were measured from each sample: the length and the width of the sepals and petals, in centimeters. Based on the combination of these four features, Fisher developed a linear discriminant model to distinguish the species from each other."
                                   ),
                                   tags$a(href = "https://en.wikipedia.org/wiki/Iris_(plant)", "Source: Wikipedia")
                               ),
                               box(title = "Preview",
                                   width = "100%",
                                   dataTableOutput("preview")
                               ),
                               box(title = "Summary",
                                   width = "100%",
                                   verbatimTextOutput("summary")
                               )
                        ),
                        column(width = 1)
                    )
            )
        )
    ),
    tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css", href = "style.css")
    )
)



# Server ------------------------------------------------------------------


server <- function(input, output) {
    output$summary <- renderPrint({
        summary(iris)
    })
    
    output$preview <- renderDataTable({
        datatable(
            sample_n(iris, 8),
            options = list(
                searching = FALSE,
                paging = FALSE,
                info = FALSE
            ),
            rownames = FALSE,
            style = "bootstrap4"
        )
    })
    
    output$plot <- renderPlotly({
        x_axis <- input$selected_x
        y_axis <- input$selected_y
        firstup <- function(x) {
            substr(x, 1, 1) <- toupper(substr(x, 1, 1))
            x
        }
        p <- ggplot(data = iris, aes(x = get(x_axis), y = get(y_axis),
            group = 1,
            text = paste(
                x_axis, ":", get(x_axis),
                "<br>",
                y_axis, ":", get(y_axis),
                "<br>",
                "Species :", Species
            )
        )) +
            geom_point(aes(colour = Species), size = 3) +
            labs(x = paste(input$selected_x),
                 y = paste(input$selected_y)) +
            theme_minimal() +
            theme(legend.title = element_blank())
        
        ggplotly(p, tooltip = "text") %>% 
            config(
                collaborate = FALSE,
                displaylogo = FALSE,
                modeBarButtonsToRemove = list("sendDataToCloud",
                                              "lasso2d",
                                              "toggleSpikelines")
                ) %>% 
            layout(legend = list(orientation = "v", y = 0.5, x = 1))
    })
    
    output$aggregated <- renderDataTable({
        datatable(
            iris %>% 
                group_by(Species) %>% 
                summarise_all(mean),
            options = list(
                searching = FALSE,
                paging = FALSE,
                info = FALSE
            ),
            rownames = FALSE,
            style = "bootstrap4"
        )
    })
    
    
    classifier = rpart(formula = Species ~ ., data = iris)
    
    
    output$predicted_species_text <- renderText({
        Sepal.Length <- input$sepal_length
        Sepal.Width <- input$sepal_width
        Petal.Length <- input$petal_length
        Petal.Width <- input$petal_width
        input_data <- data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
        y_pred = predict(classifier, newdata = input_data, type = 'class')
        firstup <- function(x) {
            substr(x, 1, 1) <- toupper(substr(x, 1, 1))
            x
        }
        paste(firstup(as.character(y_pred)))
    })
    
    output$predicted_species_image <- renderUI({
        Sepal.Length <- input$sepal_length
        Sepal.Width <- input$sepal_width
        Petal.Length <- input$petal_length
        Petal.Width <- input$petal_width
        input_data <- data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
        y_pred = predict(classifier, newdata = input_data, type = 'class')
        if (as.character(y_pred) == "setosa") {
            tags$img(src = "IRIS_SETOSA.jpg", height = "481px")
        } else if (as.character(y_pred) == "versicolor") {
            img(src = "IRIS_VERSICOLOR.jpg",  height = "481px")
        } else if (as.character(y_pred) == "virginica") {
            img(src = "IRIS_VIRGINICA.jpg", height = "481px")
        }
    })
    
    output$decision_tree <- renderPlot({
        rpart.plot(classifier, type = 5, fallen.leaves = FALSE, shadow = "grey")
    })
    
}



# Run the application -----------------------------------------------------


shinyApp(ui = ui, server = server)