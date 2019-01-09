library(shiny)

shinyApp(
    ui = fluidPage(
        titlePanel("Gun-Related Deaths"),
        tabsetPanel(
            tabPanel("Quantitative EDA", fluid = TRUE,
                     sidebarLayout(
                         sidebarPanel(
                             h3("Data Selection"),
                             numericInput(inputId = "obs", label = "Number of Observations:", value = 3),
                         submitButton(text="View EDA")
                         ),
                         mainPanel(
                             h3("Head of the Dataset"),
                             tableOutput("view"), 
                             h3("Dataset Summary"),
                             verbatimTextOutput("summary")
                         )
                     )
            ),
            tabPanel("Visual EDA - Time Series", fluid = TRUE,
                     sidebarLayout(
                         sidebarPanel(
                             h3("Type of Graph"),
                             radioButtons(inputId = "intent", label = "Intent:", choices = levels(cases_new$intent), selected = "Homicide"),
                             selectInput(inputId = "trendline_color", label = "Color:", choices = colnames(cases_new)[c(5:6,8,10:11)], selected = "race"),
                             submitButton(text="View EDA")
                         ),
                         mainPanel(
                             h3("Trendlines"),
                             plotOutput("plot1")
                         )
                     )
            ),
            tabPanel("Visual EDA - Discrete", fluid = TRUE,
                     sidebarLayout(
                         sidebarPanel(
                             h3("Type of Graph"),
                             radioButtons(inputId = "variable", label = "Variable:", choices = colnames(cases_new)[c(4:6,8,10:11)], selected = "intent"),
                             selectInput(inputId = "discrete_color", label = "Color:", choices = colnames(cases_new)[c(4:6,8,10:11)], selected = "race"),
                             submitButton(text="View EDA")
                         ),
                         mainPanel(
                             h3("Discrete Variable"),
                             plotOutput("plot2")
                         )
                     )
            ),
            tabPanel("Visual EDA - Continuous", fluid = TRUE,
                     sidebarLayout(
                         sidebarPanel(
                             h3("Type of Graph"),
                             h5("Independent Variable: Age"),
                             radioButtons(inputId = "plot_type", label = "Plot:", choices = c("density plot","box plot"), selected = "density plot"),
                             selectInput(inputId = "continuous_color", label = "Color:", choices = colnames(cases_new)[c(4:6,8,10:11)], selected = "race"),
                             submitButton(text="View EDA")
                         ),
                         mainPanel(
                             h3("Continuous Varible"),
                             plotOutput("plot3")
                         )
                     )
            )
        )
    ), 
    server = function(input, output) {
        active_dataset_trendline= reactive({
            if(input$intent == "Homicide") {
                count_transformation(homicide_cases, input$trendline_color)
            } else if (input$intent == "Suicide") {
                count_transformation(suicide_cases, input$trendline_color)
            } else if(input$intent == "Accidental"){
                count_transformation(accidental_cases, input$trendline_color)
            } else if (input$intent == "Undetermined"){
                count_transformation(undetermined_cases, input$trendline_color)
            }
        })
        
        active_graph= reactive({
            if(input$plot_type == "density plot") {
                density_plot(cases_new, "age", color = input$continuous_color)+labs(title = paste("Age by",input$continuous_color))
            } else if (input$plot_type == "box plot") {
                box_plot(cases_new, input$continuous_color, "age", color = input$continuous_color) + theme(axis.text.x = element_text(angle = 65, hjust = 1))+ labs(title = paste("Age by",input$continuous_color))
            }
        })
        
        
        output$view = renderTable({
            head(case_transformation(cases_new),n = input$obs)
        })
        output$summary = renderPrint({
            summary(case_transformation(cases_new))
        })
        output$plot1 = renderPlot({
            line_plot(active_dataset_trendline(), "date", "Total_cases", color = input$trendline_color) + 
                labs(title = paste(input$intent,"by",input$trendline_color),  x = "Date", y = "Total Cases")
        }) 
        output$plot2 = renderPlot({
            bar_plot(cases_new, input$variable, fill = input$discrete_color, dodge = TRUE) + 
                theme(axis.text.x = element_text(angle = 65, hjust = 1))+
                labs(title = paste(input$variable,"by",input$discrete_color))
        })
        output$plot3 = renderPlot({
            active_graph()
        })
    }
)
