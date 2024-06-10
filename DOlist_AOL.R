setwd("C:/Users/aolim/Desktop/organizer")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(gridExtra)

# Define UI
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "My Do List"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Add Task", tabName = "add_task", icon = icon("plus")),
      menuItem("Delete Task", tabName = "remove_task", icon = icon("minus")),
      downloadButton("download_script", "Download R Script")
    ),
    width = 250
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "add_task",
        h2("Add Task", style = "font-size: 14px;"),
        selectInput("category", "Category:",
                    choices = c("Hawkins Tasks", "Marina Tasks", "TopMed Tasks", "JP tasks", "Others")),
        textInput("subcategory", "Subcategory:"),
        textInput("begin", "Start Time:", value = "00:00"),
        textInput("end", "End Time:", value = "00:00"),
        actionButton("addButton", "Add Task"),
        hr(),
        h2("Do List", style = "font-size: 14px;"),
        tableOutput("tasks")
      ),
      tabItem(
        tabName = "delete_task",
        h2("Delete Task", style = "font-size: 14px;"),
        selectInput("remove_task_id", "Select the remove task:", choices = NULL),
        actionButton("removeButton", "Delete Task"),
        hr(),
        h2("Do List", style = "font-size: 14px;"),
        tableOutput("tasks_delete")
      )
    ),
    fluidRow(
      column(
        width = 12,
        fluidRow(
          column(6, plotOutput("pie_chart")),
          column(6, plotOutput("barplot_chart")),
          column(12, plotOutput("mosaic_plot")),
          column(12, plotlyOutput("interactive_plot"))
        ),
        downloadButton("download_csv", "Download CSV")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initializing reactive value to store data
  data <- reactiveVal(data.frame(
    Category = character(0),
    Subcategory = character(0),
    Begin = character(0),
    End = character(0)
  ))
  
  # Adding tasks
  observeEvent(input$addButton, {
    new_task <- isolate({
      data.frame(
        Category = input$category,
        Subcategory = input$subcategory,
        Begin = input$begin,
        End = input$end
      )
    })
    data(rbind(data(), new_task))
  })
  
  # Displaying task list
  output$tasks <- renderTable({
    data()
  })
  
  # Displaying task list for removal
  output$tasks_delete <- renderTable({
    data()
  })
  
  # Updating task options for removal
  observe({
    choices <- as.character(seq_len(nrow(data())))
    updateSelectInput(session, "remove_task_id", choices = choices)
  })
  
  # Removing tasks
  observeEvent(input$removeButton, {
    data(data()[-as.numeric(input$remove_task_id), , drop = FALSE])
  })
  
  # Creating the pie chart
  nb.cols <- 70
  mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)
  output$pie_chart <- renderPlot({
    data() %>%
      group_by(Category) %>%
      summarise(Sum_Hours = sum(as.numeric(as.POSIXct(End, format = "%H:%M")) -
                                  as.numeric(as.POSIXct(Begin, format = "%H:%M")))) %>%
      ggplot(aes(x = "", y = Sum_Hours, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = mycolors) +
      labs(title = "Sum Hours per Category") +
      theme_bw()
  })
  
  # Creating the bar plot
  output$barplot_chart <- renderPlot({
    data() %>%
      group_by(Subcategory, Category) %>%
      summarise(Sum_Hours = sum(as.numeric(as.POSIXct(End, format = "%H:%M")) -
                                  as.numeric(as.POSIXct(Begin, format = "%H:%M")))) %>%
      ggplot(aes(x = Subcategory, y = Sum_Hours, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "Sum Hours per Subcategories",
           x = "Subcategories",
           y = "Sum Hours",
           fill = "Category") +
      scale_fill_manual(values = mycolors) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Adding the mosaic plot
  output$mosaic_plot <- renderPlot({
    data() %>%
      ggplot(aes(x = "", fill = Subcategory)) +
      geom_bar(position = "fill") +
      facet_wrap(~ Category) +
      labs(title = "Per(%) of Subcategories per Categories",
           fill = "Subcategory") +
      scale_fill_manual(values = mycolors) +
      theme_bw() +
      theme(axis.title.x = element_blank())
  })
  
  # Create interactive plot
  output$interactive_plot <- renderPlotly({
    data() %>%
      plot_ly(x = ~Category, y = ~End, type = "scatter", mode = "markers")
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      "organizer_tasks.csv"
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Download R Script for Visualization
  output$download_script <- downloadHandler(
    filename = function() {
      "visualization_script.R"
    },
    content = function(file) {
      # Writing R script to file
      script_content <- "
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(gridExtra)
library(htmlwidgets)
library(plotly)
library(shiny)

## Set working directory
# include your path using the function setwd

### Load data
data <- read.csv('organizer_tasks.csv')

# Create pie chart
nb.cols <- 70
mycolors <- colorRampPalette(brewer.pal(12,\"Set3\"))(nb.cols)

pie_chart <- data %>%
  group_by(Category) %>%
  summarise(Sum_Hours= sum(as.numeric(as.POSIXct(End, format = '%H:%M')) -
                                as.numeric(as.POSIXct(Begin, format = '%H:%M')))) %>%
  ggplot(aes(x = '', y = Sum_Hours, fill = Category)) +
  geom_bar(stat = 'identity') +
  coord_polar('y', start = 0) +
 
  scale_fill_manual(values = mycolors) +
  labs(title = 'Sum Hours per Categories') +
  theme_bw()

# Create bar plot
barplot_chart <- data %>%
  group_by(Subcategory, Category) %>%
  summarise(Sum_Hours = sum(as.numeric(as.POSIXct(End, format = '%H:%M')) -
                                as.numeric(as.POSIXct(Begin, format = '%H:%M')))) %>%
  ggplot(aes(x = Subcategory, y = Sum_Hours, fill = Category)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Sum Hours per Subcategories',
       x = 'Subcategories',
       y = 'Sum Hours',
       fill = 'Category') +

  scale_fill_manual(values = mycolors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create mosaic plot
mosaic_plot <- data %>%
  ggplot(aes(x = '', fill = Subcategory)) +
  geom_bar(position = 'fill') +
  facet_wrap(~ Category) +
  labs(title = 'Per(%) of Subcategories per Categories',
       fill = 'Subcategory') +
  scale_fill_manual(values = mycolors) +
  theme_bw() +
  theme(axis.title.x = element_blank())

# Create table description
table_desc <- tableGrob(data, rows = NULL)

# Save plots to PDF
#pdf('organizer_plots.pdf', width = 11, height = 8.5)
pdf(paste0('organizer_plots_', Sys.Date(), '.pdf'), width = 11, height = 8.5)
grid.arrange(table_desc,pie_chart, barplot_chart, mosaic_plot, nrow = 2)
dev.off()

# Convert each plot into htmlwidgets
pie_chart_widget <- as_widget(pie_chart)
barplot_chart_widget <- as_widget(barplot_chart)
mosaic_plot_widget <- as_widget(mosaic_plot)

# Combine all widgets into a single HTML page
combined_widget <- tagList(
  pie_chart_widget,
  barplot_chart_widget,
  mosaic_plot_widget)
plotly_plot <- ggplotly(mosaic_plot)
# Save the combined widget as HTML
htmlwidgets::saveWidget(combined_widget, 'combined_plots.html')
#plot=grid.arrange(pie_chart, barplot_chart, mosaic_plot, nrow = 2)
"
      #write script to file      
      cat(script_content, file = file)
    }
  )
}

# Run the application 
shinyApp(ui, server)

