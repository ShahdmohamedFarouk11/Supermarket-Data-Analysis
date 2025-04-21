library(arules)
library(readxl)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(DT)
library(scales)

ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("file-upload")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-line")),
      menuItem("Analysis Configuration", tabName = "analysis_config", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_box"),
      valueBoxOutput("average_spending_box"),
      valueBoxOutput("record_count_box")
    ),
    tabItems(
      tabItem(tabName = "data_input",
              fluidRow(
                box(title = "Dataset Input", status = "primary", solidHeader = TRUE, width = 12,
                    fileInput("dataset_path", "Select Dataset (Excel file):", accept = c(".xlsx")),
                    actionButton("run_button", "Run Analysis", class = "btn-primary")
                ),
                box(title = "Uploaded Data", status = "info", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("uploaded_data"))
              )
      ),
      tabItem(tabName = "visualizations",
              fluidRow(
                box(title = "Age vs Spending", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("age_vs_spending")
                ),
                box(title = "City vs Spending", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("city_vs_spending")
                ),
                box(title = "Cash vs Credit Totals", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("cash_vs_credit")
                ),
                box(title = "Spending Distribution", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("spending_distribution")
                )
              )
      ),
      tabItem(tabName = "analysis_config",
              fluidRow(
                box(title = "Aggregated Data by Cluster", status = "info", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("aggregated_results")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactiveVal()
  
  observeEvent(input$run_button, {
    req(input$dataset_path)
    
    raw_data <- readxl::read_excel(input$dataset_path$datapath)
    raw_data <- raw_data %>% filter_at(vars(total, paymentType, age, city), all_vars(!is.na(.)))
    data(raw_data)
    
    total <- sum(raw_data$total, na.rm = TRUE)
    avg_spending <- mean(raw_data$total, na.rm = TRUE)
    record_count <- nrow(raw_data)
    
    output$total_box <- renderValueBox({
      valueBox(value = total, subtitle = "Total Spending", icon = icon("dollar-sign"), color = "green")
    })
    output$average_spending_box <- renderValueBox({
      valueBox(value = round(avg_spending, 2), subtitle = "Average Spending", icon = icon("chart-line"), color = "blue")
    })
    output$record_count_box <- renderValueBox({
      valueBox(value = record_count, subtitle = "Total Records", icon = icon("file"), color = "purple")
    })
    
    output$uploaded_data <- DT::renderDataTable({
      raw_data
    })
    
    output$age_vs_spending <- renderPlot({
      raw_data %>%
        group_by(age) %>%
        summarise(total_spending = sum(total)) %>%
        ggplot() +
        theme_minimal() +
        labs(title = "Age vs Total Spending", x = "Age", y = "Total Spending") +
        scale_fill_manual(values = c("#A9DFBF", "#AED6F1", "#FDEBD0", "#E8DAEF", 
                                     "#F5B7B1", "#F9E79F", "#B0C4DE", "#FADBD8", 
                                     "#FCF3CF", "#D4EFDF", "#D1F2EB", "#EBDEF0"))+
        geom_bar(aes(x = as.factor(age), y = total_spending, fill = as.factor(age)), stat = "identity")
    })
    
    output$city_vs_spending <- renderPlot({
      raw_data %>%
        group_by(city) %>%
        summarise(total_spending = sum(total)) %>%
        ggplot() +
        theme_minimal() +
        labs(title = "City vs Total Spending", x = "City", y = "Total Spending") +
        geom_bar(aes(x = reorder(city, -total_spending), y = total_spending, fill = city), stat = "identity")+
        scale_fill_manual(values = c("#AED6F1", "#FDEBD0", "#E8DAEF", 
                                     "#F9E79F", "#B0C4DE", "#FADBD8", 
                                     "#FCF3CF", "#D4EFDF", "#D1F2EB", "#EBDEF0"))
    })
    
    output$cash_vs_credit <- renderPlot({
      raw_data %>%
        group_by(paymentType) %>%
        summarise(total_spending = sum(total)) %>%
        ggplot() +
        theme_minimal() +
        labs(title = "Cash vs Credit Totals", x = "Payment Type", y = "Total Spending") +
        geom_bar(aes(x = paymentType, y = total_spending, fill = paymentType), stat = "identity") +
        scale_fill_manual(values = c("Cash" = "#AED6F1", "Credit" = "#F9E79F")) + 
        scale_y_continuous(labels = scales::label_comma())
    })
    
    output$spending_distribution <- renderPlot({
      ggplot(raw_data, aes(x = total)) +
        theme_minimal() +
        labs(title = "Spending Distribution", x = "Spending", y = "Density") +
        geom_density(fill = "#FFFFBF", alpha = 0.6) +
        scale_y_continuous(labels = label_comma())    
    })
    
    Min_support <- as.numeric(readline("Enter a minimum support between 0.001 and 1:"))
    
    while (Min_support > 1 || Min_support < 0.001) {
      print("Out of range number!!")
      Min_support <- as.numeric(readline("Enter a minimum support between 0.001 and 1:"))
    }
    
    file_path <- "C:\\Users\\LENOVO\\Downloads\\data science project\\cleaning_data.xlsx"
    datafile <- read_xlsx(file_path)
    print(colnames(datafile))
    datamatrix <- datafile[, c("age", "total")]
    
    k <- as.integer(readline("Enter the number of clusters (between 2 and 4): "))
    while(k < 2 | k > 4) {
      print("Out of limit! Please enter a number between 2 and 4.")
      k <- as.integer(readline("Enter the number of clusters (between 2 and 4): "))
    }
    result <- kmeans(datamatrix, centers=k)
    datafile$cluster <- result$cluster
    finaldata <- datafile[, c("customer", "age", "total", "cluster")]
    View(finaldata)
    print(finaldata, n = 9482)
    
    aggregated_data <- aggregate(cbind(age, total) ~ cluster, data = datafile , FUN = mean)
    
    print("Aggregated Data by Cluster:")
    print(aggregated_data)
    output$aggregated_results <- DT::renderDataTable({ aggregated_data })
    
    cleaned_data <- read_excel(input$dataset_path$datapath, col_types = c("text", "skip", "skip", "skip", "skip", "skip", "skip", "skip"))
    cleaned_data_csv <- "C:\\Users\\LENOVO\\Downloads\\data science project\\cleaned_data.csv"
    write.csv(cleaned_data, file = cleaned_data_csv, row.names = FALSE)
    my_data <- read.transactions(cleaned_data_csv, sep = ",")
    
    my_data <- apriori(my_data, parameter = list(supp = Min_support, conf = 0.6))
    AR_file <- as(my_data, "data.frame")
    AR_csv <- "C:\\Users\\LENOVO\\Downloads\\data science project\\AR_file.csv"
    write.csv(AR_file, file = AR_csv, row.names = FALSE)
  })
  
}

shinyApp(ui = ui, server = server)
