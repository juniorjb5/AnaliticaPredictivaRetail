#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(corrplot)
library(DT)
library(shinyWidgets)
library(readxl)
library(psych)
library(plotly)

# Cargar los datos
adidas_data <- read_excel("Adidas.xlsx", col_types = c("text", 
                                                       "date", "text", "text", "text", "text", 
                                                       "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "text"))
#View(adidas_data)



adidas_data$retailer <- as.factor(adidas_data$retailer)
adidas_data$Region <- as.factor(adidas_data$Region)
adidas_data$State <- as.factor(adidas_data$State)
adidas_data$City <- as.factor(adidas_data$City)
adidas_data$Product <- as.factor(adidas_data$Product)
adidas_data$sales_method <- as.factor(adidas_data$sales_method)



#View(adidas_data)

# UI del Tablero
ui <- dashboardPage(
  dashboardHeader(title = "Adidas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estadísticas Descriptivas", tabName = "descriptives", icon = icon("chart-bar")),
      menuItem("Matriz de Correlación", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Modelo de Regresión", tabName = "regression", icon = icon("calculator")),
      menuItem("Predicción", tabName = "prediction", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Estadísticas Descriptivas
      tabItem(tabName = "descriptives",
              fluidRow(
                box(title = "Distribuciones de Variables Cuantitativas", status = "primary", solidHeader = TRUE, width = 6,
                    selectInput("variable", "Selecciona Variable", choices = names(adidas_data[, sapply(adidas_data, is.numeric)])),
                    plotOutput("histPlot")),
                box(title = "Distribuciones de Variables Categóricas", status = "primary", solidHeader = TRUE, width = 6,
                    selectInput("cat_variable", "Selecciona Variable Categórica", choices = names(adidas_data[, sapply(adidas_data, is.factor)])),
                    plotOutput("barPlot"))
              ),
              fluidRow(
                box(title = "Resumen de promedios por Estado, Retailer y Producto", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("Estado", "Selecciona el Estado", choices = levels(adidas_data$State)),
                    selectInput("variable1", "Selecciona Variable", choices = names(adidas_data[, sapply(adidas_data, is.numeric)])),
                    actionButton("filtro", "Filtrar"),
                    plotOutput("SEMplot"))
              ),
              fluidRow(
                box(title = "Resumen Descriptivo General", status = "primary", solidHeader = TRUE, width = 12,
                    dataTableOutput("summaryTable1"))
              )),
      
      # Matriz de Correlación
      tabItem(tabName = "correlation",
              fluidRow(
                box(title = "Matriz de Correlación", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("correlationPlot"))
              )),
      
      # Modelo de Regresión
      tabItem(tabName = "regression",
              fluidRow(
                box(title = "Resultado del Modelo", status = "primary", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("modelSummary")),
                box(title = "Diagrama de Dispersión", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("regresionPlot")),
                box(title = "Diagrama de Dispersión", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("regresionPlot2")),
                box(title = "Diagrama de Dispersión", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("regresionPlot3")),
                box(title = "Diagrama de Dispersión", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("regresionPlot4")),
                box(title = "Diagnóstico de Residuos", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("residualPlot")),
                box(title = "Supuestos", status = "primary", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("Supuestos1")),
                box(title = "Supuestos", status = "primary", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("Supuestos2"))
              )),
      
      # Predicción
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Ingresar Nueva Información", status = "primary", solidHeader = TRUE, width = 6,
                    numericInput("price_per_unit", "Precio por Unidad", value = 50),
                    numericInput("units_sold", "Unidades Vendidas", value = 100),
                    selectInput("sales_method", "Método de Venta", choices = unique(adidas_data$sales_method)),
                    selectInput("retailer", "Retailer", choices = unique(adidas_data$retailer)),
                    actionButton("predictBtn", "Predecir")),
                box(title = "Resultado de Predicción", status = "primary", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("predictionResult"))
              ))
    )
  )
)

# Server del Tablero
server <- function(input, output) {
  
  # Estadísticas descriptivas
  output$histPlot <- renderPlot({
    ggplot(adidas_data, aes_string(x = input$variable)) + 
      geom_histogram(bins = 30, fill = "steelblue", color = "white") + 
      theme_minimal() + labs(x = input$variable, y = "Frecuencia")
  })
  
  output$barPlot <- renderPlot({
    ggplot(adidas_data, aes_string(x = input$cat_variable)) + 
      geom_bar(fill = "lightgreen") + 
      theme_minimal() + labs(x = input$cat_variable, y = "Frecuencia")
  })
  
  
  observeEvent(input$filtro, {  
    
    output$SEMplot <- renderPlot({
      
      plotdata<-adidas_data %>%
        filter(State == input$Estado) %>%
        group_by(Product,retailer) %>% 
        summarize(n = n(),
                  mean = mean(.data[[input$variable1]]),  # Utiliza .data para acceder a input$variable1
                  sd = sd(.data[[input$variable1]]),
                  se = sd / sqrt(n),
                  .groups = 'drop') 
      
      pd <- position_dodge(0.2)
      
      ggplot(plotdata, 
             aes(x = factor(Product), 
                 y = mean, 
                 group=retailer, 
                 color=retailer)) +
        geom_point(position=pd, 
                   size = 3) +
        geom_line(position = pd, 
                  size = 1) +
        geom_errorbar(aes(ymin = mean - se, 
                          ymax = mean + se), 
                      width = .1, 
                      position = pd, 
                      size = 1) +
        scale_y_continuous(label = scales::dollar) +
        scale_color_brewer(palette="Set1") +
        theme_minimal() +
        labs(title = "Promedios por Productos and Retailers",
             subtitle = "(promedio +/- error estándar)",
             x = "", 
             y = "",
             color = "Retailer")
      
      
    })
    
  })
  
  
  output$summaryTable1<-renderDataTable({
    summary_data <- describe(adidas_data)
    
    desc_filtered <- summary_data[, !colnames(summary_data) %in% c("trimmed","mad")]
    
    print(desc_filtered, digits=1)
  })
  
  # Matriz de correlación
  output$correlationPlot <- renderPlot({
    corr_matrix <- cor(adidas_data %>% select_if(is.numeric))
    corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
  })
  
  # Modelo de regresión
  modelo <- lm(operating_profit ~ price_per_unit + units_sold + as.factor(sales_method) + as.factor(retailer), data = adidas_data)
  
  output$modelSummary <- renderPrint({
    summary(modelo)
  })
  
  output$regresionPlot <- renderPlotly({
    
    relacion1<-ggplot(adidas_data, 
                      aes(x = price_per_unit     , 
                          y = operating_profit, 
                          color = retailer)) +
      geom_point(size = 3, 
                 alpha = .6) +
      labs(title = "Relación entre Operating profit y Price per Unit - Retailer")
    
    
    plotly::ggplotly(relacion1)
    
  })
  
  
  output$regresionPlot2 <- renderPlotly({
    
    relacion2<-ggplot(adidas_data, 
                      aes(x = units_sold, 
                          y = operating_profit, 
                          color = retailer)) +
      geom_point(size = 3, 
                 alpha = .6) +
      labs(title = "Relación entre Operating profit y Units Sold - Retailer")
    
    
    plotly::ggplotly(relacion2)
    
  })
  
  
  output$regresionPlot3 <- renderPlotly({
    
    relacion3<-ggplot(adidas_data, 
                      aes(x = price_per_unit, 
                          y = operating_profit, 
                          color = sales_method)) +
      geom_point(size = 3, 
                 alpha = .6) +
      labs(title = "Relación entre Operating profit y Price per Unit - Sales Method")
    
    
    plotly::ggplotly(relacion3)
    
  })
  
  output$regresionPlot4 <- renderPlotly({
    
    relacion4<-ggplot(adidas_data, 
                      aes(x = units_sold, 
                          y = operating_profit, 
                          color = sales_method)) +
      geom_point(size = 3, 
                 alpha = .6) +
      labs(title = "Relación entre Operating profit y Units Sold - Sales Method")
    
    
    plotly::ggplotly(relacion4)
    
  })
  
  
  output$residualPlot <- renderPlot({
    plot(modelo$residuals, main = "Residuos del Modelo", ylab = "Residuos", xlab = "Índice", col = "blue")
    abline(h = 0, col = "red", lty = 2)
  })
  
  
  
  output$Supuestos1 <- renderPrint({
    shapiro.test(modelo$residuals[1:5000])
    
    
  })
  
  output$Supuestos2 <- renderPrint({
    library(lmtest)
    bptest(modelo)
  })
  
  
  # Predicción
  observeEvent(input$predictBtn, {
    new_data <- data.frame(
      price_per_unit = input$price_per_unit,
      units_sold = input$units_sold,
      sales_method = factor(input$sales_method, levels = levels(adidas_data$sales_method)),
      retailer = factor(input$retailer, levels = levels(adidas_data$retailer))
    )
    
    prediction <- predict(modelo, newdata = new_data, interval = "prediction")
    
    output$predictionResult <- renderPrint({
      print(paste("La predicción de ventas totales es: $", round(prediction[1], 2)))
      
      
      print(paste("El intervalo de confianza para la predicción es:", round(prediction[2], 2), " - ", round(prediction[3], 2), "con una confianza del 95%"))
      
    })
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
