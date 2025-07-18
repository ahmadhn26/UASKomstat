# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(corrplot)
library(car)
library(tseries)
library(nortest)
library(officer)
library(flextable)
library(dplyr)
library(tidyr)  # Added for spread function
library(reshape2)
library(RColorBrewer)

# Load data with relative paths
sovi_data <- read.csv("sovi_data.csv")
distance_data <- read.csv("distance.csv")

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(title = "Socio-Economic Vulnerability Analyzer (SEVA)"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-pie")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("vial")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("calculator")),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Modern color scheme and improved styling */
        .content-wrapper, .right-side {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          min-height: 100vh;
        }
        
        .main-sidebar {
          background-color: #2c3e50 !important;
        }
        
        .main-sidebar .sidebar .sidebar-menu > li > a {
          color: #ecf0f1 !important;
          border-left: 3px solid transparent;
          transition: all 0.3s ease;
        }
        
        .main-sidebar .sidebar .sidebar-menu > li.active > a,
        .main-sidebar .sidebar .sidebar-menu > li:hover > a {
          background-color: #34495e !important;
          border-left: 3px solid #3498db;
          color: #ffffff !important;
        }
        
        .box {
          margin-bottom: 25px;
          border-radius: 15px;
          box-shadow: 0 8px 25px rgba(0,0,0,0.15);
          border: none;
          background: rgba(255,255,255,0.95);
          backdrop-filter: blur(10px);
          transition: transform 0.3s ease, box-shadow 0.3s ease;
        }
        
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 15px 35px rgba(0,0,0,0.2);
        }
        
        .box-header {
          border-radius: 15px 15px 0 0;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white !important;
          padding: 20px;
        }
        
        .box-header .box-title {
          color: white !important;
          font-weight: 600;
          font-size: 18px;
        }
        
        .box-body {
          padding: 25px;
          background: rgba(255,255,255,0.98);
          border-radius: 0 0 15px 15px;
        }
        
        /* Modern button styling */
        .btn {
          border-radius: 25px;
          padding: 10px 25px;
          font-weight: 500;
          text-transform: uppercase;
          letter-spacing: 1px;
          transition: all 0.3s ease;
          border: none;
          margin: 8px 5px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        }
        
        .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(0,0,0,0.3);
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
        }
        
        .btn-success {
          background: linear-gradient(135deg, #56ab2f 0%, #a8e6cf 100%);
          color: white;
        }
        
        .btn-warning {
          background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
          color: white;
        }
        
        .btn-info {
          background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
          color: white;
        }
        
        .btn-danger {
          background: linear-gradient(135deg, #fa709a 0%, #fee140 100%);
          color: white;
        }
        
        /* Form controls */
        .form-control {
          border-radius: 10px;
          border: 2px solid #e9ecef;
          padding: 12px 15px;
          transition: all 0.3s ease;
        }
        
        .form-control:focus {
          border-color: #667eea;
          box-shadow: 0 0 15px rgba(102, 126, 234, 0.3);
        }
        
        .selectize-input {
          border-radius: 10px !important;
          border: 2px solid #e9ecef !important;
          padding: 12px 15px !important;
        }
        
        .selectize-input.focus {
          border-color: #667eea !important;
          box-shadow: 0 0 15px rgba(102, 126, 234, 0.3) !important;
        }
        
        /* Data tables */
        .dataTables_wrapper {
          background: white;
          border-radius: 15px;
          padding: 20px;
          box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        }
        
        /* Main header */
        .main-header .navbar {
          background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%) !important;
          border: none;
        }
        
        .main-header .logo {
          background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%) !important;
          color: white !important;
          font-weight: 600;
        }
        
        /* Improved spacing and typography */
        h3, h4 {
          color: #2c3e50;
          font-weight: 600;
          margin-bottom: 20px;
        }
        
        p, li {
          color: #555;
          line-height: 1.6;
          margin-bottom: 10px;
        }
        
        .download-btn {
          margin: 10px 5px;
          min-width: 200px;
        }
        
        /* Modern cards for content sections */
        .info-card {
          background: rgba(255,255,255,0.95);
          border-radius: 15px;
          padding: 25px;
          margin-bottom: 20px;
          box-shadow: 0 8px 25px rgba(0,0,0,0.1);
          border-left: 5px solid #667eea;
        }
        
        /* Loading and active states */
        .shiny-output-error {
          background: #fee;
          border: 1px solid #fcc;
          border-radius: 10px;
          padding: 15px;
        }
        
        .shiny-output-error:before {
          content: '⚠ ';
          color: #f66;
        }
        
        /* Responsive design improvements */
        @media (max-width: 768px) {
          .box {
            margin-bottom: 15px;
          }
          
          .download-btn {
            min-width: 150px;
            margin: 5px 2px;
          }
        }
      "))
    ),
    
    tabItems(
      # Beranda Tab
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "🏠 Selamat Datang di SEVA", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "info-card",
                    h3("📊 Socio-Economic Vulnerability Analyzer"),
                    p("Dashboard ini dirancang untuk menganalisis kerentanan sosial-ekonomi berbagai daerah menggunakan berbagai indikator demografis, ekonomi, dan sosial."),
                    
                    h4("🎯 Fitur Utama:"),
                    div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin: 20px 0;",
                      div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 15px; text-align: center;",
                        h5("🗄️ Manajemen Data"),
                        p("Kategorisasi dan pengelolaan dataset")
                      ),
                      div(style = "background: linear-gradient(135deg, #56ab2f 0%, #a8e6cf 100%); color: white; padding: 20px; border-radius: 15px; text-align: center;",
                        h5("📈 Eksplorasi Data"),
                        p("Visualisasi interaktif dan analisis deskriptif")
                      ),
                      div(style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); color: white; padding: 20px; border-radius: 15px; text-align: center;",
                        h5("🧪 Uji Asumsi"),
                        p("Pengujian normalitas dan homogenitas")
                      ),
                      div(style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 20px; border-radius: 15px; text-align: center;",
                        h5("📊 Statistik Inferensia"),
                        p("T-test, ANOVA, dan uji proporsi")
                      ),
                      div(style = "background: linear-gradient(135deg, #fa709a 0%, #fee140 100%); color: white; padding: 20px; border-radius: 15px; text-align: center;",
                        h5("📉 Regresi Linear"),
                        p("Analisis regresi berganda dan diagnostik")
                      )
                    )
                  ),
                  br(),
                  div(style = "text-align: center;",
                    downloadButton("download_welcome", "📄 Download Laporan Beranda", class = "btn btn-primary download-btn")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Dataset Metadata", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  div(class = "info-card",
                    h4("📊 Informasi Dataset"),
                    p("Berikut adalah metadata dari dataset yang digunakan dalam analisis kerentanan sosial-ekonomi:")
                  ),
                  DT::dataTableOutput("metadata_table"),
                  br(),
                  div(style = "text-align: center;",
                    downloadButton("download_metadata", "📥 Download Metadata", class = "btn btn-success download-btn")
                  )
                )
              )
      ),
      
      # Manajemen Data Tab
      tabItem(tabName = "manajemen",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("cont_var", "Pilih Variabel Kontinu:",
                              choices = names(select_if(sovi_data, is.numeric))),
                  numericInput("n_bins", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                  actionButton("categorize", "Kategorisasi Data", class = "btn btn-warning")
                ),
                
                box(
                  title = "📊 Hasil Kategorisasi Data", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  DT::dataTableOutput("categorized_data"),
                  br(),
                  verbatimTextOutput("categorization_interpretation"),
                  br(),
                  downloadButton("download_categorization", "Download Hasil Kategorisasi", class = "btn btn-primary download-btn"),
                  downloadButton("download_management_report", "Download Laporan Lengkap", class = "btn btn-success download-btn")
                )
              )
      ),
      
      # Eksplorasi Data Tab
      tabItem(tabName = "eksplorasi",
              tabBox(
                title = "📈 Eksplorasi Data",
                width = 12,
                
                # Tab 1: Statistik Deskriptif
                tabPanel("📊 Statistik Deskriptif",
                         fluidRow(
                           box(
                             title = "⚙️ Pengaturan Variabel",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 4,
                             checkboxGroupInput("desc_vars", "Pilih Variabel:",
                                                choices = names(select_if(sovi_data, is.numeric)),
                                                selected = names(select_if(sovi_data, is.numeric))[1:5])
                           ),
                           
                           box(
                             title = "Statistik Deskriptif",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 8,
                             DT::dataTableOutput("descriptive_stats"),
                             br(),
                             verbatimTextOutput("descriptive_interpretation"),
                             br(),
                             downloadButton("download_descriptive", "Download Statistik Deskriptif", class = "btn btn-primary download-btn")
                           )
                         )
                ),
                
                # Tab 2: Visualisasi Grafis
                tabPanel("Visualisasi Grafis",
                         fluidRow(
                           box(
                             title = "Pengaturan Plot",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 4,
                             selectInput("plot_type", "Jenis Plot:",
                                         choices = c("Histogram" = "hist", "Box Plot" = "box", "Scatter Plot" = "scatter")),
                             selectInput("plot_var1", "Variabel 1:", choices = names(select_if(sovi_data, is.numeric))),
                             conditionalPanel(
                               condition = "input.plot_type == 'scatter'",
                               selectInput("plot_var2", "Variabel 2:", choices = names(select_if(sovi_data, is.numeric)))
                             )
                           ),
                           
                           box(
                             title = "Visualisasi",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 8,
                             plotOutput("exploration_plot", height = "400px"),
                             br(),
                             verbatimTextOutput("plot_interpretation"),
                             br(),
                             downloadButton("download_plot", "Download Plot", class = "btn btn-primary download-btn")
                           )
                         )
                ),
                
                # Tab 3: Peta Korelasi
                tabPanel("Peta Korelasi",
                         fluidRow(
                           box(
                             title = "Correlation Heatmap",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             plotOutput("correlation_heatmap", height = "600px"),
                             br(),
                             verbatimTextOutput("correlation_interpretation"),
                             br(),
                             downloadButton("download_correlation", "Download Peta Korelasi", class = "btn btn-primary download-btn"),
                             downloadButton("download_exploration_report", "Download Laporan Eksplorasi", class = "btn btn-success download-btn")
                           )
                         )
                )
              )
      ),
      
      # Uji Asumsi Tab
      tabItem(tabName = "asumsi",
              fluidRow(
                box(
                  title = "Pengaturan Uji",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("normality_var", "Variabel untuk Uji Normalitas:",
                              choices = names(select_if(sovi_data, is.numeric))),
                  selectInput("group_var", "Variabel Kelompok (untuk Homogenitas):",
                              choices = c("None", names(sovi_data))),
                  actionButton("run_tests", "Jalankan Uji", class = "btn btn-warning")
                ),
                
                box(
                  title = "Hasil Uji Normalitas",
                  status = "success",
                  solidHeader = TRUE,
                  width = 8,
                  verbatimTextOutput("normality_test"),
                  plotOutput("qq_plot", height = "300px"),
                  br(),
                  verbatimTextOutput("normality_interpretation"),
                  br(),
                  downloadButton("download_normality", "Download Uji Normalitas", class = "btn btn-primary download-btn")
                )
              ),
              
              fluidRow(
                box(
                  title = "Hasil Uji Homogenitas",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("homogeneity_test"),
                  br(),
                  verbatimTextOutput("homogeneity_interpretation"),
                  br(),
                  downloadButton("download_homogeneity", "Download Uji Homogenitas", class = "btn btn-primary download-btn"),
                  downloadButton("download_assumption_report", "Download Laporan Uji Asumsi", class = "btn btn-success download-btn")
                )
              )
      ),
      
      # Statistik Inferensia Tab
      tabItem(tabName = "inferensia",
              tabBox(
                title = "Statistik Inferensia",
                width = 12,
                
                # Tab 1: Uji Beda Rata-Rata
                tabPanel("Uji Beda Rata-Rata",
                         fluidRow(
                           box(
                             title = "1-Sample T-Test",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,
                             selectInput("ttest1_var", "Variabel:", choices = names(select_if(sovi_data, is.numeric))),
                             numericInput("ttest1_mu", "Nilai Hipotesis (μ₀):", value = 0),
                             actionButton("run_ttest1", "Jalankan Uji", class = "btn btn-primary"),
                             br(), br(),
                             verbatimTextOutput("ttest1_result"),
                             verbatimTextOutput("ttest1_interpretation"),
                             downloadButton("download_ttest1", "Download Hasil", class = "btn btn-success download-btn")
                           ),
                           
                           box(
                             title = "2-Sample Independent T-Test",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,
                             selectInput("ttest2_var", "Variabel Numerik:", choices = names(select_if(sovi_data, is.numeric))),
                             selectInput("ttest2_group", "Variabel Kelompok:", choices = names(sovi_data)),
                             actionButton("run_ttest2", "Jalankan Uji", class = "btn btn-primary"),
                             br(), br(),
                             verbatimTextOutput("ttest2_result"),
                             verbatimTextOutput("ttest2_interpretation"),
                             downloadButton("download_ttest2", "Download Hasil", class = "btn btn-success download-btn")
                           )
                         )
                ),
                
                # Tab 2: Uji Proporsi & Ragam
                tabPanel("Uji Proporsi & Ragam",
                         fluidRow(
                           box(
                             title = "1-Sample Proportion Test",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,
                             selectInput("prop1_var", "Variabel:", choices = names(sovi_data)),
                             numericInput("prop1_p", "Proporsi Hipotesis (p₀):", value = 0.5, min = 0, max = 1, step = 0.01),
                             actionButton("run_prop1", "Jalankan Uji", class = "btn btn-info"),
                             br(), br(),
                             verbatimTextOutput("prop1_result"),
                             verbatimTextOutput("prop1_interpretation"),
                             downloadButton("download_prop1", "Download Hasil", class = "btn btn-success download-btn")
                           ),
                           
                           box(
                             title = "1-Sample Variance Test",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,
                             selectInput("var1_var", "Variabel:", choices = names(select_if(sovi_data, is.numeric))),
                             numericInput("var1_sigma", "Varians Hipotesis (σ²₀):", value = 1),
                             actionButton("run_var1", "Jalankan Uji", class = "btn btn-info"),
                             br(), br(),
                             verbatimTextOutput("var1_result"),
                             verbatimTextOutput("var1_interpretation"),
                             downloadButton("download_var1", "Download Hasil", class = "btn btn-success download-btn")
                           )
                         )
                ),
                
                # Tab 3: ANOVA
                tabPanel("ANOVA",
                         fluidRow(
                           box(
                             title = "One-Way ANOVA",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 6,
                             selectInput("anova1_dep", "Variabel Dependen:", choices = names(select_if(sovi_data, is.numeric))),
                             selectInput("anova1_indep", "Variabel Independen:", choices = names(sovi_data)),
                             actionButton("run_anova1", "Jalankan ANOVA", class = "btn btn-warning"),
                             br(), br(),
                             verbatimTextOutput("anova1_result"),
                             verbatimTextOutput("anova1_interpretation"),
                             downloadButton("download_anova1", "Download Hasil", class = "btn btn-success download-btn")
                           ),
                           
                           box(
                             title = "Two-Way ANOVA",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 6,
                             selectInput("anova2_dep", "Variabel Dependen:", choices = names(select_if(sovi_data, is.numeric))),
                             selectInput("anova2_indep1", "Variabel Independen 1:", choices = names(sovi_data)),
                             selectInput("anova2_indep2", "Variabel Independen 2:", choices = names(sovi_data)),
                             actionButton("run_anova2", "Jalankan ANOVA", class = "btn btn-warning"),
                             br(), br(),
                             verbatimTextOutput("anova2_result"),
                             verbatimTextOutput("anova2_interpretation"),
                             downloadButton("download_anova2", "Download Hasil", class = "btn btn-success download-btn"),
                             br(),
                             downloadButton("download_inferential_report", "Download Laporan Inferensia", class = "btn btn-danger download-btn")
                           )
                         )
                )
              )
      ),
      
      # Regresi Linear Berganda Tab
      tabItem(tabName = "regresi",
              fluidRow(
                column(4,
                       box(
                         title = "Pengaturan Model",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 12,
                         selectInput("reg_dep", "Variabel Dependen:", 
                                     choices = names(select_if(sovi_data, is.numeric))),
                         checkboxGroupInput("reg_indep", "Variabel Independen:",
                                            choices = names(select_if(sovi_data, is.numeric)),
                                            selected = names(select_if(sovi_data, is.numeric))[1:3]),
                         actionButton("run_regression", "Jalankan Regresi", class = "btn btn-warning")
                       )
                ),
                
                column(8,
                       tabBox(
                         title = "Hasil Regresi",
                         width = 12,
                         
                         tabPanel("Ringkasan Model",
                                  verbatimTextOutput("regression_summary"),
                                  br(),
                                  verbatimTextOutput("regression_interpretation"),
                                  br(),
                                  downloadButton("download_regression_summary", "Download Ringkasan", class = "btn btn-primary download-btn")
                         ),
                         
                         tabPanel("Uji Asumsi Model",
                                  h4("Multicollinearity (VIF)"),
                                  verbatimTextOutput("vif_test"),
                                  br(),
                                  h4("Normalitas Residual"),
                                  plotOutput("residual_qq", height = "300px"),
                                  br(),
                                  h4("Homoskedastisitas"),
                                  plotOutput("residual_fitted", height = "300px"),
                                  br(),
                                  verbatimTextOutput("assumption_interpretation"),
                                  br(),
                                  downloadButton("download_assumptions", "Download Uji Asumsi", class = "btn btn-primary download-btn"),
                                  downloadButton("download_regression_report", "Download Laporan Regresi", class = "btn btn-success download-btn")
                         )
                       )
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Metadata table
  metadata <- data.frame(
    Label = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
              "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", 
              "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
    Variable = c("District Code", "Children", "Female", "Elderly", "Female household", 
                 "Household members", "Non-electric household", "Low education", 
                 "Population growth", "Poverty", "Illiteracy", "Training", 
                 "Disaster prone", "Homeownership", "Drainage", "Water source", "Population"),
    Description = c("Code of the region/district", 
                    "Percentage of under five years old population",
                    "Percentage of female population",
                    "Percentage of 65 years old and over population",
                    "Percentage of households with female head of household",
                    "The average number of household members in one district",
                    "Percentage of households that do not use electricity as lighting sources",
                    "Percentage of 15 years and over population with low education",
                    "Percentage of population change",
                    "Percentage of poor people",
                    "Percentage of population that cannot read and write",
                    "Percentage of households that did not get disaster training",
                    "Percentage of households living in disaster-prone areas",
                    "Percentage of households renting a house",
                    "Percentage of households that did not have a drainage system",
                    "Percentage of households that use piped water",
                    "Number of Population")
  )
  
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(metadata, options = list(pageLength = 17, scrollX = TRUE))
  })
  
  # Data Management
  categorized_data_reactive <- eventReactive(input$categorize, {
    req(input$cont_var, input$n_bins)
    
    var_data <- sovi_data[[input$cont_var]]
    breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_bins + 1), na.rm = TRUE)
    
    categorized <- cut(var_data, breaks = breaks, include.lowest = TRUE, 
                       labels = paste0("Kategori_", 1:input$n_bins))
    
    result_data <- data.frame(
      Original = var_data,
      Categorized = categorized
    )
    names(result_data) <- c(input$cont_var, paste0(input$cont_var, "_Kategori"))
    
    return(result_data)
  })
  
  output$categorized_data <- DT::renderDataTable({
    req(categorized_data_reactive())
    DT::datatable(categorized_data_reactive(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$categorization_interpretation <- renderText({
    req(categorized_data_reactive())
    paste("Interpretasi: Variabel", input$cont_var, "telah berhasil dikategorisasi menjadi", 
          input$n_bins, "kategori berdasarkan kuantil. Kategorisasi ini membantu dalam",
          "analisis data dengan mengubah variabel kontinu menjadi variabel kategorikal",
          "yang dapat digunakan untuk analisis lebih lanjut seperti ANOVA atau Chi-square test.")
  })
  
  # Descriptive Statistics - FIXED
  output$descriptive_stats <- DT::renderDataTable({
    req(input$desc_vars)
    
    desc_data <- sovi_data[, input$desc_vars, drop = FALSE]
    
    # Calculate statistics for each variable
    desc_summary <- data.frame(
      Variable = names(desc_data),
      Mean = sapply(desc_data, function(x) round(mean(x, na.rm = TRUE), 3)),
      Median = sapply(desc_data, function(x) round(median(x, na.rm = TRUE), 3)),
      SD = sapply(desc_data, function(x) round(sd(x, na.rm = TRUE), 3)),
      Min = sapply(desc_data, function(x) round(min(x, na.rm = TRUE), 3)),
      Max = sapply(desc_data, function(x) round(max(x, na.rm = TRUE), 3)),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(desc_summary, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$descriptive_interpretation <- renderText({
    req(input$desc_vars)
    paste("Interpretasi: Statistik deskriptif menunjukkan karakteristik dasar dari variabel yang dipilih.",
          "Mean dan median memberikan gambaran tentang tendensi sentral, sedangkan standar deviasi",
          "menunjukkan variabilitas data. Perbedaan antara mean dan median dapat mengindikasikan",
          "adanya skewness dalam distribusi data.")
  })
  
  # Visualization
  output$exploration_plot <- renderPlot({
    req(input$plot_var1)
    
    if(input$plot_type == "hist") {
      ggplot(sovi_data, aes_string(x = input$plot_var1)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram of", input$plot_var1),
             x = input$plot_var1, y = "Frequency")
    } else if(input$plot_type == "box") {
      ggplot(sovi_data, aes_string(y = input$plot_var1)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Box Plot of", input$plot_var1),
             y = input$plot_var1)
    } else if(input$plot_type == "scatter" && !is.null(input$plot_var2)) {
      ggplot(sovi_data, aes_string(x = input$plot_var1, y = input$plot_var2)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        theme_minimal() +
        labs(title = paste("Scatter Plot:", input$plot_var1, "vs", input$plot_var2),
             x = input$plot_var1, y = input$plot_var2)
    }
  })
  
  output$plot_interpretation <- renderText({
    if(input$plot_type == "hist") {
      paste("Interpretasi: Histogram menunjukkan distribusi frekuensi dari variabel", input$plot_var1,
            ". Bentuk distribusi dapat memberikan informasi tentang normalitas data dan adanya outliers.")
    } else if(input$plot_type == "box") {
      paste("Interpretasi: Box plot menampilkan ringkasan lima angka (minimum, Q1, median, Q3, maksimum)",
            "dari variabel", input$plot_var1, ". Titik-titik di luar whiskers menunjukkan potensi outliers.")
    } else if(input$plot_type == "scatter") {
      paste("Interpretasi: Scatter plot menunjukkan hubungan antara", input$plot_var1, "dan", input$plot_var2,
            ". Garis regresi membantu memvisualisasikan tren hubungan linear antara kedua variabel.")
    }
  })
  
  # Correlation Heatmap
  output$correlation_heatmap <- renderPlot({
    numeric_data <- select_if(sovi_data, is.numeric)
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    melted_cor <- melt(cor_matrix)
    
    ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      coord_fixed()
  })
  
  output$correlation_interpretation <- renderText({
    numeric_data <- select_if(sovi_data, is.numeric)
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    # Find strongest positive and negative correlations
    cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
    max_cor <- which(cor_matrix == max(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    min_cor <- which(cor_matrix == min(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    
    paste("Interpretasi: Peta korelasi menunjukkan hubungan linear antara semua variabel numerik.",
          "Korelasi positif terkuat terjadi antara", rownames(cor_matrix)[max_cor[1]], "dan", 
          colnames(cor_matrix)[max_cor[2]], "dengan nilai", round(cor_matrix[max_cor], 3),
          ". Korelasi negatif terkuat terjadi antara", rownames(cor_matrix)[min_cor[1]], "dan",
          colnames(cor_matrix)[min_cor[2]], "dengan nilai", round(cor_matrix[min_cor], 3))
  })
  
  # Assumption Tests
  observeEvent(input$run_tests, {
    # Normality Test
    output$normality_test <- renderPrint({
      req(input$normality_var)
      var_data <- sovi_data[[input$normality_var]]
      var_data <- var_data[!is.na(var_data)]
      
      shapiro_test <- shapiro.test(var_data)
      ks_test <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
      
      cat("Uji Normalitas untuk variabel:", input$normality_var, "\n\n")
      cat("H0: Data berdistribusi normal\n")
      cat("H1: Data tidak berdistribusi normal\n\n")
      cat("Shapiro-Wilk Test:\n")
      print(shapiro_test)
      cat("\nKolmogorov-Smirnov Test:\n")
      print(ks_test)
    })
    
    output$qq_plot <- renderPlot({
      req(input$normality_var)
      var_data <- sovi_data[[input$normality_var]]
      ggplot(data.frame(sample = var_data), aes(sample = sample)) +
        stat_qq() + stat_qq_line() +
        theme_minimal() +
        labs(title = paste("Q-Q Plot for", input$normality_var))
    })
    
    output$normality_interpretation <- renderText({
      req(input$normality_var)
      var_data <- sovi_data[[input$normality_var]]
      var_data <- var_data[!is.na(var_data)]
      shapiro_test <- shapiro.test(var_data)
      
      if(shapiro_test$p.value > 0.05) {
        paste("Interpretasi: Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
              "), kita gagal menolak H0. Data variabel", input$normality_var, 
              "dapat dianggap berdistribusi normal pada tingkat signifikansi 5%.")
      } else {
        paste("Interpretasi: Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
              "), kita menolak H0. Data variabel", input$normality_var, 
              "tidak berdistribusi normal pada tingkat signifikansi 5%.")
      }
    })
    
    # Homogeneity Test
    output$homogeneity_test <- renderPrint({
      req(input$normality_var, input$group_var)
      if(input$group_var != "None") {
        var_data <- sovi_data[[input$normality_var]]
        group_data <- sovi_data[[input$group_var]]
        
        # Create groups for testing
        if(is.numeric(group_data)) {
          group_data <- cut(group_data, breaks = 3, labels = c("Low", "Medium", "High"))
        }
        
        complete_cases <- complete.cases(var_data, group_data)
        var_data <- var_data[complete_cases]
        group_data <- group_data[complete_cases]
        
        levene_test <- car::leveneTest(var_data, group_data)
        
        cat("Uji Homogenitas Varians (Levene's Test)\n\n")
        cat("H0: Varians antar kelompok homogen\n")
        cat("H1: Varians antar kelompok tidak homogen\n\n")
        print(levene_test)
      } else {
        cat("Pilih variabel kelompok untuk melakukan uji homogenitas")
      }
    })
    
    output$homogeneity_interpretation <- renderText({
      req(input$normality_var, input$group_var)
      if(input$group_var != "None") {
        var_data <- sovi_data[[input$normality_var]]
        group_data <- sovi_data[[input$group_var]]
        
        if(is.numeric(group_data)) {
          group_data <- cut(group_data, breaks = 3, labels = c("Low", "Medium", "High"))
        }
        
        complete_cases <- complete.cases(var_data, group_data)
        var_data <- var_data[complete_cases]
        group_data <- group_data[complete_cases]
        
        levene_test <- car::leveneTest(var_data, group_data)
        p_value <- levene_test$`Pr(>F)`[1]
        
        if(p_value > 0.05) {
          paste("Interpretasi: Berdasarkan uji Levene (p-value =", round(p_value, 4),
                "), kita gagal menolak H0. Varians antar kelompok dapat dianggap homogen",
                "pada tingkat signifikansi 5%.")
        } else {
          paste("Interpretasi: Berdasarkan uji Levene (p-value =", round(p_value, 4),
                "), kita menolak H0. Varians antar kelompok tidak homogen",
                "pada tingkat signifikansi 5%.")
        }
      }
    })
  })
  
  # Inferential Statistics
  
  # 1-Sample T-Test
  observeEvent(input$run_ttest1, {
    output$ttest1_result <- renderPrint({
      req(input$ttest1_var, input$ttest1_mu)
      var_data <- sovi_data[[input$ttest1_var]]
      var_data <- var_data[!is.na(var_data)]
      
      ttest_result <- t.test(var_data, mu = input$ttest1_mu)
      
      cat("One-Sample T-Test\n\n")
      cat("H0: μ =", input$ttest1_mu, "\n")
      cat("H1: μ ≠", input$ttest1_mu, "\n\n")
      print(ttest_result)
    })
    
    output$ttest1_interpretation <- renderText({
      req(input$ttest1_var, input$ttest1_mu)
      var_data <- sovi_data[[input$ttest1_var]]
      var_data <- var_data[!is.na(var_data)]
      ttest_result <- t.test(var_data, mu = input$ttest1_mu)
      
      if(ttest_result$p.value < 0.05) {
        paste("Interpretasi: Dengan p-value =", round(ttest_result$p.value, 4),
              "< 0.05, kita menolak H0. Terdapat perbedaan signifikan antara rata-rata sampel",
              "dengan nilai hipotesis", input$ttest1_mu, "pada tingkat signifikansi 5%.")
      } else {
        paste("Interpretasi: Dengan p-value =", round(ttest_result$p.value, 4),
              "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan antara rata-rata sampel",
              "dengan nilai hipotesis", input$ttest1_mu, "pada tingkat signifikansi 5%.")
      }
    })
  })
  
  # 2-Sample T-Test
  observeEvent(input$run_ttest2, {
    output$ttest2_result <- renderPrint({
      req(input$ttest2_var, input$ttest2_group)
      
      var_data <- sovi_data[[input$ttest2_var]]
      group_data <- sovi_data[[input$ttest2_group]]
      
      # Create binary groups if needed
      if(is.numeric(group_data)) {
        median_val <- median(group_data, na.rm = TRUE)
        group_data <- ifelse(group_data <= median_val, "Low", "High")
      }
      
      # Get unique groups and take first two
      unique_groups <- unique(group_data[!is.na(group_data)])
      if(length(unique_groups) >= 2) {
        group1_data <- var_data[group_data == unique_groups[1] & !is.na(var_data) & !is.na(group_data)]
        group2_data <- var_data[group_data == unique_groups[2] & !is.na(var_data) & !is.na(group_data)]
        
        ttest_result <- t.test(group1_data, group2_data)
        
        cat("Two-Sample Independent T-Test\n\n")
        cat("H0: μ1 = μ2\n")
        cat("H1: μ1 ≠ μ2\n\n")
        cat("Group 1 (", unique_groups[1], "):", length(group1_data), "observations\n")
        cat("Group 2 (", unique_groups[2], "):", length(group2_data), "observations\n\n")
        print(ttest_result)
      } else {
        cat("Error: Need at least 2 groups for comparison")
      }
    })
    
    output$ttest2_interpretation <- renderText({
      req(input$ttest2_var, input$ttest2_group)
      
      var_data <- sovi_data[[input$ttest2_var]]
      group_data <- sovi_data[[input$ttest2_group]]
      
      if(is.numeric(group_data)) {
        median_val <- median(group_data, na.rm = TRUE)
        group_data <- ifelse(group_data <= median_val, "Low", "High")
      }
      
      unique_groups <- unique(group_data[!is.na(group_data)])
      if(length(unique_groups) >= 2) {
        group1_data <- var_data[group_data == unique_groups[1] & !is.na(var_data) & !is.na(group_data)]
        group2_data <- var_data[group_data == unique_groups[2] & !is.na(var_data) & !is.na(group_data)]
        
        ttest_result <- t.test(group1_data, group2_data)
        
        if(ttest_result$p.value < 0.05) {
          paste("Interpretasi: Dengan p-value =", round(ttest_result$p.value, 4),
                "< 0.05, kita menolak H0. Terdapat perbedaan signifikan rata-rata",
                input$ttest2_var, "antara kelompok", unique_groups[1], "dan", unique_groups[2],
                "pada tingkat signifikansi 5%.")
        } else {
          paste("Interpretasi: Dengan p-value =", round(ttest_result$p.value, 4),
                "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan rata-rata",
                input$ttest2_var, "antara kelompok", unique_groups[1], "dan", unique_groups[2],
                "pada tingkat signifikansi 5%.")
        }
      }
    })
  })
  
  # Proportion Tests
  observeEvent(input$run_prop1, {
    output$prop1_result <- renderPrint({
      req(input$prop1_var, input$prop1_p)
      
      var_data <- sovi_data[[input$prop1_var]]
      
      # Convert to binary if needed
      if(is.numeric(var_data)) {
        median_val <- median(var_data, na.rm = TRUE)
        var_data <- ifelse(var_data > median_val, 1, 0)
      } else {
        unique_vals <- unique(var_data[!is.na(var_data)])
        var_data <- ifelse(var_data == unique_vals[1], 1, 0)
      }
      
      var_data <- var_data[!is.na(var_data)]
      successes <- sum(var_data)
      n <- length(var_data)
      
      prop_test <- prop.test(successes, n, p = input$prop1_p)
      
      cat("One-Sample Proportion Test\n\n")
      cat("H0: p =", input$prop1_p, "\n")
      cat("H1: p ≠", input$prop1_p, "\n\n")
      cat("Sample proportion:", round(successes/n, 4), "\n")
      cat("Sample size:", n, "\n")
      cat("Successes:", successes, "\n\n")
      print(prop_test)
    })
    
    output$prop1_interpretation <- renderText({
      req(input$prop1_var, input$prop1_p)
      
      var_data <- sovi_data[[input$prop1_var]]
      
      if(is.numeric(var_data)) {
        median_val <- median(var_data, na.rm = TRUE)
        var_data <- ifelse(var_data > median_val, 1, 0)
      } else {
        unique_vals <- unique(var_data[!is.na(var_data)])
        var_data <- ifelse(var_data == unique_vals[1], 1, 0)
      }
      
      var_data <- var_data[!is.na(var_data)]
      successes <- sum(var_data)
      n <- length(var_data)
      
      prop_test <- prop.test(successes, n, p = input$prop1_p)
      
      if(prop_test$p.value < 0.05) {
        paste("Interpretasi: Dengan p-value =", round(prop_test$p.value, 4),
              "< 0.05, kita menolak H0. Proporsi sampel berbeda signifikan dengan",
              "proporsi hipotesis", input$prop1_p, "pada tingkat signifikansi 5%.")
      } else {
        paste("Interpretasi: Dengan p-value =", round(prop_test$p.value, 4),
              "> 0.05, kita gagal menolak H0. Proporsi sampel tidak berbeda signifikan dengan",
              "proporsi hipotesis", input$prop1_p, "pada tingkat signifikansi 5%.")
      }
    })
  })
  
  # Variance Test
  observeEvent(input$run_var1, {
    output$var1_result <- renderPrint({
      req(input$var1_var, input$var1_sigma)
      
      var_data <- sovi_data[[input$var1_var]]
      var_data <- var_data[!is.na(var_data)]
      
      # Chi-square test for variance
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$var1_sigma
      p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
      
      cat("One-Sample Variance Test (Chi-square)\n\n")
      cat("H0: σ² =", input$var1_sigma, "\n")
      cat("H1: σ² ≠", input$var1_sigma, "\n\n")
      cat("Sample variance:", round(sample_var, 4), "\n")
      cat("Sample size:", n, "\n")
      cat("Chi-square statistic:", round(chi_stat, 4), "\n")
      cat("Degrees of freedom:", n-1, "\n")
      cat("P-value:", round(p_value, 4), "\n")
    })
    
    output$var1_interpretation <- renderText({
      req(input$var1_var, input$var1_sigma)
      
      var_data <- sovi_data[[input$var1_var]]
      var_data <- var_data[!is.na(var_data)]
      
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$var1_sigma
      p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
      
      if(p_value < 0.05) {
        paste("Interpretasi: Dengan p-value =", round(p_value, 4),
              "< 0.05, kita menolak H0. Varians sampel berbeda signifikan dengan",
              "varians hipotesis", input$var1_sigma, "pada tingkat signifikansi 5%.")
      } else {
        paste("Interpretasi: Dengan p-value =", round(p_value, 4),
              "> 0.05, kita gagal menolak H0. Varians sampel tidak berbeda signifikan dengan",
              "varians hipotesis", input$var1_sigma, "pada tingkat signifikansi 5%.")
      }
    })
  })
  
  # ANOVA Tests
  observeEvent(input$run_anova1, {
    output$anova1_result <- renderPrint({
      req(input$anova1_dep, input$anova1_indep)
      
      dep_var <- sovi_data[[input$anova1_dep]]
      indep_var <- sovi_data[[input$anova1_indep]]
      
      # Create groups if numeric
      if(is.numeric(indep_var)) {
        indep_var <- cut(indep_var, breaks = 3, labels = c("Low", "Medium", "High"))
      }
      
      # Remove missing values
      complete_cases <- complete.cases(dep_var, indep_var)
      dep_var <- dep_var[complete_cases]
      indep_var <- indep_var[complete_cases]
      
      # Perform ANOVA
      anova_result <- aov(dep_var ~ indep_var)
      anova_summary <- summary(anova_result)
      
      cat("One-Way ANOVA\n\n")
      cat("H0: μ1 = μ2 = μ3 = ... (all group means are equal)\n")
      cat("H1: At least one group mean is different\n\n")
      print(anova_summary)
      
      # Post-hoc test if significant
      if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
        cat("\n\nPost-hoc Test (Tukey HSD):\n")
        tukey_result <- TukeyHSD(anova_result)
        print(tukey_result)
      }
    })
    
    output$anova1_interpretation <- renderText({
      req(input$anova1_dep, input$anova1_indep)
      
      dep_var <- sovi_data[[input$anova1_dep]]
      indep_var <- sovi_data[[input$anova1_indep]]
      
      if(is.numeric(indep_var)) {
        indep_var <- cut(indep_var, breaks = 3, labels = c("Low", "Medium", "High"))
      }
      
      complete_cases <- complete.cases(dep_var, indep_var)
      dep_var <- dep_var[complete_cases]
      indep_var <- indep_var[complete_cases]
      
      anova_result <- aov(dep_var ~ indep_var)
      anova_summary <- summary(anova_result)
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      
      if(p_value < 0.05) {
        paste("Interpretasi: Dengan p-value =", round(p_value, 4),
              "< 0.05, kita menolak H0. Terdapat perbedaan signifikan rata-rata",
              input$anova1_dep, "antar kelompok", input$anova1_indep,
              "pada tingkat signifikansi 5%. Post-hoc test menunjukkan kelompok mana yang berbeda.")
      } else {
        paste("Interpretasi: Dengan p-value =", round(p_value, 4),
              "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan rata-rata",
              input$anova1_dep, "antar kelompok", input$anova1_indep,
              "pada tingkat signifikansi 5%.")
      }
    })
  })
  
  # Two-Way ANOVA
  observeEvent(input$run_anova2, {
    output$anova2_result <- renderPrint({
      req(input$anova2_dep, input$anova2_indep1, input$anova2_indep2)
      
      dep_var <- sovi_data[[input$anova2_dep]]
      indep_var1 <- sovi_data[[input$anova2_indep1]]
      indep_var2 <- sovi_data[[input$anova2_indep2]]
      
      # Create groups if numeric
      if(is.numeric(indep_var1)) {
        indep_var1 <- cut(indep_var1, breaks = 2, labels = c("Low", "High"))
      }
      if(is.numeric(indep_var2)) {
        indep_var2 <- cut(indep_var2, breaks = 2, labels = c("Low", "High"))
      }
      
      # Remove missing values
      complete_cases <- complete.cases(dep_var, indep_var1, indep_var2)
      dep_var <- dep_var[complete_cases]
      indep_var1 <- indep_var1[complete_cases]
      indep_var2 <- indep_var2[complete_cases]
      
      # Perform Two-Way ANOVA
      anova_result <- aov(dep_var ~ indep_var1 * indep_var2)
      anova_summary <- summary(anova_result)
      
      cat("Two-Way ANOVA\n\n")
      cat("Testing main effects and interaction effect\n\n")
      print(anova_summary)
    })
    
    output$anova2_interpretation <- renderText({
      req(input$anova2_dep, input$anova2_indep1, input$anova2_indep2)
      
      dep_var <- sovi_data[[input$anova2_dep]]
      indep_var1 <- sovi_data[[input$anova2_indep1]]
      indep_var2 <- sovi_data[[input$anova2_indep2]]
      
      if(is.numeric(indep_var1)) {
        indep_var1 <- cut(indep_var1, breaks = 2, labels = c("Low", "High"))
      }
      if(is.numeric(indep_var2)) {
        indep_var2 <- cut(indep_var2, breaks = 2, labels = c("Low", "High"))
      }
      
      complete_cases <- complete.cases(dep_var, indep_var1, indep_var2)
      dep_var <- dep_var[complete_cases]
      indep_var1 <- indep_var1[complete_cases]
      indep_var2 <- indep_var2[complete_cases]
      
      anova_result <- aov(dep_var ~ indep_var1 * indep_var2)
      anova_summary <- summary(anova_result)
      
      p_values <- anova_summary[[1]]$`Pr(>F)`
      
      interpretation <- paste("Interpretasi Two-Way ANOVA:\n",
                              "1. Main effect", input$anova2_indep1, ": p-value =", round(p_values[1], 4),
                              ifelse(p_values[1] < 0.05, "(signifikan)", "(tidak signifikan)"), "\n",
                              "2. Main effect", input$anova2_indep2, ": p-value =", round(p_values[2], 4),
                              ifelse(p_values[2] < 0.05, "(signifikan)", "(tidak signifikan)"), "\n",
                              "3. Interaction effect: p-value =", round(p_values[3], 4),
                              ifelse(p_values[3] < 0.05, "(signifikan)", "(tidak signifikan)"))
      
      return(interpretation)
    })
  })
  
  # Multiple Linear Regression
  regression_model <- eventReactive(input$run_regression, {
    req(input$reg_dep, input$reg_indep)
    
    # Prepare data
    dep_var <- sovi_data[[input$reg_dep]]
    indep_vars <- sovi_data[, input$reg_indep, drop = FALSE]
    
    # Remove missing values
    complete_data <- cbind(dep_var, indep_vars)
    complete_data <- complete_data[complete.cases(complete_data), ]
    
    # Create formula
    formula_str <- paste(input$reg_dep, "~", paste(input$reg_indep, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Fit model
    model <- lm(formula_obj, data = complete_data)
    
    return(model)
  })
  
  output$regression_summary <- renderPrint({
    req(regression_model())
    summary(regression_model())
  })
  
  output$regression_interpretation <- renderText({
    req(regression_model())
    model <- regression_model()
    model_summary <- summary(model)
    
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_stat <- model_summary$fstatistic[1]
    f_p_value <- pf(f_stat, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    
    interpretation <- paste(
      "Interpretasi Model Regresi:\n",
      "1. R-squared =", round(r_squared, 4), "- Model menjelaskan", round(r_squared * 100, 2), 
      "% variabilitas dalam", input$reg_dep, "\n",
      "2. Adjusted R-squared =", round(adj_r_squared, 4), "\n",
      "3. F-statistic =", round(f_stat, 4), "dengan p-value =", round(f_p_value, 4),
      ifelse(f_p_value < 0.05, "- Model secara keseluruhan signifikan", "- Model secara keseluruhan tidak signifikan"), "\n",
      "4. Koefisien yang signifikan (p < 0.05) menunjukkan variabel yang berpengaruh signifikan terhadap", input$reg_dep
    )
    
    return(interpretation)
  })
  
  # VIF Test
  output$vif_test <- renderPrint({
    req(regression_model())
    model <- regression_model()
    
    if(length(input$reg_indep) > 1) {
      vif_values <- car::vif(model)
      cat("Variance Inflation Factor (VIF):\n\n")
      print(vif_values)
      cat("\nInterpretasi VIF:\n")
      cat("VIF < 5: Tidak ada masalah multikolinearitas\n")
      cat("5 ≤ VIF < 10: Multikolinearitas sedang\n")
      cat("VIF ≥ 10: Multikolinearitas tinggi\n")
    } else {
      cat("VIF memerlukan minimal 2 variabel independen")
    }
  })
  
  # Residual plots
  output$residual_qq <- renderPlot({
    req(regression_model())
    model <- regression_model()
    
    ggplot(data.frame(residuals = residuals(model)), aes(sample = residuals)) +
      stat_qq() + stat_qq_line() +
      theme_minimal() +
      labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")
  })
  
  output$residual_fitted <- renderPlot({
    req(regression_model())
    model <- regression_model()
    
    ggplot(data.frame(fitted = fitted(model), residuals = residuals(model)), 
           aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_smooth(se = FALSE, color = "blue") +
      theme_minimal() +
      labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")
  })
  
  output$assumption_interpretation <- renderText({
    req(regression_model())
    model <- regression_model()
    
    # Shapiro test for residuals
    residuals_data <- residuals(model)
    shapiro_test <- shapiro.test(residuals_data)
    
    interpretation <- paste(
      "Interpretasi Uji Asumsi:\n",
      "1. Multikolinearitas: Lihat nilai VIF di atas\n",
      "2. Normalitas Residual: Shapiro-Wilk test p-value =", round(shapiro_test$p.value, 4),
      ifelse(shapiro_test$p.value > 0.05, "- Residual berdistribusi normal", "- Residual tidak berdistribusi normal"), "\n",
      "3. Homoskedastisitas: Lihat plot Residuals vs Fitted Values. Pola acak menunjukkan homoskedastisitas terpenuhi"
    )
    
    return(interpretation)
  })
  
  # DOWNLOAD HANDLERS - FIXED AND WORKING
  
  # Welcome Report Download
  output$download_welcome <- downloadHandler(
    filename = function() { 
      paste("SEVA_Welcome_Report_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      content <- paste(
        "SEVA Dashboard - Welcome Report",
        "Generated on:", Sys.Date(),
        "",
        "Socio-Economic Vulnerability Analyzer",
        "",
        "Dashboard ini dirancang untuk menganalisis kerentanan sosial-ekonomi berbagai daerah",
        "menggunakan berbagai indikator demografis, ekonomi, dan sosial.",
        "",
        "Fitur utama dashboard ini meliputi:",
        "- Manajemen dan kategorisasi data",
        "- Eksplorasi data dengan visualisasi interaktif", 
        "- Uji asumsi statistik",
        "- Analisis statistik inferensia",
        "- Analisis regresi linear berganda",
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  # Metadata Download
  output$download_metadata <- downloadHandler(
    filename = function() { 
      paste("SEVA_Metadata_", Sys.Date(), ".csv", sep = "") 
    },
    content = function(file) {
      write.csv(metadata, file, row.names = FALSE)
    }
  )
  
  # Categorization Download
  output$download_categorization <- downloadHandler(
    filename = function() { 
      paste("SEVA_Categorization_", Sys.Date(), ".csv", sep = "") 
    },
    content = function(file) {
      req(categorized_data_reactive())
      write.csv(categorized_data_reactive(), file, row.names = FALSE)
    }
  )
  
  # Management Report Download
  output$download_management_report <- downloadHandler(
    filename = function() { 
      paste("SEVA_Management_Report_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(categorized_data_reactive())
      content <- paste(
        "SEVA - Data Management Report",
        "Generated on:", Sys.Date(),
        "",
        "Kategorisasi Data:",
        paste("Variabel:", input$cont_var),
        paste("Jumlah Kategori:", input$n_bins),
        "",
        "Interpretasi:",
        paste("Variabel", input$cont_var, "telah berhasil dikategorisasi menjadi", 
              input$n_bins, "kategori berdasarkan kuantil."),
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  # Descriptive Statistics Download
  output$download_descriptive <- downloadHandler(
    filename = function() { 
      paste("SEVA_Descriptive_Stats_", Sys.Date(), ".csv", sep = "") 
    },
    content = function(file) {
      req(input$desc_vars)
      desc_data <- sovi_data[, input$desc_vars, drop = FALSE]
      desc_summary <- data.frame(
        Variable = names(desc_data),
        Mean = sapply(desc_data, function(x) round(mean(x, na.rm = TRUE), 3)),
        Median = sapply(desc_data, function(x) round(median(x, na.rm = TRUE), 3)),
        SD = sapply(desc_data, function(x) round(sd(x, na.rm = TRUE), 3)),
        Min = sapply(desc_data, function(x) round(min(x, na.rm = TRUE), 3)),
        Max = sapply(desc_data, function(x) round(max(x, na.rm = TRUE), 3)),
        stringsAsFactors = FALSE
      )
      write.csv(desc_summary, file, row.names = FALSE)
    }
  )
  
  # Plot Download
  output$download_plot <- downloadHandler(
    filename = function() { 
      paste("SEVA_Plot_", input$plot_type, "_", Sys.Date(), ".jpg", sep = "") 
    },
    content = function(file) {
      req(input$plot_var1)
      
      if(input$plot_type == "hist") {
        p <- ggplot(sovi_data, aes_string(x = input$plot_var1)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$plot_var1),
               x = input$plot_var1, y = "Frequency")
      } else if(input$plot_type == "box") {
        p <- ggplot(sovi_data, aes_string(y = input$plot_var1)) +
          geom_boxplot(fill = "lightblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Box Plot of", input$plot_var1),
               y = input$plot_var1)
      } else if(input$plot_type == "scatter" && !is.null(input$plot_var2)) {
        p <- ggplot(sovi_data, aes_string(x = input$plot_var1, y = input$plot_var2)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          geom_smooth(method = "lm", se = TRUE, color = "red") +
          theme_minimal() +
          labs(title = paste("Scatter Plot:", input$plot_var1, "vs", input$plot_var2),
               x = input$plot_var1, y = input$plot_var2)
      }
      
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Correlation Heatmap Download
  output$download_correlation <- downloadHandler(
    filename = function() { 
      paste("SEVA_Correlation_Heatmap_", Sys.Date(), ".jpg", sep = "") 
    },
    content = function(file) {
      numeric_data <- select_if(sovi_data, is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      melted_cor <- melt(cor_matrix)
      
      p <- ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        labs(title = "Correlation Heatmap", x = "", y = "") +
        coord_fixed()
      
      ggsave(file, plot = p, width = 12, height = 10, dpi = 300)
    }
  )
  
  # Exploration Report Download
  output$download_exploration_report <- downloadHandler(
    filename = function() { 
      paste("SEVA_Exploration_Report_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(input$desc_vars)
      
      # Get descriptive stats
      desc_data <- sovi_data[, input$desc_vars, drop = FALSE]
      desc_summary <- data.frame(
        Variable = names(desc_data),
        Mean = sapply(desc_data, function(x) round(mean(x, na.rm = TRUE), 3)),
        Median = sapply(desc_data, function(x) round(median(x, na.rm = TRUE), 3)),
        SD = sapply(desc_data, function(x) round(sd(x, na.rm = TRUE), 3)),
        stringsAsFactors = FALSE
      )
      
      # Get correlation info
      numeric_data <- select_if(sovi_data, is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
      max_cor <- which(cor_matrix == max(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
      min_cor <- which(cor_matrix == min(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
      
      content <- paste(
        "SEVA - Data Exploration Report",
        "Generated on:", Sys.Date(),
        "",
        "=== DESCRIPTIVE STATISTICS ===",
        "",
        paste(capture.output(print(desc_summary)), collapse = "\n"),
        "",
        "=== CORRELATION ANALYSIS ===",
        "",
        paste("Strongest positive correlation:", rownames(cor_matrix)[max_cor[1]], "and", 
              colnames(cor_matrix)[max_cor[2]], "=", round(cor_matrix[max_cor], 3)),
        paste("Strongest negative correlation:", rownames(cor_matrix)[min_cor[1]], "and",
              colnames(cor_matrix)[min_cor[2]], "=", round(cor_matrix[min_cor], 3)),
        "",
        "=== INTERPRETATIONS ===",
        "",
        "Statistik deskriptif menunjukkan karakteristik dasar dari variabel yang dipilih.",
        "Peta korelasi menunjukkan hubungan linear antara semua variabel numerik.",
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  # Normality Test Download - Enhanced with better error handling
  output$download_normality <- downloadHandler(
    filename = function() { 
      paste("SEVA_Normality_Test_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      tryCatch({
        # Use default variable if none selected
        var_name <- if(is.null(input$normality_var) || input$normality_var == "") {
          names(select_if(sovi_data, is.numeric))[1]
        } else {
          input$normality_var
        }
        
        var_data <- sovi_data[[var_name]]
        var_data <- var_data[!is.na(var_data)]
        
        if(length(var_data) < 3) {
          content <- paste(
            "SEVA - Normality Test Report",
            "Generated on:", Sys.Date(),
            "",
            "ERROR: Insufficient data for normality test.",
            "Please select a variable with more observations.",
            sep = "\n"
          )
        } else {
          shapiro_test <- shapiro.test(var_data)
          
          content <- paste(
            "SEVA - Normality Test Report",
            "Generated on:", Sys.Date(),
            "",
            paste("Variable tested:", var_name),
            paste("Sample size:", length(var_data)),
            "",
            "H0: Data berdistribusi normal",
            "H1: Data tidak berdistribusi normal",
            "",
            "Shapiro-Wilk Test Results:",
            paste("W =", round(shapiro_test$statistic, 4)),
            paste("p-value =", round(shapiro_test$p.value, 4)),
            "",
            "Interpretation:",
            if(shapiro_test$p.value > 0.05) {
              paste("Dengan p-value =", round(shapiro_test$p.value, 4),
                    "> 0.05, kita gagal menolak H0. Data dapat dianggap berdistribusi normal.")
            } else {
              paste("Dengan p-value =", round(shapiro_test$p.value, 4),
                    "< 0.05, kita menolak H0. Data tidak berdistribusi normal.")
            },
            "",
            "Note: Shapiro-Wilk test is most reliable for sample sizes between 3 and 5000.",
            sep = "\n"
          )
        }
        writeLines(content, file)
      }, error = function(e) {
        content <- paste(
          "SEVA - Normality Test Report",
          "Generated on:", Sys.Date(),
          "",
          "ERROR: Unable to perform normality test.",
          paste("Error message:", e$message),
          "",
          "Please ensure a valid numeric variable is selected.",
          sep = "\n"
        )
        writeLines(content, file)
      })
    }
  )
  
  # Homogeneity Test Download
  output$download_homogeneity <- downloadHandler(
    filename = function() { 
      paste("SEVA_Homogeneity_Test_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(input$normality_var, input$group_var)
      if(input$group_var != "None") {
        var_data <- sovi_data[[input$normality_var]]
        group_data <- sovi_data[[input$group_var]]
        
        if(is.numeric(group_data)) {
          group_data <- cut(group_data, breaks = 3, labels = c("Low", "Medium", "High"))
        }
        
        complete_cases <- complete.cases(var_data, group_data)
        var_data <- var_data[complete_cases]
        group_data <- group_data[complete_cases]
        
        levene_test <- car::leveneTest(var_data, group_data)
        p_value <- levene_test$`Pr(>F)`[1]
        
        content <- paste(
          "SEVA - Homogeneity Test Report",
          "Generated on:", Sys.Date(),
          "",
          paste("Variable tested:", input$normality_var),
          paste("Grouping variable:", input$group_var),
          "",
          "H0: Varians antar kelompok homogen",
          "H1: Varians antar kelompok tidak homogen",
          "",
          "Levene's Test Results:",
          paste("F =", round(levene_test$`F value`[1], 4)),
          paste("p-value =", round(p_value, 4)),
          "",
          "Interpretation:",
          if(p_value > 0.05) {
            paste("Dengan p-value =", round(p_value, 4),
                  "> 0.05, kita gagal menolak H0. Varians antar kelompok homogen.")
          } else {
            paste("Dengan p-value =", round(p_value, 4),
                  "< 0.05, kita menolak H0. Varians antar kelompok tidak homogen.")
          },
          sep = "\n"
        )
        writeLines(content, file)
      }
    }
  )
  
  # Assumption Tests Report Download - Enhanced
  output$download_assumption_report <- downloadHandler(
    filename = function() { 
      paste("SEVA_Assumption_Tests_Report_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      tryCatch({
        # Use default variable if none selected
        var_name <- if(is.null(input$normality_var) || input$normality_var == "") {
          names(select_if(sovi_data, is.numeric))[1]
        } else {
          input$normality_var
        }
        
        # Normality test
        var_data <- sovi_data[[var_name]]
        var_data <- var_data[!is.na(var_data)]
        
        content_parts <- c(
          "SEVA - Assumption Tests Complete Report",
          paste("Generated on:", Sys.Date()),
          "",
          "=== NORMALITY TEST ===",
          "",
          paste("Variable:", var_name),
          paste("Sample size:", length(var_data)),
          "H0: Data berdistribusi normal",
          "H1: Data tidak berdistribusi normal",
          ""
        )
        
        if(length(var_data) >= 3) {
          shapiro_test <- shapiro.test(var_data)
          content_parts <- c(content_parts,
            paste("Shapiro-Wilk W =", round(shapiro_test$statistic, 4)),
            paste("p-value =", round(shapiro_test$p.value, 4)),
            "",
            if(shapiro_test$p.value > 0.05) {
              "Conclusion: Data berdistribusi normal (gagal menolak H0)"
            } else {
              "Conclusion: Data tidak berdistribusi normal (menolak H0)"
            }
          )
        } else {
          content_parts <- c(content_parts,
            "ERROR: Insufficient data for normality test (need at least 3 observations)"
          )
        }
        
        content_parts <- c(content_parts,
          "",
          "=== HOMOGENEITY TEST ===",
          ""
        )
        
        # Check for grouping variable
        group_var <- input$group_var
        if(!is.null(group_var) && group_var != "None" && group_var != "") {
          content_parts <- c(content_parts,
            paste("Grouping variable:", group_var),
            "See separate homogeneity test results for detailed analysis."
          )
        } else {
          content_parts <- c(content_parts,
            "No grouping variable selected for homogeneity test.",
            "To perform homogeneity tests, please select a grouping variable."
          )
        }
        
        content_parts <- c(content_parts,
          "",
          "=== SUMMARY ===",
          "",
          "This report provides assumption tests commonly used before:",
          "- t-tests and ANOVA (normality assumption)",
          "- Independent samples tests (homogeneity of variance)",
          "- Regression analysis (residual normality)",
          "",
          "Always verify assumptions before interpreting statistical test results."
        )
        
        content <- paste(content_parts, collapse = "\n")
        writeLines(content, file)
        
      }, error = function(e) {
        content <- paste(
          "SEVA - Assumption Tests Complete Report",
          paste("Generated on:", Sys.Date()),
          "",
          "ERROR: Unable to generate assumption tests report.",
          paste("Error message:", e$message),
          "",
          "Please ensure variables are properly selected and try again.",
          sep = "\n"
        )
        writeLines(content, file)
      })
    }
  )
  
  # T-Test Downloads
  output$download_ttest1 <- downloadHandler(
    filename = function() { 
      paste("SEVA_One_Sample_TTest_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(input$ttest1_var, input$ttest1_mu)
      var_data <- sovi_data[[input$ttest1_var]]
      var_data <- var_data[!is.na(var_data)]
      ttest_result <- t.test(var_data, mu = input$ttest1_mu)
      
      content <- paste(
        "SEVA - One-Sample T-Test Report",
        "Generated on:", Sys.Date(),
        "",
        paste("Variable:", input$ttest1_var),
        paste("Hypothesized mean:", input$ttest1_mu),
        "",
        paste("H0: μ =", input$ttest1_mu),
        paste("H1: μ ≠", input$ttest1_mu),
        "",
        "Results:",
        paste("t =", round(ttest_result$statistic, 4)),
        paste("df =", ttest_result$parameter),
        paste("p-value =", round(ttest_result$p.value, 4)),
        paste("Sample mean =", round(ttest_result$estimate, 4)),
        "",
        "Interpretation:",
        if(ttest_result$p.value < 0.05) {
          paste("Dengan p-value =", round(ttest_result$p.value, 4),
                "< 0.05, kita menolak H0. Terdapat perbedaan signifikan.")
        } else {
          paste("Dengan p-value =", round(ttest_result$p.value, 4),
                "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan.")
        },
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  output$download_ttest2 <- downloadHandler(
    filename = function() { 
      paste("SEVA_Two_Sample_TTest_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(input$ttest2_var, input$ttest2_group)
      
      var_data <- sovi_data[[input$ttest2_var]]
      group_data <- sovi_data[[input$ttest2_group]]
      
      if(is.numeric(group_data)) {
        median_val <- median(group_data, na.rm = TRUE)
        group_data <- ifelse(group_data <= median_val, "Low", "High")
      }
      
      unique_groups <- unique(group_data[!is.na(group_data)])
      if(length(unique_groups) >= 2) {
        group1_data <- var_data[group_data == unique_groups[1] & !is.na(var_data) & !is.na(group_data)]
        group2_data <- var_data[group_data == unique_groups[2] & !is.na(var_data) & !is.na(group_data)]
        
        ttest_result <- t.test(group1_data, group2_data)
        
        content <- paste(
          "SEVA - Two-Sample T-Test Report",
          "Generated on:", Sys.Date(),
          "",
          paste("Variable:", input$ttest2_var),
          paste("Grouping variable:", input$ttest2_group),
          "",
          paste("Group 1 (", unique_groups[1], "):", length(group1_data), "observations"),
          paste("Group 2 (", unique_groups[2], "):", length(group2_data), "observations"),
          "",
          "H0: μ1 = μ2",
          "H1: μ1 ≠ μ2",
          "",
          "Results:",
          paste("t =", round(ttest_result$statistic, 4)),
          paste("df =", round(ttest_result$parameter, 2)),
          paste("p-value =", round(ttest_result$p.value, 4)),
          "",
          "Interpretation:",
          if(ttest_result$p.value < 0.05) {
            paste("Dengan p-value =", round(ttest_result$p.value, 4),
                  "< 0.05, kita menolak H0. Terdapat perbedaan signifikan.")
          } else {
            paste("Dengan p-value =", round(ttest_result$p.value, 4),
                  "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan.")
          },
          sep = "\n"
        )
        writeLines(content, file)
      }
    }
  )
  
  # Proportion Test Download
  output$download_prop1 <- downloadHandler(
    filename = function() { 
      paste("SEVA_Proportion_Test_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(input$prop1_var, input$prop1_p)
      
      var_data <- sovi_data[[input$prop1_var]]
      
      if(is.numeric(var_data)) {
        median_val <- median(var_data, na.rm = TRUE)
        var_data <- ifelse(var_data > median_val, 1, 0)
      } else {
        unique_vals <- unique(var_data[!is.na(var_data)])
        var_data <- ifelse(var_data == unique_vals[1], 1, 0)
      }
      
      var_data <- var_data[!is.na(var_data)]
      successes <- sum(var_data)
      n <- length(var_data)
      
      prop_test <- prop.test(successes, n, p = input$prop1_p)
      
      content <- paste(
        "SEVA - One-Sample Proportion Test Report",
        "Generated on:", Sys.Date(),
        "",
        paste("Variable:", input$prop1_var),
        paste("Hypothesized proportion:", input$prop1_p),
        "",
        paste("H0: p =", input$prop1_p),
        paste("H1: p ≠", input$prop1_p),
        "",
        "Results:",
        paste("Sample size:", n),
        paste("Successes:", successes),
        paste("Sample proportion:", round(successes/n, 4)),
        paste("X-squared =", round(prop_test$statistic, 4)),
        paste("p-value =", round(prop_test$p.value, 4)),
        "",
        "Interpretation:",
        if(prop_test$p.value < 0.05) {
          paste("Dengan p-value =", round(prop_test$p.value, 4),
                "< 0.05, kita menolak H0. Proporsi berbeda signifikan.")
        } else {
          paste("Dengan p-value =", round(prop_test$p.value, 4),
                "> 0.05, kita gagal menolak H0. Proporsi tidak berbeda signifikan.")
        },
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  # Variance Test Download
  output$download_var1 <- downloadHandler(
    filename = function() { 
      paste("SEVA_Variance_Test_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(input$var1_var, input$var1_sigma)
      
      var_data <- sovi_data[[input$var1_var]]
      var_data <- var_data[!is.na(var_data)]
      
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$var1_sigma
      p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
      
      content <- paste(
        "SEVA - One-Sample Variance Test Report",
        "Generated on:", Sys.Date(),
        "",
        paste("Variable:", input$var1_var),
        paste("Hypothesized variance:", input$var1_sigma),
        "",
        paste("H0: σ² =", input$var1_sigma),
        paste("H1: σ² ≠", input$var1_sigma),
        "",
        "Results:",
        paste("Sample size:", n),
        paste("Sample variance:", round(sample_var, 4)),
        paste("Chi-square statistic:", round(chi_stat, 4)),
        paste("Degrees of freedom:", n-1),
        paste("p-value =", round(p_value, 4)),
        "",
        "Interpretation:",
        if(p_value < 0.05) {
          paste("Dengan p-value =", round(p_value, 4),
                "< 0.05, kita menolak H0. Varians berbeda signifikan.")
        } else {
          paste("Dengan p-value =", round(p_value, 4),
                "> 0.05, kita gagal menolak H0. Varians tidak berbeda signifikan.")
        },
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  # ANOVA Downloads
  output$download_anova1 <- downloadHandler(
    filename = function() { 
      paste("SEVA_One_Way_ANOVA_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(input$anova1_dep, input$anova1_indep)
      
      dep_var <- sovi_data[[input$anova1_dep]]
      indep_var <- sovi_data[[input$anova1_indep]]
      
      if(is.numeric(indep_var)) {
        indep_var <- cut(indep_var, breaks = 3, labels = c("Low", "Medium", "High"))
      }
      
      complete_cases <- complete.cases(dep_var, indep_var)
      dep_var <- dep_var[complete_cases]
      indep_var <- indep_var[complete_cases]
      
      anova_result <- aov(dep_var ~ indep_var)
      anova_summary <- summary(anova_result)
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      
      content <- paste(
        "SEVA - One-Way ANOVA Report",
        "Generated on:", Sys.Date(),
        "",
        paste("Dependent variable:", input$anova1_dep),
        paste("Independent variable:", input$anova1_indep),
        "",
        "H0: μ1 = μ2 = μ3 = ... (all group means are equal)",
        "H1: At least one group mean is different",
        "",
        "ANOVA Results:",
        paste(capture.output(print(anova_summary)), collapse = "\n"),
        "",
        "Interpretation:",
        if(p_value < 0.05) {
          paste("Dengan p-value =", round(p_value, 4),
                "< 0.05, kita menolak H0. Terdapat perbedaan signifikan antar kelompok.")
        } else {
          paste("Dengan p-value =", round(p_value, 4),
                "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan antar kelompok.")
        },
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  output$download_anova2 <- downloadHandler(
    filename = function() { 
      paste("SEVA_Two_Way_ANOVA_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(input$anova2_dep, input$anova2_indep1, input$anova2_indep2)
      
      dep_var <- sovi_data[[input$anova2_dep]]
      indep_var1 <- sovi_data[[input$anova2_indep1]]
      indep_var2 <- sovi_data[[input$anova2_indep2]]
      
      if(is.numeric(indep_var1)) {
        indep_var1 <- cut(indep_var1, breaks = 2, labels = c("Low", "High"))
      }
      if(is.numeric(indep_var2)) {
        indep_var2 <- cut(indep_var2, breaks = 2, labels = c("Low", "High"))
      }
      
      complete_cases <- complete.cases(dep_var, indep_var1, indep_var2)
      dep_var <- dep_var[complete_cases]
      indep_var1 <- indep_var1[complete_cases]
      indep_var2 <- indep_var2[complete_cases]
      
      anova_result <- aov(dep_var ~ indep_var1 * indep_var2)
      anova_summary <- summary(anova_result)
      p_values <- anova_summary[[1]]$`Pr(>F)`
      
      content <- paste(
        "SEVA - Two-Way ANOVA Report",
        "Generated on:", Sys.Date(),
        "",
        paste("Dependent variable:", input$anova2_dep),
        paste("Independent variable 1:", input$anova2_indep1),
        paste("Independent variable 2:", input$anova2_indep2),
        "",
        "Two-Way ANOVA Results:",
        paste(capture.output(print(anova_summary)), collapse = "\n"),
        "",
        "Interpretation:",
        paste("1. Main effect", input$anova2_indep1, ": p-value =", round(p_values[1], 4),
              ifelse(p_values[1] < 0.05, "(signifikan)", "(tidak signifikan)")),
        paste("2. Main effect", input$anova2_indep2, ": p-value =", round(p_values[2], 4),
              ifelse(p_values[2] < 0.05, "(signifikan)", "(tidak signifikan)")),
        paste("3. Interaction effect: p-value =", round(p_values[3], 4),
              ifelse(p_values[3] < 0.05, "(signifikan)", "(tidak signifikan)")),
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  # Inferential Statistics Report Download - Enhanced
  output$download_inferential_report <- downloadHandler(
    filename = function() { 
      paste("SEVA_Inferential_Statistics_Report_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      tryCatch({
        content_parts <- c(
          "SEVA - Complete Inferential Statistics Report",
          paste("Generated on:", Sys.Date()),
          "",
          "=== OVERVIEW ===",
          "",
          "This report provides a comprehensive guide to inferential statistical analyses",
          "available in the SEVA dashboard. Each test can be performed individually",
          "and results downloaded separately.",
          "",
          "=== AVAILABLE STATISTICAL TESTS ===",
          "",
          "1. ONE-SAMPLE T-TEST",
          "   Purpose: Test if a sample mean differs from a hypothesized value",
          "   Assumptions: Normal distribution, random sampling",
          "   Use when: Comparing sample mean to known population mean",
          "",
          "2. TWO-SAMPLE INDEPENDENT T-TEST",
          "   Purpose: Compare means of two independent groups",
          "   Assumptions: Normal distribution, equal variances, independence",
          "   Use when: Comparing two groups (e.g., treatment vs control)",
          "",
          "3. ONE-SAMPLE PROPORTION TEST",
          "   Purpose: Test if a sample proportion differs from hypothesized proportion",
          "   Assumptions: Large sample size, random sampling",
          "   Use when: Testing proportions or percentages",
          "",
          "4. ONE-SAMPLE VARIANCE TEST",
          "   Purpose: Test if sample variance differs from hypothesized variance",
          "   Assumptions: Normal distribution",
          "   Use when: Testing variability or consistency",
          "",
          "5. ONE-WAY ANOVA",
          "   Purpose: Compare means across multiple groups",
          "   Assumptions: Normal distribution, equal variances, independence",
          "   Use when: Comparing 3 or more groups simultaneously",
          "",
          "6. TWO-WAY ANOVA",
          "   Purpose: Examine effects of two factors and their interaction",
          "   Assumptions: Normal distribution, equal variances, independence",
          "   Use when: Testing effects of two categorical variables",
          "",
          "=== GENERAL GUIDELINES ===",
          "",
          "Before performing any test:",
          "- Check assumptions using the 'Uji Asumsi' tab",
          "- Ensure adequate sample size",
          "- Verify data quality and completeness",
          "",
          "Interpretation guidelines:",
          "- α = 0.05 is the standard significance level",
          "- p < 0.05 indicates statistical significance",
          "- Always consider practical significance alongside statistical significance",
          "- Report confidence intervals when available",
          "",
          "=== DATASET INFORMATION ===",
          "",
          paste("Dataset contains", ncol(sovi_data), "variables and", nrow(sovi_data), "observations"),
          paste("Numeric variables:", length(names(select_if(sovi_data, is.numeric)))),
          paste("Categorical variables:", length(names(select_if(sovi_data, function(x) !is.numeric(x))))),
          "",
          "=== HOW TO USE ===",
          "",
          "1. Navigate to the 'Statistik Inferensia' tab",
          "2. Select the appropriate test for your research question",
          "3. Choose variables and set parameters",
          "4. Run the test and interpret results",
          "5. Download individual test reports for detailed analysis",
          "",
          "=== CONTACT AND SUPPORT ===",
          "",
          "For questions about statistical methods or interpretation:",
          "- Consult statistical textbooks for detailed methodology",
          "- Consider professional statistical consultation for complex analyses",
          "- Validate results with alternative software when possible"
        )
        
        content <- paste(content_parts, collapse = "\n")
        writeLines(content, file)
        
      }, error = function(e) {
        content <- paste(
          "SEVA - Complete Inferential Statistics Report",
          paste("Generated on:", Sys.Date()),
          "",
          "ERROR: Unable to generate inferential statistics report.",
          paste("Error message:", e$message),
          "",
          "This appears to be a system error. Please try again or contact support.",
          sep = "\n"
        )
        writeLines(content, file)
      })
    }
  )
  
  # Regression Downloads
  output$download_regression_summary <- downloadHandler(
    filename = function() { 
      paste("SEVA_Regression_Summary_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(regression_model())
      model <- regression_model()
      model_summary <- summary(model)
      
      content <- paste(
        "SEVA - Multiple Linear Regression Summary",
        "Generated on:", Sys.Date(),
        "",
        paste("Dependent variable:", input$reg_dep),
        paste("Independent variables:", paste(input$reg_indep, collapse = ", ")),
        "",
        "Regression Results:",
        paste(capture.output(print(model_summary)), collapse = "\n"),
        "",
        "Model Interpretation:",
        paste("R-squared =", round(model_summary$r.squared, 4)),
        paste("Adjusted R-squared =", round(model_summary$adj.r.squared, 4)),
        paste("F-statistic =", round(model_summary$fstatistic[1], 4)),
        paste("Model explains", round(model_summary$r.squared * 100, 2), "% of variance"),
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  output$download_assumptions <- downloadHandler(
    filename = function() { 
      paste("SEVA_Regression_Assumptions_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      req(regression_model())
      model <- regression_model()
      residuals_data <- residuals(model)
      shapiro_test <- shapiro.test(residuals_data)
      
      vif_content <- ""
      if(length(input$reg_indep) > 1) {
        vif_values <- car::vif(model)
        vif_content <- paste(
          "VIF Values:",
          paste(capture.output(print(vif_values)), collapse = "\n"),
          "",
          "VIF Interpretation:",
          "VIF < 5: No multicollinearity problem",
          "5 ≤ VIF < 10: Moderate multicollinearity", 
          "VIF ≥ 10: High multicollinearity",
          "",
          sep = "\n"
        )
      }
      
      content <- paste(
        "SEVA - Regression Assumptions Test Report",
        "Generated on:", Sys.Date(),
        "",
        "=== MULTICOLLINEARITY TEST ===",
        "",
        vif_content,
        "=== NORMALITY OF RESIDUALS ===",
        "",
        "Shapiro-Wilk Test for Residuals:",
        paste("W =", round(shapiro_test$statistic, 4)),
        paste("p-value =", round(shapiro_test$p.value, 4)),
        "",
        if(shapiro_test$p.value > 0.05) {
          "Residuals are normally distributed"
        } else {
          "Residuals are not normally distributed"
        },
        "",
        "=== HOMOSCEDASTICITY ===",
        "",
        "Check the Residuals vs Fitted Values plot:",
        "- Random scatter indicates homoscedasticity",
        "- Patterns indicate heteroscedasticity",
        "",
        "Overall Assessment:",
        "Review all assumption tests before interpreting regression results.",
        sep = "\n"
      )
      writeLines(content, file)
    }
  )
  
  output$download_regression_report <- downloadHandler(
    filename = function() { 
      paste("SEVA_Complete_Regression_Report_", Sys.Date(), ".txt", sep = "") 
    },
    content = function(file) {
      tryCatch({
        # Check if regression model exists
        if(exists("regression_model") && !is.null(regression_model()) && 
           !is.null(input$reg_dep) && !is.null(input$reg_indep)) {
          
          model <- regression_model()
          model_summary <- summary(model)
          residuals_data <- residuals(model)
          
          content_parts <- c(
            "SEVA - Complete Multiple Linear Regression Report",
            paste("Generated on:", Sys.Date()),
            "",
            "=== MODEL SPECIFICATION ===",
            "",
            paste("Dependent variable:", input$reg_dep),
            paste("Independent variables:", paste(input$reg_indep, collapse = ", ")),
            paste("Sample size:", nobs(model)),
            "",
            "=== REGRESSION RESULTS ===",
            "",
            paste(capture.output(print(model_summary)), collapse = "\n"),
            "",
            "=== MODEL INTERPRETATION ===",
            "",
            paste("R-squared =", round(model_summary$r.squared, 4), 
                  "- Model explains", round(model_summary$r.squared * 100, 2), "% of variance"),
            paste("Adjusted R-squared =", round(model_summary$adj.r.squared, 4)),
            paste("F-statistic =", round(model_summary$fstatistic[1], 4)),
            paste("Degrees of freedom:", model_summary$fstatistic[2], "and", model_summary$fstatistic[3]),
            ""
          )
          
          # Assumption tests
          content_parts <- c(content_parts,
            "=== ASSUMPTION TESTS ===",
            "",
            "1. Normality of Residuals:"
          )
          
          if(length(residuals_data) >= 3) {
            shapiro_test <- shapiro.test(residuals_data)
            content_parts <- c(content_parts,
              paste("   Shapiro-Wilk p-value =", round(shapiro_test$p.value, 4)),
              if(shapiro_test$p.value > 0.05) {
                "   ✓ Residuals are normally distributed"
              } else {
                "   ✗ Residuals are not normally distributed"
              }
            )
          } else {
            content_parts <- c(content_parts,
              "   Insufficient data for normality test"
            )
          }
          
          content_parts <- c(content_parts,
            "",
            "2. Multicollinearity:"
          )
          
          if(length(input$reg_indep) > 1) {
            content_parts <- c(content_parts,
              "   Check VIF values in separate assumptions report",
              "   VIF > 5 indicates potential multicollinearity issues"
            )
          } else {
            content_parts <- c(content_parts,
              "   Not applicable (single predictor model)"
            )
          }
          
          content_parts <- c(content_parts,
            "",
            "3. Homoscedasticity:",
            "   Check residual vs fitted plots for constant variance",
            "   Look for random scatter around horizontal line",
            "",
            "=== CONCLUSIONS ===",
            ""
          )
          
          # Model significance
          f_p_value <- pf(model_summary$fstatistic[1], model_summary$fstatistic[2], 
                         model_summary$fstatistic[3], lower.tail = FALSE)
          
          content_parts <- c(content_parts,
            paste("Overall model is", 
                  if(f_p_value < 0.05) {
                    paste("statistically significant (p =", round(f_p_value, 4), ")")
                  } else {
                    paste("not statistically significant (p =", round(f_p_value, 4), ")")
                  }
            ),
            ""
          )
          
          # Significant predictors
          sig_predictors <- names(which(model_summary$coefficients[,4] < 0.05))
          if(length(sig_predictors) > 0) {
            content_parts <- c(content_parts,
              "Significant predictors (p < 0.05):",
              paste("  ", sig_predictors, collapse = "\n")
            )
          } else {
            content_parts <- c(content_parts,
              "No significant predictors at α = 0.05 level"
            )
          }
          
          content_parts <- c(content_parts,
            "",
            "=== RECOMMENDATIONS ===",
            "",
            "- Verify all regression assumptions before interpreting results",
            "- Consider data transformations if assumptions are violated", 
            "- Check for outliers and influential observations",
            "- Validate model with additional data if possible"
          )
          
        } else {
          # No regression model available
          content_parts <- c(
            "SEVA - Complete Multiple Linear Regression Report",
            paste("Generated on:", Sys.Date()),
            "",
            "ERROR: No regression model available.",
            "",
            "To generate a regression report:",
            "1. Go to the 'Regresi Linear Berganda' tab",
            "2. Select dependent and independent variables",
            "3. Click 'Jalankan Regresi' button",
            "4. Then download the report",
            "",
            "The regression model must be created before downloading the report."
          )
        }
        
        content <- paste(content_parts, collapse = "\n")
        writeLines(content, file)
        
      }, error = function(e) {
        content <- paste(
          "SEVA - Complete Multiple Linear Regression Report",
          paste("Generated on:", Sys.Date()),
          "",
          "ERROR: Unable to generate regression report.",
          paste("Error message:", e$message),
          "",
          "Please ensure:",
          "- Regression model has been created",
          "- Variables are properly selected",
          "- Data is available for analysis",
          sep = "\n"
        )
        writeLines(content, file)
      })
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
