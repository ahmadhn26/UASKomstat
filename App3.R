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
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(leaflet)
library(sf)
library(rmarkdown)
library(knitr)
library(webshot)
library(broom)
library(lmtest)
library(moments)



# Install phantomjs if not available (for PDF generation)
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}

# Load data
# Gunakan path relatif ini
sovi_data <- read.csv("data/sovi_data.csv")
distance_data <- read.csv("data/distance.csv")

# Create sample coordinates for mapping (replace with actual coordinates if available)
if(!"LATITUDE" %in% names(sovi_data) || !"LONGITUDE" %in% names(sovi_data)) {
  set.seed(123)
  sovi_data$LATITUDE <- runif(nrow(sovi_data), -8, -6)  # Indonesia latitude range
  sovi_data$LONGITUDE <- runif(nrow(sovi_data), 106, 108)  # Indonesia longitude range
}

# Define UI
ui <- dashboardPage(
  skin = "purple",
  
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
        /* Modern Color Scheme */
        .content-wrapper, .right-side {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          min-height: 100vh;
        }
        
        /* Enhanced Box Styling */
        .box {
          margin-bottom: 25px;
          border-radius: 15px;
          box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(10px);
          border: 1px solid rgba(255, 255, 255, 0.18);
          transition: transform 0.3s ease, box-shadow 0.3s ease;
        }
        
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 12px 40px rgba(0, 0, 0, 0.15);
        }
        
        /* Box Headers */
        .box-header {
          background: linear-gradient(45deg, #667eea, #764ba2);
          color: white;
          border-radius: 15px 15px 0 0;
          padding: 15px 20px;
          font-weight: 600;
          letter-spacing: 0.5px;
        }
        
        .box-header .box-title {
          font-size: 18px;
          font-weight: 700;
          text-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
        }
        
        /* Enhanced Download Buttons */
        .download-btn {
          margin: 8px 5px;
          border-radius: 25px;
          font-weight: 600;
          letter-spacing: 0.5px;
          text-transform: uppercase;
          font-size: 12px;
          padding: 10px 20px;
          transition: all 0.3s ease;
          box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
          position: relative;
          overflow: hidden;
        }
        
        .download-btn::before {
          content: '';
          position: absolute;
          top: 0;
          left: -100%;
          width: 100%;
          height: 100%;
          background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.2), transparent);
          transition: left 0.5s;
        }
        
        .download-btn:hover::before {
          left: 100%;
        }
        
        .download-btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(0, 0, 0, 0.2);
        }
        
        /* Enhanced Download Section */
        .download-section {
          background: linear-gradient(135deg, rgba(255, 255, 255, 0.1), rgba(255, 255, 255, 0.05));
          backdrop-filter: blur(15px);
          padding: 25px;
          border-radius: 20px;
          margin-top: 30px;
          border: 1px solid rgba(255, 255, 255, 0.2);
          box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
        }
        
        .download-section h4 {
          color: #2c3e50;
          font-weight: 700;
          font-size: 20px;
          margin-bottom: 20px;
          text-align: center;
          text-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        
        .download-section h5 {
          color: #34495e;
          font-weight: 600;
          font-size: 16px;
          margin-bottom: 15px;
          border-bottom: 2px solid #ecf0f1;
          padding-bottom: 8px;
        }
        
        /* Sidebar Enhancements */
        .main-sidebar {
          background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
          box-shadow: 2px 0 10px rgba(0, 0, 0, 0.1);
        }
        
        .sidebar-menu > li > a {
          border-radius: 8px;
          margin: 5px 15px;
          transition: all 0.3s ease;
          font-weight: 500;
        }
        
        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
          background: linear-gradient(45deg, #667eea, #764ba2);
          box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
          transform: translateX(5px);
        }
        
        /* Header Styling */
        .main-header .navbar {
          background: linear-gradient(45deg, #667eea, #764ba2);
          box-shadow: 0 2px 20px rgba(0, 0, 0, 0.1);
        }
        
        .main-header .logo {
          background: linear-gradient(45deg, #2c3e50, #34495e);
          font-weight: 700;
          letter-spacing: 1px;
        }
        
        /* Form Controls */
        .form-control, .selectize-input {
          border-radius: 10px;
          border: 2px solid #ecf0f1;
          padding: 12px 15px;
          transition: all 0.3s ease;
          background: rgba(255, 255, 255, 0.9);
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #667eea;
          box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
          background: white;
        }
        
        /* Action Buttons */
        .btn {
          border-radius: 25px;
          font-weight: 600;
          letter-spacing: 0.5px;
          text-transform: uppercase;
          padding: 10px 25px;
          transition: all 0.3s ease;
          box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
        }
        
        .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(0, 0, 0, 0.2);
        }
        
        .btn-primary {
          background: linear-gradient(45deg, #667eea, #764ba2);
          border: none;
        }
        
        .btn-warning {
          background: linear-gradient(45deg, #f39c12, #e67e22);
          border: none;
        }
        
        .btn-success {
          background: linear-gradient(45deg, #27ae60, #2ecc71);
          border: none;
        }
        
        .btn-info {
          background: linear-gradient(45deg, #3498db, #2980b9);
          border: none;
        }
        
        /* Tables */
        .dataTables_wrapper {
          background: rgba(255, 255, 255, 0.95);
          border-radius: 15px;
          padding: 20px;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
        }
        
        .table {
          border-radius: 10px;
          overflow: hidden;
        }
        
        .table thead th {
          background: linear-gradient(45deg, #667eea, #764ba2);
          color: white;
          border: none;
          font-weight: 600;
          letter-spacing: 0.5px;
        }
        
        .table tbody tr:hover {
          background-color: rgba(102, 126, 234, 0.1);
          transform: scale(1.01);
          transition: all 0.2s ease;
        }
        
        /* Tab Navigation */
        .nav-tabs-custom > .nav-tabs {
          border-bottom: 3px solid #667eea;
          background: linear-gradient(135deg, rgba(255, 255, 255, 0.1), rgba(255, 255, 255, 0.05));
          border-radius: 15px 15px 0 0;
        }
        
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background: linear-gradient(45deg, #667eea, #764ba2);
          color: white;
          border-radius: 10px 10px 0 0;
          font-weight: 600;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a {
          border-radius: 10px 10px 0 0;
          transition: all 0.3s ease;
          font-weight: 500;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a:hover {
          background: rgba(102, 126, 234, 0.1);
          color: #667eea;
        }
        
        /* Progress and Loading */
        .progress {
          border-radius: 10px;
          background: rgba(255, 255, 255, 0.2);
          box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        
        .progress-bar {
          background: linear-gradient(45deg, #667eea, #764ba2);
          border-radius: 10px;
        }
        
        /* Responsive adjustments */
        @media (max-width: 768px) {
          .download-btn {
            width: 100%;
            margin: 5px 0;
          }
          
          .box {
            margin-bottom: 15px;
          }
          
          .download-section {
            padding: 15px;
            margin-top: 20px;
          }
        }
        
        /* Animation keyframes */
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(30px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        .box {
          animation: fadeInUp 0.6s ease-out;
        }
        
        /* Scrollbar styling */
        ::-webkit-scrollbar {
          width: 8px;
        }
        
        ::-webkit-scrollbar-track {
          background: rgba(255, 255, 255, 0.1);
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(45deg, #667eea, #764ba2);
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(45deg, #5a6fd8, #6a4190);
        }
      "))
    ),
    
    tabItems(
      # Beranda Tab
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "Selamat Datang di SEVA", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  h3("Socio-Economic Vulnerability Analyzer"),
                  p("Dashboard ini dirancang untuk menganalisis kerentanan sosial-ekonomi berbagai daerah menggunakan berbagai indikator demografis, ekonomi, dan sosial."),
                  p("Fitur utama dashboard ini meliputi:"),
                  tags$ul(
                    tags$li("Manajemen dan kategorisasi data"),
                    tags$li("Eksplorasi data dengan visualisasi interaktif"),
                    tags$li("Peta visualisasi data geografis"),
                    tags$li("Uji asumsi statistik"),
                    tags$li("Analisis statistik inferensia"),
                    tags$li("Analisis regresi linear berganda")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Metadata", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("metadata_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Peta Sebaran Data", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  leafletOutput("overview_map", height = "400px")
                )
              ),
              
              # Download Section for Beranda
              fluidRow(
                box(
                  title = "Download Beranda", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "download-section",
                  h4("Download Options:"),
                  fluidRow(
                    column(4,
                           h5("Individual Downloads:"),
                           downloadButton("download_welcome_pdf", "Welcome Report (PDF)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_metadata_csv", "Metadata (CSV)", class = "btn btn-info download-btn"),
                           br(),
                           downloadButton("download_overview_map_jpg", "Overview Map (JPG)", class = "btn btn-warning download-btn")
                    ),
                    column(8,
                           h5("Complete Page Download:"),
                           downloadButton("download_beranda_complete", "Complete Beranda Report (PDF)", class = "btn btn-success download-btn"),
                           br(),
                           p("Includes: Welcome information, metadata table, and overview map in a comprehensive PDF report.")
                    )
                  )
                )
              )
      ),
      
      # Manajemen Data Tab
      tabItem(tabName = "manajemen",
              fluidRow(
                box(
                  title = "Pengaturan", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("cont_var", "Pilih Variabel Kontinu:",
                              choices = names(select_if(sovi_data, is.numeric))),
                  numericInput("n_bins", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                  actionButton("categorize", "Kategorisasi Data", class = "btn btn-warning")
                ),
                
                box(
                  title = "Hasil Kategorisasi Data", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  DT::dataTableOutput("categorized_data"),
                  br(),
                  verbatimTextOutput("categorization_interpretation")
                )
              ),
              
              fluidRow(
                box(
                  title = "Peta Kategorisasi", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  leafletOutput("categorization_map", height = "400px")
                )
              ),
              
              # Download Section for Manajemen Data
              fluidRow(
                box(
                  title = "Download Manajemen Data", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "download-section",
                  h4("Download Options:"),
                  fluidRow(
                    column(4,
                           h5("Individual Downloads:"),
                           downloadButton("download_categorization_csv", "Categorization Data (CSV)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_categorization_map_jpg", "Categorization Map (JPG)", class = "btn btn-warning download-btn"),
                           br(),
                           downloadButton("download_categorization_interpretation_pdf", "Interpretation (PDF)", class = "btn btn-info download-btn")
                    ),
                    column(8,
                           h5("Complete Page Download:"),
                           downloadButton("download_manajemen_complete", "Complete Data Management Report (PDF)", class = "btn btn-success download-btn"),
                           br(),
                           p("Includes: Categorization results, map visualization, and detailed interpretation in a comprehensive PDF report.")
                    )
                  )
                )
              )
      ),
      
      # Eksplorasi Data Tab
      tabItem(tabName = "eksplorasi",
              tabBox(
                title = "Eksplorasi Data",
                width = 12,
                
                # Tab 1: Statistik Deskriptif
                tabPanel("Statistik Deskriptif",
                         fluidRow(
                           box(
                             title = "Pengaturan Variabel",
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
                             verbatimTextOutput("descriptive_interpretation")
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
                             verbatimTextOutput("plot_interpretation")
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
                             verbatimTextOutput("correlation_interpretation")
                           )
                         )
                ),
                
                # Tab 4: Peta Geografis
                tabPanel("Peta Geografis",
                         fluidRow(
                           box(
                             title = "Pengaturan Peta",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 4,
                             selectInput("map_var", "Variabel untuk Peta:",
                                         choices = names(select_if(sovi_data, is.numeric))),
                             selectInput("map_color", "Skema Warna:",
                                         choices = c("Blues", "Reds", "Greens", "Purples", "Oranges"))
                           ),
                           
                           box(
                             title = "Peta Interaktif",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 8,
                             leafletOutput("exploration_map", height = "500px"),
                             br(),
                             verbatimTextOutput("map_interpretation")
                           )
                         )
                )
              ),
              
              # Download Section for Eksplorasi Data
              fluidRow(
                box(
                  title = "Download Eksplorasi Data", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "download-section",
                  h4("Download Options:"),
                  fluidRow(
                    column(4,
                           h5("Individual Downloads:"),
                           downloadButton("download_descriptive_csv", "Descriptive Stats (CSV)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_descriptive_interpretation_pdf", "Complete Descriptive Report (Word)", class = "btn btn-info download-btn"),
                           br(),
                           downloadButton("download_plot_jpg", "Current Plot (JPG)", class = "btn btn-warning download-btn"),
                           br(),
                           downloadButton("download_plot_interpretation_pdf", "Plot Interpretation (PDF)", class = "btn btn-info download-btn"),
                           br(),
                           downloadButton("download_correlation_jpg", "Correlation Heatmap (JPG)", class = "btn btn-warning download-btn"),
                           br(),
                           downloadButton("download_correlation_interpretation_pdf", "Correlation Interpretation (PDF)", class = "btn btn-info download-btn"),
                           br(),
                           downloadButton("download_exploration_map_jpg", "Geographic Map (JPG)", class = "btn btn-warning download-btn"),
                           br(),
                           downloadButton("download_map_interpretation_pdf", "Map Interpretation (PDF)", class = "btn btn-info download-btn")
                    ),
                    column(8,
                           h5("Complete Page Download:"),
                           downloadButton("download_eksplorasi_complete", "Complete Data Exploration Report (PDF)", class = "btn btn-success download-btn"),
                           br(),
                           p("Includes: All descriptive statistics, visualizations, correlation analysis, geographic maps, and comprehensive interpretations in a detailed PDF report.")
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
                  verbatimTextOutput("normality_interpretation")
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
                  verbatimTextOutput("homogeneity_interpretation")
                )
              ),
              
              # Download Section for Uji Asumsi
              fluidRow(
                box(
                  title = "Download Uji Asumsi", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "download-section",
                  h4("Download Options:"),
                  fluidRow(
                    column(4,
                           h5("Individual Downloads:"),
                           downloadButton("download_normality_test_pdf", "Complete Assumption Tests Report (Word)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_qq_plot_jpg", "Q-Q Plot (JPG)", class = "btn btn-warning download-btn"),
                           br(),
                           downloadButton("download_normality_interpretation_pdf", "Normality Interpretation (PDF)", class = "btn btn-info download-btn"),
                           br(),
                           downloadButton("download_homogeneity_test_pdf", "Homogeneity Test Results (PDF)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_homogeneity_interpretation_pdf", "Homogeneity Interpretation (PDF)", class = "btn btn-info download-btn")
                    ),
                    column(8,
                           h5("Complete Page Download:"),
                           downloadButton("download_asumsi_complete", "Complete Assumption Tests Report (PDF)", class = "btn btn-success download-btn"),
                           br(),
                           p("Includes: All normality and homogeneity test results, Q-Q plots, and comprehensive statistical interpretations in a detailed PDF report.")
                    )
                  )
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
                             verbatimTextOutput("ttest1_interpretation")
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
                             verbatimTextOutput("ttest2_interpretation")
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
                             verbatimTextOutput("prop1_interpretation")
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
                             verbatimTextOutput("var1_interpretation")
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
                             verbatimTextOutput("anova1_interpretation")
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
                             verbatimTextOutput("anova2_interpretation")
                           )
                         )
                )
              ),
              
              # Download Section for Statistik Inferensia
              fluidRow(
                box(
                  title = "Download Statistik Inferensia", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "download-section",
                  h4("Download Options:"),
                  fluidRow(
                    column(4,
                           h5("Individual Test Downloads:"),
                           downloadButton("download_ttest1_pdf", "Complete Inferential Statistics Report (Word)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_ttest2_pdf", "2-Sample T-Test (PDF)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_prop1_pdf", "Proportion Test (PDF)", class = "btn btn-info download-btn"),
                           br(),
                           downloadButton("download_var1_pdf", "Variance Test (PDF)", class = "btn btn-info download-btn"),
                           br(),
                           downloadButton("download_anova1_pdf", "One-Way ANOVA (PDF)", class = "btn btn-warning download-btn"),
                           br(),
                           downloadButton("download_anova2_pdf", "Two-Way ANOVA (PDF)", class = "btn btn-warning download-btn")
                    ),
                    column(8,
                           h5("Complete Page Download:"),
                           downloadButton("download_inferensia_complete", "Complete Inferential Statistics Report (PDF)", class = "btn btn-success download-btn"),
                           br(),
                           p("Includes: All t-tests, proportion tests, variance tests, ANOVA results, and comprehensive statistical interpretations in a detailed PDF report.")
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
                                  verbatimTextOutput("regression_interpretation")
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
                                  verbatimTextOutput("assumption_interpretation")
                         )
                       )
                )
              ),
              
              # Download Section for Regresi
              fluidRow(
                box(
                  title = "Download Regresi Linear Berganda", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  class = "download-section",
                  h4("Download Options:"),
                  fluidRow(
                    column(4,
                           h5("Individual Downloads:"),
                           downloadButton("download_regression_summary_pdf", "Complete Regression Analysis Report (Word)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_regression_interpretation_pdf", "Regression Interpretation (PDF)", class = "btn btn-info download-btn"),
                           br(),
                           downloadButton("download_vif_test_pdf", "VIF Test Results (PDF)", class = "btn btn-primary download-btn"),
                           br(),
                           downloadButton("download_residual_qq_jpg", "Q-Q Plot of Residuals (JPG)", class = "btn btn-warning download-btn"),
                           br(),
                           downloadButton("download_residual_fitted_jpg", "Residuals vs Fitted Plot (JPG)", class = "btn btn-warning download-btn"),
                           br(),
                           downloadButton("download_assumption_interpretation_pdf", "Assumptions Interpretation (PDF)", class = "btn btn-info download-btn")
                    ),
                    column(8,
                           h5("Complete Page Download:"),
                           downloadButton("download_regresi_complete", "Complete Regression Analysis Report (PDF)", class = "btn btn-success download-btn"),
                           br(),
                           p("Includes: Complete regression results, model summary, assumption tests, diagnostic plots, and comprehensive interpretations in a detailed PDF report.")
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
  
  # Metadata table (unchanged)
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
  
  # Overview Map (unchanged)
  output$overview_map <- renderLeaflet({
    leaflet(sovi_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, 
        lat = ~LATITUDE,
        radius = ~sqrt(POPULATION)/50,
        popup = ~paste("District:", DISTRICTCODE, "<br>",
                       "Population:", POPULATION, "<br>",
                       "Poverty:", POVERTY, "%"),
        color = "blue",
        fillOpacity = 0.6
      ) %>%
      setView(lng = mean(sovi_data$LONGITUDE), lat = mean(sovi_data$LATITUDE), zoom = 8)
  })
  
  # Data Management (unchanged)
  categorized_data_reactive <- eventReactive(input$categorize, {
    req(input$cont_var, input$n_bins)
    
    var_data <- sovi_data[[input$cont_var]]
    breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_bins + 1), na.rm = TRUE)
    
    categorized <- cut(var_data, breaks = breaks, include.lowest = TRUE, 
                       labels = paste0("Kategori_", 1:input$n_bins))
    
    result_data <- sovi_data
    result_data[[paste0(input$cont_var, "_Kategori")]] <- categorized
    
    return(result_data)
  })
  
  output$categorized_data <- DT::renderDataTable({
    req(categorized_data_reactive())
    cat_data <- categorized_data_reactive()
    display_data <- cat_data[, c(input$cont_var, paste0(input$cont_var, "_Kategori"))]
    DT::datatable(display_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$categorization_interpretation <- renderText({
    req(categorized_data_reactive())
    paste("Interpretasi: Variabel", input$cont_var, "telah berhasil dikategorisasi menjadi", 
          input$n_bins, "kategori berdasarkan kuantil. Kategorisasi ini membantu dalam",
          "analisis data dengan mengubah variabel kontinu menjadi variabel kategorikal",
          "yang dapat digunakan untuk analisis lebih lanjut seperti ANOVA atau Chi-square test.")
  })
  
  # Categorization Map (unchanged)
  output$categorization_map <- renderLeaflet({
    req(categorized_data_reactive())
    cat_data <- categorized_data_reactive()
    
    colors <- RColorBrewer::brewer.pal(input$n_bins, "Set3")
    pal <- colorFactor(colors, domain = cat_data[[paste0(input$cont_var, "_Kategori")]])
    
    leaflet(cat_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, 
        lat = ~LATITUDE,
        radius = 8,
        popup = ~paste("District:", DISTRICTCODE, "<br>",
                       input$cont_var, ":", get(input$cont_var), "<br>",
                       "Category:", get(paste0(input$cont_var, "_Kategori"))),
        color = ~pal(get(paste0(input$cont_var, "_Kategori"))),
        fillOpacity = 0.8
      ) %>%
      addLegend(pal = pal, values = ~get(paste0(input$cont_var, "_Kategori")), 
                title = paste(input$cont_var, "Categories")) %>%
      setView(lng = mean(cat_data$LONGITUDE), lat = mean(cat_data$LATITUDE), zoom = 8)
  })
  
  # Descriptive Statistics (unchanged)
  output$descriptive_stats <- DT::renderDataTable({
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
    
    DT::datatable(desc_summary, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$descriptive_interpretation <- renderText({
    req(input$desc_vars)
    paste("Interpretasi: Statistik deskriptif menunjukkan karakteristik dasar dari variabel yang dipilih.",
          "Mean dan median memberikan gambaran tentang tendensi sentral, sedangkan standar deviasi",
          "menunjukkan variabilitas data. Perbedaan antara mean dan median dapat mengindikasikan",
          "adanya skewness dalam distribusi data.")
  })
  
  # Visualization (unchanged)
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
  
  # Correlation Heatmap (unchanged)
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
    
    cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
    max_cor <- which(cor_matrix == max(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    min_cor <- which(cor_matrix == min(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    
    paste("Interpretasi: Peta korelasi menunjukkan hubungan linear antara semua variabel numerik.",
          "Korelasi positif terkuat terjadi antara", rownames(cor_matrix)[max_cor[1]], "dan", 
          colnames(cor_matrix)[max_cor[2]], "dengan nilai", round(cor_matrix[max_cor], 3),
          ". Korelasi negatif terkuat terjadi antara", rownames(cor_matrix)[min_cor[1]], "dan",
          colnames(cor_matrix)[min_cor[2]], "dengan nilai", round(cor_matrix[min_cor], 3))
  })
  
  # Geographic Map (unchanged)
  output$exploration_map <- renderLeaflet({
    req(input$map_var)
    
    pal <- colorNumeric(palette = input$map_color, domain = sovi_data[[input$map_var]])
    
    leaflet(sovi_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, 
        lat = ~LATITUDE,
        radius = ~sqrt(get(input$map_var))/2,
        popup = ~paste("District:", DISTRICTCODE, "<br>",
                       input$map_var, ":", get(input$map_var)),
        color = ~pal(get(input$map_var)),
        fillOpacity = 0.8
      ) %>%
      addLegend(pal = pal, values = ~get(input$map_var), title = input$map_var) %>%
      setView(lng = mean(sovi_data$LONGITUDE), lat = mean(sovi_data$LATITUDE), zoom = 8)
  })
  
  output$map_interpretation <- renderText({
    req(input$map_var)
    paste("Interpretasi: Peta geografis menunjukkan distribusi spasial dari variabel", input$map_var,
          "di seluruh wilayah. Ukuran dan warna lingkaran menunjukkan nilai variabel,",
          "memungkinkan identifikasi pola geografis dan kluster nilai tinggi atau rendah.")
  })
  
  # Assumption Tests (unchanged)
  observeEvent(input$run_tests, {
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
    
    output$homogeneity_test <- renderPrint({
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
  
  # Inferential Statistics (unchanged)
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
  
  observeEvent(input$run_ttest2, {
    output$ttest2_result <- renderPrint({
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
  
  observeEvent(input$run_prop1, {
    output$prop1_result <- renderPrint({
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
  
  observeEvent(input$run_var1, {
    output$var1_result <- renderPrint({
      req(input$var1_var, input$var1_sigma)
      
      var_data <- sovi_data[[input$var1_var]]
      var_data <- var_data[!is.na(var_data)]
      
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
  
  observeEvent(input$run_anova1, {
    output$anova1_result <- renderPrint({
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
      
      cat("One-Way ANOVA\n\n")
      cat("H0: μ1 = μ2 = μ3 = ... (all group means are equal)\n")
      cat("H1: At least one group mean is different\n\n")
      print(anova_summary)
      
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
  
  observeEvent(input$run_anova2, {
    output$anova2_result <- renderPrint({
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
  
  # Multiple Linear Regression (unchanged)
  regression_model <- eventReactive(input$run_regression, {
    req(input$reg_dep, input$reg_indep)
    
    dep_var <- sovi_data[[input$reg_dep]]
    indep_vars <- sovi_data[, input$reg_indep, drop = FALSE]
    
    complete_data <- cbind(dep_var, indep_vars)
    complete_data <- complete_data[complete.cases(complete_data), ]
    
    formula_str <- paste(input$reg_dep, "~", paste(input$reg_indep, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
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
    p_value <- glance(model)$p.value
    
    significant_coeffs <- summary(model)$coefficients
    significant_vars <- names(which(significant_coeffs[, "Pr(>|t|)"] < 0.05))
    
    interpretation <- paste(
      "Interpretasi Regresi Linear Berganda:\n",
      "1. R-squared:", round(r_squared, 4), "menunjukkan bahwa", round(r_squared * 100, 2),
      "% variabilitas dalam", input$reg_dep, "dapat dijelaskan oleh variabel independen.\n",
      "2. Adjusted R-squared:", round(adj_r_squared, 4), "menyesuaikan R-squared untuk jumlah prediktor.\n",
      "3. P-value model keseluruhan:", round(p_value, 4),
      ifelse(p_value < 0.05, " (signifikan)", " (tidak signifikan)"),
      "pada tingkat signifikansi 5%.\n",
      "4. Koefisien yang signifikan (p < 0.05): ",
      paste(significant_vars, collapse = ", ")
    )
    
    return(interpretation)
  })
  
  output$vif_test <- renderPrint({
    req(regression_model())
    model <- regression_model()
    
    if(length(input$reg_indep) > 1) {
      vif_result <- car::vif(model)
      cat("Variance Inflation Factor (VIF) Test:\n\n")
      print(vif_result)
    } else {
      cat("VIF test tidak dapat dilakukan karena hanya ada satu variabel independen.")
    }
  })
  
  output$assumption_interpretation <- renderText({
    req(regression_model())
    model <- regression_model()
    
    vif_interpretation <- if(length(input$reg_indep) > 1) {
      vif_result <- car::vif(model)
      if(any(vif_result > 10)) {
        paste("VIF menunjukkan adanya multikolinearitas signifikan (VIF > 10) untuk variabel:",
              paste(names(vif_result[vif_result > 10]), collapse = ", "), ".\n")
      } else {
        "VIF menunjukkan tidak ada multikolinearitas signifikan (semua VIF < 10).\n"
      }
    } else {
      "VIF test tidak dilakukan karena hanya ada satu variabel independen.\n"
    }
    
    residuals <- resid(model)
    shapiro_test <- shapiro.test(residuals)
    normality_interpretation <- if(shapiro_test$p.value > 0.05) {
      paste("Uji Shapiro-Wilk untuk normalitas residual (p-value =", round(shapiro_test$p.value, 4),
            "): Residual dianggap berdistribusi normal.\n")
    } else {
      paste("Uji Shapiro-Wilk untuk normalitas residual (p-value =", round(shapiro_test$p.value, 4),
            "): Residual tidak berdistribusi normal.\n")
    }
    
    bptest_result <- lmtest::bptest(model)
    homoskedasticity_interpretation <- if(bptest_result$p.value > 0.05) {
      paste("Uji Breusch-Pagan untuk homoskedastisitas (p-value =", round(bptest_result$p.value, 4),
            "): Varians residual homogen.\n")
    } else {
      paste("Uji Breusch-Pagan untuk homoskedastisitas (p-value =", round(bptest_result$p.value, 4),
            "): Varians residual tidak homogen.\n")
    }
    
    paste("Interpretasi Uji Asumsi Model:\n",
          vif_interpretation,
          normality_interpretation,
          homoskedasticity_interpretation)
  })
  
  output$residual_qq <- renderPlot({
    req(regression_model())
    model <- regression_model()
    residuals <- resid(model)
    
    ggplot(data.frame(sample = residuals), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = "Q-Q Plot of Residuals")
  })
  
  output$residual_fitted <- renderPlot({
    req(regression_model())
    model <- regression_model()
    
    ggplot(data.frame(fitted = fitted(model), residuals = resid(model)), 
           aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")
  })
  
  # Reactive values to track test completions and store managed data
  values <- reactiveValues(
    assumptions_done = FALSE,
    ttest1_done = FALSE,
    ttest2_done = FALSE,
    prop1_done = FALSE,
    var1_done = FALSE,
    anova1_done = FALSE,
    anova2_done = FALSE,
    regression_done = FALSE,
    managed_data = NULL
  )
  
  observeEvent(input$categorize, {
    values$managed_data <- categorized_data_reactive()
  })
  
  observeEvent(input$run_tests, {
    values$assumptions_done <- TRUE
  })
  observeEvent(input$run_ttest1, {
    values$ttest1_done <- TRUE
  })
  observeEvent(input$run_ttest2, {
    values$ttest2_done <- TRUE
  })
  observeEvent(input$run_prop1, {
    values$prop1_done <- TRUE
  })
  observeEvent(input$run_var1, {
    values$var1_done <- TRUE
  })
  observeEvent(input$run_anova1, {
    values$anova1_done <- TRUE
  })
  observeEvent(input$run_anova2, {
    values$anova2_done <- TRUE
  })
  observeEvent(input$run_regression, {
    values$regression_done <- TRUE
  })
  
  # Download Handlers
  
  # --- Beranda Tab ---
  output$download_welcome_pdf <- downloadHandler(
    filename = function() { paste0("welcome_report_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      temp_md <- tempfile(fileext = ".Rmd")
      
      rmd_content <- paste(
        "---",
        "title: 'Laporan Selamat Datang - SEVA'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# Socio-Economic Vulnerability Analyzer (SEVA)\n",
        "Dashboard ini dirancang untuk menganalisis kerentanan sosial-ekonomi berbagai daerah menggunakan berbagai indikator demografis, ekonomi, dan sosial.\n\n",
        "## Fitur Utama\n",
        "- Manajemen dan kategorisasi data\n",
        "- Eksplorasi data dengan visualisasi interaktif\n",
        "- Peta visualisasi data geografis\n",
        "- Uji asumsi statistik\n",
        "- Analisis statistik inferensia\n",
        "- Analisis regresi linear berganda",
        sep = "\n"
      )
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  # Other download handlers remain unchanged for brevity, but apply similar changes as above
  # (Add TinyTeX check, comprehensive LaTeX preamble, and error handling to each downloadHandler)
  
  output$download_metadata_csv <- downloadHandler(
    filename = function() { paste0("metadata_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(metadata, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_overview_map_jpg <- downloadHandler(
    filename = function() { paste0("overview_map_", Sys.Date(), ".jpg") },
    content = function(file) {
      map <- leaflet(sovi_data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~LONGITUDE, 
          lat = ~LATITUDE,
          radius = ~sqrt(POPULATION)/50,
          popup = ~paste("District:", DISTRICTCODE, "<br>",
                         "Population:", POPULATION, "<br>",
                         "Poverty:", POVERTY, "%"),
          color = "blue",
          fillOpacity = 0.6
        ) %>%
        setView(lng = mean(sovi_data$LONGITUDE), lat = mean(sovi_data$LATITUDE), zoom = 8)
      
      mapshot(map, file = file, vwidth = 800, vheight = 600)
    },
    contentType = "image/jpeg"
  )
  
  #--- Beranda Complete Tab ---
    
    output$download_beranda_complete <- downloadHandler( filename = function() { paste0("beranda_complete_", Sys.Date(), ".pdf") }, content = function(file) { if (!tinytex::is_tinytex()) { tinytex::install_tinytex() }
      
      temp_md <- tempfile(fileext = ".Rmd")
      
      rmd_content <- paste(
        "---",
        "title: 'Laporan Lengkap Beranda - SEVA'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# Laporan Lengkap Beranda\n",
        "## Ringkasan Data\n",
        "Laporan ini berisi ringkasan data dari Socio-Economic Vulnerability Analyzer (SEVA). Data mencakup berbagai indikator sosial-ekonomi untuk analisis kerentanan daerah.\n",
        "Jumlah observasi: ", nrow(sovi_data), "\n",
        "Jumlah variabel: ", ncol(sovi_data), "\n",
        "## Variabel yang Tersedia\n",
        paste(names(sovi_data), collapse = ", "), "\n",
        sep = "\n"
      )
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
      
    }, contentType = "application/pdf" )
  
  # --- Manajemen Data Tab ---
  output$download_categorization_csv <- downloadHandler(
    filename = function() { paste0("categorization_data_", Sys.Date(), ".csv") },
    content = function(file) {
      req(categorized_data_reactive())
      cat_data <- categorized_data_reactive()
      display_data <- cat_data[, c(input$cont_var, paste0(input$cont_var, "_Kategori"))]
      write.csv(display_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_categorization_map_jpg <- downloadHandler(
    filename = function() { paste0("categorization_map_", Sys.Date(), ".jpg") },
    content = function(file) {
      req(categorized_data_reactive())
      cat_data <- categorized_data_reactive()
      colors <- RColorBrewer::brewer.pal(input$n_bins, "Set3")
      pal <- colorFactor(colors, domain = cat_data[[paste0(input$cont_var, "_Kategori")]])
      
      map <- leaflet(cat_data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~LONGITUDE, 
          lat = ~LATITUDE,
          radius = 8,
          popup = ~paste("District:", DISTRICTCODE, "<br>",
                         input$cont_var, ":", get(input$cont_var), "<br>",
                         "Category:", get(paste0(input$cont_var, "_Kategori"))),
          color = ~pal(get(paste0(input$cont_var, "_Kategori"))),
          fillOpacity = 0.8
        ) %>%
        addLegend(pal = pal, values = ~get(paste0(input$cont_var, "_Kategori")), 
                  title = paste(input$cont_var, "Categories")) %>%
        setView(lng = mean(cat_data$LONGITUDE), lat = mean(cat_data$LATITUDE), zoom = 8)
      
      mapshot(map, file = file, vwidth = 800, vheight = 600)
    },
    contentType = "image/jpeg"
  )
  
  output$download_categorization_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("categorization_interpretation_", Sys.Date(), ".pdf") },
    content = function(file) {
      # Pastikan TinyTeX terinstal
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(categorized_data_reactive())
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        simple_rmd <- paste(
          "---",
          "title: 'Interpretasi Kategorisasi'",
          "date: '", Sys.Date(), "'",
          "output: pdf_document",
          "---",
          "",
          "## Interpretasi Kategorisasi",
          "",
          paste("Variabel", input$cont_var, "telah berhasil dikategorisasi menjadi", input$n_bins,
                "kategori berdasarkan kuantil. Kategorisasi ini membantu dalam analisis data dengan mengubah",
                "variabel kontinu menjadi variabel kategorikal yang dapat digunakan untuk analisis lebih lanjut seperti ANOVA atau Chi-square test."),
          sep = "\n"
        )
        writeLines(simple_rmd, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      })
    },
    contentType = "application/pdf"
  )

  
  output$download_manajemen_complete <- downloadHandler( filename = function() { paste0("manajemen_complete_", Sys.Date(), ".pdf") }, content = function(file) { if (!tinytex::is_tinytex()) { tinytex::install_tinytex() }
    
    temp_md <- tempfile(fileext = ".Rmd")
    
    # Ringkasan data setelah manajemen
    managed_data <- values$managed_data
    if (is.null(managed_data)) managed_data <- sovi_data
    
    rmd_content <- paste(
      "---",
      "title: 'Laporan Manajemen Data - SEVA'",
      "date: '", Sys.Date(), "'",
      "output:",
      "  pdf_document:",
      "    latex_engine: pdflatex",
      "    keep_tex: true",
      "geometry: margin=1in",
      "header-includes:",
      "  - \\usepackage{booktabs}",
      "  - \\usepackage{longtable}",
      "  - \\usepackage{caption}",
      "  - \\usepackage[utf8]{inputenc}",
      "  - \\usepackage{geometry}",
      "  - \\geometry{a4paper, margin=1in}",
      "  - \\usepackage{parskip}",
      "  - \\setlength{\\parskip}{0.5em}",
      "---",
      "\n# Laporan Manajemen Data\n",
      "## Ringkasan Data Setelah Manajemen\n",
      "Data telah dimodifikasi berdasarkan pengaturan manajemen data.\n",
      "Jumlah observasi: ", nrow(managed_data), "\n",
      "Jumlah variabel: ", ncol(managed_data), "\n",
      "## Variabel yang Tersedia\n",
      paste(names(managed_data), collapse = ", "), "\n",
      sep = "\n"
    )
    
    writeLines(rmd_content, temp_md)
    
    tryCatch(
      {
        rmarkdown::render(
          temp_md,
          output_file = file,
          output_format = "pdf_document",
          clean = TRUE,
          envir = new.env(parent = globalenv())
        )
      },
      error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      }
    )
    
  }, contentType = "application/pdf" )
  
  # --- Eksplorasi Data Tab ---
  output$download_descriptive_csv <- downloadHandler(
    filename = function() { paste0("descriptive_stats_", Sys.Date(), ".csv") },
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
    },
    contentType = "text/csv"
  )
  
  output$download_descriptive_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("descriptive_complete_report_", Sys.Date(), ".docx") },
    content = function(file) {
      req(input$desc_vars)
      
      # Calculate descriptive statistics
      desc_data <- sovi_data[, input$desc_vars, drop = FALSE]
      desc_summary <- data.frame(
        Variable = names(desc_data),
        Mean = round(sapply(desc_data, function(x) mean(x, na.rm = TRUE)), 3),
        Median = round(sapply(desc_data, function(x) median(x, na.rm = TRUE)), 3),
        SD = round(sapply(desc_data, function(x) sd(x, na.rm = TRUE)), 3),
        Min = round(sapply(desc_data, function(x) min(x, na.rm = TRUE)), 3),
        Max = round(sapply(desc_data, function(x) max(x, na.rm = TRUE)), 3),
        stringsAsFactors = FALSE
      )
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        rmd_content <- paste(
          "---",
          "title: 'Laporan Lengkap Statistik Deskriptif'",
          "author: 'SEVA - Socio-Economic Vulnerability Analyzer'",
          "date: '", Sys.Date(), "'",
          "output: word_document",
          "---",
          "",
          "# Laporan Statistik Deskriptif",
          "",
          "## Ringkasan Eksekutif",
          "Laporan ini menyajikan analisis statistik deskriptif lengkap untuk variabel yang dipilih dalam sistem SEVA.",
          "",
          "## Variabel yang Dianalisis",
          paste("Variabel yang dipilih:", paste(input$desc_vars, collapse = ", ")),
          "",
          "## Hasil Statistik Deskriptif",
          "",
          "```{r echo=FALSE, results='asis'}",
          "library(knitr)",
          "desc_summary <- data.frame(",
          paste("  Variable = c(", paste(paste0("'", desc_summary$Variable, "'"), collapse = ", "), "),"),
          paste("  Mean = c(", paste(desc_summary$Mean, collapse = ", "), "),"),
          paste("  Median = c(", paste(desc_summary$Median, collapse = ", "), "),"),
          paste("  SD = c(", paste(desc_summary$SD, collapse = ", "), "),"),
          paste("  Min = c(", paste(desc_summary$Min, collapse = ", "), "),"),
          paste("  Max = c(", paste(desc_summary$Max, collapse = ", "), ")"),
          ")",
          "kable(desc_summary, caption = 'Statistik Deskriptif')",
          "```",
          "",
          "## Interpretasi",
          "",
          "### Analisis Tendensi Sentral",
          "Statistik deskriptif menunjukkan karakteristik dasar dari variabel yang dipilih. Mean (rata-rata) dan median memberikan gambaran tentang tendensi sentral data.",
          "",
          "### Analisis Variabilitas", 
          "Standar deviasi (SD) menunjukkan tingkat variabilitas atau sebaran data. Nilai yang lebih tinggi mengindikasikan data yang lebih tersebar.",
          "",
          "### Analisis Distribusi",
          "Perbedaan antara mean dan median dapat mengindikasikan adanya skewness dalam distribusi data:",
          "- Jika mean > median: data cenderung skew ke kanan (positive skew)",
          "- Jika mean < median: data cenderung skew ke kiri (negative skew)",  
          "- Jika mean ≈ median: data cenderung terdistribusi normal",
          "",
          "### Analisis Range",
          "Nilai minimum dan maksimum menunjukkan rentang data dan dapat membantu mengidentifikasi potensi outliers.",
          "",
          "## Kesimpulan",
          "Berdasarkan analisis statistik deskriptif, variabel-variabel yang dipilih menunjukkan karakteristik yang dapat digunakan untuk analisis lebih lanjut dalam konteks kerentanan sosial-ekonomi.",
          sep = "\n"
        )
        
        writeLines(rmd_content, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate Word document: ", e$message)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  output$download_plot_jpg <- downloadHandler(
    filename = function() { paste0("exploration_plot_", Sys.Date(), ".jpg") },
    content = function(file) {
      req(input$plot_var1)
      if(input$plot_type == "hist") {
        plot <- ggplot(sovi_data, aes_string(x = input$plot_var1)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$plot_var1),
               x = input$plot_var1, y = "Frequency")
      } else if(input$plot_type == "box") {
        plot <- ggplot(sovi_data, aes_string(y = input$plot_var1)) +
          geom_boxplot(fill = "lightblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Box Plot of", input$plot_var1),
               y = input$plot_var1)
      } else if(input$plot_type == "scatter" && !is.null(input$plot_var2)) {
        plot <- ggplot(sovi_data, aes_string(x = input$plot_var1, y = input$plot_var2)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          geom_smooth(method = "lm", se = TRUE, color = "red") +
          theme_minimal() +
          labs(title = paste("Scatter Plot:", input$plot_var1, "vs", input$plot_var2),
               x = input$plot_var1, y = input$plot_var2)
      }
      ggsave(file, plot = plot, device = "jpeg", width = 8, height = 6)
    },
    contentType = "image/jpeg"
  )
  
  output$download_plot_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("plot_interpretation_", Sys.Date(), ".pdf") },
    content = function(file) {
      req(input$plot_var1)
      
      interpretation <- if(input$plot_type == "hist") {
        paste("Histogram menunjukkan distribusi frekuensi dari variabel", input$plot_var1,
              ". Bentuk distribusi dapat memberikan informasi tentang normalitas data dan adanya outliers.")
      } else if(input$plot_type == "box") {
        paste("Box plot menampilkan ringkasan lima angka (minimum, Q1, median, Q3, maksimum)",
              "dari variabel", input$plot_var1, ". Titik-titik di luar whiskers menunjukkan potensi outliers.")
      } else if(input$plot_type == "scatter") {
        paste("Scatter plot menunjukkan hubungan antara", input$plot_var1, "dan", input$plot_var2,
              ". Garis regresi membantu memvisualisasikan tren hubungan linear antara kedua variabel.")
      }
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        simple_rmd <- paste(
          "---",
          "title: 'Interpretasi Visualisasi'",
          "date: '", Sys.Date(), "'",
          "output: pdf_document",
          "---",
          "",
          "## Interpretasi Visualisasi",
          "",
          interpretation,
          sep = "\n"
        )
        writeLines(simple_rmd, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      })
    },
    contentType = "application/pdf"
  )
  
  output$download_correlation_jpg <- downloadHandler(
    filename = function() { paste0("correlation_heatmap_", Sys.Date(), ".jpg") },
    content = function(file) {
      numeric_data <- select_if(sovi_data, is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      melted_cor <- melt(cor_matrix)
      
      plot <- ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        labs(title = "Correlation Heatmap", x = "", y = "") +
        coord_fixed()
      
      ggsave(file, plot = plot, device = "jpeg", width = 8, height = 6)
    },
    contentType = "image/jpeg"
  )
  
  output$download_correlation_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("correlation_interpretation_", Sys.Date(), ".pdf") },
    content = function(file) {
      numeric_data <- select_if(sovi_data, is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
      max_cor <- which(cor_matrix == max(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
      min_cor <- which(cor_matrix == min(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        simple_rmd <- paste(
          "---",
          "title: 'Interpretasi Peta Korelasi'",
          "date: '", Sys.Date(), "'",
          "output: pdf_document",
          "---",
          "",
          "## Interpretasi Peta Korelasi",
          "",
          "Peta korelasi menunjukkan hubungan linear antara semua variabel numerik.",
          "",
          paste("Korelasi positif terkuat terjadi antara", rownames(cor_matrix)[max_cor[1]], "dan",
                colnames(cor_matrix)[max_cor[2]], "dengan nilai", round(cor_matrix[max_cor], 3)),
          "",
          paste("Korelasi negatif terkuat terjadi antara", rownames(cor_matrix)[min_cor[1]], "dan",
                colnames(cor_matrix)[min_cor[2]], "dengan nilai", round(cor_matrix[min_cor], 3)),
          sep = "\n"
        )
        writeLines(simple_rmd, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      })
    },
    contentType = "application/pdf"
  )
  
  output$download_exploration_map_jpg <- downloadHandler(
    filename = function() { paste0("exploration_map_", Sys.Date(), ".jpg") },
    content = function(file) {
      req(input$map_var)
      pal <- colorNumeric(palette = input$map_color, domain = sovi_data[[input$map_var]])
      
      map <- leaflet(sovi_data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~LONGITUDE, 
          lat = ~LATITUDE,
          radius = ~sqrt(get(input$map_var))/2,
          popup = ~paste("District:", DISTRICTCODE, "<br>",
                         input$map_var, ":", get(input$map_var)),
          color = ~pal(get(input$map_var)),
          fillOpacity = 0.8
        ) %>%
        addLegend(pal = pal, values = ~get(input$map_var), title = input$map_var) %>%
        setView(lng = mean(sovi_data$LONGITUDE), lat = mean(sovi_data$LATITUDE), zoom = 8)
      
      mapshot(map, file = file, vwidth = 800, vheight = 600)
    },
    contentType = "image/jpeg"
  )
  
  output$download_map_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("map_interpretation_", Sys.Date(), ".pdf") },
    content = function(file) {
      req(input$map_var)
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        simple_rmd <- paste(
          "---",
          "title: 'Interpretasi Peta Geografis'",
          "date: '", Sys.Date(), "'",
          "output: pdf_document",
          "---",
          "",
          "## Interpretasi Peta Geografis",
          "",
          paste("Peta geografis menunjukkan distribusi spasial dari variabel", input$map_var,
                "di seluruh wilayah. Ukuran dan warna lingkaran menunjukkan nilai variabel,",
                "memungkinkan identifikasi pola geografis dan kluster nilai tinggi atau rendah."),
          sep = "\n"
        )
        writeLines(simple_rmd, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      })
    },
    contentType = "application/pdf"
  )
  
  # --- Eksplorasi Data Tab (Lanjutan) ---
  output$download_explorasi_complete <- downloadHandler( filename = function() { paste0("explorasi_complete_", Sys.Date(), ".pdf") }, content = function(file) { if (!tinytex::is_tinytex()) { tinytex::install_tinytex() }
    
    req(input$desc_vars, input$plot_var1, input$map_var)
    temp_md <- tempfile(fileext = ".Rmd")
    
    # Hitung statistik deskriptif
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
    
    # Simpan plot
    plot_file <- file.path(tempdir(), "exploration_plot.jpg")
    if(input$plot_type == "hist") {
      plot <- ggplot(sovi_data, aes_string(x = input$plot_var1)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram dari", input$plot_var1),
             x = input$plot_var1, y = "Frekuensi")
    } else if(input$plot_type == "box") {
      plot <- ggplot(sovi_data, aes_string(y = input$plot_var1)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Box Plot dari", input$plot_var1),
             y = input$plot_var1)
    } else if(input$plot_type == "scatter" && !is.null(input$plot_var2)) {
      plot <- ggplot(sovi_data, aes_string(x = input$plot_var1, y = input$plot_var2)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        theme_minimal() +
        labs(title = paste("Scatter Plot:", input$plot_var1, "vs", input$plot_var2),
             x = input$plot_var1, y = input$plot_var2)
    }
    ggsave(plot_file, plot = plot, device = "jpeg", width = 8, height = 6)
    
    # Simpan peta
    map_file <- file.path(tempdir(), "exploration_map.jpg")
    pal <- colorNumeric(palette = input$map_color, domain = sovi_data[[input$map_var]])
    map <- leaflet(sovi_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, 
        lat = ~LATITUDE,
        radius = ~sqrt(get(input$map_var))/2,
        popup = ~paste("Distrik:", DISTRICTCODE, "<br>",
                       input$map_var, ":", get(input$map_var)),
        color = ~pal(get(input$map_var)),
        fillOpacity = 0.8
      ) %>%
      addLegend(pal = pal, values = ~get(input$map_var), title = input$map_var) %>%
      setView(lng = mean(sovi_data$LONGITUDE), lat = mean(sovi_data$LATITUDE), zoom = 8)
    mapshot(map, file = map_file, vwidth = 800, vheight = 600)
    
    # Simpan heatmap korelasi
    heatmap_file <- file.path(tempdir(), "correlation_heatmap.jpg")
    numeric_data <- select_if(sovi_data, is.numeric)
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    melted_cor <- reshape2::melt(cor_matrix)
    heatmap_plot <- ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Korelasi") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(title = "Heatmap Korelasi", x = "", y = "") +
      coord_fixed()
    ggsave(heatmap_file, plot = heatmap_plot, device = "jpeg", width = 8, height = 6)
    
    # Interpretasi korelasi
    cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
    max_cor <- which(cor_matrix == max(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    min_cor <- which(cor_matrix == min(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    
    # Konten Rmd
    rmd_content <- paste(
      "---",
      "title: 'Laporan Eksplorasi Data Lengkap - SEVA'",
      "date: '", Sys.Date(), "'",
      "output:",
      "  pdf_document:",
      "    latex_engine: pdflatex",
      "    keep_tex: true",
      "geometry: margin=1in",
      "header-includes:",
      "  - \\usepackage{booktabs}",
      "  - \\usepackage{longtable}",
      "  - \\usepackage{caption}",
      "  - \\usepackage{pdflscape}",
      "  - \\usepackage[utf8]{inputenc}",
      "  - \\usepackage{geometry}",
      "  - \\geometry{a4paper, margin=1in}",
      "  - \\usepackage{parskip}",
      "  - \\setlength{\\parskip}{0.5em}",
      "---",
      "\n# Eksplorasi Data\n",
      "## Statistik Deskriptif\n",
      "Statistik deskriptif untuk variabel: ", paste(input$desc_vars, collapse = ", "), ".\n",
      "```{r echo=FALSE, results='asis'}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      "pander::pander(desc_summary)",
      "```",
      "\n\\newpage\n",
      "## Visualisasi Data\n",
      "```{r echo=FALSE, fig.cap='Plot Data', out.width='80%'}",
      "knitr::include_graphics('", plot_file, "')",
      "```",
      "\n## Interpretasi Visualisasi\n",
      if(input$plot_type == "hist") {
        paste("Histogram menunjukkan distribusi frekuensi dari variabel ", input$plot_var1,
              ". Bentuk distribusi dapat memberikan informasi tentang normalitas data dan adanya outliers.")
      } else if(input$plot_type == "box") {
        paste("Box plot menampilkan ringkasan lima angka (minimum, Q1, median, Q3, maksimum) ",
              "dari variabel ", input$plot_var1, ". Titik-titik di luar whiskers menunjukkan potensi outliers.")
      } else if(input$plot_type == "scatter") {
        paste("Scatter plot menunjukkan hubungan antara ", input$plot_var1, " dan ", input$plot_var2,
              ". Garis regresi membantu memvisualisasikan tren hubungan linear antara kedua variabel.")
      },
      "\n\\newpage\n",
      "## Peta Geografis\n",
      "Peta berikut menunjukkan distribusi spasial dari variabel ", input$map_var, ".\n",
      "```{r echo=FALSE, fig.cap='Peta Geografis', out.width='80%'}",
      "knitr::include_graphics('", map_file, "')",
      "```",
      "\n## Interpretasi Peta\n",
      "Peta geografis menunjukkan distribusi spasial dari variabel ", input$map_var,
      ". Ukuran dan warna lingkaran menunjukkan nilai variabel, memungkinkan identifikasi pola geografis dan kluster nilai tinggi atau rendah.",
      "\n\\newpage\n",
      "## Heatmap Korelasi\n",
      "```{r echo=FALSE, fig.cap='Heatmap Korelasi', out.width='80%'}",
      "knitr::include_graphics('", heatmap_file, "')",
      "```",
      "\n## Interpretasi Korelasi\n",
      "Korelasi positif terkuat terjadi antara ", rownames(cor_matrix)[max_cor[1]], " dan ",
      colnames(cor_matrix)[max_cor[2]], " dengan nilai ", round(cor_matrix[max_cor], 3), ".\n",
      "Korelasi negatif terkuat terjadi antara ", rownames(cor_matrix)[min_cor[1]], " dan ",
      colnames(cor_matrix)[min_cor[2]], " dengan nilai ", round(cor_matrix[min_cor], 3), ".",
      sep = "\n"
    )
    
    writeLines(rmd_content, temp_md)
    
    tryCatch(
      {
        rmarkdown::render(
          temp_md,
          output_file = file,
          output_format = "pdf_document",
          clean = TRUE,
          envir = new.env(parent = globalenv())
        )
      },
      error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      }
    )
    
  }, contentType = "application/pdf" )
  
  # --- Uji Asumsi Tab ---
  output$download_normality_test_pdf <- downloadHandler(
    filename = function() { paste0("assumption_tests_complete_report_", Sys.Date(), ".docx") },
    content = function(file) {
      req(input$normality_var, values$assumptions_done)
      
      var_data <- sovi_data[[input$normality_var]]
      var_data <- var_data[!is.na(var_data)]
      shapiro_test <- shapiro.test(var_data)
      ks_test <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
      
      # Calculate descriptive statistics for the variable
      desc_stats <- data.frame(
        Statistik = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum", "Skewness", "Kurtosis"),
        Nilai = c(
          round(mean(var_data), 4),
          round(median(var_data), 4), 
          round(sd(var_data), 4),
          round(min(var_data), 4),
          round(max(var_data), 4),
          round(moments::skewness(var_data), 4),
          round(moments::kurtosis(var_data), 4)
        )
      )
      
      # Homogeneity test if applicable
      homogeneity_content <- ""
      if(input$group_var != "None") {
        group_data <- sovi_data[[input$group_var]]
        if(is.numeric(group_data)) {
          group_data <- cut(group_data, breaks = 3, labels = c("Low", "Medium", "High"))
        }
        complete_cases <- complete.cases(var_data, group_data)
        var_data_hom <- var_data[complete_cases]
        group_data_hom <- group_data[complete_cases]
        levene_test <- car::leveneTest(var_data_hom, group_data_hom)
        
        homogeneity_content <- paste(
          "",
          "## Uji Homogenitas Varians",
          "",
          paste("Variabel Kelompok:", input$group_var),
          "",
          "**H0:** Varians antar kelompok homogen",
          "**H1:** Varians antar kelompok tidak homogen",
          "",
          paste("F-statistic:", round(levene_test$`F value`[1], 4)),
          paste("P-value:", round(levene_test$`Pr(>F)`[1], 4)),
          "",
          "### Interpretasi Homogenitas",
          if(levene_test$`Pr(>F)`[1] > 0.05) {
            "Berdasarkan uji Levene, tidak terdapat perbedaan varians yang signifikan antar kelompok (p > 0.05). Asumsi homogenitas varians terpenuhi."
          } else {
            "Berdasarkan uji Levene, terdapat perbedaan varians yang signifikan antar kelompok (p < 0.05). Asumsi homogenitas varians tidak terpenuhi."
          },
          sep = "\n"
        )
      }
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        rmd_content <- paste(
          "---",
          "title: 'Laporan Lengkap Uji Asumsi Statistik'",
          "author: 'SEVA - Socio-Economic Vulnerability Analyzer'",
          "date: '", Sys.Date(), "'",
          "output: word_document",
          "---",
          "",
          "# Laporan Uji Asumsi Statistik",
          "",
          "## Ringkasan Eksekutif",
          "Laporan ini menyajikan hasil uji asumsi statistik yang diperlukan sebelum melakukan analisis inferensia. Uji yang dilakukan meliputi uji normalitas dan uji homogenitas varians.",
          "",
          paste("## Variabel yang Diuji:", input$normality_var),
          "",
          "## Statistik Deskriptif",
          "",
          "```{r echo=FALSE, results='asis'}",
          "library(knitr)",
          "desc_stats <- data.frame(",
          paste("  Statistik = c(", paste(paste0("'", desc_stats$Statistik, "'"), collapse = ", "), "),"),
          paste("  Nilai = c(", paste(desc_stats$Nilai, collapse = ", "), ")"),
          ")",
          "kable(desc_stats, caption = 'Statistik Deskriptif Variabel')",
          "```",
          "",
          "## Uji Normalitas",
          "",
          "### Hipotesis",
          "- **H0:** Data berdistribusi normal",
          "- **H1:** Data tidak berdistribusi normal",
          "",
          "### Shapiro-Wilk Test",
          paste("- W-statistic:", round(shapiro_test$statistic, 4)),
          paste("- P-value:", round(shapiro_test$p.value, 4)),
          "",
          "### Kolmogorov-Smirnov Test",
          paste("- D-statistic:", round(ks_test$statistic, 4)),
          paste("- P-value:", round(ks_test$p.value, 4)),
          "",
          "### Interpretasi Normalitas",
          if(shapiro_test$p.value > 0.05) {
            paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4), "> 0.05), kita gagal menolak H0. Data variabel", input$normality_var, "dapat dianggap berdistribusi normal pada tingkat signifikansi 5%. Hal ini mengindikasikan bahwa asumsi normalitas terpenuhi untuk analisis parametrik.")
          } else {
            paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4), "< 0.05), kita menolak H0. Data variabel", input$normality_var, "tidak berdistribusi normal pada tingkat signifikansi 5%. Disarankan untuk menggunakan transformasi data atau metode non-parametrik.")
          },
          homogeneity_content,
          "",
          "## Kesimpulan dan Rekomendasi",
          "",
          "### Status Asumsi",
          if(shapiro_test$p.value > 0.05) {
            "- ✓ Asumsi normalitas: TERPENUHI"
          } else {
            "- ✗ Asumsi normalitas: TIDAK TERPENUHI"
          },
          if(input$group_var != "None") {
            if(exists("levene_test") && levene_test$`Pr(>F)`[1] > 0.05) {
              "- ✓ Asumsi homogenitas: TERPENUHI"
            } else if(exists("levene_test")) {
              "- ✗ Asumsi homogenitas: TIDAK TERPENUHI"
            }
          },
          "",
          "### Rekomendasi Analisis",
          if(shapiro_test$p.value > 0.05) {
            "Karena asumsi normalitas terpenuhi, dapat dilanjutkan dengan uji statistik parametrik seperti t-test, ANOVA, atau regresi linear."
          } else {
            "Karena asumsi normalitas tidak terpenuhi, disarankan untuk:"
          },
          if(shapiro_test$p.value <= 0.05) {
            paste(
              "1. Melakukan transformasi data (log, square root, dll.)",
              "2. Menggunakan uji non-parametrik (Mann-Whitney, Kruskal-Wallis, dll.)",
              "3. Meningkatkan ukuran sampel jika memungkinkan",
              sep = "\n"
            )
          },
          sep = "\n"
        )
        
        writeLines(rmd_content, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate Word document: ", e$message)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  output$download_qq_plot_jpg <- downloadHandler(
    filename = function() { paste0("qq_plot_", Sys.Date(), ".jpg") },
    content = function(file) {
      req(input$normality_var)
      var_data <- sovi_data[[input$normality_var]]
      plot <- ggplot(data.frame(sample = var_data), aes(sample = sample)) +
        stat_qq() + stat_qq_line() +
        theme_minimal() +
        labs(title = paste("Q-Q Plot for", input$normality_var))
      ggsave(file, plot = plot, device = "jpeg", width = 8, height = 6)
    },
    contentType = "image/jpeg"
  )
  
  output$download_normality_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("normality_interpretation_", Sys.Date(), ".pdf") },
    content = function(file) {
      req(input$normality_var)
      var_data <- sovi_data[[input$normality_var]]
      var_data <- var_data[!is.na(var_data)]
      shapiro_test <- shapiro.test(var_data)
      
      interpretation <- if(shapiro_test$p.value > 0.05) {
        paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
              "), kita gagal menolak H0. Data variabel", input$normality_var, 
              "dapat dianggap berdistribusi normal pada tingkat signifikansi 5%.")
      } else {
        paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
              "), kita menolak H0. Data variabel", input$normality_var, 
              "tidak berdistribusi normal pada tingkat signifikansi 5%.")
      }
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        simple_rmd <- paste(
          "---",
          "title: 'Interpretasi Uji Normalitas'",
          "date: '", Sys.Date(), "'",
          "output: pdf_document",
          "---",
          "",
          "## Interpretasi Uji Normalitas",
          "",
          interpretation,
          sep = "\n"
        )
        writeLines(simple_rmd, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      })
    },
    contentType = "application/pdf"
  )
  
  output$download_homogeneity_test_pdf <- downloadHandler(
    filename = function() { paste0("homogeneity_test_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
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
        
        temp_md <- tempfile(fileext = ".Rmd")
        rmd_content <- paste(
          "---",
          "title: 'Homogeneity Test Results'",
          "date: '", Sys.Date(), "'",
          "output:",
          "  pdf_document:",
          "    latex_engine: pdflatex",
          "    keep_tex: true",
          "geometry: margin=1in",
          "header-includes:",
          "  - \\usepackage{booktabs}",
          "  - \\usepackage{longtable}",
          "  - \\usepackage{caption}",
          "  - \\usepackage[utf8]{inputenc}",
          "  - \\usepackage{geometry}",
          "  - \\geometry{a4paper, margin=1in}",
          "  - \\usepackage{parskip}",
          "  - \\setlength{\\parskip}{0.5em}",
          "---",
          "\n# Uji Homogenitas Varians (Levene's Test)\n",
          "**H0:** Varians antar kelompok homogen\n",
          "**H1:** Varians antar kelompok tidak homogen\n\n",
          "F-statistic: ", round(levene_test$`F value`[1], 4), "\n",
          "P-value: ", round(levene_test$`Pr(>F)`[1], 4), "\n",
          sep = "\n"
        )
        
        writeLines(rmd_content, temp_md)
        
        tryCatch(
          {
            rmarkdown::render(
              temp_md,
              output_file = file,
              output_format = "pdf_document",
              clean = TRUE,
              envir = new.env(parent = globalenv())
            )
          },
          error = function(e) {
            stop("Failed to generate PDF: ", e$message)
          }
        )
      }
    },
    contentType = "application/pdf"
  )
  
  output$download_homogeneity_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("homogeneity_interpretation_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
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
        
        interpretation <- if(p_value > 0.05) {
          paste("Berdasarkan uji Levene (p-value =", round(p_value, 4),
                "), kita gagal menolak H0. Varians antar kelompok dapat dianggap homogen",
                "pada tingkat signifikansi 5%.")
        } else {
          paste("Berdasarkan uji Levene (p-value =", round(p_value, 4),
                "), kita menolak H0. Varians antar kelompok tidak homogen",
                "pada tingkat signifikansi 5%.")
        }
        
        temp_md <- tempfile(fileext = ".Rmd")
        rmd_content <- paste(
          "---",
          "title: 'Homogeneity Test Interpretation'",
          "date: '", Sys.Date(), "'",
          "output:",
          "  pdf_document:",
          "    latex_engine: pdflatex",
          "    keep_tex: true",
          "geometry: margin=1in",
          "header-includes:",
          "  - \\usepackage{booktabs}",
          "  - \\usepackage{longtable}",
          "  - \\usepackage{caption}",
          "  - \\usepackage[utf8]{inputenc}",
          "  - \\usepackage{geometry}",
          "  - \\geometry{a4paper, margin=1in}",
          "  - \\usepackage{parskip}",
          "  - \\setlength{\\parskip}{0.5em}",
          "---",
          "\n# Interpretasi Uji Homogenitas\n",
          interpretation,
          sep = "\n"
        )
        
        writeLines(rmd_content, temp_md)
        
        tryCatch(
          {
            rmarkdown::render(
              temp_md,
              output_file = file,
              output_format = "pdf_document",
              clean = TRUE,
              envir = new.env(parent = globalenv())
            )
          },
          error = function(e) {
            stop("Failed to generate PDF: ", e$message)
          }
        )
      }
    },
    contentType = "application/pdf"
  )
  
  output$download_asumsi_complete <- downloadHandler(
    filename = function() { paste0("asumsi_complete_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
    
    req(input$normality_var, values$assumptions_done)
    temp_md <- tempfile(fileext = ".Rmd")
    
    # Uji normalitas
    var_data <- sovi_data[[input$normality_var]]
    var_data <- var_data[!is.na(var_data)]
    shapiro_test <- shapiro.test(var_data)
    normality_interpretation <- if(shapiro_test$p.value > 0.05) {
      paste("Uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
            "): Data dianggap berdistribusi normal.")
    } else {
      paste("Uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
            "): Data tidak berdistribusi normal.")
    }
    
    # Simpan Q-Q plot
    qq_plot_file <- file.path(tempdir(), "qq_plot.jpg")
    qq_plot <- ggplot(data.frame(sample = var_data), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = paste("Q-Q Plot untuk", input$normality_var))
    ggsave(qq_plot_file, plot = qq_plot, device = "jpeg", width = 8, height = 6)
    
    # Uji homogenitas (jika group_var dipilih)
    homogeneity_text <- ""
    if(input$group_var != "None") {
      group_data <- sovi_data[[input$group_var]]
      if(is.numeric(group_data)) {
        group_data <- cut(group_data, breaks = 3, labels = c("Rendah", "Sedang", "Tinggi"))
      }
      complete_cases <- complete.cases(var_data, group_data)
      var_data <- var_data[complete_cases]
      group_data <- group_data[complete_cases]
      levene_test <- car::leveneTest(var_data, group_data)
      p_value <- levene_test$`Pr(>F)`[1]
      homogeneity_text <- paste(
        "\n## Uji Homogenitas Varians\n",
        "Uji Levene (p-value =", round(p_value, 4), "): ",
        if(p_value > 0.05) {
          "Varians antar kelompok dianggap homogen."
        } else {
          "Varians antar kelompok tidak homogen."
        }
      )
    }
    
    # Konten Rmd
    rmd_content <- paste(
      "---",
      "title: 'Laporan Uji Asumsi'",
      "date: '", Sys.Date(), "'",
      "output:",
      "  pdf_document:",
      "    latex_engine: pdflatex",
      "    keep_tex: true",
      "geometry: margin=1in",
      "header-includes:",
      "  - \\usepackage{booktabs}",
      "  - \\usepackage{longtable}",
      "  - \\usepackage{caption}",
      "  - \\usepackage[utf8]{inputenc}",
      "  - \\usepackage{geometry}",
      "  - \\geometry{a4paper, margin=1in}",
      "  - \\usepackage{parskip}",
      "  - \\setlength{\\parskip}{0.5em}",
      "---",
      "\n# Uji Asumsi Statistik\n",
      "## Uji Normalitas\n",
      "Variabel: ", input$normality_var, "\n",
      normality_interpretation, "\n",
      "```{r echo=FALSE, fig.cap='Q-Q Plot', out.width='80%'}",
      "knitr::include_graphics('", qq_plot_file, "')",
      "```",
      homogeneity_text,
      sep = "\n"
    )
    
    writeLines(rmd_content, temp_md)
    
    tryCatch(
      {
        rmarkdown::render(
          temp_md,
          output_file = file,
          output_format = "pdf_document",
          clean = TRUE,
          envir = new.env(parent = globalenv())
        )
      },
      error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      }
    )
    
  }, contentType = "application/pdf" 
  )
  
  # --- Statistik Inferensia Tab ---
  output$download_ttest1_pdf <- downloadHandler(
    filename = function() { paste0("inferential_statistics_complete_report_", Sys.Date(), ".docx") },
    content = function(file) {
      # Collect all statistical tests that have been performed
      tests_performed <- list()
      
      # One-sample t-test
      if(values$ttest1_done && !is.null(input$ttest1_var) && !is.null(input$ttest1_mu)) {
        var_data <- sovi_data[[input$ttest1_var]]
        var_data <- var_data[!is.na(var_data)]
        ttest_result <- t.test(var_data, mu = input$ttest1_mu)
        
        tests_performed$ttest1 <- list(
          title = "One-Sample T-Test",
          variable = input$ttest1_var,
          hypothesis_value = input$ttest1_mu,
          result = ttest_result,
          interpretation = if(ttest_result$p.value < 0.05) {
            paste("Terdapat perbedaan signifikan antara rata-rata sampel (", round(ttest_result$estimate, 4), ") dengan nilai hipotesis (", input$ttest1_mu, ") pada tingkat signifikansi 5%.")
          } else {
            paste("Tidak terdapat perbedaan signifikan antara rata-rata sampel (", round(ttest_result$estimate, 4), ") dengan nilai hipotesis (", input$ttest1_mu, ") pada tingkat signifikansi 5%.")
          }
        )
      }
      
      # Two-sample t-test  
      if(values$ttest2_done && !is.null(input$ttest2_var) && !is.null(input$ttest2_group)) {
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
          
          tests_performed$ttest2 <- list(
            title = "Two-Sample Independent T-Test",
            variable = input$ttest2_var,
            group_var = input$ttest2_group,
            groups = unique_groups,
            result = ttest_result,
            interpretation = if(ttest_result$p.value < 0.05) {
              paste("Terdapat perbedaan rata-rata yang signifikan antara kelompok", unique_groups[1], "dan", unique_groups[2], "pada tingkat signifikansi 5%.")
            } else {
              paste("Tidak terdapat perbedaan rata-rata yang signifikan antara kelompok", unique_groups[1], "dan", unique_groups[2], "pada tingkat signifikansi 5%.")
            }
          )
        }
      }
      
      # ANOVA tests
      if(values$anova1_done && !is.null(input$anova1_dep) && !is.null(input$anova1_indep)) {
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
        
        tests_performed$anova1 <- list(
          title = "One-Way ANOVA",
          dependent = input$anova1_dep,
          independent = input$anova1_indep,
          result = anova_summary,
          interpretation = if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
            paste("Terdapat perbedaan rata-rata yang signifikan antar kelompok", input$anova1_indep, "pada tingkat signifikansi 5%.")
          } else {
            paste("Tidak terdapat perbedaan rata-rata yang signifikan antar kelompok", input$anova1_indep, "pada tingkat signifikansi 5%.")
          }
        )
      }
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        
        # Build content based on tests performed
        content_sections <- c(
          "---",
          "title: 'Laporan Lengkap Statistik Inferensia'", 
          "author: 'SEVA - Socio-Economic Vulnerability Analyzer'",
          "date: '", Sys.Date(), "'",
          "output: word_document",
          "---",
          "",
          "# Laporan Statistik Inferensia",
          "",
          "## Ringkasan Eksekutif",
          "Laporan ini menyajikan hasil analisis statistik inferensia yang dilakukan untuk menguji hipotesis penelitian menggunakan berbagai uji statistik.",
          "",
          paste("## Total Uji yang Dilakukan:", length(tests_performed)),
          ""
        )
        
        # Add each test result
        for(test_name in names(tests_performed)) {
          test <- tests_performed[[test_name]]
          
          if(test_name == "ttest1") {
            content_sections <- c(content_sections,
              paste("## ", test$title),
              "",
              paste("**Variabel:** ", test$variable),
              paste("**Nilai Hipotesis:** ", test$hypothesis_value),
              "",
              "### Hipotesis",
              paste("- H0: μ =", test$hypothesis_value),
              paste("- H1: μ ≠", test$hypothesis_value),
              "",
              "### Hasil Uji",
              paste("- T-statistic:", round(test$result$statistic, 4)),
              paste("- Degrees of freedom:", test$result$parameter),
              paste("- P-value:", round(test$result$p.value, 4)),
              paste("- 95% Confidence Interval: [", round(test$result$conf.int[1], 4), ",", round(test$result$conf.int[2], 4), "]"),
              paste("- Sample mean:", round(test$result$estimate, 4)),
              "",
              "### Interpretasi",
              test$interpretation,
              ""
            )
          } else if(test_name == "ttest2") {
            content_sections <- c(content_sections,
              paste("## ", test$title),
              "",
              paste("**Variabel:** ", test$variable),
              paste("**Kelompok:** ", test$group_var),
              "",
              "### Hipotesis",
              "- H0: μ1 = μ2 (tidak ada perbedaan rata-rata antar kelompok)",
              "- H1: μ1 ≠ μ2 (ada perbedaan rata-rata antar kelompok)",
              "",
              "### Hasil Uji",
              paste("- T-statistic:", round(test$result$statistic, 4)),
              paste("- Degrees of freedom:", round(test$result$parameter, 2)),
              paste("- P-value:", round(test$result$p.value, 4)),
              paste("- 95% Confidence Interval: [", round(test$result$conf.int[1], 4), ",", round(test$result$conf.int[2], 4), "]"),
              "",
              "### Interpretasi",
              test$interpretation,
              ""
            )
          } else if(test_name == "anova1") {
            content_sections <- c(content_sections,
              paste("## ", test$title),
              "",
              paste("**Variabel Dependen:** ", test$dependent),
              paste("**Variabel Independen:** ", test$independent),
              "",
              "### Hipotesis",
              "- H0: μ1 = μ2 = μ3 = ... (semua rata-rata kelompok sama)",
              "- H1: Minimal ada satu rata-rata kelompok yang berbeda",
              "",
              "### Hasil Uji",
              paste("- F-statistic:", round(test$result[[1]]$`F value`[1], 4)),
              paste("- Degrees of freedom:", test$result[[1]]$Df[1], ",", test$result[[1]]$Df[2]),
              paste("- P-value:", round(test$result[[1]]$`Pr(>F)`[1], 4)),
              "",
              "### Interpretasi",
              test$interpretation,
              ""
            )
          }
        }
        
        # Add conclusion
        content_sections <- c(content_sections,
          "## Kesimpulan Umum",
          "",
          if(length(tests_performed) > 0) {
            significant_tests <- sapply(tests_performed, function(test) {
              if(test$title == "One-Sample T-Test" || test$title == "Two-Sample Independent T-Test") {
                test$result$p.value < 0.05
              } else if(test$title == "One-Way ANOVA") {
                test$result[[1]]$`Pr(>F)`[1] < 0.05
              }
            })
            paste("Dari", length(tests_performed), "uji statistik yang dilakukan,", sum(significant_tests), "uji menunjukkan hasil yang signifikan pada tingkat α = 0.05.")
          } else {
            "Tidak ada uji statistik yang berhasil dilakukan. Pastikan untuk menjalankan uji statistik terlebih dahulu."
          },
          "",
          "### Rekomendasi",
          "Berdasarkan hasil uji statistik, disarankan untuk:",
          "1. Mempertimbangkan temuan signifikan dalam konteks penelitian",
          "2. Melakukan analisis lanjutan jika diperlukan",
          "3. Memvalidasi hasil dengan data tambahan jika memungkinkan"
        )
        
        rmd_content <- paste(content_sections, collapse = "\n")
        writeLines(rmd_content, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate Word document: ", e$message)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  output$download_ttest2_pdf <- downloadHandler(
    filename = function() { paste0("ttest2_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(input$ttest2_var, input$ttest2_group, values$ttest2_done)
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
        
        temp_md <- tempfile(fileext = ".Rmd")
        rmd_content <- paste(
          "---",
          "title: 'Two-Sample T-Test Results'",
          "date: '", Sys.Date(), "'",
          "output:",
          "  pdf_document:",
          "    latex_engine: pdflatex",
          "    keep_tex: true",
          "geometry: margin=1in",
          "header-includes:",
          "  - \\usepackage{booktabs}",
          "  - \\usepackage{longtable}",
          "  - \\usepackage{caption}",
          "  - \\usepackage[utf8]{inputenc}",
          "  - \\usepackage{geometry}",
          "  - \\geometry{a4paper, margin=1in}",
          "  - \\usepackage{parskip}",
          "  - \\setlength{\\parskip}{0.5em}",
          "---",
          "\n# Two-Sample Independent T-Test\n",
          "Variable: ", input$ttest2_var, "\n",
          "Groups: ", input$ttest2_group, "\n\n",
          "**H0:** μ1 = μ2\n",
          "**H1:** μ1 ≠ μ2\n\n",
          "Group 1 (", unique_groups[1], "): ", length(group1_data), " observations\n",
          "Group 2 (", unique_groups[2], "): ", length(group2_data), " observations\n\n",
          "T-statistic: ", round(ttest_result$statistic, 4), "\n",
          "P-value: ", round(ttest_result$p.value, 4), "\n",
          "Confidence Interval: [", round(ttest_result$conf.int[1], 4), ", ", round(ttest_result$conf.int[2], 4), "]\n",
          sep = "\n"
        )
        
        writeLines(rmd_content, temp_md)
        
        tryCatch(
          {
            rmarkdown::render(
              temp_md,
              output_file = file,
              output_format = "pdf_document",
              clean = TRUE,
              envir = new.env(parent = globalenv())
            )
          },
          error = function(e) {
            stop("Failed to generate PDF: ", e$message)
          }
        )
      }
    },
    contentType = "application/pdf"
  )
  
  output$download_prop1_pdf <- downloadHandler(
    filename = function() { paste0("prop1_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(input$prop1_var, input$prop1_p, values$prop1_done)
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
      
      temp_md <- tempfile(fileext = ".Rmd")
      rmd_content <- paste(
        "---",
        "title: 'One-Sample Proportion Test Results'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# One-Sample Proportion Test\n",
        "Variable: ", input$prop1_var, "\n",
        "Hypothesized proportion: ", input$prop1_p, "\n\n",
        "**H0:** p = ", input$prop1_p, "\n",
        "**H1:** p ≠ ", input$prop1_p, "\n\n",
        "Sample proportion: ", round(successes/n, 4), "\n",
        "Sample size: ", n, "\n",
        "Successes: ", successes, "\n",
        "Chi-squared statistic: ", round(prop_test$statistic, 4), "\n",
        "P-value: ", round(prop_test$p.value, 4), "\n",
        sep = "\n"
      )
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  output$download_var1_pdf <- downloadHandler(
    filename = function() { paste0("var1_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(input$var1_var, input$var1_sigma, values$var1_done)
      var_data <- sovi_data[[input$var1_var]]
      var_data <- var_data[!is.na(var_data)]
      
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$var1_sigma
      p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
      
      temp_md <- tempfile(fileext = ".Rmd")
      rmd_content <- paste(
        "---",
        "title: 'One-Sample Variance Test Results'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# One-Sample Variance Test (Chi-square)\n",
        "Variable: ", input$var1_var, "\n",
        "Hypothesized variance: ", input$var1_sigma, "\n\n",
        "**H0:** σ² = ", input$var1_sigma, "\n",
        "**H1:** σ² ≠ ", input$var1_sigma, "\n\n",
        "Sample variance: ", round(sample_var, 4), "\n",
        "Sample size: ", n, "\n",
        "Chi-square statistic: ", round(chi_stat, 4), "\n",
        "Degrees of freedom: ", n-1, "\n",
        "P-value: ", round(p_value, 4), "\n",
        sep = "\n"
      )
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  output$download_anova1_pdf <- downloadHandler(
    filename = function() { paste0("anova1_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(input$anova1_dep, input$anova1_indep, values$anova1_done)
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
      
      temp_md <- tempfile(fileext = ".Rmd")
      rmd_content <- paste(
        "---",
        "title: 'One-Way ANOVA Results'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# One-Way ANOVA\n",
        "Dependent Variable: ", input$anova1_dep, "\n",
        "Independent Variable: ", input$anova1_indep, "\n\n",
        "**H0:** μ1 = μ2 = μ3 = ... (all group means are equal)\n",
        "**H1:** At least one group mean is different\n\n",
        "F-statistic: ", round(anova_summary[[1]]$`F value`[1], 4), "\n",
        "P-value: ", round(anova_summary[[1]]$`Pr(>F)`[1], 4), "\n",
        sep = "\n"
      )
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  output$download_anova2_pdf <- downloadHandler(
    filename = function() { paste0("anova2_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(input$anova2_dep, input$anova2_indep1, input$anova2_indep2, values$anova2_done)
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
      
      temp_md <- tempfile(fileext = ".Rmd")
      rmd_content <- paste(
        "---",
        "title: 'Two-Way ANOVA Results'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# Two-Way ANOVA\n",
        "Dependent Variable: ", input$anova2_dep, "\n",
        "Independent Variables: ", input$anova2_indep1, ", ", input$anova2_indep2, "\n\n",
        "Testing main effects and interaction effect\n\n",
        "Main effect ", input$anova2_indep1, " F-statistic: ", round(anova_summary[[1]]$`F value`[1], 4), "\n",
        "Main effect ", input$anova2_indep1, " P-value: ", round(anova_summary[[1]]$`Pr(>F)`[1], 4), "\n\n",
        "Main effect ", input$anova2_indep2, " F-statistic: ", round(anova_summary[[1]]$`F value`[2], 4), "\n",
        "Main effect ", input$anova2_indep2, " P-value: ", round(anova_summary[[1]]$`Pr(>F)`[2], 4), "\n\n",
        "Interaction effect F-statistic: ", round(anova_summary[[1]]$`F value`[3], 4), "\n",
        "Interaction effect P-value: ", round(anova_summary[[1]]$`Pr(>F)`[3], 4), "\n",
        sep = "\n"
      )
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  output$download_inferensia_complete <- downloadHandler(
    filename = function() { paste0("inferensia_complete_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
    
    temp_md <- tempfile(fileext = ".Rmd")
    rmd_content <- paste(
      "---",
      "title: 'Laporan Statistik Inferensia'",
      "date: '", Sys.Date(), "'",
      "output:",
      "  pdf_document:",
      "    latex_engine: pdflatex",
      "    keep_tex: true",
      "geometry: margin=1in",
      "header-includes:",
      "  - \\usepackage{booktabs}",
      "  - \\usepackage{longtable}",
      "  - \\usepackage{caption}",
      "  - \\usepackage[utf8]{inputenc}",
      "  - \\usepackage{geometry}",
      "  - \\geometry{a4paper, margin=1in}",
      "  - \\usepackage{parskip}",
      "  - \\setlength{\\parskip}{0.5em}",
      "---",
      "\n# Statistik Inferensia\n",
      sep = "\n"
    )
    
    # Tambahkan hasil uji statistik yang telah dilakukan
    if(values$ttest1_done) {
      req(input$ttest1_var, input$ttest1_mu)
      var_data <- sovi_data[[input$ttest1_var]]
      var_data <- var_data[!is.na(var_data)]
      ttest_result <- t.test(var_data, mu = input$ttest1_mu)
      ttest1_interpretation <- if(ttest_result$p.value < 0.05) {
        paste("Dengan p-value =", round(ttest_result$p.value, 4),
              "< 0.05, kita menolak H0. Terdapat perbedaan signifikan antara rata-rata sampel",
              "dengan nilai hipotesis", input$ttest1_mu, ".")
      } else {
        paste("Dengan p-value =", round(ttest_result$p.value, 4),
              "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan antara rata-rata sampel",
              "dengan nilai hipotesis", input$ttest1_mu, ".")
      }
      rmd_content <- paste(rmd_content,
                           "## One-Sample T-Test\n",
                           "Variabel: ", input$ttest1_var, "\n",
                           "Hipotesis Nol: μ =", input$ttest1_mu, "\n",
                           "P-value: ", round(ttest_result$p.value, 4), "\n",
                           ttest1_interpretation, "\n",
                           sep = "\n")
    }
    
    if(values$ttest2_done) {
      req(input$ttest2_var, input$ttest2_group)
      var_data <- sovi_data[[input$ttest2_var]]
      group_data <- sovi_data[[input$ttest2_group]]
      if(is.numeric(group_data)) {
        median_val <- median(group_data, na.rm = TRUE)
        group_data <- ifelse(group_data <= median_val, "Rendah", "Tinggi")
      }
      unique_groups <- unique(group_data[!is.na(group_data)])
      if(length(unique_groups) >= 2) {
        group1_data <- var_data[group_data == unique_groups[1] & !is.na(var_data) & !is.na(group_data)]
        group2_data <- var_data[group_data == unique_groups[2] & !is.na(var_data) & !is.na(group_data)]
        ttest_result <- t.test(group1_data, group2_data)
        ttest2_interpretation <- if(ttest_result$p.value < 0.05) {
          paste("Dengan p-value =", round(ttest_result$p.value, 4),
                "< 0.05, kita menolak H0. Terdapat perbedaan signifikan rata-rata",
                input$ttest2_var, "antara kelompok", unique_groups[1], "dan", unique_groups[2], ".")
        } else {
          paste("Dengan p-value =", round(ttest_result$p.value, 4),
                "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan rata-rata",
                input$ttest2_var, "antara kelompok", unique_groups[1], "dan", unique_groups[2], ".")
        }
        rmd_content <- paste(rmd_content,
                             "## Two-Sample T-Test\n",
                             "Variabel: ", input$ttest2_var, "\nKelompok: ", input$ttest2_group, "\n",
                             "P-value: ", round(ttest_result$p.value, 4), "\n",
                             ttest2_interpretation, "\n",
                             sep = "\n")
      }
    }
    
    if(values$prop1_done) {
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
      prop1_interpretation <- if(prop_test$p.value < 0.05) {
        paste("Dengan p-value =", round(prop_test$p.value, 4),
              "< 0.05, kita menolak H0. Proporsi sampel berbeda signifikan dengan",
              "proporsi hipotesis", input$prop1_p, ".")
      } else {
        paste("Dengan p-value =", round(prop_test$p.value, 4),
              "> 0.05, kita gagal menolak H0. Proporsi sampel tidak berbeda signifikan dengan",
              "proporsi hipotesis", input$prop1_p, ".")
      }
      rmd_content <- paste(rmd_content,
                           "## One-Sample Proportion Test\n",
                           "Variabel: ", input$prop1_var, "\n",
                           "Proporsi Hipotesis: ", input$prop1_p, "\n",
                           "P-value: ", round(prop_test$p.value, 4), "\n",
                           prop1_interpretation, "\n",
                           sep = "\n")
    }
    
    if(values$var1_done) {
      req(input$var1_var, input$var1_sigma)
      var_data <- sovi_data[[input$var1_var]]
      var_data <- var_data[!is.na(var_data)]
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$var1_sigma
      p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
      var1_interpretation <- if(p_value < 0.05) {
        paste("Dengan p-value =", round(p_value, 4),
              "< 0.05, kita menolak H0. Varians sampel berbeda signifikan dengan",
              "varians hipotesis", input$var1_sigma, ".")
      } else {
        paste("Dengan p-value =", round(p_value, 4),
              "> 0.05, kita gagal menolak H0. Varians sampel tidak berbeda signifikan dengan",
              "varians hipotesis", input$var1_sigma, ".")
      }
      rmd_content <- paste(rmd_content,
                           "## One-Sample Variance Test\n",
                           "Variabel: ", input$var1_var, "\n",
                           "Varians Hipotesis: ", input$var1_sigma, "\n",
                           "P-value: ", round(p_value, 4), "\n",
                           var1_interpretation, "\n",
                           sep = "\n")
    }
    
    if(values$anova1_done) {
      req(input$anova1_dep, input$anova1_indep)
      dep_var <- sovi_data[[input$anova1_dep]]
      indep_var <- sovi_data[[input$anova1_indep]]
      if(is.numeric(indep_var)) {
        indep_var <- cut(indep_var, breaks = 3, labels = c("Rendah", "Sedang", "Tinggi"))
      }
      complete_cases <- complete.cases(dep_var, indep_var)
      dep_var <- dep_var[complete_cases]
      indep_var <- indep_var[complete_cases]
      anova_result <- aov(dep_var ~ indep_var)
      anova_summary <- summary(anova_result)
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      anova1_interpretation <- if(p_value < 0.05) {
        paste("Dengan p-value =", round(p_value, 4),
              "< 0.05, kita menolak H0. Terdapat perbedaan signifikan rata-rata",
              input$anova1_dep, "antar kelompok", input$anova1_indep, ".")
      } else {
        paste("Dengan p-value =", round(p_value, 4),
              "> 0.05, kita gagal menolak H0. Tidak terdapat perbedaan signifikan rata-rata",
              input$anova1_dep, "antar kelompok", input$anova1_indep, ".")
      }
      rmd_content <- paste(rmd_content,
                           "## One-Way ANOVA\n",
                           "Variabel Dependen: ", input$anova1_dep, "\n",
                           "Variabel Independen: ", input$anova1_indep, "\n",
                           "P-value: ", round(p_value, 4), "\n",
                           anova1_interpretation, "\n",
                           sep = "\n")
    }
    
    if(values$anova2_done) {
      req(input$anova2_dep, input$anova2_indep1, input$anova2_indep2)
      dep_var <- sovi_data[[input$anova2_dep]]
      indep_var1 <- sovi_data[[input$anova2_indep1]]
      indep_var2 <- sovi_data[[input$anova2_indep2]]
      if(is.numeric(indep_var1)) {
        indep_var1 <- cut(indep_var1, breaks = 2, labels = c("Rendah", "Tinggi"))
      }
      if(is.numeric(indep_var2)) {
        indep_var2 <- cut(indep_var2, breaks = 2, labels = c("Rendah", "Tinggi"))
      }
      complete_cases <- complete.cases(dep_var, indep_var1, indep_var2)
      dep_var <- dep_var[complete_cases]
      indep_var1 <- indep_var1[complete_cases]
      indep_var2 <- indep_var2[complete_cases]
      anova_result <- aov(dep_var ~ indep_var1 * indep_var2)
      anova_summary <- summary(anova_result)
      p_values <- anova_summary[[1]]$`Pr(>F)`
      anova2_interpretation <- paste(
        "Efek utama ", input$anova2_indep1, ": p-value =", round(p_values[1], 4),
        ifelse(p_values[1] < 0.05, " (signifikan)", " (tidak signifikan)"), "\n",
        "Efek utama ", input$anova2_indep2, ": p-value =", round(p_values[2], 4),
        ifelse(p_values[2] < 0.05, " (signifikan)", " (tidak signifikan)"), "\n",
        "Efek interaksi: p-value =", round(p_values[3], 4),
        ifelse(p_values[3] < 0.05, " (signifikan)", " (tidak signifikan)")
      )
      rmd_content <- paste(rmd_content,
                           "## Two-Way ANOVA\n",
                           "Variabel Dependen: ", input$anova2_dep, "\n",
                           "Variabel Independen: ", input$anova2_indep1, ", ", input$anova2_indep2, "\n",
                           anova2_interpretation, "\n",
                           sep = "\n")
    }
    
    writeLines(rmd_content, temp_md)
    
    tryCatch(
      {
        rmarkdown::render(
          temp_md,
          output_file = file,
          output_format = "pdf_document",
          clean = TRUE,
          envir = new.env(parent = globalenv())
        )
      },
      error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      }
    )
    
  }, contentType = "application/pdf" )
  
  # --- Regresi Linear Berganda Tab ---
  output$download_regression_summary_pdf <- downloadHandler(
    filename = function() { paste0("regression_complete_report_", Sys.Date(), ".docx") },
    content = function(file) {
      req(values$regression_done)
      model <- regression_model()
      model_summary <- summary(model)
      
      # Get regression statistics
      r_squared <- model_summary$r.squared
      adj_r_squared <- model_summary$adj.r.squared
      f_statistic <- model_summary$fstatistic[1]
      f_p_value <- pf(f_statistic, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
      
      # Get coefficients
      coefficients <- model_summary$coefficients
      significant_vars <- rownames(coefficients)[coefficients[, "Pr(>|t|)"] < 0.05]
      
      # VIF test for multicollinearity
      vif_content <- ""
      if(length(input$reg_indep) > 1) {
        vif_result <- car::vif(model)
        vif_content <- paste(
          "## Uji Multikolinearitas (VIF)",
          "",
          "```{r echo=FALSE, results='asis'}",
          "library(knitr)",
          "vif_data <- data.frame(",
          paste("  Variable = c(", paste(paste0("'", names(vif_result), "'"), collapse = ", "), "),"),
          paste("  VIF = c(", paste(round(vif_result, 4), collapse = ", "), ")"),
          ")",
          "kable(vif_data, caption = 'Variance Inflation Factor (VIF)')",
          "```",
          "",
          "### Interpretasi VIF",
          if(any(vif_result > 10)) {
            paste("⚠️ **Peringatan:** Terdeteksi multikolinearitas tinggi (VIF > 10) pada variabel:", paste(names(vif_result[vif_result > 10]), collapse = ", "))
          } else if(any(vif_result > 5)) {
            paste("⚠️ **Perhatian:** Terdeteksi multikolinearitas sedang (VIF > 5) pada variabel:", paste(names(vif_result[vif_result > 5]), collapse = ", "))
          } else {
            "✅ **Baik:** Tidak ada masalah multikolinearitas yang serius (semua VIF < 5)"
          },
          "",
          sep = "\n"
        )
      } else {
        vif_content <- paste(
          "## Uji Multikolinearitas",
          "Tidak dilakukan karena hanya menggunakan satu variabel independen.",
          "",
          sep = "\n"
        )
      }
      
      # Assumption tests
      residuals <- resid(model)
      shapiro_test <- shapiro.test(residuals)
      bp_test <- lmtest::bptest(model)
      
      tryCatch({
        temp_rmd <- tempfile(fileext = ".Rmd")
        rmd_content <- paste(
          "---",
          "title: 'Laporan Lengkap Analisis Regresi Linear Berganda'",
          "author: 'SEVA - Socio-Economic Vulnerability Analyzer'",
          "date: '", Sys.Date(), "'",
          "output: word_document",
          "---",
          "",
          "# Laporan Analisis Regresi Linear Berganda",
          "",
          "## Ringkasan Eksekutif",
          "Laporan ini menyajikan hasil analisis regresi linear berganda lengkap termasuk interpretasi koefisien, uji signifikansi, dan pemeriksaan asumsi model.",
          "",
          "## Spesifikasi Model",
          "",
          paste("**Variabel Dependen:** ", input$reg_dep),
          paste("**Variabel Independen:** ", paste(input$reg_indep, collapse = ", ")),
          paste("**Jumlah Observasi:** ", nobs(model)),
          "",
          "### Persamaan Regresi",
          paste(input$reg_dep, "= β₀ +", paste(paste("β", 1:length(input$reg_indep), "×", input$reg_indep, sep = ""), collapse = " + "), "+ ε"),
          "",
          "## Hasil Estimasi Model",
          "",
          "### Koefisien Regresi",
          "",
          "```{r echo=FALSE, results='asis'}",
          "library(knitr)",
          "coef_data <- data.frame(",
          paste("  Variable = c(", paste(paste0("'", rownames(coefficients), "'"), collapse = ", "), "),"),
          paste("  Estimate = c(", paste(round(coefficients[, "Estimate"], 4), collapse = ", "), "),"),
          paste("  Std_Error = c(", paste(round(coefficients[, "Std. Error"], 4), collapse = ", "), "),"),
          paste("  t_value = c(", paste(round(coefficients[, "t value"], 4), collapse = ", "), "),"),
          paste("  p_value = c(", paste(round(coefficients[, "Pr(>|t|)"], 4), collapse = ", "), "),"),
          paste("  Significant = c(", paste(paste0("'", ifelse(coefficients[, "Pr(>|t|)"] < 0.05, "***", 
                                                            ifelse(coefficients[, "Pr(>|t|)"] < 0.01, "**",
                                                                  ifelse(coefficients[, "Pr(>|t|)"] < 0.1, "*", ""))), "'"), collapse = ", "), ")"),
          ")",
          "kable(coef_data, caption = 'Koefisien Regresi dan Signifikansi')",
          "```",
          "",
          "**Catatan:** *** p < 0.001, ** p < 0.01, * p < 0.05",
          "",
          "### Goodness of Fit",
          "",
          paste("- **R-squared:** ", round(r_squared, 4), " (", round(r_squared * 100, 2), "% variabilitas dijelaskan)"),
          paste("- **Adjusted R-squared:** ", round(adj_r_squared, 4)),
          paste("- **F-statistic:** ", round(f_statistic, 4)),
          paste("- **F test p-value:** ", round(f_p_value, 4), if(f_p_value < 0.05) " (Model signifikan)" else " (Model tidak signifikan)"),
          "",
          "## Interpretasi Koefisien",
          "",
          if(length(significant_vars) > 0) {
            paste("### Variabel Signifikan (p < 0.05)")
          } else {
            "### Tidak Ada Variabel yang Signifikan"
          },
          "",
          if(length(significant_vars) > 0) {
            paste(sapply(significant_vars[-1], function(var) { # exclude intercept
              coef_val <- coefficients[var, "Estimate"]
              p_val <- coefficients[var, "Pr(>|t|)"]
              paste("**", var, ":** Setiap peningkatan satu unit", var, 
                    ifelse(coef_val > 0, "meningkatkan", "menurunkan"), 
                    input$reg_dep, "sebesar", round(abs(coef_val), 4), "unit (p =", round(p_val, 4), ")")
            }), collapse = "\n\n")
          } else {
            "Tidak ada variabel independen yang memiliki pengaruh signifikan terhadap variabel dependen."
          },
          "",
          vif_content,
          "",
          "## Uji Asumsi Model",
          "",
          "### Normalitas Residual",
          paste("- **Shapiro-Wilk test:** W =", round(shapiro_test$statistic, 4), ", p-value =", round(shapiro_test$p.value, 4)),
          if(shapiro_test$p.value > 0.05) {
            "- ✅ **Interpretasi:** Residual berdistribusi normal (asumsi terpenuhi)"
          } else {
            "- ❌ **Interpretasi:** Residual tidak berdistribusi normal (asumsi dilanggar)"
          },
          "",
          "### Homoskedastisitas",
          paste("- **Breusch-Pagan test:** BP =", round(bp_test$statistic, 4), ", p-value =", round(bp_test$p.value, 4)),
          if(bp_test$p.value > 0.05) {
            "- ✅ **Interpretasi:** Varians residual homogen (asumsi terpenuhi)"
          } else {
            "- ❌ **Interpretasi:** Terjadi heteroskedastisitas (asumsi dilanggar)"
          },
          "",
          "## Kesimpulan",
          "",
          "### Signifikansi Model",
          if(f_p_value < 0.05) {
            paste("Model regresi secara keseluruhan **signifikan** (F =", round(f_statistic, 4), ", p <", ifelse(f_p_value < 0.001, "0.001", round(f_p_value, 3)), "). Model dapat menjelaskan", round(r_squared * 100, 2), "% variabilitas dalam", input$reg_dep, ".")
          } else {
            paste("Model regresi secara keseluruhan **tidak signifikan** (F =", round(f_statistic, 4), ", p =", round(f_p_value, 4), "). Model tidak dapat menjelaskan variabilitas dalam", input$reg_dep, "secara bermakna.")
          },
          "",
          "### Rekomendasi",
          if(f_p_value < 0.05 && length(significant_vars) > 1) {
            "1. Model dapat digunakan untuk prediksi dengan memperhatikan variabel signifikan\n2. Lakukan validasi model dengan data baru jika memungkinkan\n3. Pertimbangkan transformasi jika asumsi dilanggar"
          } else {
            "1. Pertimbangkan penambahan variabel independen lain\n2. Evaluasi spesifikasi model (transformasi, interaksi)\n3. Cek outliers dan influential observations"
          },
          sep = "\n"
        )
        
        writeLines(rmd_content, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        stop("Failed to generate Word document: ", e$message)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  output$download_regression_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("regression_interpretation_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(values$regression_done)
      model <- regression_model()
      model_summary <- summary(model)
      r_squared <- model_summary$r.squared
      adj_r_squared <- model_summary$adj.r.squared
      p_value <- broom::glance(model)$p.value
      significant_coeffs <- model_summary$coefficients
      significant_vars <- names(which(significant_coeffs[, "Pr(>|t|)"] < 0.05))
      
      interpretation <- paste(
        "R-squared:", round(r_squared, 4), "menunjukkan bahwa", round(r_squared * 100, 2),
        "% variabilitas dalam", input$reg_dep, "dapat dijelaskan oleh variabel independen.",
        "Adjusted R-squared:", round(adj_r_squared, 4), "menyesuaikan R-squared untuk jumlah prediktor.",
        "P-value model keseluruhan:", round(p_value, 4),
        ifelse(p_value < 0.05, " (signifikan)", " (tidak signifikan)"),
        "pada tingkat signifikansi 5%.",
        "Koefisien yang signifikan (p < 0.05): ",
        paste(significant_vars, collapse = ", ")
      )
      
      temp_md <- tempfile(fileext = ".Rmd")
      rmd_content <- paste(
        "---",
        "title: 'Regression Interpretation'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# Regression Interpretation\n",
        interpretation,
        sep = "\n"
      )
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  output$download_vif_test_pdf <- downloadHandler(
    filename = function() { paste0("vif_test_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(values$regression_done)
      model <- regression_model()
      
      temp_md <- tempfile(fileext = ".Rmd")
      rmd_content <- paste(
        "---",
        "title: 'VIF Test Results'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# Variance Inflation Factor (VIF) Test\n",
        sep = "\n"
      )
      
      if(length(input$reg_indep) > 1) {
        vif_result <- car::vif(model)
        vif_text <- paste("VIF Values:\n", paste(names(vif_result), ":", round(vif_result, 4), collapse = "\n"), "\n")
        rmd_content <- paste(rmd_content, vif_text, sep = "\n")
      } else {
        rmd_content <- paste(rmd_content, "VIF test cannot be performed with only one independent variable.", sep = "\n")
      }
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  output$download_residual_qq_jpg <- downloadHandler(
    filename = function() { paste0("residual_qq_", Sys.Date(), ".jpg") },
    content = function(file) {
      req(values$regression_done)
      model <- regression_model()
      plot <- ggplot(data.frame(sample = resid(model)), aes(sample = sample)) +
        stat_qq() +
        stat_qq_line() +
        theme_minimal() +
        labs(title = "Q-Q Plot of Residuals")
      ggsave(file, plot = plot, device = "jpeg", width = 8, height = 6)
    },
    contentType = "image/jpeg"
  )
  
  output$download_residual_fitted_jpg <- downloadHandler(
    filename = function() { paste0("residual_fitted_", Sys.Date(), ".jpg") },
    content = function(file) {
      req(values$regression_done)
      model <- regression_model()
      plot <- ggplot(data.frame(fitted = fitted(model), residuals = resid(model)), 
                     aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        theme_minimal() +
        labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")
      ggsave(file, plot = plot, device = "jpeg", width = 8, height = 6)
    },
    contentType = "image/jpeg"
  )
  
  output$download_assumption_interpretation_pdf <- downloadHandler(
    filename = function() { paste0("assumption_interpretation_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
      
      req(values$regression_done)
      model <- regression_model()
      
      vif_interpretation <- if(length(input$reg_indep) > 1) {
        vif_result <- car::vif(model)
        if(any(vif_result > 10)) {
          paste("VIF menunjukkan adanya multikolinearitas signifikan (VIF > 10) untuk variabel:",
                paste(names(vif_result[vif_result > 10]), collapse = ", "), ".")
        } else {
          "VIF menunjukkan tidak ada multikolinearitas signifikan (semua VIF < 10)."
        }
      } else {
        "VIF test tidak dilakukan karena hanya ada satu variabel independen."
      }
      
      residuals <- resid(model)
      shapiro_test <- shapiro.test(residuals)
      normality_interpretation <- if(shapiro_test$p.value > 0.05) {
        paste("Uji Shapiro-Wilk untuk normalitas residual (p-value =", round(shapiro_test$p.value, 4),
              "): Residual dianggap berdistribusi normal.")
      } else {
        paste("Uji Shapiro-Wilk untuk normalitas residual (p-value =", round(shapiro_test$p.value, 4),
              "): Residual tidak berdistribusi normal.")
      }
      
      bptest_result <- lmtest::bptest(model)
      homoskedasticity_interpretation <- if(bptest_result$p.value > 0.05) {
        paste("Uji Breusch-Pagan untuk homoskedastisitas (p-value =", round(bptest_result$p.value, 4),
              "): Varians residual homogen.")
      } else {
        paste("Uji Breusch-Pagan untuk homoskedastisitas (p-value =", round(bptest_result$p.value, 4),
              "): Varians residual tidak homogen.")
      }
      
      temp_md <- tempfile(fileext = ".Rmd")
      rmd_content <- paste(
        "---",
        "title: 'Regression Assumptions Interpretation'",
        "date: '", Sys.Date(), "'",
        "output:",
        "  pdf_document:",
        "    latex_engine: pdflatex",
        "    keep_tex: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{caption}",
        "  - \\usepackage[utf8]{inputenc}",
        "  - \\usepackage{geometry}",
        "  - \\geometry{a4paper, margin=1in}",
        "  - \\usepackage{parskip}",
        "  - \\setlength{\\parskip}{0.5em}",
        "---",
        "\n# Interpretasi Uji Asumsi Model\n",
        vif_interpretation, "\n\n",
        normality_interpretation, "\n\n",
        homoskedasticity_interpretation,
        sep = "\n"
      )
      
      writeLines(rmd_content, temp_md)
      
      tryCatch(
        {
          rmarkdown::render(
            temp_md,
            output_file = file,
            output_format = "pdf_document",
            clean = TRUE,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          stop("Failed to generate PDF: ", e$message)
        }
      )
    },
    contentType = "application/pdf"
  )
  
  output$download_regresi_complete <- downloadHandler(
    filename = function() { paste0("regresi_complete_", Sys.Date(), ".pdf") },
    content = function(file) {
      if (!tinytex::is_tinytex()) {
        tinytex::install_tinytex()
      }
    
    req(values$regression_done)
    temp_md <- tempfile(fileext = ".Rmd")
    
    model <- regression_model()
    model_summary <- summary(model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r_squared
    p_value <- broom::glance(model)$p.value
    significant_coeffs <- model_summary$coefficients
    significant_vars <- names(which(significant_coeffs[, "Pr(>|t|)"] < 0.05))
    
    # Simpan residual plots
    qq_plot_file <- file.path(tempdir(), "residual_qq.jpg")
    qq_plot <- ggplot(data.frame(sample = resid(model)), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = "Q-Q Plot dari Residual")
    ggsave(qq_plot_file, plot = qq_plot, device = "jpeg", width = 8, height = 6)
    
    residual_fitted_file <- file.path(tempdir(), "residual_fitted.jpg")
    residual_fitted_plot <- ggplot(data.frame(fitted = fitted(model), residuals = resid(model)), 
                                   aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Residual vs Nilai Fitted", x = "Nilai Fitted", y = "Residual")
    ggsave(residual_fitted_file, plot = residual_fitted_plot, device = "jpeg", width = 8, height = 6)
    
    # Uji asumsi
    vif_interpretation <- if(length(input$reg_indep) > 1) {
      vif_result <- car::vif(model)
      if(any(vif_result > 10)) {
        paste("VIF menunjukkan adanya multikolinearitas signifikan (VIF > 10) untuk variabel: ",
              paste(names(vif_result[vif_result > 10]), collapse = ", "), ".")
      } else {
        "VIF menunjukkan tidak ada multikolinearitas signifikan (semua VIF < 10)."
      }
    } else {
      "Uji VIF tidak dilakukan karena hanya ada satu variabel independen."
    }
    
    residuals <- resid(model)
    shapiro_test <- shapiro.test(residuals)
    normality_interpretation <- if(shapiro_test$p.value > 0.05) {
      paste("Uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
            "): Residual dianggap berdistribusi normal.")
    } else {
      paste("Uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
            "): Residual tidak berdistribusi normal.")
    }
    
    bptest_result <- lmtest::bptest(model)
    homoskedasticity_interpretation <- if(bptest_result$p.value > 0.05) {
      paste("Uji Breusch-Pagan (p-value =", round(bptest_result$p.value, 4),
            "): Varians residual homogen.")
    } else {
      paste("Uji Breusch-Pagan (p-value =", round(bptest_result$p.value, 4),
            "): Varians residual tidak homogen.")
    }
    
    # Konten Rmd
    rmd_content <- paste(
      "---",
      "title: 'Laporan Regresi Linear Berganda'",
      "date: '", Sys.Date(), "'",
      "output:",
      "  pdf_document:",
      "    latex_engine: pdflatex",
      "    keep_tex: true",
      "geometry: margin=1in",
      "header-includes:",
      "  - \\usepackage{booktabs}",
      "  - \\usepackage{longtable}",
      "  - \\usepackage{caption}",
      "  - \\usepackage{pdflscape}",
      "  - \\usepackage[utf8]{inputenc}",
      "  - \\usepackage{geometry}",
      "  - \\geometry{a4paper, margin=1in}",
      "  - \\usepackage{parskip}",
      "  - \\setlength{\\parskip}{0.5em}",
      "---",
      "\n# Regresi Linear Berganda\n",
      "## Ringkasan Model\n",
      "Variabel Dependen: ", input$reg_dep, "\n",
      "Variabel Independen: ", paste(input$reg_indep, collapse = ", "), "\n",
      "R-squared: ", round(r_squared, 4), "\n",
      "Adjusted R-squared: ", round(adj_r_squared, 4), "\n",
      "P-value model: ", round(p_value, 4), "\n",
      "Koefisien signifikan (p < 0.05): ", paste(significant_vars, collapse = ", "), "\n",
      "```{r echo=FALSE, results='asis'}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      "pander::pander(summary(model)$coefficients)",
      "```",
      "\n\\newpage\n",
      "## Uji Asumsi\n",
      vif_interpretation, "\n",
      normality_interpretation, "\n",
      homoskedasticity_interpretation, "\n",
      "\n## Q-Q Plot Residual\n",
      "```{r echo=FALSE, fig.cap='Q-Q Plot Residual', out.width='80%'}",
      "knitr::include_graphics('", qq_plot_file, "')",
      "```",
      "\n## Residual vs Fitted\n",
      "```{r echo=FALSE, fig.cap='Residual vs Fitted', out.width='80%'}",
      "knitr::include_graphics('", residual_fitted_file, "')",
      "```",
      sep = "\n"
    )
    
    writeLines(rmd_content, temp_md)
    
    tryCatch(
      {
        rmarkdown::render(
          temp_md,
          output_file = file,
          output_format = "pdf_document",
          clean = TRUE,
          envir = new.env(parent = globalenv())
        )
      },
      error = function(e) {
        stop("Failed to generate PDF: ", e$message)
      }
    )
    
  }, contentType = "application/pdf" )
}

# Run the application
shinyApp(ui = ui, server = server)