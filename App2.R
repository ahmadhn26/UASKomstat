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
library(pagedown)


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
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          margin-bottom: 20px;
        }
        .download-btn {
          margin: 10px 5px;
        }
        .download-section {
          background-color: #f9f9f9;
          padding: 15px;
          border-radius: 5px;
          margin-top: 20px;
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
                           downloadButton("download_descriptive_interpretation_pdf", "Descriptive Interpretation (PDF)", class = "btn btn-info download-btn"),
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
                           downloadButton("download_normality_test_pdf", "Normality Test Results (PDF)", class = "btn btn-primary download-btn"),
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
                           downloadButton("download_ttest1_pdf", "1-Sample T-Test (PDF)", class = "btn btn-primary download-btn"),
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
                           downloadButton("download_regression_summary_pdf", "Regression Summary (PDF)", class = "btn btn-primary download-btn"),
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
  
  # Overview Map
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
  
  # Data Management
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
  
  # Categorization Map
  output$categorization_map <- renderLeaflet({
    req(categorized_data_reactive())
    cat_data <- categorized_data_reactive()
    
    # Create color palette
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
  
  # Descriptive Statistics
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
    
    cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
    max_cor <- which(cor_matrix == max(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    min_cor <- which(cor_matrix == min(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    
    paste("Interpretasi: Peta korelasi menunjukkan hubungan linear antara semua variabel numerik.",
          "Korelasi positif terkuat terjadi antara", rownames(cor_matrix)[max_cor[1]], "dan", 
          colnames(cor_matrix)[max_cor[2]], "dengan nilai", round(cor_matrix[max_cor], 3),
          ". Korelasi negatif terkuat terjadi antara", rownames(cor_matrix)[min_cor[1]], "dan",
          colnames(cor_matrix)[min_cor[2]], "dengan nilai", round(cor_matrix[min_cor], 3))
  })
  
  # Geographic Map
  output$exploration_map <- renderLeaflet({
    req(input$map_var)
    
    # Create color palette
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
  
  # Proportion Tests
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
  
  # Variance Test
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
  
  # ANOVA Tests
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
  
  # Two-Way ANOVA
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
  
  # Multiple Linear Regression
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
  

# Multiple Linear Regression (Lanjutan)
  output$regression_interpretation <- renderText({
    req(regression_model())
    model <- regression_model()
    model_summary <- summary(model)
    
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    p_value <- glance(model)$p.value
    
    # Ambil nama koefisien yang signifikan
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
  
  # VIF interpretation
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
  
  # Normality of residuals
  residuals <- resid(model)
  shapiro_test <- shapiro.test(residuals)
  normality_interpretation <- if(shapiro_test$p.value > 0.05) {
    paste("Uji Shapiro-Wilk untuk normalitas residual (p-value =", round(shapiro_test$p.value, 4),
          "): Residual dianggap berdistribusi normal.\n")
  } else {
    paste("Uji Shapiro-Wilk untuk normalitas residual (p-value =", round(shapiro_test$p.value, 4),
          "): Residual tidak berdistribusi normal.\n")
  }
  
  # Homoskedasticity
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

# Reactive values to track test completions
values <- reactiveValues(
  assumptions_done = FALSE,
  ttest1_done = FALSE,
  ttest2_done = FALSE,
  prop1_done = FALSE,
  var1_done = FALSE,
  anova1_done = FALSE,
  anova2_done = FALSE,
  regression_done = FALSE
)

# Update reactive values when tests are run
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
# CONTOH PERBAIKAN UNTUK SATU TOMBOL
output$download_welcome_pdf <- downloadHandler(
  filename = function() { paste0("welcome_report_", Sys.Date(), ".pdf") },
  content = function(file) {
    temp_rmd <- tempfile(fileext = ".Rmd")
    
    # Header YAML yang sudah diubah untuk pagedown (tanpa perintah LaTeX)
    rmd_header <- "
---
title: 'Welcome Report - SEVA'
date: '`r Sys.Date()`'
output: pagedown::html_paged
---
"
    # Isi laporan (tanpa perintah LaTeX)
    rmd_body <- "
# Socio-Economic Vulnerability Analyzer (SEVA)
Selamat datang di dashboard analisis kerentanan sosial-ekonomi.
"
    
    # Gabungkan header dan isi, lalu tulis ke file sementara
    writeLines(paste(rmd_header, rmd_body), temp_rmd)
    
    # Perintah ini sekarang akan menggunakan pagedown secara otomatis
    rmarkdown::render(temp_rmd, output_file = file)
  }
)

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

output$download_beranda_complete <- downloadHandler(
  filename = function() { paste0("beranda_complete_", Sys.Date(), ".pdf") },
  content = function(file) {
    temp_md <- tempfile(fileext = ".Rmd")
    rmd_content <- paste(
      "---",
      "title: 'Complete Beranda Report - SEVA'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\\usepackage{pdflscape}",
      "\n# Socio-Economic Vulnerability Analyzer (SEVA)\n",
      "Dashboard ini dirancang untuk menganalisis kerentanan sosial-ekonomi berbagai daerah menggunakan berbagai indikator demografis, ekonomi, dan sosial.\n\n",
      "## Fitur Utama\n",
      "- Manajemen dan kategorisasi data\n",
      "- Eksplorasi data dengan visualisasi interaktif\n",
      "- Peta visualisasi data geografis\n",
      "- Uji asumsi statistik\n",
      "- Analisis statistik inferensia\n",
      "- Analisis regresi linear berganda\n\n",
      "## Metadata\n",
      "```{r echo=FALSE, results='asis'}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      "pander::pander(metadata)",
      "```",
      "\n\\newpage\n",
      "## Peta Sebaran Data\n",
      "Peta berikut menunjukkan distribusi spasial populasi dan tingkat kemiskinan.\n",
      "![Overview Map](overview_map.jpg)",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    # Save map as JPG for inclusion in PDF
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
    
    mapshot(map, file = file.path(tempdir(), "overview_map.jpg"), vwidth = 800, vheight = 600)
    
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

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
    req(categorized_data_reactive())
    temp_md <- tempfile(fileext = ".Rmd")
    rmd_content <- paste(
      "---",
      "title: 'Categorization Interpretation'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Interpretasi Kategorisasi\n",
      "Variabel ", input$cont_var, " telah berhasil dikategorisasi menjadi ", input$n_bins,
      " kategori berdasarkan kuantil. Kategorisasi ini membantu dalam analisis data dengan mengubah",
      " variabel kontinu menjadi variabel kategorikal yang dapat digunakan untuk analisis lebih lanjut seperti ANOVA atau Chi-square test.",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_manajemen_complete <- downloadHandler(
  filename = function() { paste0("manajemen_complete_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(categorized_data_reactive())
    temp_md <- tempfile(fileext = ".Rmd")
    cat_data <- categorized_data_reactive()
    display_data <- cat_data[, c(input$cont_var, paste0(input$cont_var, "_Kategori"))]
    
    rmd_content <- paste(
      "---",
      "title: 'Complete Data Management Report'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\\usepackage{pdflscape}",
      "\n# Hasil Kategorisasi\n",
      "Variabel ", input$cont_var, " telah dikategorisasi menjadi ", input$n_bins, " kategori.\n",
      "```{r echo=FALSE, results='asis'}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      "pander::pander(display_data)",
      "```",
      "\n\\newpage\n",
      "## Peta Kategorisasi\n",
      "Peta berikut menunjukkan distribusi spasial dari kategorisasi variabel ", input$cont_var, ".\n",
      "![Categorization Map](categorization_map.jpg)",
      "\n\\newpage\n",
      "## Interpretasi\n",
      "Variabel ", input$cont_var, " telah berhasil dikategorisasi menjadi ", input$n_bins,
      " kategori berdasarkan kuantil. Kategorisasi ini membantu dalam analisis data dengan mengubah",
      " variabel kontinu menjadi variabel kategorikal yang dapat digunakan untuk analisis lebih lanjut seperti ANOVA atau Chi-square test.",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    # Save map as JPG
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
    
    mapshot(map, file = file.path(tempdir(), "categorization_map.jpg"), vwidth = 800, vheight = 600)
    
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

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
  filename = function() { paste0("descriptive_interpretation_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(input$desc_vars)
    temp_md <- tempfile(fileext = ".Rmd")
    rmd_content <- paste(
      "---",
      "title: 'Descriptive Statistics Interpretation'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Interpretasi Statistik Deskriptif\n",
      "Statistik deskriptif menunjukkan karakteristik dasar dari variabel yang dipilih: ",
      paste(input$desc_vars, collapse = ", "), ".\n",
      "Mean dan median memberikan gambaran tentang tendensi sentral, sedangkan standar deviasi",
      " menunjukkan variabilitas data. Perbedaan antara mean dan median dapat mengindikasikan",
      " adanya skewness dalam distribusi data.",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
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
    temp_md <- tempfile(fileext = ".Rmd")
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
    
    rmd_content <- paste(
      "---",
      "title: 'Plot Interpretation'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Interpretasi Visualisasi\n",
      interpretation,
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
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
    
    temp_md <- tempfile(fileext = ".Rmd")
    rmd_content <- paste(
      "---",
      "title: 'Correlation Interpretation'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Interpretasi Peta Korelasi\n",
      "Peta korelasi menunjukkan hubungan linear antara semua variabel numerik.\n",
      "Korelasi positif terkuat terjadi antara ", rownames(cor_matrix)[max_cor[1]], " dan ",
      colnames(cor_matrix)[max_cor[2]], " dengan nilai ", round(cor_matrix[max_cor], 3), ".\n",
      "Korelasi negatif terkuat terjadi antara ", rownames(cor_matrix)[min_cor[1]], " dan ",
      colnames(cor_matrix)[min_cor[2]], " dengan nilai ", round(cor_matrix[min_cor], 3), ".",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
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
    temp_md <- tempfile(fileext = ".Rmd")
    rmd_content <- paste(
      "---",
      "title: 'Geographic Map Interpretation'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Interpretasi Peta Geografis\n",
      "Peta geografis menunjukkan distribusi spasial dari variabel ", input$map_var,
      " di seluruh wilayah. Ukuran dan warna lingkaran menunjukkan nilai variabel,",
      " memungkinkan identifikasi pola geografis dan kluster nilai tinggi atau rendah.",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_eksplorasi_complete <- downloadHandler(
  filename = function() { paste0("eksplorasi_complete_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(input$desc_vars, input$map_var)
    temp_md <- tempfile(fileext = ".Rmd")
    
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
    
    numeric_data <- select_if(sovi_data, is.numeric)
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
    max_cor <- which(cor_matrix == max(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    min_cor <- which(cor_matrix == min(cor_matrix, na.rm = TRUE), arr.ind = TRUE)
    
    rmd_content <- paste(
      "---",
      "title: 'Complete Data Exploration Report'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\\usepackage{pdflscape}",
      "\n# Statistik Deskriptif\n",
      "```{r echo=FALSE, results='asis'}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      "pander::pander(desc_summary)",
      "```",
      "\n## Interpretasi Statistik Deskriptif\n",
      "Statistik deskriptif menunjukkan karakteristik dasar dari variabel yang dipilih: ",
      paste(input$desc_vars, collapse = ", "), ".\n",
      "Mean dan median memberikan gambaran tentang tendensi sentral, sedangkan standar deviasi",
      " menunjukkan variabilitas data. Perbedaan antara mean dan median dapat mengindikasikan",
      " adanya skewness dalam distribusi data.\n",
      "\n\\newpage\n",
      "## Visualisasi\n",
      "![Exploration Plot](exploration_plot.jpg)",
      "\n## Interpretasi Visualisasi\n",
      if(input$plot_type == "hist") {
        paste("Histogram menunjukkan distribusi frekuensi dari variabel ", input$plot_var1,
              ". Bentuk distribusi dapat memberikan informasi tentang normalitas data dan adanya outliers.")
      } else if(input$plot_type == "box") {
        paste("Box plot menampilkan ringkasan lima angka (minimum, Q1, median, Q3, maksimum)",
              " dari variabel ", input$plot_var1, ". Titik-titik di luar whiskers menunjukkan potensi outliers.")
      } else if(input$plot_type == "scatter") {
        paste("Scatter plot menunjukkan hubungan antara ", input$plot_var1, " dan ", input$plot_var2,
              ". Garis regresi membantu memvisualisasikan tren hubungan linear antara kedua variabel.")
      },
      "\n\\newpage\n",
      "## Peta Korelasi\n",
      "![Correlation Heatmap](correlation_heatmap.jpg)",
      "\n## Interpretasi Peta Korelasi\n",
      "Peta korelasi menunjukkan hubungan linear antara semua variabel numerik.\n",
      "Korelasi positif terkuat terjadi antara ", rownames(cor_matrix)[max_cor[1]], " dan ",
      colnames(cor_matrix)[max_cor[2]], " dengan nilai ", round(cor_matrix[max_cor], 3), ".\n",
      "Korelasi negatif terkuat terjadi antara ", rownames(cor_matrix)[min_cor[1]], " dan ",
      colnames(cor_matrix)[min_cor[2]], " dengan nilai ", round(cor_matrix[min_cor], 3), ".\n",
      "\n\\newpage\n",
      "## Peta Geografis\n",
      "![Exploration Map](exploration_map.jpg)",
      "\n## Interpretasi Peta Geografis\n",
      "Peta geografis menunjukkan distribusi spasial dari variabel ", input$map_var,
      " di seluruh wilayah. Ukuran dan warna lingkaran menunjukkan nilai variabel,",
      " memungkinkan identifikasi pola geografis dan kluster nilai tinggi atau rendah.",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    # Save plots and maps
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
    ggsave(file.path(tempdir(), "exploration_plot.jpg"), plot = plot, device = "jpeg", width = 8, height = 6)
    
    numeric_data <- select_if(sovi_data, is.numeric)
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    melted_cor <- melt(cor_matrix)
    cor_plot <- ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      coord_fixed()
    ggsave(file.path(tempdir(), "correlation_heatmap.jpg"), plot = cor_plot, device = "jpeg", width = 8, height = 6)
    
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
    mapshot(map, file = file.path(tempdir(), "exploration_map.jpg"), vwidth = 800, vheight = 600)
    
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

# --- Uji Asumsi Tab ---
output$download_normality_test_pdf <- downloadHandler(
  filename = function() { paste0("normality_test_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$assumptions_done, input$normality_var)
    temp_md <- tempfile(fileext = ".Rmd")
    
    var_data <- sovi_data[[input$normality_var]]
    var_data <- var_data[!is.na(var_data)]
    shapiro_test <- shapiro.test(var_data)
    ks_test <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
    
    normality_output <- capture.output({
      cat("Uji Normalitas untuk variabel:", input$normality_var, "\n\n")
      cat("H0: Data berdistribusi normal\n")
      cat("H1: Data tidak berdistribusi normal\n\n")
      cat("Shapiro-Wilk Test:\n")
      print(shapiro_test)
      cat("\nKolmogorov-Smirnov Test:\n")
      print(ks_test)
    })
    
    rmd_content <- paste(
      "---",
      "title: 'Normality Test Results'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Hasil Uji Normalitas\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(normality_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    # Save Q-Q plot
    qq_plot <- ggplot(data.frame(sample = var_data), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = paste("Q-Q Plot for", input$normality_var))
    ggsave(file.path(tempdir(), "qq_plot.jpg"), plot = qq_plot, device = "jpeg", width = 8, height = 6)
    
    rmd_content <- paste(rmd_content, "\n\\newpage\n", "## Q-Q Plot\n", "![Q-Q Plot](qq_plot.jpg)", sep = "\n")
    writeLines(rmd_content, temp_md)
    
    # Debugging: Save Rmd file
    file.copy(temp_md, file.path(getwd(), "debug_normality_test.Rmd"), overwrite = TRUE)
    message("File Rmd disimpan di: ", file.path(getwd(), "debug_normality_test.Rmd"))
    
    tryCatch(
      {
        rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
        message("Rendering PDF berhasil: ", file)
      },
      error = function(e) {
        message("Rendering PDF gagal: ", e$message)
        stop("Rendering PDF gagal: ", e$message)
      }
    )
  },
  contentType = "application/pdf"
)

output$download_qq_plot_jpg <- downloadHandler(
  filename = function() { paste0("qq_plot_", Sys.Date(), ".jpg") },
  content = function(file) {
    req(input$normality_var)
    var_data <- sovi_data[[input$normality_var]]
    qq_plot <- ggplot(data.frame(sample = var_data), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = paste("Q-Q Plot for", input$normality_var))
    ggsave(file, plot = qq_plot, device = "jpeg", width = 8, height = 6)
  },
  contentType = "image/jpeg"
)

output$download_normality_interpretation_pdf <- downloadHandler(
  filename = function() { paste0("normality_interpretation_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$assumptions_done, input$normality_var)
    temp_md <- tempfile(fileext = ".Rmd")
    
    var_data <- sovi_data[[input$normality_var]]
    var_data <- var_data[!is.na(var_data)]
    shapiro_test <- shapiro.test(var_data)
    
    interpretation <- if(shapiro_test$p.value > 0.05) {
      paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
            "), kita gagal menolak H0. Data variabel ", input$normality_var, 
            " dapat dianggap berdistribusi normal pada tingkat signifikansi 5%.")
    } else {
      paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
            "), kita menolak H0. Data variabel ", input$normality_var, 
            " tidak berdistribusi normal pada tingkat signifikansi 5%.")
    }
    
    rmd_content <- paste(
      "---",
      "title: 'Normality Interpretation'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Interpretasi Uji Normalitas\n",
      interpretation,
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_homogeneity_test_pdf <- downloadHandler(
  filename = function() { paste0("homogeneity_test_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$assumptions_done, input$normality_var, input$group_var)
    if(input$group_var != "None") {
      temp_md <- tempfile(fileext = ".Rmd")
      
      var_data <- sovi_data[[input$normality_var]]
      group_data <- sovi_data[[input$group_var]]
      if(is.numeric(group_data)) {
        group_data <- cut(group_data, breaks = 3, labels = c("Low", "Medium", "High"))
      }
      complete_cases <- complete.cases(var_data, group_data)
      var_data <- var_data[complete_cases]
      group_data <- group_data[complete_cases]
      
      levene_test <- car::leveneTest(var_data, group_data)
      
      homogeneity_output <- capture.output({
        cat("Uji Homogenitas Varians (Levene's Test)\n\n")
        cat("H0: Varians antar kelompok homogen\n")
        cat("H1: Varians antar kelompok tidak homogen\n\n")
        print(levene_test)
      })
      
      rmd_content <- paste(
        "---",
        "title: 'Homogeneity Test Results'",
        "date: '", Sys.Date(), "'",
        "output: pdf_document",
        "geometry: margin=1in",
        "---",
        "\\usepackage{booktabs}",
        "\\usepackage{longtable}",
        "\\usepackage{caption}",
        "\n# Hasil Uji Homogenitas\n",
        "```{r echo=FALSE, comment=''}",
        "library(pander)",
        "panderOptions('table.style', 'rmarkdown')",
        paste(homogeneity_output, collapse = "\n"),
        "```",
        sep = "\n"
      )
      writeLines(rmd_content, temp_md)
      
      # Debugging: Save Rmd file
      file.copy(temp_md, file.path(getwd(), "debug_homogeneity_test.Rmd"), overwrite = TRUE)
      message("File Rmd disimpan di: ", file.path(getwd(), "debug_homogeneity_test.Rmd"))
      
      tryCatch(
        {
          rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
          message("Rendering PDF berhasil: ", file)
        },
        error = function(e) {
          message("Rendering PDF gagal: ", e$message)
          stop("Rendering PDF gagal: ", e$message)
        }
      )
    } else {
      stop("Pilih variabel kelompok untuk melakukan uji homogenitas")
    }
  },
  contentType = "application/pdf"
)

output$download_homogeneity_interpretation_pdf <- downloadHandler(
  filename = function() { paste0("homogeneity_interpretation_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$assumptions_done, input$normality_var, input$group_var)
    if(input$group_var != "None") {
      temp_md <- tempfile(fileext = ".Rmd")
      
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
              " pada tingkat signifikansi 5%.")
      } else {
        paste("Berdasarkan uji Levene (p-value =", round(p_value, 4),
              "), kita menolak H0. Varians antar kelompok tidak homogen",
              " pada tingkat signifikansi 5%.")
      }
      
      rmd_content <- paste(
        "---",
        "title: 'Homogeneity Interpretation'",
        "date: '", Sys.Date(), "'",
        "output: pdf_document",
        "geometry: margin=1in",
        "---",
        "\\usepackage{booktabs}",
        "\\usepackage{longtable}",
        "\\usepackage{caption}",
        "\n# Interpretasi Uji Homogenitas\n",
        interpretation,
        sep = "\n"
      )
      writeLines(rmd_content, temp_md)
      rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
    }
  },
  contentType = "application/pdf"
)

output$download_asumsi_complete <- downloadHandler(
  filename = function() { paste0("asumsi_complete_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$assumptions_done, input$normality_var)
    temp_md <- tempfile(fileext = ".Rmd")
    
    var_data <- sovi_data[[input$normality_var]]
    var_data <- var_data[!is.na(var_data)]
    shapiro_test <- shapiro.test(var_data)
    ks_test <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
    
    normality_output <- capture.output({
      cat("Uji Normalitas untuk variabel:", input$normality_var, "\n\n")
      cat("H0: Data berdistribusi normal\n")
      cat("H1: Data tidak berdistribusi normal\n\n")
      cat("Shapiro-Wilk Test:\n")
      print(shapiro_test)
      cat("\nKolmogorov-Smirnov Test:\n")
      print(ks_test)
    })
    
    normality_interpretation <- if(shapiro_test$p.value > 0.05) {
      paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
            "), kita gagal menolak H0. Data variabel ", input$normality_var, 
            " dapat dianggap berdistribusi normal pada tingkat signifikansi 5%.")
    } else {
      paste("Berdasarkan uji Shapiro-Wilk (p-value =", round(shapiro_test$p.value, 4),
            "), kita menolak H0. Data variabel ", input$normality_var, 
            " tidak berdistribusi normal pada tingkat signifikansi 5%.")
    }
    
    homogeneity_output <- ""
    homogeneity_interpretation <- ""
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
      homogeneity_output <- capture.output({
        cat("Uji Homogenitas Varians (Levene's Test)\n\n")
        cat("H0: Varians antar kelompok homogen\n")
        cat("H1: Varians antar kelompok tidak homogen\n\n")
        print(levene_test)
      })
      
      p_value <- levene_test$`Pr(>F)`[1]
      homogeneity_interpretation <- if(p_value > 0.05) {
        paste("Berdasarkan uji Levene (p-value =", round(p_value, 4),
              "), kita gagal menolak H0. Varians antar kelompok dapat dianggap homogen",
              " pada tingkat signifikansi 5%.")
      } else {
        paste("Berdasarkan uji Levene (p-value =", round(p_value, 4),
              "), kita menolak H0. Varians antar kelompok tidak homogen",
              " pada tingkat signifikansi 5%.")
      }
    }
    
    rmd_content <- paste(
      "---",
      "title: 'Complete Assumption Tests Report'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\\usepackage{pdflscape}",
      "\n# Hasil Uji Normalitas\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(normality_output, collapse = "\n"),
      "```",
      "\n## Q-Q Plot\n",
      "![Q-Q Plot](qq_plot.jpg)",
      "\n## Interpretasi Uji Normalitas\n",
      normality_interpretation,
      if(input$group_var != "None") {
        paste(
          "\n\\newpage\n",
          "# Hasil Uji Homogenitas\n",
          "```{r echo=FALSE, comment=''}",
          "library(pander)",
          "panderOptions('table.style', 'rmarkdown')",
          paste(homogeneity_output, collapse = "\n"),
          "```",
          "\n## Interpretasi Uji Homogenitas\n",
          homogeneity_interpretation
        )
      },
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    # Save Q-Q plot
    qq_plot <- ggplot(data.frame(sample = var_data), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = paste("Q-Q Plot for", input$normality_var))
    ggsave(file.path(tempdir(), "qq_plot.jpg"), plot = qq_plot, device = "jpeg", width = 8, height = 6)
    
    # Debugging: Save Rmd file
    file.copy(temp_md, file.path(getwd(), "debug_asumsi_complete.Rmd"), overwrite = TRUE)
    message("File Rmd disimpan di: ", file.path(getwd(), "debug_asumsi_complete.Rmd"))
    
    tryCatch(
      {
        rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
        message("Rendering PDF berhasil: ", file)
      },
      error = function(e) {
        message("Rendering PDF gagal: ", e$message)
        stop("Rendering PDF gagal: ", e$message)
      }
    )
  },
  contentType = "application/pdf"
)

# --- Statistik Inferensia Tab ---
output$download_ttest1_pdf <- downloadHandler(
  filename = function() { paste0("ttest1_result_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$ttest1_done, input$ttest1_var, input$ttest1_mu)
    temp_md <- tempfile(fileext = ".Rmd")
    
    var_data <- sovi_data[[input$ttest1_var]]
    var_data <- var_data[!is.na(var_data)]
    ttest_result <- t.test(var_data, mu = input$ttest1_mu)
    
    ttest_output <- capture.output({
      cat("One-Sample T-Test\n\n")
      cat("H0: μ =", input$ttest1_mu, "\n")
      cat("H1: μ ≠", input$ttest1_mu, "\n\n")
      print(ttest_result)
    })
    
    rmd_content <- paste(
      "---",
      "title: '1-Sample T-Test Results'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Hasil 1-Sample T-Test\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(ttest_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_ttest2_pdf <- downloadHandler(
  filename = function() { paste0("ttest2_result_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$ttest2_done, input$ttest2_var, input$ttest2_group)
    temp_md <- tempfile(fileext = ".Rmd")
    
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
      
      ttest_output <- capture.output({
        cat("Two-Sample Independent T-Test\n\n")
        cat("H0: μ1 = μ2\n")
        cat("H1: μ1 ≠ μ2\n\n")
        cat("Group 1 (", unique_groups[1], "):", length(group1_data), "observations\n")
        cat("Group 2 (", unique_groups[2], "):", length(group2_data), "observations\n\n")
        print(ttest_result)
      })
      
      rmd_content <- paste(
        "---",
        "title: '2-Sample T-Test Results'",
        "date: '", Sys.Date(), "'",
        "output: pdf_document",
        "geometry: margin=1in",
        "---",
        "\\usepackage{booktabs}",
        "\\usepackage{longtable}",
        "\\usepackage{caption}",
        "\n# Hasil 2-Sample T-Test\n",
        "```{r echo=FALSE, comment=''}",
        "library(pander)",
        "panderOptions('table.style', 'rmarkdown')",
        paste(ttest_output, collapse = "\n"),
        "```",
        sep = "\n"
      )
      writeLines(rmd_content, temp_md)
      rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
    } else {
      stop("Error: Need at least 2 groups for comparison")
    }
  },
  contentType = "application/pdf"
)

output$download_prop1_pdf <- downloadHandler(
  filename = function() { paste0("prop1_result_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$prop1_done, input$prop1_var, input$prop1_p)
    temp_md <- tempfile(fileext = ".Rmd")
    
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
    
    prop_output <- capture.output({
      cat("One-Sample Proportion Test\n\n")
      cat("H0: p =", input$prop1_p, "\n")
      cat("H1: p ≠", input$prop1_p, "\n\n")
      cat("Sample proportion:", round(successes/n, 4), "\n")
      cat("Sample size:", n, "\n")
      cat("Successes:", successes, "\n\n")
      print(prop_test)
    })
    
    rmd_content <- paste(
      "---",
      "title: 'Proportion Test Results'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Hasil Uji Proporsi\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(prop_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_var1_pdf <- downloadHandler(
  filename = function() { paste0("var1_result_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$var1_done, input$var1_var, input$var1_sigma)
    temp_md <- tempfile(fileext = ".Rmd")
    
    var_data <- sovi_data[[input$var1_var]]
    var_data <- var_data[!is.na(var_data)]
    n <- length(var_data)
    sample_var <- var(var_data)
    chi_stat <- (n - 1) * sample_var / input$var1_sigma
    p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
    
    var_output <- capture.output({
      cat("One-Sample Variance Test (Chi-square)\n\n")
      cat("H0: σ² =", input$var1_sigma, "\n")
      cat("H1: σ² ≠", input$var1_sigma, "\n\n")
      cat("Sample variance:", round(sample_var, 4), "\n")
      cat("Sample size:", n, "\n")
      cat("Chi-square statistic:", round(chi_stat, 4), "\n")
      cat("Degrees of freedom:", n-1, "\n")
      cat("P-value:", round(p_value, 4), "\n")
    })
    
    rmd_content <- paste(
      "---",
      "title: 'Variance Test Results'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Hasil Uji Varians\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(var_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_anova1_pdf <- downloadHandler(
  filename = function() { paste0("anova1_result_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$anova1_done, input$anova1_dep, input$anova1_indep)
    temp_md <- tempfile(fileext = ".Rmd")
    
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
    
    anova_output <- capture.output({
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
    
    rmd_content <- paste(
      "---",
      "title: 'One-Way ANOVA Results'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Hasil One-Way ANOVA\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(anova_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_anova2_pdf <- downloadHandler(
  filename = function() { paste0("anova2_result_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$anova2_done, input$anova2_dep, input$anova2_indep1, input$anova2_indep2)
    temp_md <- tempfile(fileext = ".Rmd")
    
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
    
    anova_output <- capture.output({
      cat("Two-Way ANOVA\n\n")
      cat("Testing main effects and interaction effect\n\n")
      print(anova_summary)
    })
    
    rmd_content <- paste(
      "---",
      "title: 'Two-Way ANOVA Results'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Hasil Two-Way ANOVA\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(anova_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_inferensia_complete <- downloadHandler(
  filename = function() { paste0("inferensia_complete_", Sys.Date(), ".pdf") },
  content = function(file) {
    temp_md <- tempfile(fileext = ".Rmd")
    
    ttest1_output <- if(values$ttest1_done) {
      var_data <- sovi_data[[input$ttest1_var]]
      var_data <- var_data[!is.na(var_data)]
      ttest_result <- t.test(var_data, mu = input$ttest1_mu)
      capture.output({
        cat("One-Sample T-Test\n\n")
        cat("H0: μ =", input$ttest1_mu, "\n")
        cat("H1: μ ≠", input$ttest1_mu, "\n\n")
        print(ttest_result)
      })
    } else {
      "Uji 1-Sample T-Test belum dilakukan."
    }
    
    ttest2_output <- if(values$ttest2_done) {
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
        capture.output({
          cat("Two-Sample Independent T-Test\n\n")
          cat("H0: μ1 = μ2\n")
          cat("H1: μ1 ≠ μ2\n\n")
          cat("Group 1 (", unique_groups[1], "):", length(group1_data), "observations\n")
          cat("Group 2 (", unique_groups[2], "):", length(group2_data), "observations\n\n")
          print(ttest_result)
        })
      } else {
        "Error: Need at least 2 groups for 2-Sample T-Test"
      }
    } else {
      "Uji 2-Sample T-Test belum dilakukan."
    }
    
    prop_output <- if(values$prop1_done) {
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
      capture.output({
        cat("One-Sample Proportion Test\n\n")
        cat("H0: p =", input$prop1_p, "\n")
        cat("H1: p ≠", input$prop1_p, "\n\n")
        cat("Sample proportion:", round(successes/n, 4), "\n")
        cat("Sample size:", n, "\n")
        cat("Successes:", successes, "\n\n")
        print(prop_test)
      })
    } else {
      "Uji Proporsi belum dilakukan."
    }
    
    var_output <- if(values$var1_done) {
      var_data <- sovi_data[[input$var1_var]]
      var_data <- var_data[!is.na(var_data)]
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$var1_sigma
      p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
      capture.output({
        cat("One-Sample Variance Test (Chi-square)\n\n")
        cat("H0: σ² =", input$var1_sigma, "\n")
        cat("H1: σ² ≠", input$var1_sigma, "\n\n")
        cat("Sample variance:", round(sample_var, 4), "\n")
        cat("Sample size:", n, "\n")
        cat("Chi-square statistic:", round(chi_stat, 4), "\n")
        cat("Degrees of freedom:", n-1, "\n")
        cat("P-value:", round(p_value, 4), "\n")
      })
    } else {
      "Uji Varians belum dilakukan."
    }
    
    anova1_output <- if(values$anova1_done) {
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
      capture.output({
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
    } else {
      "Uji One-Way ANOVA belum dilakukan."
    }
    
    anova2_output <- if(values$anova2_done) {
      dep_var <- sovi_data[[input$anova2_dep]]
      indep_var1 <- sovi_data[[input$anova2_indep1]]
      indep_var2 <- sovi_data[[input$anova2_indep2]]
      if(is.numeric(indep_var1)) {
        indep_var1 <- cut(indep_var1, breaks = 2, labels = c("Low", "High"))
      }
      if(is.numeric(indep_var2)) {
        indep_var2 <- cut(indep_var2, breaks = 2, labels = c("Low", "High"))
      }
      complete.cases(dep_var, indep_var1, indep_var2)
      dep_var <- dep_var[complete_cases]
      indep_var1 <- indep_var1[complete_cases]
      indep_var2 <- indep_var2[complete_cases]
      anova_result <- aov(dep_var ~ indep_var1 * indep_var2)
      anova_summary <- summary(anova_result)
      capture.output({
        cat("Two-Way ANOVA\n\n")
        cat("Testing main effects and interaction effect\n\n")
        print(anova_summary)
      })
    } else {
      "Uji Two-Way ANOVA belum dilakukan."
    }
    
    rmd_content <- paste(
      "---",
      "title: 'Complete Inferential Statistics Report'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\\usepackage{pdflscape}",
      "\n# Hasil Statistik Inferensia\n",
      "## 1-Sample T-Test\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(ttest1_output, collapse = "\n"),
      "```",
      "\n## 2-Sample T-Test\n",
      "```{r echo=FALSE, comment=''}",
      paste(ttest2_output, collapse = "\n"),
      "```",
      "\n\\newpage\n",
      "## Proportion Test\n",
      "```{r echo=FALSE, comment=''}",
      paste(prop_output, collapse = "\n"),
      "```",
      "\n## Variance Test\n",
      "```{r echo=FALSE, comment=''}",
      paste(var_output, collapse = "\n"),
      "```",
      "\n\\newpage\n",
      "## One-Way ANOVA\n",
      "```{r echo=FALSE, comment=''}",
      paste(anova1_output, collapse = "\n"),
      "```",
      "\n\\newpage\n",
      "## Two-Way ANOVA\n",
      "```{r echo=FALSE, comment=''}",
      paste(anova2_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    # Debugging: Save Rmd file
    file.copy(temp_md, file.path(getwd(), "debug_inferensia_complete.Rmd"), overwrite = TRUE)
    message("File Rmd disimpan di: ", file.path(getwd(), "debug_inferensia_complete.Rmd"))
    
    tryCatch(
      {
        rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
        message("Rendering PDF berhasil: ", file)
      },
      error = function(e) {
        message("Rendering PDF gagal: ", e$message)
        stop("Rendering PDF gagal: ", e$message)
      }
    )
  },
  contentType = "application/pdf"
)

# --- Regresi Linear Berganda Tab ---
output$download_regression_summary_pdf <- downloadHandler(
  filename = function() { paste0("regression_summary_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$regression_done, regression_model())
    temp_md <- tempfile(fileext = ".Rmd")
    
    model <- regression_model()
    model_summary <- summary(model)
    
    regression_output <- capture.output({
      print(model_summary)
    })
    
    rmd_content <- paste(
      "---",
      "title: 'Regression Summary'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Ringkasan Model Regresi\n",
      "Variabel dependen: ", input$reg_dep, "\n",
      "Variabel independen: ", paste(input$reg_indep, collapse = ", "), "\n\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(regression_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    # Debugging: Save Rmd file
    file.copy(temp_md, file.path(getwd(), "debug_regression_summary.Rmd"), overwrite = TRUE)
    message("File Rmd disimpan di: ", file.path(getwd(), "debug_regression_summary.Rmd"))
    
    tryCatch(
      {
        rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
        message("Rendering PDF berhasil: ", file)
      },
      error = function(e) {
        message("Rendering PDF gagal: ", e$message)
        stop("Rendering PDF gagal: ", e$message)
      }
    )
  },
  contentType = "application/pdf"
)

output$download_regression_interpretation_pdf <- downloadHandler(
  filename = function() { paste0("regression_interpretation_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$regression_done, regression_model())
    temp_md <- tempfile(fileext = ".Rmd")
    
    model <- regression_model()
    model_summary <- summary(model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    p_value <- broom::glance(model)$p.value
    
    interpretation <- paste(
      "Interpretasi Regresi Linear Berganda:\n",
      "1. R-squared: ", round(r_squared, 4), " menunjukkan bahwa ", round(r_squared * 100, 2),
      "% variabilitas dalam ", input$reg_dep, " dapat dijelaskan oleh variabel independen.\n",
      "2. Adjusted R-squared: ", round(adj_r_squared, 4), " menyesuaikan R-squared untuk jumlah prediktor.\n",
      "3. P-value model keseluruhan: ", round(p_value, 4),
      ifelse(p_value < 0.05, " (signifikan)", " (tidak signifikan)"),
      " pada tingkat signifikansi 5%.\n",
      "4. Koefisien signifikan (p < 0.05): ",
      paste(names(coef(model_summary))[which(model_summary$coefficients[, 4] < 0.05)], collapse = ", ")
    )
    
    rmd_content <- paste(
      "---",
      "title: 'Regression Interpretation'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Interpretasi Regresi Linear Berganda\n",
      interpretation,
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_vif_test_pdf <- downloadHandler(
  filename = function() { paste0("vif_test_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$regression_done, regression_model())
    temp_md <- tempfile(fileext = ".Rmd")
    
    model <- regression_model()
    vif_output <- if(length(input$reg_indep) > 1) {
      vif_result <- car::vif(model)
      capture.output({
        cat("Variance Inflation Factor (VIF) Test:\n\n")
        print(vif_result)
      })
    } else {
      "VIF test tidak dapat dilakukan karena hanya ada satu variabel independen."
    }
    
    rmd_content <- paste(
      "---",
      "title: 'VIF Test Results'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Hasil Uji VIF\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(vif_output, collapse = "\n"),
      "```",
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_residual_qq_jpg <- downloadHandler(
  filename = function() { paste0("residual_qq_", Sys.Date(), ".jpg") },
  content = function(file) {
    req(values$regression_done, regression_model())
    model <- regression_model()
    residuals <- resid(model)
    
    qq_plot <- ggplot(data.frame(sample = residuals), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = "Q-Q Plot of Residuals")
    
    ggsave(file, plot = qq_plot, device = "jpeg", width = 8, height = 6)
  },
  contentType = "image/jpeg"
)

output$download_residual_fitted_jpg <- downloadHandler(
  filename = function() { paste0("residual_fitted_", Sys.Date(), ".jpg") },
  content = function(file) {
    req(values$regression_done, regression_model())
    model <- regression_model()
    
    residual_plot <- ggplot(data.frame(fitted = fitted(model), residuals = resid(model)), 
                            aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")
    
    ggsave(file, plot = residual_plot, device = "jpeg", width = 8, height = 6)
  },
  contentType = "image/jpeg"
)

output$download_assumption_interpretation_pdf <- downloadHandler(
  filename = function() { paste0("assumption_interpretation_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$regression_done, regression_model())
    temp_md <- tempfile(fileext = ".Rmd")
    
    model <- regression_model()
    
    vif_interpretation <- if(length(input$reg_indep) > 1) {
      vif_result <- car::vif(model)
      if(any(vif_result > 10)) {
        paste("VIF menunjukkan adanya multikolinearitas signifikan (VIF > 10) untuk variabel: ",
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
    
    rmd_content <- paste(
      "---",
      "title: 'Regression Assumptions Interpretation'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\n# Interpretasi Uji Asumsi Model\n",
      "## Multikolinearitas\n",
      vif_interpretation,
      "\n## Normalitas Residual\n",
      normality_interpretation,
      "\n## Homoskedastisitas\n",
      homoskedasticity_interpretation,
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
  },
  contentType = "application/pdf"
)

output$download_regresi_complete <- downloadHandler(
  filename = function() { paste0("regresi_complete_", Sys.Date(), ".pdf") },
  content = function(file) {
    req(values$regression_done, regression_model())
    temp_md <- tempfile(fileext = ".Rmd")
    
    model <- regression_model()
    model_summary <- summary(model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    p_value <- broom::glance(model)$p.value
    
    regression_output <- capture.output({
      print(model_summary)
    })
    
    vif_output <- if(length(input$reg_indep) > 1) {
      vif_result <- car::vif(model)
      capture.output({
        cat("Variance Inflation Factor (VIF) Test:\n\n")
        print(vif_result)
      })
    } else {
      "VIF test tidak dapat dilakukan karena hanya ada satu variabel independen."
    }
    
    vif_interpretation <- if(length(input$reg_indep) > 1) {
      vif_result <- car::vif(model)
      if(any(vif_result > 10)) {
        paste("VIF menunjukkan adanya multikolinearitas signifikan (VIF > 10) untuk variabel: ",
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
    
    rmd_content <- paste(
      "---",
      "title: 'Complete Regression Analysis Report'",
      "date: '", Sys.Date(), "'",
      "output: pdf_document",
      "geometry: margin=1in",
      "---",
      "\\usepackage{booktabs}",
      "\\usepackage{longtable}",
      "\\usepackage{caption}",
      "\\usepackage{pdflscape}",
      "\n# Ringkasan Model Regresi\n",
      "Variabel dependen: ", input$reg_dep, "\n",
      "Variabel independen: ", paste(input$reg_indep, collapse = ", "), "\n\n",
      "```{r echo=FALSE, comment=''}",
      "library(pander)",
      "panderOptions('table.style', 'rmarkdown')",
      paste(regression_output, collapse = "\n"),
      "```",
      "\n## Interpretasi Model\n",
      "1. R-squared: ", round(r_squared, 4), " menunjukkan bahwa ", round(r_squared * 100, 2),
      "% variabilitas dalam ", input$reg_dep, " dapat dijelaskan oleh variabel independen.\n",
      "2. Adjusted R-squared: ", round(adj_r_squared, 4), " menyesuaikan R-squared untuk jumlah prediktor.\n",
      "3. P-value model keseluruhan: ", round(p_value, 4),
      ifelse(p_value < 0.05, " (signifikan)", " (tidak signifikan)"),
      " pada tingkat signifikansi 5%.\n",
      "4. Koefisien signifikan (p < 0.05): ",
      paste(names(coef(model_summary))[which(model_summary$coefficients[, 4] < 0.05)], collapse = ", "), "\n",
      "\n\\newpage\n",
      "## Uji Multikolinearitas\n",
      "```{r echo=FALSE, comment=''}",
      paste(vif_output, collapse = "\n"),
      "```",
      "\n## Interpretasi Multikolinearitas\n",
      vif_interpretation,
      "\n\\newpage\n",
      "## Normalitas Residual\n",
      "![Q-Q Plot](residual_qq.jpg)",
      "\n## Interpretasi Normalitas\n",
      normality_interpretation,
      "\n\\newpage\n",
      "## Homoskedastisitas\n",
      "![Residuals vs Fitted Plot](residual_fitted.jpg)",
      "\n## Interpretasi Homoskedastisitas\n",
      homoskedasticity_interpretation,
      sep = "\n"
    )
    writeLines(rmd_content, temp_md)
    
    # Save diagnostic plots
    qq_plot <- ggplot(data.frame(sample = residuals), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = "Q-Q Plot of Residuals")
    ggsave(file.path(tempdir(), "residual_qq.jpg"), plot = qq_plot, device = "jpeg", width = 8, height = 6)
    
    residual_plot <- ggplot(data.frame(fitted = fitted(model), residuals = resid(model)), 
                            aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")
    ggsave(file.path(tempdir(), "residual_fitted.jpg"), plot = residual_plot, device = "jpeg", width = 8, height = 6)
    
    # Debugging: Save Rmd file
    file.copy(temp_md, file.path(getwd(), "debug_regresi_complete.Rmd"), overwrite = TRUE)
    message("File Rmd disimpan di: ", file.path(getwd(), "debug_regresi_complete.Rmd"))
    
    tryCatch(
      {
        rmarkdown::render(temp_md, output_file = file, output_format = "pdf_document", quiet = FALSE)
        message("Rendering PDF berhasil: ", file)
      },
      error = function(e) {
        message("Rendering PDF gagal: ", e$message)
        stop("Rendering PDF gagal: ", e$message)
      }
    )
  },
  contentType = "application/pdf"
)
}

# Run the application
shinyApp(ui = ui, server = server)