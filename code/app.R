#comentario
#library(shiny) #keep commented for app to be deployed on R Shiny
library(bslib)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(shinythemes)
library(reactable)
library(tidyr)
library(patchwork)
library(DT)
library(stringr)
library(lubridate)
library(sf)
library(leaflet)

# Load dataframes -----------------------------------------------------------------------
namru_biofire_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/namru_biofire_summary_updated.csv")
gihsn_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/gihsn_summary.csv")
vigicasa_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/vigicasa_summary.csv")
vigifinca_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/vigifinca_summary.csv")
gihsn_ages <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/gihsn_ages.csv")
gihsn_ages_vsr <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/gihsn_ages_vsr.csv")
demo_lab_info <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/todas_las_fichas.csv")
conteo_criterio <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/conteo_por_criterio.csv")
conteo_individuo <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/conteo_por_individuo.csv")
conteo_comunidad <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/conteo_por_comunidad.csv")
# load map objects
vigifinca_joined <- readRDS("vigifinca_joined.rds")
guate_json <- readRDS("guate_json.rds")

# Information about each study ------------------------
Header_Biofire <- "Estudio Biofire: Pruebas de Enfermedades Infecciosas y Febriles"

Header_Biofire_eng <- "Biofire Study: Respiratory and Febrile Illness Tests"

Info_Biofire <- "En el Hospital Nacional de Coatepeque, a los pacientes que experimentan tos se les corre un panel respiratorio de Biofire. A los pacientes que experimentan fiebre 
(sin tos) se les corre una prueba para identificar  antígeno NS1 de Dengue. Dependiendo en la disponabilidad de pruebas, a los sujetos que salen con una prueba negativa para Dengue 
en la prueba de tamizaje, se les corre el panel febril de Biofire. Los paneles  de Biofire son pruebas de PCR para un amplio abanico de posibles resultados de enfermedades infecciosas. 
Los resultados acumulados de las pruebas de Biofire y Dengue NS1 están compartidos aquí, organizados por semana epidemiológica."

Info_Biofire_eng <- "At the National Hospital of Coatepeque, patients who experience cough are tested with a Biofire respiratory panel. Patients who experience fever (without cough) are tested for 
the Dengue NS1 antigen. Depending on the availability of tests, subjects who test negative for Dengue in the screening test have their sample tested on the Biofire febrile panel. Biofire panels are 
PCR tests for a wide range of possible infectious disease outcomes. The cumulative results of the Biofire and Dengue NS1 tests are shared here, organized by epidemiological week."

##
Header_GIHSN <- "Red Mundial de Vigilancia de Influenza en Hospitales"

Header_GIHSN_eng <- "Global Influenza Hospital Surveillance Network"

Info_GIHSN <- "Somos un sitio que forma parte de la Red Mundial de Vigilancia de Influenza (<a href='https://gihsn.org' target='_blank'>GIHSN</a>). En el Hospital Nacional 
de Coatepeque y el Hospital de Chimaltenango. Forman parte de esta vigilancia los pacientes que experimenten síntomas de fiebre y/o tos, a ellos se les corren pruebas de 
Sars-CoV-2, Influenza A/B y VSR y se secuencia los resultados positivos. Todos los datos son compartidos con la red mundial para mejorar capacidad de vigilancia, la decisiones 
vacunales de Influenza anual y respuesta a viruses respiratorios."

Info_GIHSN_eng <- "We are a site that is part of the Global Influenza Surveillance Network (<a href='https://gihsn.org' target='_blank'>GIHSN</a>). At the National Hospital of Coatepeque and the Hospital of Chimaltenango, 
patients who experience symptoms of fever and/or cough are part of this surveillance. They are tested for Sars-CoV-2, Influenza A/B, and VSR, and the positive results are sequenced. All data is shared with the global network to 
improve surveillance capacity, annual influenza vaccination decisions, and response to respiratory viruses."

##
Header_VCasa <- "Vigilancia de enfermedades respiratorias y Dengue en casas del Trifinio"

Header_VCasa_eng <- "Respiratory and Dengue Illness Surveillance in Trifinio Houses"

Info_VCasa <- "Estamos realizando vigilancia activa en hogares de las comunidades del Trifinio para detectar enfermedades respiratorias y enfermedades como dengue. 
Los miembros del hogar son evaluados dos veces por semana en busca de síntomas, y si se cumple una o ambas definiciones de caso, se toma una muestra nasal y/o una muestra de sangre, 
y se realizan pruebas de laboratorio."

Info_VCasa_eng <- "We are conducting active surveillance in houses in the Trifinio communities for respiratory illnesses and dengue-like illnesses. Household members are screened 
twice a week for symptoms and if either or both case definitions are met, a nasal swab and/or blood sample is taken and lab tests are run."

##
Header_VFinca <- "Vigilancia de enfermedades respiratorias y dengue en fincas del sur centro de Guatemala."

Header_VFinca_eng <- "Respiratory and Dengue Illness Surveillance in South-Central Guatemalan Farms"

Info_VFinca <- "Estamos realizando vigilancia activa en hogares de las comunidades del Trifinio para detectar enfermedades respiratorias y enfermedades similares al dengue. 
Los miembros del hogar son evaluados dos veces por semana en busca de síntomas, y si se cumple una o ambas definiciones de caso, se toma una muestra nasal y/o una muestra de sangre, 
y se realizan pruebas de laboratorio."

Info_VFinca_eng <- "We are conducting active surveillance in banana farmworkers in the Trifinio region and sugarcane farmworkers central Guatemala. Farmworkers reporting symptoms that
meet the case definition(s) will be tested for Flu A/B/Sars-CoV-2/RSV by nasal swab and/or Dengue by blood sample."

# Define any needed functions -------------------------
# Function to format date labels in Spanish
format_date_spanish <- function(x) {
  months <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  paste(months[as.numeric(format(x, "%m"))], format(x, "\n %Y"), sep = " ")
}

# Define any variables needed for both server and ui ------------

# NAMRU/BIOFIRE:
# Vector mapping column names to pathogen names
pathogen_names <- c( # removed dengue
  #"res_dengue" = "Dengue", 
  #"res_dengue_2" = "Dengue",
  "patogenos_positivos_sangre___1" = "Chikungunya",
  "patogenos_positivos_sangre___2" = "Fiebre hemorrágica de Crimean-Congo",
  "patogenos_positivos_sangre___3" = "Dengue",
  "patogenos_positivos_sangre___4" = "Ebola",
  "patogenos_positivos_sangre___5" = "Lassa",
  "patogenos_positivos_sangre___6" = "Marburg",
  "patogenos_positivos_sangre___7" = "West Nile",
  "patogenos_positivos_sangre___8" = "Fiebre amarilla",
  "patogenos_positivos_sangre___9" = "Zika",
  "patogenos_positivos_sangre___10" = "Bacillus anthracis",
  "patogenos_positivos_sangre___11" = "Francisella tularensis",
  "patogenos_positivos_sangre___12" = "Leptospira spp.",
  "patogenos_positivos_sangre___13" = "Salmonella enterica, Typhi",
  "patogenos_positivos_sangre___14" = "Salmonella enterica, Paratyphi A",
  "patogenos_positivos_sangre___15" = "Yersinia pestis",
  "patogenos_positivos_sangre___16" = "Leishmania spp.",
  "patogenos_positivos_sangre___17" = "Plasmodium spp.",
  "patogenos_positivos_sangre___18" = "P. falciparum",
  "patogenos_positivos_sangre___19" = "P. vivax/ovale",
  "patogenos_positivos_hisnaso___1" = "Adenovirus",
  "patogenos_positivos_hisnaso___2" = "Coronavirus HKU1",
  "patogenos_positivos_hisnaso___3" = "Coronavirus NL63",
  "patogenos_positivos_hisnaso___4" = "Coronavirus 229E",
  "patogenos_positivos_hisnaso___5" = "Coronavirus OC43",
  "patogenos_positivos_hisnaso___6" = "Metapneumovirus humano",
  "patogenos_positivos_hisnaso___7" = "Human Rhinovirus/ Enterovirus",
  "patogenos_positivos_hisnaso___8" = "Influenza A",
  "patogenos_positivos_hisnaso___9" = "Influenza A/H1",
  "patogenos_positivos_hisnaso___10" = "Influenza A/H1-2009",
  "patogenos_positivos_hisnaso___11" = "Influenza A/H3",
  "patogenos_positivos_hisnaso___12" = "Influenza B",
  "patogenos_positivos_hisnaso___13" = "Parainfluenza 1",
  "patogenos_positivos_hisnaso___14" = "Parainfluenza 2",
  "patogenos_positivos_hisnaso___15" = "Parainfluenza 3",
  "patogenos_positivos_hisnaso___16" = "Parainfluenza 4",
  "patogenos_positivos_hisnaso___17" = "Virus Sincitial Respiratorio",
  "patogenos_positivos_hisnaso___18" = "Bordetella pertussis",
  "patogenos_positivos_hisnaso___19" = "Chlamydophila pneumonia",
  "patogenos_positivos_hisnaso___20" = "Mycoplasma pneumoniae",
  "patogenos_positivos_hisnaso___21" = "SARS-CoV-2",
  "Negativo_sangre" = "Negativo",
  "Negativo_hisnaso" = "Negativo"
)

columns_of_interest_biofire <- c( # removed dengue
  #"res_dengue",
  #"res_dengue_2",
  "patogenos_positivos_sangre___1",
  "patogenos_positivos_sangre___2",
  "patogenos_positivos_sangre___3",
  "patogenos_positivos_sangre___4",
  "patogenos_positivos_sangre___5",
  "patogenos_positivos_sangre___6",
  "patogenos_positivos_sangre___7",
  "patogenos_positivos_sangre___8",
  "patogenos_positivos_sangre___9",
  "patogenos_positivos_sangre___10",
  "patogenos_positivos_sangre___11",
  "patogenos_positivos_sangre___12",
  "patogenos_positivos_sangre___13",
  "patogenos_positivos_sangre___14",
  "patogenos_positivos_sangre___15",
  "patogenos_positivos_sangre___16",
  "patogenos_positivos_sangre___17",
  "patogenos_positivos_sangre___18",
  "patogenos_positivos_sangre___19",
  "patogenos_positivos_hisnaso___1",
  "patogenos_positivos_hisnaso___2",
  "patogenos_positivos_hisnaso___3",
  "patogenos_positivos_hisnaso___4",
  "patogenos_positivos_hisnaso___5",
  "patogenos_positivos_hisnaso___6",
  "patogenos_positivos_hisnaso___7",
  "patogenos_positivos_hisnaso___8",
  "patogenos_positivos_hisnaso___9",
  "patogenos_positivos_hisnaso___10",
  "patogenos_positivos_hisnaso___11",
  "patogenos_positivos_hisnaso___12",
  "patogenos_positivos_hisnaso___13",
  "patogenos_positivos_hisnaso___14",
  "patogenos_positivos_hisnaso___15",
  "patogenos_positivos_hisnaso___16",
  "patogenos_positivos_hisnaso___17",
  "patogenos_positivos_hisnaso___18",
  "patogenos_positivos_hisnaso___19",
  "patogenos_positivos_hisnaso___20",
  "patogenos_positivos_hisnaso___21",
  "Negativo_sangre",
  "Negativo_hisnaso"
)

# Create list without negatives separated by sample type
columns_sangre <- columns_of_interest_biofire[grep("sangre", # removed dengue
                                                   columns_of_interest_biofire)]
columns_sangre <- columns_sangre[columns_sangre != "Negativo_sangre"]

columns_hisnaso <- columns_of_interest_biofire[grep("hisnaso",
                                                    columns_of_interest_biofire)]
columns_hisnaso <- columns_hisnaso[columns_hisnaso != "Negativo_hisnaso"]

#for report tab
demo_lab_info$f_visita_f <- as.Date(demo_lab_info$f_visita_f)


# ------------------------------------------------------
# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----

# Define UI for Summary Tab
ui_tab_summary <- function() {
  fluidPage(
    titlePanel(NULL), # No large title since it's already in the main UI
    sidebarLayout(
      sidebarPanel(
        width = 2,  # Reduces the width of the sidebar
        radioButtons("language", "Idioma / Language:", 
                     choices = c("Español" = "es", "English" = "en"), 
                     selected = "es")
      ),
      mainPanel(
        uiOutput("summary_content")
      )
    )
  )
}
#ui_tab_summary <- function() { 
# fluidPage(
#  titlePanel(""),
# mainPanel(
#  h2("", style = "color: orange; text-align: center;")
#)
#)
#}

ui_tab3 <- function() { 
  fluidPage(
    titlePanel(""),
    
    sidebarLayout(
      sidebarPanel(
        width = 2,
        # Language selection moved to the sidebar
        radioButtons("language_biofire", "Idioma / Language:", 
                     choices = c("Español" = "es", "English" = "en"), 
                     selected = "es")
      ),
      
      mainPanel(
        # Study information
        h2(textOutput("header_biofire_text"), style = "color: orange;"),
        textOutput("info_biofire_text"),
        br(),
        # Date range input
        dateRangeInput("date_range_input_tab3", "Período del tiempo / Time period:",
                       start = "2020-06-29", end = Sys.Date(), separator = " a "),
        
        # Title and table centered
        div(style = "text-align: center;",
            h3("Paneles de Biofire")
        ),
        div(style = "text-align: center;",
            reactableOutput("table_tab3")
        ),
        
        # Combined plot
        div(style = "text-align: center;", 
            plotOutput("combined_plot_tab3", height = "400px", width = "80%")
        ),
        
        br(),
        
        # New section: Arbovirus Additional Tests
        div(style = "text-align: center; margin-top: 550px; margin-bottom: 30px;",
            h3("Pruebas Adicionales de Arboviruses")
        ),
        
        div(style = "display: flex; justify-content: center;",
            reactableOutput("arbovirus_table_tab3", width = "80%")
        ),
        
        # Horizontal line for separation
        hr(style = "border-top: 2px solid black; margin-top: 30px; margin-bottom: 30px;"),
        
        # Dengue PCR Section
        div(style = "text-align: center; margin-bottom: 10px;",
            h3("Dengue PCR")
        ),
        div(style = "display: flex; justify-content: center;",
            plotOutput("dengue_pcr_plot_tab3", height = "400px", width = "80%")
        ),
        
        br(),
        
        # Title for Dengue Plot section
        div(style = "text-align: center;", 
            h3("Dengue Pruebas Rápidas")  # Title before Dengue plot
        ),
        
        div(style = "display: flex; justify-content: center;",
            reactableOutput("dengue_table_tab3", width = "80%")
        ),
        br(),
        
        # Buttons to switch dengue test type
        div(style = "text-align: center; margin-bottom: 20px;",
            radioButtons("dengue_test_type", NULL,
                         choices = c("NS1", "IgG", "IgM"),
                         selected = "NS1",
                         inline = TRUE)  # Make buttons horizontal
        ),
        
        # Dengue plot below the line and further down
        div(style = "display: flex; justify-content: center;", 
            plotOutput("dengue_plot_tab3", height = "400px", width = "80%")
        ),
        br(),
        br()
        
      )
    )
  )
}


# Define UI for Tab 4 (GIHSN)
ui_tab4 <- function() { 
  fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        # Language selection
        radioButtons("language_gihsn", "Idioma / Language:", 
                     choices = c("Español" = "es", "English" = "en"), 
                     selected = "es"),
        # Date range input
        dateRangeInput("date_range_input_tab4", "Período del tiempo / Time period:",
                       start = "2025-03-04", end = Sys.Date(), separator = " a "),
        
        # Dropdown menu for selecting hospital
        radioButtons("hospital", "Hospital:",
                     choices = c("Ambos Hospitales",
                                 "Hospital de Coatepeque", 
                                 "Hospital de Chimaltenango")
        ),
        
        # Dropdown menu for selecting virus
        selectInput("virus", 
                    "Selecciona Virus(es) / Select Virus(es):",
                    choices = c("Todos Virus", "Influenza A y B", "Influenza A", "Influenza B", "SARS-CoV-2", "VSR"))
      ),
      
      mainPanel(
        # Add information about the study
        h2(textOutput("header_gihsn_text"), style = "color: orange;"),
        uiOutput("info_gihsn_text"),
        br(),
        br(),
        # Table output for the data summary
        DTOutput("summary_table_tab4", height="125px"),
        
        # Plot output for the graph
        plotOutput("disease_plot_tab4"),
        conditionalPanel(
          condition = "input.virus == 'Influenza A'",
          br(),
          h4("Distribución de Subtipos de Influenza A", style = "color: steelblue;"),
          plotOutput("influenza_a_subtypes_plot")),
        conditionalPanel(
          condition = "input.virus == 'Influenza A'",
          br(),
          h4("Distribución grupo de edad de individuos positivos por influenza A", style = "color: steelblue;"),
          plotOutput("influenza_a_ages")),
        plotOutput("disease_plot_tab4"),
        conditionalPanel(
          condition = "input.virus == 'Influenza A'",
          br(),
          h4("Distribución de Subtipos de Influenza A", style = "color: steelblue;"),
          plotOutput("influenza_a_subtypes_plot")),
        #grafica vsr
        conditionalPanel(
          condition = "input.virus == 'VSR'",
          br(),
          h4("Distribución grupo de edad de individuos positivos por VSR", style = "color: steelblue;"),
          plotOutput("vsr_ages")),
        conditionalPanel(
          condition = "input.virus == 'VSR'",
          br(),
          h4("Distribución de VSR por subgrupo", style = "color: steelblue;"),
          plotOutput("vsr_subgroups")),
        
        downloadButton("download_vsr_subgroups", "Download plot")
      )
    )
  )
}

# Define UI for Tab 5 (VIGICASA)
ui_tab5 <- function() { 
  fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        # Language selection
        radioButtons("language_VCasa", "Idioma / Language:", 
                     choices = c("Español" = "es", "English" = "en"), 
                     selected = "es"),
        # Date range input
        dateRangeInput("date_range_input_tab5", "Período del tiempo / Time period:",
                       start = "2025-06-16", end = Sys.Date(), separator = " a "),
        
        
        # Dropdown menu for selecting virus
        selectInput("virus", 
                    "Selecciona Virus(es) / Select Virus(es):",
                    choices = c("Todos Virus Respiratorios", "Influenza A y B", "Influenza A", "Influenza B", "SARS-CoV-2", "VSR", "Dengue")),
        # Conditionally show dengue test type radio buttons only when Dengue is selected
        conditionalPanel(
          condition = "input.virus == 'Dengue'",
          radioButtons("dengue_test_type_tab5", "Selecciona tipo de prueba Dengue / Select Dengue test:",
                       choices = c("NS1", "IgM", "IgG"),
                       selected = "NS1")
        )
      ),
      
      mainPanel(
        # Add information about the study
        h2(textOutput("header_VCasa_text"), style = "color: orange;"),
        uiOutput("info_VCasa_text"),
        br(),
        uiOutput("dynamic_plot_title"),
        conditionalPanel(
          condition = "input.virus != 'Dengue'",
          plotOutput("resp_plot_tab5")
        ),
        conditionalPanel(
          condition = "input.virus == 'Dengue'",
          plotOutput("dengue_plot_tab5")
        )
      )
    )
  )
}

# Define UI for Tab 6 (VIGIFINCA)
ui_tab6 <- function() { 
  fluidPage(
    tags$style(HTML("
      .sidebar {
        position: fixed;
        top: 120px;
        left: 15px;
        width: 300px;
        z-index: 1000;
        overflow-y: auto;
        max-height: calc(100vh - 100px);
        background: #f0f0f0;
        padding-right: 15px;
        padding-left: 15px;
      }
      .content {
        margin-left: 330px;
      }
    ")),
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        # Language selection
        radioButtons("language_VFinca", "Idioma / Language:", 
                     choices = c("Español" = "es", "English" = "en"), 
                     selected = "es"),
        # Date range input
        dateRangeInput("date_range_input_tab6", "Período del tiempo / Time period:",
                       start = "2025-06-16", end = Sys.Date(), separator = " a "),
        
        # Dropdown menu for selecting farm
        radioButtons("lugar", "Lugar:",
                     choices = c("Fincas de Banasa - Trifinio", 
                                 "Fincas de Pantaleon - Escuintla",
                                 "Ambos Sitios")
        ),
        
        # Dropdown menu for selecting virus
        selectInput("virus_tab6",
                    "Selecciona Virus(es) / Select Virus(es):",
                    choices = c("Todos Virus Respiratorios", "Influenza A y B", "Influenza A", "Influenza B", "SARS-CoV-2", "VSR", "Dengue")),
        
        # Conditionally show dengue test type radio buttons only when Dengue is selected
        conditionalPanel(
          condition = "input.virus_tab6 == 'Dengue'",
          radioButtons("dengue_test_type_tab6", "Selecciona tipo de prueba Dengue / Select Dengue test:",
                       choices = c("NS1", "IgM", "IgG"),
                       selected = "NS1")
        ),
        class = "sidebar"
      ),
      
      mainPanel(
        # Add information about the study
        h2(textOutput("header_VFinca_text"), style = "color: orange;"),
        uiOutput("info_VFinca_text"),
        br(),
        uiOutput("dynamic_plot_title_tab6"),
        
        conditionalPanel(
          condition = "input.virus_tab6 != 'Dengue'",
          plotOutput("resp_plot_tab6")
        ),
        conditionalPanel(
          condition = "input.virus_tab6 == 'Dengue'",
          plotOutput("dengue_plot_tab6")
        ),
        br(),
        br(),
        leafletOutput("map_tab6", height = "800px"),
        br(),
        br(),
        class = "content"
      )
    )
  )
}

#UI for tab report (7)
ui_tab7 <- function() {
  
  fluidPage(
    
    sidebarLayout(
      
      sidebarPanel(
        
        dateRangeInput(
          "cdc_dates",
          "Periodo",
          start = Sys.Date() - 30,
          end = Sys.Date()
        )
        
      ),
      
      mainPanel(
        
        h3("Enrolamiento"),
        
        DTOutput("cdc_tabla_comunidad"),
        br(),
        
        DTOutput("cdc_tabla_participantes"),
        br(),
        
        DTOutput("cdc_tabla_criterios"),
        
        hr(),
        
        h3("Vigilancia Respiratoria"),
        
        DTOutput("cdc_tabla_fichas"),
        br(),
        
        DTOutput("cdc_tabla_lab")
        
      )
      
    )
    
  )
  
}




# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Enfermedades Respiratorias y Febriles en Guatemala"),
  
  # Theme
  theme = shinytheme("united"),
  
  # Main panel content goes here
  tabsetPanel(
    # Define the three tabs
    tabPanel("Introducción", ui_tab_summary()),
    tabPanel("Estudio Biofire", ui_tab3()),
    tabPanel("Vigilancia GIHSN", ui_tab4()),
    tabPanel("VIGICASA", ui_tab5()),
    tabPanel("VIGIFINCA", ui_tab6()),
    tabPanel("CDC Dashboard", ui_tab7())
  )
)


# Define server logic ----
server <- function(input, output) {
  
  # ----------------------------------------------------------------------------
  #                               SUMMARY/INTRODUCCIÓN
  #----------------------------------------------------------------------------
  output$summary_content <- renderUI({
    if (input$language == "es") {
      tagList(
        h3(strong("Bienvenidos a nuestro Dashboard donde realizamos un seguimiento de las enfermedades respiratorias y febriles en Guatemala")),
        br(),
        p("Nuestros datos se publican semanalmente a partir de nuestros diversos proyectos de vigilancia en trabajadores agrícolas, comunidades y hospitales en el suroeste de Guatemala. Este proyecto es una colaboración entre la Fundación para la Salud Integral de los Guatemaltecos (FUNSALUD) y los Centros para el Control y la Prevención de Enfermedades (CDC). Trabajamos con trabajadores agrícolas de banano y sus familias, trabajadores de caña de azúcar, el Hospital Nacional de Coatepeque, el Hospital Nacional de Chimaltenango, así como con organizaciones a nivel mundial. Los datos de iteraciones anteriores de este estudio se pueden encontrar en https://funsaludinvestigacion.shinyapps.io/archivo_agri_agricasa/"),
        br(),
        img(src = "photos/drone_site_image.png", width = "100%"),
        tags$small(
          tags$a(href = "https://www.npr.org/sections/goatsandsoda/2023/02/19/1153911199/a-kid-in-guatemala-had-a-dream-today-shes-a-disease-detective",
                 tags$em("Luis Echeverria - NPR"), 
                 style = "color: black;", 
                 target = "_blank")
        ),
        br(),
        br(),
        p("FUNSALUD es una colaboración única entre AgroAmerica y la Universidad de Colorado dedicada a servir a las comunidades rurales del suroeste de Guatemala. El sitio ofrece atención clínica, programas de atención primaria en salud en las comunidades y sirve como un centro de investigación científica de clase mundial. FUNSALUD está ubicado entre plantaciones de banano en las tierras bajas de la costa, de la región del Trifinio."),
        br(),
        img(src = "photos/map_image.png", width = "100%"),
        br(),
        br(),
        p("El sitio ha realizado investigaciones durante más de 10 años, centrándose en las enfermedades infecciosas de alta carga regional, específicamente en la vigilancia de enfermedades respiratorias y febriles, en trabajadores agrícolas, hogares de la comunidad y personas hospitalizadas en los centros asistenciales locales. Trabajamos con un laboratorio catalogado nivel 4, que funciona principalmente con energía solar, cuenta con las pruebas diagnósticas básicas, capacidad de hacer PCR en tiempo real, serología y secuenciación, así como una variedad de otras pruebas de investigación."),
        br(),
        img(src = "photos/staff_photo_guate.jpg", width = "100%"),
        br(),
        br()
      )
    } else {
      tagList(
        h3("Welcome to our Dashboard tracking respiratory and febrile illnesses in Guatemala"),
        br(),
        p("Our data is published weekly from our various surveillance projects in farm workers, communities and hospitals across southwestern Guatemala. This project is a collaboration between la Fundación para la Salud Integral de los Guatemaltecos (FUNSALUD) and Centers for Disease Control (CDC). We work with banana farmworkers and their families, sugar cane farm workers, the Hospital Nacional de Coatepeque and the Hospital Nacional de Chimaltenango as well as organizations worldwide. Data from previous iterations of this study can be found at https://funsaludinvestigacion.shinyapps.io/archivo_agri_agricasa/"),
        br(),
        img(src = "photos/drone_site_image.png", width = "100%"),
        tags$small(
          tags$a(href = "https://www.npr.org/sections/goatsandsoda/2023/02/19/1153911199/a-kid-in-guatemala-had-a-dream-today-shes-a-disease-detective",
                 tags$em("Luis Echeverria - NPR"), 
                 style = "color: black;", 
                 target = "_blank")
        ),
        br(),
        br(),
        p("FUNSALUD is a unique partnership between AgroAmerica and the University of Colorado dedicated to serving rural communities in southwestern Guatemala. The site provides clinical care, community programs, and serves as a world-class research hub. FUNSALUD is located among the banana plantation in the coastal lowlands of the Trifinio region."),
        br(),
        img(src = "photos/map_image.png", width = "100%"),
        br(),
        br(),
        p("The site has conducted research for over 10+ years focusing primarily on regional high burden infectious diseases, specifically on respiratory and febrile disease surveillance in farm workers, community members and local hospitals. Our class four lab runs primarily on solar power, has basic diagnostic testing, real-time PCR capability, sequencing and serology as well as a variety of other research testing."),
        br(),
        img(src = "photos/staff_photo_guate.jpg", width = "100%"),
        br(),
        br()
      )
    }
  })
  
  # ----------------------------------------------------------------------------
  #                               HEADERS
  #----------------------------------------------------------------------------
  # Translatable headers
  output$header_biofire_text <- renderText({
    if (input$language_biofire == "es") {
      Header_Biofire
    } else {
      Header_Biofire_eng
    }
  })
  
  output$header_gihsn_text <- renderText({
    if (input$language_gihsn == "es") {
      Header_GIHSN
    } else {
      Header_GIHSN_eng
    }
  })
  
  output$header_VCasa_text <- renderText({
    if (input$language_VCasa == "es") {
      Header_VCasa
    } else {
      Header_VCasa_eng
    }
  })
  
  output$header_VFinca_text <- renderText({
    if (input$language_VFinca == "es") {
      Header_VFinca
    } else {
      Header_VFinca_eng
    }
  })
  
  # --------------------------------------------------------------------------
  #                             BIOFIRE
  # --------------------------------------------------------------------------
  
  # Reactive text output for study information
  output$info_biofire_text <- renderText({
    if (input$language_biofire == "es") {
      Info_Biofire  # Spanish version
    } else {
      Info_Biofire_eng  # English version
    }
  })
  
  # Count the number of positive and negative results for each
  # Calculate total counts of 1s for each column
  biofire_total_tested_pre <- namru_biofire_summary %>%
    group_by(epiweek_recoleccion) %>%
    dplyr::summarise(
      count_sangre_total = sum(result_sangre_complt == 1 | result_sangre_complt == 2, na.rm = TRUE), # removed dengue, check commit 9c2d9d0 for what gabi had, she used "maxes"
      count_nasof_total = sum(result_hispd_nasof == 1 | result_hispd_nasof == 2, na.rm = TRUE)
    )
  
  # Add negative column
  namru_biofire_summary_anonymized_wneg <- namru_biofire_summary %>%
    mutate(Negativo_sangre = ifelse(result_sangre_complt==2, 1, 0), # removed dengue
           Negativo_hisnaso = ifelse(result_hispd_nasof==2, 1, 0))
  
  # Function to create summary dataset for specified columns
  create_individual_summaries <- function(data, columns_to_summarize) {
    summaries <- lapply(columns_to_summarize, function(column) {
      summary <- data %>%
        group_by(epiweek_recoleccion) %>%
        summarise(count = sum(.data[[column]] == 1, na.rm = TRUE))
      names(summary)[2] <- paste("count_", column, sep="")
      return(summary)
    })
    
    # Merge summaries into one dataframe
    summary_data <- Reduce(function(x, y) full_join(x, y, by = "epiweek_recoleccion"), summaries)
    
    return(summary_data)
  }
  
  # Apply counting positive tests by week to all columns of interest
  namru_biofire_summary_counts_premerge <- create_individual_summaries(namru_biofire_summary_anonymized_wneg, columns_of_interest_biofire)
  
  # Add totals
  namru_biofire_summary_counts_pre <- merge(biofire_total_tested_pre, namru_biofire_summary_counts_premerge, by="epiweek_recoleccion", all=TRUE)
  # If epiweek is not recorded, do not count it
  namru_biofire_summary_counts_pre <- namru_biofire_summary_counts_pre %>%
    filter(!is.na(epiweek_recoleccion))
  
  # ISSUE: Including all weeks despite sparse testing
  # List of dates for epiweeks
  # Not all weeks are included, so we need to make a list of all Epiweeks and merge into dataset
  # Define start and end dates
  start_date <- as.Date("2020-06-29")
  end_date <- Sys.Date()
  # Generate sequence of dates from start to end
  all_dates <- seq(start_date, end_date, by = "1 day")
  # Filter for Mondays
  mondays <- all_dates[format(all_dates, "%A") == "Monday"]
  # Convert to format "YYYY-MM-DD"
  mondays <- format(mondays, "%Y-%m-%d")
  all_epiweeks <- tibble("epiweek_recoleccion" = mondays)
  namru_biofire_summary_counts <- merge(namru_biofire_summary_counts_pre,
                                        all_epiweeks, by="epiweek_recoleccion", all=TRUE)
  # Every time there is an NA in the dataset (because we just added a date in), replace with a 0
  namru_biofire_summary_counts[is.na(namru_biofire_summary_counts)] <- 0
  # Do same thing with all tests
  biofire_total_tested <- merge(biofire_total_tested_pre,
                                all_epiweeks, by="epiweek_recoleccion", all=TRUE)
  # Every time there is an NA in the dataset (because we just added a date in), replace with a 0
  biofire_total_tested[is.na(biofire_total_tested)] <- 0
  
  # Get list of column names
  colnames_namru_counts_pre <- colnames(namru_biofire_summary_counts)
  colnames_namru_counts <- setdiff(colnames_namru_counts_pre, c("epiweek_recoleccion"))
  colnames_namru_counts_sangre <- grep("^count_patogenos_positivos_sangre", # removed dengue
                                       colnames_namru_counts, value = TRUE)
  colnames_namru_counts_hisnaso <- grep("^count_patogenos_positivos_hisnaso", 
                                        colnames_namru_counts, value = TRUE)
  colnames_namru_counts_wtotals <- setdiff(colnames_namru_counts, c("epiweek_recoleccion",
                                                                    "count_sangre_total",
                                                                    "count_nasof_total"))
  colnames_namru_counts_wtotals_wneg <- setdiff(colnames_namru_counts, c("epiweek_recoleccion",
                                                                         "count_sangre_total",
                                                                         "count_nasof_total",
                                                                         "count_Negativo_sangre",
                                                                         "count_Negativo_hisnaso"))
  
  
  # Define reactive expression to filter and transform data
  filtered_biofire_data <- reactive({
    # Filter data based on selected date range
    namru_biofire_summary_counts_filtered <- subset(namru_biofire_summary_counts,
                                                    epiweek_recoleccion >= input$date_range_input_tab3[1] &
                                                      epiweek_recoleccion <= input$date_range_input_tab3[2])
    
    # Perform data transformation as needed
    namru_long_data <- namru_biofire_summary_counts_filtered %>%
      tidyr::pivot_longer(cols = colnames_namru_counts_wtotals_wneg, 
                          names_to = "pathogen_code", 
                          values_to = "count",
                          values_drop_na = TRUE) %>%
      mutate(pathogen_code = recode(pathogen_code, !!!setNames(names(pathogen_names), paste0("count_", names(pathogen_names)))),
             `Patógeno` = pathogen_names[as.character(pathogen_code)],
             `Todas pruebas de sangre` = count_sangre_total,
             `Todas pruebas naso/orofaríngeo` = count_nasof_total,
             `Tipo de Muestra` = case_when(
               grepl("sangre", pathogen_code, ignore.case = TRUE) ~ "Sangre", # removed dengue
               grepl("hisnaso", pathogen_code, ignore.case = TRUE) ~ "Naso/orofaríngeo",
               TRUE ~ NA_character_)) %>%
      dplyr::select(-c("count_sangre_total", "count_nasof_total"))
    
    namru_biofire_filtered_df <- namru_long_data %>%
      group_by(Patógeno)%>%
      dplyr::summarize(
        `# Personas Positivas` = sum(count, na.rm=TRUE),
        `Tipo de Muestra` = `Tipo de Muestra`)%>%
      slice(1)
    
    biofire_total_counts_sangre <- sum(namru_biofire_summary_counts_filtered$count_sangre_total,
                                       na.rm=TRUE)
    biofire_total_counts_nasof <- sum(namru_biofire_summary_counts_filtered$count_nasof_total,
                                      na.rm=TRUE)
    
    namru_biofire_filtered_df$`Todas Personas Probadas` <- ifelse(
      namru_biofire_filtered_df$`Tipo de Muestra`=="Sangre",
      biofire_total_counts_sangre,
      ifelse(
        namru_biofire_filtered_df$`Tipo de Muestra`=="Naso/orofaríngeo",
        biofire_total_counts_nasof, NA))
    
    namru_biofire_filtered_df
  })
  
  # Render the table using reactable
  output$table_tab3 <- renderReactable({
    reactable(filtered_biofire_data())
  })
  
  # Create graphs ----------------------------------------------------------------
  
  # Prepare Sample Count Data -----------------------------------------
  # Define reactive expression to filter and transform data
  filtered_biofire_plot_df <- reactive({
    # Filter data based on selected date range
    namru_biofire_summary_counts_filtered <- subset(namru_biofire_summary_counts,
                                                    epiweek_recoleccion >= input$date_range_input_tab3[1] &
                                                      epiweek_recoleccion <= input$date_range_input_tab3[2])
    
    # Perform data transformation as needed
    namru_long_data <- namru_biofire_summary_counts_filtered %>%
      tidyr::pivot_longer(cols = c(colnames_namru_counts_wtotals, count_Negativo_sangre, count_Negativo_hisnaso), 
                          names_to = "pathogen_code", 
                          values_to = "count",
                          values_drop_na = TRUE) %>%
      mutate(pathogen_code = recode(pathogen_code, !!!setNames(names(pathogen_names), paste0("count_", names(pathogen_names)))),
             `Patógeno` = pathogen_names[as.character(pathogen_code)],
             `Todas pruebas de sangre` = count_sangre_total,
             `Todas pruebas naso/orofaríngeo` = count_nasof_total,
             `Tipo de Muestra` = case_when(
               grepl("sangre", pathogen_code, ignore.case = TRUE) ~ "Sangre", # removed dengue
               grepl("hisnaso", pathogen_code, ignore.case = TRUE) ~ "Naso/orofaríngeo",
               TRUE ~ NA_character_)) %>%
      dplyr::select(-c("count_sangre_total", "count_nasof_total"))
    
    namru_long_data
  })
  
  # Create overall count ----------------------------
  filtered_biofire_totals_plot_df <- reactive({
    
    # Filter data based on selected date range
    biofire_total_tested_filtered <- subset(biofire_total_tested,
                                            epiweek_recoleccion >= input$date_range_input_tab3[1] &
                                              epiweek_recoleccion <= input$date_range_input_tab3[2])
    
    biofire_total_tested_filtered
  })
  
  # Create plot -------------------------------------
  # Render plot based on filtered data
  output$combined_plot_tab3 <- renderPlot({
    
    # Plot total number of tests
    # Get filtered totals data
    biofire_total_tested_filtered <- filtered_biofire_totals_plot_df()
    
    # Plot for total tests
    total_biofire_test_plot <- biofire_total_tested_filtered %>%
      mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion))%>%
      ggplot() +
      geom_bar(aes(x = epiweek_recoleccion, y = count_nasof_total),
               stat = "identity",  fill = "royalblue", alpha=0.6, color="black", size = 0.3) +
      geom_bar(aes(x = epiweek_recoleccion, y = count_sangre_total),
               fill = "red", stat = "identity", alpha=0.6, color="black", size = 0.3) +
      theme_classic() +
      labs(
        title = "\n
      \n
      \n
      Número de personas probadas por semana",
        x = "",
        y = ""
      ) +
      scale_y_continuous(breaks = seq(0, max(
        max(biofire_total_tested_filtered$count_nasof_total, na.rm = TRUE),
        max(biofire_total_tested_filtered$count_sangre_total, na.rm = TRUE)
      ),
      by = 1)) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(size = 12, face = "bold", hjust=0.5),  # Adjust title size and style
            axis.title.y = element_text(angle = 0, vjust = 0.5,face = "bold", margin = margin(r = -30)),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12, angle = 0, hjust = 1)  # Adjusted y-axis text size
            #axis.text.x = element_blank(),
            #axis.ticks.x = element_blank()
      )+
      scale_x_date(
        labels = format_date_spanish,
        limits = as.Date(c(input$date_range_input_tab3[1], input$date_range_input_tab3[2])),
        expand = c(0, 0)  # Remove extra space at ends
      )  
    
    # Get filtered data for both Sangre and Naso/orofaríngeo
    filtered_data <- filtered_biofire_plot_df() %>%
      mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion))
    
    # Define colors for each Tipo de Muestra
    color_palette <- c("Naso/orofaríngeo (7)" = "#1e214e",
                       "Naso/orofaríngeo (5)" = "#271c8a",
                       "Naso/orofaríngeo (4)" = "#3706c0",
                       "Naso/orofaríngeo (3)" = "#5339d1",
                       "Naso/orofaríngeo (2)" = "#7599eb",
                       "Naso/orofaríngeo (1)" = "lightblue",
                       "Naso/orofaríngeo (0)" = "white",
                       "Sangre (0)" = "white",
                       "Sangre (1)" = "#fe8181",
                       "Sangre (2)" = "#fe5757",
                       "Sangre (3)" = "#fe2e2e",
                       "Sangre (4)" = "#cb2424",
                       "Sangre (5)" = "#b62020")
    
    # Convert color_palette to a dataframe
    color_palette_df <- data.frame(
      category = names(color_palette),
      color = as.character(color_palette),
      stringsAsFactors = FALSE
    )
    
    filtered_data$forColor <-
      factor(paste0(filtered_data$`Tipo de Muestra`, " (", filtered_data$count , ")"))
    
    # Create a single ggplot object
    tile_plot <- filtered_data %>%
      arrange(`Tipo de Muestra`) %>%
      ggplot(aes(x = epiweek_recoleccion, y = forcats::fct_relevel(`Patógeno`, "Negativo"))) +
      geom_tile(aes(fill = forColor), color='black') +
      facet_wrap(~ `Tipo de Muestra`, ncol = 1, scales = "free_y") +  # Facet by Tipo de Muestra
      labs(
        title="Número de personas con un dado resultado",
        x = "Semana",
        y = "",
        fill= ""
      ) +
      theme_classic() +
      scale_x_date(
        labels = format_date_spanish,
        limits = as.Date(c(input$date_range_input_tab3[1], input$date_range_input_tab3[2])),
        expand = c(0, 0)  # Remove extra space at ends
      ) +
      #scale_fill_gradient(low = "#FFFFFF", high = "#FF0000")+
      theme(
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold", hjust=0.5),  # Adjust title size and style
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(fill = 'white'),
        axis.line = element_blank(), 
        axis.text.x = element_text(size = 12),
        plot.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, face = "bold", margin = margin(r = -10)),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 1)  # Adjusted y-axis text size and color
      )+
      scale_fill_manual(values = color_palette_df$color, breaks = color_palette_df$category)
    
    # Add the total count to the top of the chart
    combined_plot <- total_biofire_test_plot / tile_plot + plot_layout(heights = c(1, 3), widths = c(1, 1))
    
    # Display the combined plot with specified width and height
    combined_plot
  }, height = 900, width = 875) 
  
  # Create OTHER ARBOVIRUS table -------------------------------------
  output$arbovirus_table_tab3 <- renderReactable({
    req(input$date_range_input_tab3)
    
    arbo <- namru_biofire_summary %>%
      filter(epiweek_recoleccion >= input$date_range_input_tab3[1],
             epiweek_recoleccion <= input$date_range_input_tab3[2])
    
    arbovirus_summary <- tibble(
      Patógeno = c("Oropouche", "Otros Arboviruses (FLAV/GRCV/GROV/ALPHA)"),
      `# Personas Positivas` = c(
        sum(arbo$r_oropouche == 1, na.rm = TRUE),
        sum(arbo$r_arbovirus == 1, na.rm = TRUE)
      ),
      `Todas Personas Probadas` = c(
        sum(!is.na(arbo$r_oropouche)),
        sum(!is.na(arbo$r_arbovirus))
      )
    )
    
    reactable(arbovirus_summary, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
  
  # Create DENGUE PCR graph -------------------------------------
  output$dengue_pcr_plot_tab3 <- renderPlot({
    req(input$date_range_input_tab3)
    
    dengue_pcr <- namru_biofire_summary %>%
      filter(!is.na(r_dengue)) %>%
      mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion)) %>%
      filter(epiweek_recoleccion >= input$date_range_input_tab3[1],
             epiweek_recoleccion <= input$date_range_input_tab3[2])
    
    df_plot <- dengue_pcr %>%
      mutate(
        resultado = case_when(
          r_dengue == 2 ~ "Negativo",
          r_dengue == 4 ~ "DENV-1",
          r_dengue == 5 ~ "DENV-2",
          r_dengue == 6 ~ "DENV-3",
          TRUE ~ "Otro/Desconocido"
        )
      ) %>%
      count(epiweek_recoleccion, resultado) %>%
      tidyr::complete(epiweek_recoleccion, resultado, fill = list(n = 0))
    
    ggplot(df_plot, aes(x = epiweek_recoleccion, y = n, fill = resultado)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(
        "Negativo" = "#999999",
        "DENV-1" = "#E41A1C",
        "DENV-2" = "#377EB8",
        "DENV-3" = "#4DAF4A",
        "Otro/Desconocido" = "#CCCCCC"
      )) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      labs(x = "Semana Epidemiológica", y = "Número de Casos", fill = "Resultado PCR") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  
  # Create DENGUE table -------------------------------------
  output$dengue_table_tab3 <- renderReactable({
    req(input$date_range_input_tab3)
    
    start_date <- input$date_range_input_tab3[1]
    end_date <- input$date_range_input_tab3[2]
    
    filtered_data <- namru_biofire_summary %>%
      filter(epiweek_recoleccion >= start_date & epiweek_recoleccion <= end_date)
    
    dengue_summary <- tibble::tibble(
      `Tipo de Prueba` = c("Dengue NS1", "Dengue IgG", "Dengue IgM"),
      `# Personas Positivas` = c(
        sum(filtered_data$res_dengue == 1, na.rm = TRUE),
        sum(filtered_data$dengue_igg == 1, na.rm = TRUE),
        sum(filtered_data$dengue_igm == 1, na.rm = TRUE)
      ),
      `Todas Personas Probadas` = c(
        sum(!is.na(filtered_data$res_dengue)),
        sum(!is.na(filtered_data$dengue_igg)),
        sum(!is.na(filtered_data$dengue_igm))
      )
    )
    
    reactable::reactable(
      dengue_summary,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      pagination = FALSE,
      minRows = nrow(dengue_summary)
    )
  })
  
  # Create DENGUE plot -------------------------------------
  output$dengue_plot_tab3 <- renderPlot({
    start_date <- input$date_range_input_tab3[1]
    end_date <- input$date_range_input_tab3[2]
    test_type <- input$dengue_test_type  # Get selected test type
    
    # Map test type to column name and plot label
    test_column <- dplyr::case_when(
      test_type == "NS1" ~ "res_dengue",
      test_type == "IgG" ~ "dengue_igg",
      test_type == "IgM" ~ "dengue_igm"
    )
    
    # Filter and prepare data
    filtered_data <- namru_biofire_summary %>%
      mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion)) %>%
      filter(epiweek_recoleccion >= as.Date(start_date),
             epiweek_recoleccion <= as.Date(end_date))
    
    if (nrow(filtered_data) == 0 || sum(!is.na(filtered_data[[test_column]])) == 0) {
      plot.new()
      grid::grid.text("No hay datos disponibles para esta selección / No data available for this selection", 
                      x = 0.5, y = 0.5, 
                      gp = grid::gpar(fontsize = 16))
    } else {
      dengue_summary <- filtered_data %>%
        group_by(epiweek_recoleccion) %>%
        summarise(
          total_tested = sum(!is.na(.data[[test_column]])),
          total_pos_dengue = sum(.data[[test_column]] == 1, na.rm = TRUE),
          .groups = "drop"
        )
      
      ggplot(dengue_summary, aes(x = epiweek_recoleccion)) +
        geom_bar(aes(y = total_tested, fill = "Total Muestreados"), stat = "identity", alpha = 0.5) +
        geom_bar(aes(y = total_pos_dengue, fill = "Total Positivos"), stat = "identity") +
        scale_fill_manual(values = c("Total Muestreados" = "grey", "Total Positivos" = "red")) +
        scale_x_date(labels = format_date_spanish,
                     limits = as.Date(c(start_date, end_date)),
                     expand = c(0, 0)) +
        scale_y_continuous(
          breaks = seq(0, ceiling(max(c(dengue_summary$total_tested, dengue_summary$total_pos_dengue))), by = 1),
          labels = scales::comma,
          limits = c(0, NA)
        ) +
        labs(x = "Epiweek", y = "# Muestreados", fill = NULL) +
        theme_minimal() +
        theme(
          legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()
        )
    }
  })
  
  
  # --------------------------------------------------------------------------
  #                             GIHSN
  # --------------------------------------------------------------------------
  
  # Reactive text output for study information
  output$info_gihsn_text <- renderUI({
    if (input$language_gihsn == "es") {
      HTML(Info_GIHSN)  # Spanish version
    } else {
      HTML(Info_GIHSN_eng)  # English version
    }
  })
  
  
  ####### This is the table
  # Reactive expression for the filtered data based on date range and selected hospital
  # Filter data based on date range and hospital selection
  filtered_data_gihsn <- reactive({
    date_range <- input$date_range_input_tab4
    hospital_filter <- input$hospital
    
    # Convert the date range into epiweeks and years
    start_date <- as.Date(date_range[1])
    end_date <- as.Date(date_range[2])
    
    start_epiweek <- epiweek(start_date)  
    end_epiweek <- epiweek(end_date)
    start_year <- year(start_date)       
    end_year <- year(end_date)           
    
    # Ungroup the data if it has any grouping variables
    gihsn_summary_ungrouped <- gihsn_summary %>% ungroup()
    
    # If "Ambos Hospitales" is selected, combine both hospitals
    if (hospital_filter == "Ambos Hospitales") {
      filtered <- gihsn_summary_ungrouped %>%
        filter(
          (year == start_year & epiweek >= start_epiweek) |
            (year == end_year & epiweek <= end_epiweek) |
            (year > start_year & year < end_year)
        )
    } else {
      filtered <- gihsn_summary_ungrouped %>%
        filter(hospital == hospital_filter) %>%
        filter(
          (year == start_year & epiweek >= start_epiweek) |
            (year == end_year & epiweek <= end_epiweek) |
            (year > start_year & year < end_year)
        )
    }
    
    return(filtered)
  })
  
  output$summary_table_tab4 <- renderDT({
    filtered_data <- filtered_data_gihsn()
    
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      return(datatable(data.frame(Message = "No hay datos disponibles para esta selección / No data available for this selection")))
    }
    
    datatable(filtered_data %>%
                select(year, epiweek, total_tested, total_pos, sars_cov2_pos, inf_a_pos, inf_b_pos, vsr_pos) %>%
                rename(
                  Año = year,
                  Epiweek = epiweek,
                  `Total Muestreados` = total_tested,
                  `Total Positivos` = total_pos,
                  `Positivo Sars-CoV-2` = sars_cov2_pos,
                  `Positivo Influenza A` = inf_a_pos,
                  `Positivo Influenza B` = inf_b_pos,
                  `Positivo VSR` = vsr_pos
                ), options = list(searching = FALSE), rownames = FALSE)
  })
  
  ####### Now for the Graph
  output$disease_plot_tab4 <- renderPlot({
    # Get filtered data based on user selections
    filtered_data <- filtered_data_gihsn()
    
    # If there's no data after filtering, return an empty plot
    if (nrow(filtered_data) == 0) {
      return(ggplot() + labs(title = "No hay datos disponibles para esta selección / No data available for this selection"))
    }
    
    # Determine which variable to use for "Total Positivos"
    filtered_data <- filtered_data %>%
      mutate(
        total_pos_dynamic = case_when(
          input$virus == "Influenza A" ~ inf_a_pos,
          input$virus == "Influenza B" ~ inf_b_pos,
          input$virus == "Influenza A y B" ~ inf_a_pos + inf_b_pos,
          input$virus == "SARS-CoV-2" ~ sars_cov2_pos,
          input$virus == "VSR" ~ vsr_pos,
          TRUE ~ total_pos  # Default: all positives
        ),
        epiweek_label = paste(year, epiweek, sep = "-")
      )
    
    filtered_data <- filtered_data %>%
      arrange(year, epiweek) %>%
      mutate(
        epiweek_label = paste(year, epiweek, sep = "-"),
        epiweek_label = factor(
          epiweek_label,
          levels = unique(epiweek_label)
        )
      )
    
    # Filter to label every other week on the x-axis
    x_labels <- unique(filtered_data$epiweek_label)
    x_labels <- x_labels[seq(1, length(x_labels), by = 4)]
    
    # Generate the plot
    ggplot(filtered_data, aes(x = factor(epiweek_label))) +
      geom_bar(aes(y = total_tested, fill = "Total Muestreados"), stat = "identity", alpha = 0.5) +
      geom_bar(aes(y = total_pos_dynamic, fill = "Total Positivos"), stat = "identity") +
      scale_fill_manual(values = c("Total Muestreados" = "grey", "Total Positivos" = "skyblue")) +
      scale_y_continuous(
        breaks = seq(0, max(filtered_data$total_tested, na.rm = TRUE), by = 5)
      ) +
      scale_x_discrete(breaks = x_labels) +
      labs(x = "Semana epidemiológica", y = "# Muestreados", fill = "Resultado") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  output$influenza_a_subtypes_plot <- renderPlot({
    data <- filtered_data_gihsn()
    
    if (nrow(data) == 0) {
      return(ggplot() + labs(title = "No hay datos disponibles para Influenza A"))
    }
    
    # Convert epiweek & year into a proper date
    data <- data %>%
      mutate(epiweek_date = as.Date(paste(year, epiweek, 1), format = "%Y %U %u"))
    
    # Pivot data for Influenza A subtypes
    subtype_data <- data %>%
      select(epiweek, year, inf_a_h1n1, inf_a_h3n2, inf_a_nosub) %>%
      pivot_longer(cols = c(inf_a_h1n1, inf_a_h3n2, inf_a_nosub), 
                   names_to = "Subtype", values_to = "Count") %>%
      mutate(
        Subtype = recode(Subtype,
                         inf_a_h1n1 = "A(H1N1)",
                         inf_a_h3n2 = "A(H3N2)",
                         inf_a_nosub = "Sin subtipificar")
      )
    
    ggplot(subtype_data, aes(x = factor(paste(year, epiweek, sep = "-")), y = Count, fill = Subtype)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("A(H1N1)" = "#1b9e77", 
                                   "A(H3N2)" = "#d95f02", 
                                   "Sin subtipificar" = "#7570b3")) +
      labs(x = "Semana epidemiológica", y = "Casos positivos",
           fill = "Subtipo", title = "Subtipos de Influenza A") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  output$influenza_a_ages <- renderPlot({
    data <- gihsn_ages
    data$grupoetario <- factor(data$grupoetario, levels=c("<5", "5-49", "50-64", ">64"))
    ggplot(data, aes(n, grupoetario, fill=inf_a_final))+
      geom_col(color="black")+
      theme_bw()+ theme(legend.position = "none")+
      scale_y_discrete(drop=FALSE)+
      labs(x = "Número de resultados positivos a virus de la influenza tipo A", y="Grupo de edad")
    
  })
  
  output$vsr_ages <- renderPlot({
    data2 <- gihsn_ages_vsr
    data2$grupoetario <- factor(data2$grupoetario, levels=c("<5", "5-49", "50-64", ">64"))
    ggplot(data2, aes(n, grupoetario))+
      geom_col(color="black", fill="skyblue")+
      theme_bw()+ theme(legend.position = "none")+
      labs(x = "Número de resultados positivos a VSR", y="Grupo de edad")
    
  })
  
  vsr_subgroups_plot <- reactive({
    
    data <- filtered_data_gihsn()
    
    data <- data %>%
      mutate(epiweek_date = as.Date(paste(year, epiweek, 1), format = "%Y %U %u"))
    
    subgroup_data <- data %>%
      select(epiweek, year, vsr_a, vsr_b, vsr_nosub) %>%
      pivot_longer(
        cols = c(vsr_a, vsr_b, vsr_nosub),
        names_to = "Subgroup",
        values_to = "Count"
      ) %>%
      mutate(
        Subgroup = recode(
          Subgroup,
          vsr_a = "RSV A",
          vsr_b = "RSV B",
          vsr_nosub = "No subgroup"
        )
      ) %>%
      arrange(year, epiweek) %>%
      mutate(
        week_label = paste(year, epiweek, sep = "-"),
        week_label = factor(week_label, levels = unique(week_label))
      )
    
    ggplot(
      subgroup_data,
      aes(x = week_label, y = Count, fill = Subgroup)
    ) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(
        "RSV A" = "#1b9e77",
        "RSV B" = "#d95f02",
        "No subgroup" = "#7570b3"
      )) +
      xlab("Epidemiological week")+
      scale_x_discrete(
        breaks = levels(subgroup_data$week_label)[seq(1, length(levels(subgroup_data$week_label)), by = 4)]
      )+
      ggtitle("Distribution of RSV cases by subgroup")+
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 23, hjust = 1),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        plot.title = element_text(
          size = 25,
          face = "bold",
          hjust = 0.5
        ),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_text(size = 21),
        axis.text.y = element_text(size = 23)
      )
  })
  
  output$vsr_subgroups <- renderPlot({
    vsr_subgroups_plot()
  })
  
  output$download_vsr_subgroups <- downloadHandler(
    filename = function() {
      paste0("RSV_subgroups_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = vsr_subgroups_plot(),
        width = 12,
        height = 6,
        dpi = 300
      )
    }
  )
  
  # --------------------------------------------------------------------------
  #                             VIGICASA
  # --------------------------------------------------------------------------
  output$info_VCasa_text <- renderText({
    if (input$language_VCasa == "es") {
      Info_VCasa  # Spanish version
    } else {
      Info_VCasa_eng  # English version
    }
  })
  
  output$dynamic_plot_title <- renderUI({
    title_text <- if (input$virus == "Dengue") {
      "Tasa de Positividad de Dengue"
    } else {
      "Tasa de Positividad de Enfermedades Respiratorias"
    }
    tags$h2(title_text,
            style = "color: black; font-weight: bold; font-size: 24px; text-align: center; margin-bottom: 20px;")
  })
  
  filtered_data_vigicasa <- reactive({
    vigicasa_summary %>%
      filter(
        source == "Resp",
        epiweek_date >= input$date_range_input_tab5[1],
        epiweek_date <= input$date_range_input_tab5[2]
      )
  })
  
  output$resp_plot_tab5 <- renderPlot({
    filtered_data <- vigicasa_summary %>%
      filter(
        source == "Resp",
        epiweek_date >= input$date_range_input_tab5[1],
        epiweek_date <= input$date_range_input_tab5[2]
      )
    
    if (nrow(filtered_data) == 0) {
      return(
        ggplot() + labs(title = "No hay datos disponibles para esta selección / No data available for this selection")
      )
    }
    
    # Create full range of year and epiweeks in selected date range
    full_weeks <- vigicasa_summary %>%
      filter(
        source == "Resp",
        epiweek_date >= input$date_range_input_tab5[1],
        epiweek_date <= input$date_range_input_tab5[2]
      ) %>%
      distinct(year, epiweek, epiweek_date) %>%
      complete(nesting(year), epiweek = full_seq(epiweek, 1)) %>%
      mutate(epiweek_label = factor(paste(year, epiweek, sep = "-")))
    
    # Join to filtered data
    filtered_data <- full_weeks %>%
      left_join(filtered_data, by = c("year", "epiweek", "epiweek_date"))
    
    # Recalculate dynamic columns and handle NAs
    filtered_data <- filtered_data %>%
      mutate(
        inf_a_pos = replace_na(inf_a_pos, 0),
        inf_a_neg = replace_na(inf_a_neg, 0),
        inf_b_pos = replace_na(inf_b_pos, 0),
        inf_b_neg = replace_na(inf_b_neg, 0),
        sars_cov2_pos = replace_na(sars_cov2_pos, 0),
        sars_cov2_neg = replace_na(sars_cov2_neg, 0),
        vsr_pos = replace_na(vsr_pos, 0),
        vsr_neg = replace_na(vsr_neg, 0),
        total_pos = replace_na(total_pos, 0),
        total_tested = replace_na(total_tested, 0)
      ) %>%
      mutate(
        total_pos_dynamic = case_when(
          input$virus == "Influenza A" ~ inf_a_pos,
          input$virus == "Influenza B" ~ inf_b_pos,
          input$virus == "Influenza A y B" ~ inf_a_pos + inf_b_pos,
          input$virus == "SARS-CoV-2" ~ sars_cov2_pos,
          input$virus == "VSR" ~ vsr_pos,
          TRUE ~ total_pos
        ),
        total_tested_dynamic = case_when(
          input$virus == "Influenza A" ~ inf_a_pos + inf_a_neg,
          input$virus == "Influenza B" ~ inf_b_pos + inf_b_neg,
          input$virus == "Influenza A y B" ~ inf_a_pos + inf_a_neg + inf_b_pos + inf_b_neg,
          input$virus == "SARS-CoV-2" ~ sars_cov2_pos + sars_cov2_neg,
          input$virus == "VSR" ~ vsr_pos + vsr_neg,
          TRUE ~ total_tested
        ),
        epiweek_label = factor(paste(year, epiweek, sep = "-"), levels = paste(full_weeks$year, full_weeks$epiweek, sep = "-"))
      )
    week_breaks <- levels(filtered_data$epiweek_label)[
      seq(1, length(levels(filtered_data$epiweek_label)), by = 4)
    ]
    ggplot(filtered_data, aes(x = epiweek_label)) +
      geom_bar(aes(y = total_tested_dynamic, fill = "Total Muestreados"), stat = "identity", alpha = 0.4) +
      geom_bar(aes(y = total_pos_dynamic, fill = "Total Positivos"), stat = "identity") +
      scale_fill_manual(values = c("Total Muestreados" = "grey", "Total Positivos" = "red")) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 10),
        labels = function(x) floor(x)
      ) +
      labs(x = "Semana epidemiológica", y = "# Muestreados", fill = "Resultado") +
      scale_x_discrete(breaks = week_breaks)+
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  output$dengue_plot_tab5 <- renderPlot({
    
    # 1. Create full sequence of epiweeks in date range
    full_weeks <- vigicasa_summary %>%
      filter(source == "Deng") %>%
      distinct(epiweek_date, epiweek, year) %>%
      arrange(epiweek_date) %>%
      filter(
        epiweek_date >= input$date_range_input_tab5[1],
        epiweek_date <= input$date_range_input_tab5[2]
      ) %>%
      mutate(epiweek_label = factor(paste(year, epiweek, sep = "-")))
    
    # 2. Filter actual data in the range
    filtered_data <- vigicasa_summary %>%
      filter(
        source == "Deng",
        epiweek_date >= input$date_range_input_tab5[1],
        epiweek_date <= input$date_range_input_tab5[2]
      )
    
    # 3. Determine which dengue test pos column to use
    test_column <- case_when(
      input$dengue_test_type_tab5 == "NS1" ~ "ns1_pos",
      input$dengue_test_type_tab5 == "IgM" ~ "igm_pos",
      input$dengue_test_type_tab5 == "IgG" ~ "igg_pos"
    )
    
    # 4. Handle no data case for full_weeks (important for x-axis)
    if (nrow(full_weeks) == 0) {
      return(
        ggplot() + labs(title = "No hay datos disponibles para esta selección / No data available for this selection")
      )
    }
    
    # 5. Prepare filtered_data with pos counts and epiweek labels
    filtered_data <- filtered_data %>%
      mutate(
        total_pos_dengue = .data[[test_column]]
      ) %>%
      select(epiweek_date, epiweek, year, total_pos_dengue, total_tested)
    
    # 6. Left join with full_weeks to ensure all epiweeks appear
    full_data <- full_weeks %>%
      left_join(filtered_data, by = c("epiweek_date", "epiweek", "year")) %>%
      mutate(
        total_pos_dengue = replace_na(total_pos_dengue, 0),
        total_tested = replace_na(total_tested, 0),
        epiweek_label = factor(paste(year, epiweek, sep = "-"), levels = full_weeks$epiweek_label)
      )
    
    # 7. Plot with complete epiweek axis
    ggplot(full_data, aes(x = epiweek_label)) +
      geom_bar(aes(y = total_tested, fill = "Total Muestreados"), stat = "identity", alpha = 0.4) +
      geom_bar(aes(y = total_pos_dengue, fill = "Total Positivos"), stat = "identity") +
      scale_fill_manual(values = c("Total Muestreados" = "grey", "Total Positivos" = "red")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::label_number(accuracy = 1)) +
      labs(x = "Semana epidemiológica", y = "# Muestreados", fill = "Resultado") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  
  # --------------------------------------------------------------------------
  #                             VIGIFINCA
  # --------------------------------------------------------------------------
  output$info_VFinca_text <- renderText({
    if (input$language_VFinca == "es") {
      Info_VFinca  # Spanish version
    } else {
      Info_VFinca_eng  # English version
    }
  })
  
  output$dynamic_plot_title_tab6 <- renderUI({
    title_text <- if (input$virus_tab6 == "Dengue") {
      "Tasa de Positividad de Dengue"
    } else {
      "Tasa de Positividad de Enfermedades Respiratorias"
    }
    tags$h2(title_text,
            style = "color: black; font-weight: bold; font-size: 24px; text-align: center; margin-bottom: 20px;")
  })
  
  filtered_data_vigifinca <- reactive({
    data <- vigifinca_summary %>%
      filter(
        source == "Resp",
        epiweek_date >= input$date_range_input_tab6[1],
        epiweek_date <= input$date_range_input_tab6[2]
      )
    
    # Filter by lugar
    data <- data %>%
      filter(
        case_when(
          input$lugar == "Fincas de Banasa - Trifinio" ~ lugar == "Banasa",
          input$lugar == "Fincas de Pantaleon - Escuintla" ~ lugar == "Pantaleon",
          input$lugar == "Ambos Sitios" ~ TRUE,
          TRUE ~ FALSE  # fallback safety
        )
      )
    
    return(data)
  })
  
  output$resp_plot_tab6 <- renderPlot({
    filtered_data <- filtered_data_vigifinca()
    
    if (nrow(filtered_data) == 0) {
      return(
        ggplot() + labs(title = "No hay datos disponibles para esta selección / No data available for this selection")
      )
    }
    
    # Create full range of year and epiweeks in selected date range
    full_weeks <- vigifinca_summary %>%
      filter(
        epiweek_date >= input$date_range_input_tab6[1],
        epiweek_date <= input$date_range_input_tab6[2]
      ) %>%
      distinct(year, epiweek, epiweek_date) %>%
      complete(nesting(year), epiweek = full_seq(epiweek, 1)) %>%
      mutate(epiweek_label = factor(paste(year, epiweek, sep = "-")))
    
    # Join to filtered data
    filtered_data <- full_weeks %>%
      left_join(filtered_data, by = c("year", "epiweek", "epiweek_date"))
    
    # Replace NAs with 0 for all relevant count columns
    filtered_data <- filtered_data %>%
      mutate(
        inf_a_pos = replace_na(inf_a_pos, 0),
        inf_a_neg = replace_na(inf_a_neg, 0),
        inf_b_pos = replace_na(inf_b_pos, 0),
        inf_b_neg = replace_na(inf_b_neg, 0),
        sars_cov2_pos = replace_na(sars_cov2_pos, 0),
        sars_cov2_neg = replace_na(sars_cov2_neg, 0),
        vsr_pos = replace_na(vsr_pos, 0),
        vsr_neg = replace_na(vsr_neg, 0),
        total_pos = replace_na(total_pos, 0),
        total_tested = replace_na(total_tested, 0)
      ) %>%
      mutate(
        total_pos_dynamic = case_when(
          input$virus_tab6 == "Influenza A" ~ inf_a_pos,
          input$virus_tab6 == "Influenza B" ~ inf_b_pos,
          input$virus_tab6 == "Influenza A y B" ~ inf_a_pos + inf_b_pos,
          input$virus_tab6 == "SARS-CoV-2" ~ sars_cov2_pos,
          input$virus_tab6 == "VSR" ~ vsr_pos,
          TRUE ~ total_pos
        ),
        total_tested_dynamic = case_when(
          input$virus_tab6 == "Influenza A" ~ inf_a_pos + inf_a_neg,
          input$virus_tab6 == "Influenza B" ~ inf_b_pos + inf_b_neg,
          input$virus_tab6 == "Influenza A y B" ~ inf_a_pos + inf_a_neg + inf_b_pos + inf_b_neg,
          input$virus_tab6 == "SARS-CoV-2" ~ sars_cov2_pos + sars_cov2_neg,
          input$virus_tab6 == "VSR" ~ vsr_pos + vsr_neg,
          TRUE ~ total_tested
        ),
        epiweek_label = factor(paste(year, epiweek, sep = "-"), levels = paste(full_weeks$year, full_weeks$epiweek, sep = "-"))
      )
    week_breaks <- levels(filtered_data$epiweek_label)[
      seq(1, length(levels(filtered_data$epiweek_label)), by = 4)]
    ggplot(filtered_data, aes(x = epiweek_label)) +
      geom_bar(aes(y = total_tested_dynamic, fill = "Total Muestreados"), stat = "identity", alpha = 0.4) +
      geom_bar(aes(y = total_pos_dynamic, fill = "Total Positivos"), stat = "identity") +
      scale_fill_manual(values = c("Total Muestreados" = "grey", "Total Positivos" = "red")) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 10),
        labels = function(x) floor(x)
      ) +
      labs(x = "Semana epidemiológica", y = "# Muestreados", fill = "Resultado") +
      scale_x_discrete(breaks = week_breaks)+
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  
  output$dengue_plot_tab6 <- renderPlot({
    filtered_data <- vigifinca_summary %>%
      filter(
        source == "Deng",
        epiweek_date >= input$date_range_input_tab6[1],
        epiweek_date <= input$date_range_input_tab6[2],
        case_when(
          input$lugar == "Fincas de Banasa - Trifinio" ~ lugar == "Banasa",
          input$lugar == "Fincas de Pantaleon - Escuintla" ~ lugar == "Pantaleon",
          input$lugar == "Ambos Sitios" ~ TRUE
        )
      )
    
    test_column <- case_when(
      input$dengue_test_type_tab6 == "NS1" ~ "ns1_pos",
      input$dengue_test_type_tab6 == "IgM" ~ "igm_pos",
      input$dengue_test_type_tab6 == "IgG" ~ "igg_pos"
    )
    
    if (nrow(filtered_data) == 0) {
      return(
        ggplot() + labs(title = "No hay datos disponibles para esta selección / No data available for this selection")
      )
    }
    
    filtered_data <- filtered_data %>%
      mutate(
        total_pos_dengue = .data[[test_column]],
        epiweek_label = factor(paste(year, epiweek, sep = "-"))
      )
    week_breaks <- levels(filtered_data$epiweek_label)[
      seq(1, length(levels(filtered_data$epiweek_label)), by = 4)
    ]
    
    ggplot(filtered_data, aes(x = epiweek_label)) +
      geom_bar(aes(y = total_tested, fill = "Total Muestreados"), stat = "identity", alpha = 0.4) +
      geom_bar(aes(y = total_pos_dengue, fill = "Total Positivos"), stat = "identity") +
      scale_fill_manual(values = c("Total Muestreados" = "grey", "Total Positivos" = "red")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # integer-like tick spacing 
      labs(x = "Semana epidemiológica", y = "# Muestreados", fill = "Resultado") +
      scale_x_discrete(breaks = week_breaks)+
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  ########## MAP MAP MAP  
  # Reactive: filter & summarize based on inputs
  filtered_data_tab5_map <- reactive({
    df <- vigifinca_joined %>%
      filter(
        fecha_muestra >= input$date_range_input_tab6[1],
        fecha_muestra <= input$date_range_input_tab6[2]
      )
    
    # Filter by lugar
    if (input$lugar == "Fincas de Banasa - Trifinio") {
      df <- df %>% filter(lugar == "Banasa")
    } else if (input$lugar == "Fincas de Pantaleon - Escuintla") {
      df <- df %>% filter(lugar == "Pantaleon")
    } 
    # "Ambos Sitios" = no filter
    
    # Filter by source depending on virus selection
    if (input$virus_tab6 == "Dengue") {
      df <- df %>% filter(source == "Deng")
    } else {
      df <- df %>% filter(source == "Resp")
    }
    
    # Calculate virus-specific tested/pos/positivity
    df <- df %>%
      group_by(municipio, geometry) %>%
      summarise(
        total_tested = sum(total_tested, na.rm = TRUE),
        total_pos = sum(total_pos, na.rm = TRUE),
        sars_cov2_pos = sum(sars_cov2_pos, na.rm = TRUE),
        sars_cov2_neg = sum(sars_cov2_neg, na.rm = TRUE),
        inf_a_pos = sum(inf_a_pos, na.rm = TRUE),
        inf_a_neg = sum(inf_a_neg, na.rm = TRUE),
        inf_b_pos = sum(inf_b_pos, na.rm = TRUE),
        inf_b_neg = sum(inf_b_neg, na.rm = TRUE),
        vsr_pos = sum(vsr_pos, na.rm = TRUE),
        vsr_neg = sum(vsr_neg, na.rm = TRUE),
        ns1_pos = sum(ns1_pos, na.rm = TRUE),
        ns1_neg = sum(ns1_neg, na.rm = TRUE),
        igm_pos = sum(igm_pos, na.rm = TRUE),
        igm_neg = sum(igm_neg, na.rm = TRUE),
        igg_pos = sum(igg_pos, na.rm = TRUE),
        igg_neg = sum(igg_neg, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        pos = case_when(
          input$virus_tab6 == "Todos Virus Respiratorios" ~ total_pos,
          input$virus_tab6 == "SARS-CoV-2" ~ sars_cov2_pos,
          input$virus_tab6 == "Influenza A y B" ~ (inf_a_pos + inf_b_pos),
          input$virus_tab6 == "Influenza A" ~ inf_a_pos,
          input$virus_tab6 == "Influenza B" ~ inf_b_pos,
          input$virus_tab6 == "VSR" ~ vsr_pos,
          input$virus_tab6 == "Dengue" & input$dengue_test_type_tab6 == "NS1" ~ ns1_pos,
          input$virus_tab6 == "Dengue" & input$dengue_test_type_tab6 == "IgM" ~ igm_pos,
          input$virus_tab6 == "Dengue" & input$dengue_test_type_tab6 == "IgG" ~ igg_pos,
          TRUE ~ NA_real_
        ),
        tested = case_when(
          input$virus_tab6 == "Todos Virus Respiratorios" ~ total_tested,
          input$virus_tab6 == "SARS-CoV-2" ~ (sars_cov2_pos + sars_cov2_neg),
          input$virus_tab6 == "Influenza A y B" ~ (inf_a_pos + inf_a_neg + inf_b_pos + inf_b_neg),
          input$virus_tab6 == "Influenza A" ~ (inf_a_pos + inf_a_neg),
          input$virus_tab6 == "Influenza B" ~ (inf_b_pos + inf_b_neg),
          input$virus_tab6 == "VSR" ~ (vsr_pos + vsr_neg),
          input$virus_tab6 == "Dengue" & input$dengue_test_type_tab6 == "NS1" ~ (ns1_pos + ns1_neg),
          input$virus_tab6 == "Dengue" & input$dengue_test_type_tab6 == "IgM" ~ (igm_pos + igm_neg),
          input$virus_tab6 == "Dengue" & input$dengue_test_type_tab6 == "IgG" ~ (igg_pos + igg_neg),
          TRUE ~ NA_real_
        ),
        incidence = pos / tested,
        incidence = ifelse(is.nan(incidence) | is.infinite(incidence), 0, incidence)
      )
    
    df
  })
  
  
  # Reactive color palette based on incidence
  pal <- reactive({
    colorNumeric(
      palette = "YlOrRd",
      domain = filtered_data_tab5_map()$incidence,
      na.color = "transparent"
    )
  })
  
  # Reactive labels for tooltip
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>Muestreados: %d<br/>Positivos: %d<br/>Tasa de Positividad: %.2f%%",
      toupper(filtered_data_tab5_map()$municipio),
      filtered_data_tab5_map()$tested,
      filtered_data_tab5_map()$pos,
      100 * filtered_data_tab5_map()$incidence
    ) %>% lapply(htmltools::HTML)
  })
  
  output$map_tab6 <- renderLeaflet({
    
    leaflet() %>%
      # No addTiles(), so no basemap tiles loaded
      
      # Base layer: all municipalities outlines, no fill
      addPolygons(
        data = guate_json,
        fillColor = "transparent",
        color = "black",
        weight = 1,
        opacity = 1,
        label = ~toupper(municipio),  # simple label showing municipio name
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      
      # Data layer: filtered municipalities with incidence fill
      addPolygons(
        data = filtered_data_tab5_map(),
        fillColor = ~pal()(incidence),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      
      addLegend(
        pal = pal(),
        values = filtered_data_tab5_map()$incidence,
        opacity = 0.7,
        title = "Tasa de Positividad",
        position = "bottomright",
        labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x)
      ) %>%
      
      setView(lng = -86.7, lat = 15.8, zoom = 7.5)
  })
  
  cdc_fichas_filtradas <- reactive({
    
    demo_lab_info %>%
      filter(
        between(
          f_visita_f,
          input$cdc_dates[1],
          input$cdc_dates[2]
        )
      )
    
  })
  
  output$cdc_tabla_comunidad <- renderDT({
    
    datatable(conteo_comunidad)
    
  })
  
  output$cdc_tabla_participantes <- renderDT({
    
    datatable(conteo_individuo)
    
  })
  
  output$cdc_tabla_criterios <- renderDT({
    
    datatable(conteo_criterio)
    
  })
  
  output$cdc_tabla_fichas <- renderDT({
    
    cdc_fichas_filtradas() %>%
      count(sitio) %>%
      datatable()
    
  })
  
  output$cdc_tabla_lab <- renderDT({
    
    cdc_fichas_filtradas() %>%
      
      pivot_longer(
        cols = starts_with("virus_detectado"),
        names_to = "test",
        values_to = "result"
      ) %>%
      
      filter(result == "1") %>%
      
      mutate(
        test = recode(
          test,
          "virus_detectado___1" = "Negativo",
          "virus_detectado___6" = "Inválido",
          "virus_detectado___2" = "Influenza A",
          "virus_detectado___3" = "Influenza B",
          "virus_detectado___4" = "SARS-CoV-2",
          "virus_detectado___5" = "VSR"
        )
      ) %>%
      
      count(test) %>%
      
      datatable()
    
  })
  
}

shinyApp(ui, server)

# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc