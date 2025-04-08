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

# Load dataframes -----------------------------------------------------------------------
influenza_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/influenza_summary_updated.csv")
influenza_symptom_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/influenza_symptom_summary_updated.csv")
agri_casa_symptom_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/agri_casa_summary_updated.csv")
agri_casa_incidence_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/agri_casa_incidence_summary_updated.csv")
namru_biofire_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/namru_biofire_summary_updated.csv")
gihsn_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/gihsn_summary.csv")

# Information about each study ------------------------
Header_Agri <- "Estudio Agri: Síntomas de enfermedades respiratorias en trabajadores agrícolas"
  
Header_Agri_eng <- "Agri Study: Respiratory illness symptoms in farm workers"

Info_Agri <- "Este estudio es una vigilancia en las fincas de AgroAmerica con trabajadores Agrícolas del area de Banasa. Trabajadores con síntomas 
son captados en su lugar de trabajo y posteriormente cada semana entre 8-15 trabajadores agrícolas son contactados por teléfono quienes responden a 
preguntas sobre su salud. Si experimentan síntomas que indiquen una infección respiratoria, como tos seca, fiebre, y dificultad para respirar, el equipo de 
enfermería de investigación de FUNSALUDGUATE-CU, procederá a tomar una muestra. Después de realizar pruebas de PCR, el equipo sigue a las personas con síntomas 
por un mes, haciendo visitas durante los 7 y 28 días después de la recolección de las muestras. El objetivo es entender cuales síntomas están asociados con que 
infección respiratoria y la carga clínica y económica."

Info_Agri_eng <- "This study is an agricultural worker disease surveillance in AgroAmerica's Banasa farms. Workers with symptoms are identified in their workplace and each week
between 8-15 agricultural workers are contacted by phone to answer questions about their health. If they experience symptoms indicating a respiratory infection, such as cough, fever, or difficulty breathing 
the research nursing team from FUNSALUDGUATE-CU collects a nasal swab. After conducting PCR tests, the team follows up with symptomatic individuals for a month, making visits on days 7 and 28 after sample collection. 
The goal is to understand which symptoms are associated with specific respiratory infections and their clinical and economic burdens."

Header_AgriCasa <- "Estudio AgriCasa: síntomas y número de personas con resultados positivos por semana"
  
Header_AgriCasa_eng <- "AgriCasa study: symptoms and number of people with positive tests per week"
  
Info_AgriCasa <- "Ciento cincuentas casas están inscritas en este estudio. Cada semana en la visita de rutina, el equipo de campo de los enfermeros de investigación, 
preguntan a cada miembro de la familia si se sienten bien de salud. Si algún miembro refiere tener síntomas, se le hace una prueba para SARS-COV-2, VSR, e Influenza A/B. 
Si algún miembro de la familia tiene una prueba positiva toda la familia recibirá dos visitas intensivas cada semana para entender la transmisión de estas enfermedades 
respiratorias entre la casa y de la casa a la finca y viceversa."

Info_AgriCasa_eng <- "One hundred and fifty households are enrolled in this study. Each week, during the routine visit, the field team of research nurses asks each family member 
if they are feeling well. If any member reports having symptoms, they are tested for SARS-CoV-2, RSV, and Influenza A/B. If any family member tests positive, the entire household 
will receive two intensive visits each week to understand the transmission of these respiratory diseases within the home, between the home and the farm, and vice versa."

Header_Biofire <- "Estudio Biofire: Pruebas de Enfermedades Infecciosas y Febriles"
  
Header_Biofire_eng <- "Biofire Study: Respiratory and Febrile Illness Tests"

Info_Biofire <- "En el Hospital Nacional de Coatepeque, a los pacientes que experimentan tos se les corre un panel respiratorio de Biofire. A los pacientes que experimentan fiebre 
(sin tos) se les corre una prueba para identificar  antígeno NS1 de Dengue. Dependiendo en la disponabilidad de pruebas, a los sujetos que salen con una prueba negativa para Dengue 
en la prueba de tamizaje, se les corre el panel febril de Biofire. Los paneles  de Biofire son pruebas de PCR para un amplio abanico de posibles resultados de enfermedades infecciosas. 
Los resultados acumulados de las pruebas de Biofire y Dengue NS1 están compartidos aquí, organizados por semana epidemiológica."

Info_Biofire_eng <- "At the National Hospital of Coatepeque, patients who experience cough are tested with a Biofire respiratory panel. Patients who experience fever (without cough) are tested for 
the Dengue NS1 antigen. Depending on the availability of tests, subjects who test negative for Dengue in the screening test have their sample tested on the Biofire febrile panel. Biofire panels are 
PCR tests for a wide range of possible infectious disease outcomes. The cumulative results of the Biofire and Dengue NS1 tests are shared here, organized by epidemiological week."

Header_GIHSN <- "Red Mundial de Vigilancia de Influenza en Hospitales"
  
Header_GIHSN_eng <- "Global Influenza Hospital Surveillance Network"
  
Info_GIHSN <- "Somos un sitio que forma parte de la Red Mundial de Vigilancia de Influenza (<a href='https://gihsn.org' target='_blank'>GIHSN</a>). En el Hospital Nacional 
de Coatepeque y el Hospital de Chimaltenango. Forman parte de esta vigilancia los pacientes que experimenten síntomas de fiebre y/o tos, a ellos se les corren pruebas de 
Sars-CoV-2, Influenza A/B y VSR y se secuencia los resultados positivos. Todos los datos son compartidos con la red mundial para mejorar capacidad de vigilancia, la decisiones 
vacunales de Influenza anual y respuesta a viruses respiratorios."

Info_GIHSN_eng <- "We are a site that is part of the Global Influenza Surveillance Network (<a href='https://gihsn.org' target='_blank'>GIHSN</a>). At the National Hospital of Coatepeque and the Hospital of Chimaltenango, 
patients who experience symptoms of fever and/or cough are part of this surveillance. They are tested for Sars-CoV-2, Influenza A/B, and VSR, and the positive results are sequenced. All data is shared with the global network to 
improve surveillance capacity, annual influenza vaccination decisions, and response to respiratory viruses."

# Define any needed functions -------------------------
# Function to format date labels in Spanish
format_date_spanish <- function(x) {
  months <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  paste(months[as.numeric(format(x, "%m"))], format(x, "\n %Y"), sep = " ")
}

# Define any variables needed for both server and ui ------------

# Mapping of symptoms to their corresponding columns-----------
# AGRI/INFLUENZA:

columns_agri_sintomas_clean_names <- c(
  "Tos_seca",
  "Tos_con_flema",
  "Dolor_de_garganta",
  "Dolor_de_cabeza",
  "Congestion_nasal",
  "Fiebre",
  "Dolor_de_cuerpo",
  "Fatiga",
  "Dolor_de_cuello",
  "Sueño_interrumpido",
  "Silbancias",
  "Falta_de_aire",
  "Perdida_de_apetito",
  "Disminución_de_olores",
  "Disminución_de_sabores",
  "Vomito",
  "Diarrea"
)

# AGRI-CASA:
# Symptom mapping with formatted names
symptom_map <- list(
  "Tos" = c("tos_visit_ints >= 2", "tos_flm_visit_ints >= 2", "tos_vig_rut == 1"),
  "Fiebre" = c("sensa_fiebre_visit_ints >= 2", "fiebre_vig_rut == 1"),
  "Falta de Aire" = c("falta_aire_visit_ints >= 2", "dif_resp_vig_rut == 1"),
  "Dolor de Garganta" = c("gargt_irrit_visit_ints >= 2", "dol_gargan_vig_rut == 1"),
  "Dolor de Cabeza" = c("dlr_cabeza_visit_ints >= 2", "dol_cabeza_vig_rut == 1"),
  "Congestión Nasal" = c("congest_nasal_visit_ints >= 2", "cong_nasal_vig_rut == 1"),
  "Dolor de Cuerpo" = c("dlr_cuerp_dgnl_visit_ints >= 2", "dol_cuerp_musc_vig_rut == 1"),
  "Fatiga" = c("fatiga_visit_ints >= 2", "fatica_vig_rut == 1"),
  "Silbidos al Respirar" = c("silbd_resp_visit_ints >= 2", "sibilancias_vig_rut == 1"),
  "Náuseas" = c("nausea_visit_ints >= 2", "nausea_vig_rut == 1"),
  "Diarrea" = c("diarrea_visit_ints >= 2", "diarrea_vig_rut == 1"),
  "Vómitos" = c("vomito_visit_ints >= 2", "vomitos_vig_rut == 1"),
  "Pérdida de Olfato/Gusto" = c("dism_gust_visit_ints >= 2", "dism_olf_visit_ints >= 2", "perd_olf_gust_vig_rut == 1"),
  "Pérdida de Audición/Balance" = c("dism_aud_visit_ints >= 2", "dism_bal_visit_ints >= 2"),
  "Mala Alimentación" = c("mala_alim_visit_ints >= 2", "mala_alim_vig_rut == 1"),
  "Letargo" = c("letargo_visit_ints >= 2", "letargo_vig_rut == 1")
)

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

# Define UI for Tab 1 (AGRI)
ui_tab1 <- function() {
  fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
       
        # Language selection
        radioButtons("language_agri", "Idioma / Language:", 
                     choices = c("Español" = "es", "English" = "en"), 
                     selected = "es"),
        
        # Date range input
        dateRangeInput("date_range_input_tab1", "Eligir el período del tiempo:",
                       start = "2020-06-29", end = "2024-09-30", separator = " a "),
        # Dropdown menu for selecting disease
        radioButtons("virus", "Virus:",
                     c("Todos viruses identificados" = "resul_virus_all",
                       "Influenza A" = "resul_inf_a_all",
                       "Influenza B" = "resul_inf_b_all",
                       "Influenza (A & B)" = "resul_inf_all",
                       "VSR" = "resul_rsv_all",
                       "SARS-CoV-2 confirmado por PCR" = "resul_sars_all",
                       "SARS-CoV-2 confirmado por prueba rápida de antígenos" = "resul_covid_19_all",
                       "SARS-CoV-2 (confirmado por PCR o prueba rápida)" = "resul_sars_covid_all",
                       "Enfermedad no identificada (síntomas con pruebas negativas)" = "resul_neg_all")
        )
      ),
      mainPanel(
        # Add information about the study
        h2(textOutput("header_agri_text"), style = "color: orange;"),
        textOutput("info_agri_text"),
        br(),
        br(),
        # Plot output for the graph
        plotOutput("disease_plot_tab1")
      )
    )
  )
}


# Define UI for Tab 2 (AGRICASA EXPANSION)

ui_tab2 <- function() {
  fluidPage(
    titlePanel(""),
    
    sidebarLayout(
      sidebarPanel(
        
        # Language selection
        radioButtons("language_agricasa", "Idioma / Language:", 
                     choices = c("Español" = "es", "English" = "en"), 
                     selected = "es"),
        
        dateRangeInput(
          "date_range_input_tab2",
          "Eligir el período del tiempo:",
          start = "2023-10-02",
          end = "2024-07-31",
          separator = " a "
        ),
        # Add the virus selection and date range input similar to Tab 1
        radioButtons("virus_agri", "Virus:",
                     c("SARS-CoV-2" = "sars_cov2_all",
                       "Influenza" = "influenza_all",
                       "VSR" = "vsr_all",
                       "Todos" = "virus_all")),
          # Checkboxes for symptom selection
          checkboxGroupInput("symptoms", "Seleccionar Síntomas:",
                             choices = names(symptom_map),
                             selected = NULL)  # Initially no symptoms selected
        ),
      mainPanel(
        # Add information about the study
        h2(textOutput("header_agricasa_text"), style = "color: orange;"),
        textOutput("info_agricasa_text"),
        br(),
        fluidRow(
          column(12, plotOutput("pct_plot_tab2", height = "350px")),  # Combined percentage incident symptom plot
          column(12, plotOutput("incidence_plot_tab2", height = "350px")),
          column(12, plotOutput("symptoms_plot_tab2", height = "500px"))  # Symptom count plot
        )
      )
    )
  )
}


# Define UI for Tab 3 (BIOFIRE)
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
        
        # Add space before "Paneles de Biofire"
        br(),
        
        # Date range input remains here
        dateRangeInput("date_range_input_tab3", "Eligir el período del tiempo:",
                       start = "2020-06-29", end = Sys.Date(), separator = " a "),
        
        # Centered title and table
        div(style = "text-align: center;",
            h3("Paneles de Biofire"),
            reactableOutput("table_tab3")
        ),
        
        # Slightly reduce spacing before "combined_plot_tab3"
        plotOutput("combined_plot_tab3")
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
        dateRangeInput("date_range_input_tab4", "Eligir el período del tiempo:",
                       start = "2025-03-04", end = Sys.Date(), separator = " a "),
        
        # Dropdown menu for selecting hospital
        radioButtons("hospital", "Hospital:",
                     choices = c("Ambos Hospitales",
                                 "Hospital de Coatepeque", 
                                 "Hospital de Chimaltenango")
        ),
        
        # Dropdown menu for selecting virus
        selectInput("virus", 
                    "Selecciona Virus:",
                    choices = c("Todos Virus", "Influenza A y B", "Influenza A", "Influenza B", "SARS-CoV-2", "VSR"))
      ),
      
      mainPanel(
        # Add information about the study
        h2(textOutput("header_gihsn_text"), style = "color: orange;"),
        uiOutput("info_gihsn_text"),
        br(),
        br(),
        # Table output for the data summary
        DTOutput("summary_table_tab4"),
        
        # Plot output for the graph
        plotOutput("disease_plot_tab4")
      )
    )
  )
}

# Define UI for Tab 5 (VIGICASA)
#ui_tab5 <- function() { 
 # fluidPage(
  #  titlePanel("Vigilancia de enfermedades respiratorias y enfermedades como Dengue en casas"),
   # mainPanel(
    #  h2("Viene pronto!", style = "color: orange; text-align: center;")
    #)
  #)
#}

# Define UI for Tab 6 (VIGIFINCA - BANASA)
#ui_tab6 <- function() { 
 # fluidPage(
  #  titlePanel("Vigilancia de enfermedades respiratorias y enfermedades como Dengue en fincas de Banasa"),
   # mainPanel(
    #  h2("Viene pronto!", style = "color: orange; text-align: center;")
    #)
  #)
#}

# Define UI for Tab 7 (VIGIFINCA - PANTALEON)
#ui_tab7 <- function() { 
 # fluidPage(
  #  titlePanel("Vigilancia de enfermedades respiratorias y enfermedades como Dengue en fincas de Pantaleon"),
   # mainPanel(
    #  h2("Viene pronto!", style = "color: orange; text-align: center;")
    #)
  #)
#}


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
    tabPanel("Estudio AGRI", ui_tab1()),
    tabPanel("Estudio AGRI-CASA", ui_tab2()),
    tabPanel("Estudio Biofire", ui_tab3()),
    tabPanel("Vigilancia GIHSN", ui_tab4()),
    #tabPanel("VIGICASA", ui_tab5()),
    #tabPanel("VIGIFINCA - BANASA", ui_tab6()),
    #tabPanel("VIGIFINCA - PANTALEON", ui_tab7())
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
        p("Nuestros datos se publican semanalmente a partir de nuestros diversos proyectos de vigilancia en trabajadores agrícolas, comunidades y hospitales en el suroeste de Guatemala. Este proyecto es una colaboración entre la Fundación para la Salud Integral de los Guatemaltecos (FUNSALUDGUATE-CU) y los Centros para el Control y la Prevención de Enfermedades (CDC). Trabajamos con trabajadores agrícolas de banano y sus familias, trabajadores de caña de azúcar, el Hospital Nacional de Coatepeque, el Hospital Nacional de Chimaltenango, así como con organizaciones a nivel mundial."),
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
        p("Our data is published weekly from our various surveillance projects in farm workers, communities and hospitals across southwestern Guatemala. This project is a collaboration between la Fundación para la Salud Integral de los Guatemaltecos (FUNSALUDGUATE-CU) and Centers for Disease Control (CDC). We work with banana farmworkers and their families, sugar cane farm workers, the Hospital Nacional de Coatepeque and the Hospital Nacional de Chimaltenango as well as organizations worldwide."),
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
  output$header_agri_text <- renderText({
    if (input$language_agri == "es") {
      Header_Agri
    } else {
      Header_Agri_eng
    }
  })
  
  output$header_agricasa_text <- renderText({
    if (input$language_agricasa == "es") {
      Header_AgriCasa
    } else {
      Header_AgriCasa_eng
    }
  })
  
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
  
  # ----------------------------------------------------------------------------
  #                               AGRI / INFLUENZA
  #----------------------------------------------------------------------------

  # Reactive text output for study information
  output$info_agri_text <- renderText({
    if (input$language_agri == "es") {
      Info_Agri  # Spanish version
    } else {
      Info_Agri_eng  # English version
    }
  })
  
  # Positivity data ----------------------------------------------------------
  # Reactive expression to filter data based on selected disease and date range
  filtered_data <- reactive({
    
    # Filter data based on selected date range
    subset(influenza_summary, epiweek_recolec >= input$date_range_input_tab1[1] & 
             epiweek_recolec <= input$date_range_input_tab1[2])
  })
  
  # Symptoms by positivity ----------------------------------------------------
  
  influenza_symptoms_data <- reactive({
  
    influenza_symptoms_summary_subset_pre <- influenza_symptom_summary
    influenza_symptoms_summary_subset <- influenza_symptoms_summary_subset_pre[influenza_symptoms_summary_subset_pre[[input$virus]] == 1, ]

    # Filter data based on selected date range
    influenza_symptoms_summary_subset_dates <- subset(influenza_symptoms_summary_subset,
           epiweek1 >= input$date_range_input_tab1[1] & 
             epiweek1 <= input$date_range_input_tab1[2])
    
    # Collapse symptoms based on count
    symptom_summary_wide <- influenza_symptoms_summary_subset_dates%>%
      group_by(epiweek1, visit_type) %>%
      summarise(
        across(all_of(columns_agri_sintomas_clean_names), ~ sum(. == 1))
      )
    
    # Make sure each week is represented (put a 0 as count if not represented already)-----------
    # Step 1: Generate a complete sequence of dates (epiweeks in this case)
    date_range <- seq(as.Date("2020-06-29"), as.Date("2024-09-30"), by = "weeks")
    
    # Step 2: Expand dataset to include all possible combinations of epiweek and visit_type
    expand_date_df <- expand.grid(
      epiweek1 = unique(date_range),
      visit_type = unique(symptom_summary_wide$visit_type)
    )
    
    # Step 3: Merge datasets to fill in missing combinations
    symptom_summary_wide_allweeks <- symptom_summary_wide %>%
      mutate(epiweek1 = as.Date(epiweek1))%>%
      full_join(expand_date_df, by = c("epiweek1", "visit_type"))
    # Every time there is an NA in the dataset (because we just added a date in), replace with a 0
    symptom_summary_wide_allweeks[is.na(symptom_summary_wide_allweeks)] <- 0
    
    # Next, we need to convert this from a wide to long dataset
    symptoms_summary_counts <- symptom_summary_wide_allweeks %>%
      pivot_longer(
        cols = all_of(columns_agri_sintomas_clean_names),
        names_to = "Síntoma",
        values_to = "Count"
      )

  })
  
  
  # Graph --------------------------------------------------------------------
  
  # Render the plot based on filtered data (case counts based on PCR)
  output$disease_plot_tab1 <- renderPlot({
    filtered <- filtered_data()
    influenza_symptoms_df <- influenza_symptoms_data()
    
    count_all_column_name <- paste0("count_all_", input$virus)
    count_pos_column_name <- paste0("count_pos_", input$virus)
    pct_pos_column_name <- paste0("pct_pos_", input$virus)
    
    virus_labels <- c("resul_neg_all" = "Enfermedad no identificada (síntomas con pruebas negativas)",
                      "resul_inf_a_all" = "Influenza A",
                      "resul_inf_b_all" = "Influenza B",
                      "resul_inf_all" = "Influenza (A & B)",
                      "resul_rsv_all" = "VSR",
                      "resul_sars_all" = "SARS-CoV-2 confirmado por PCR",
                      "resul_covid_19_all" = "SARS-CoV-2 confirmado por prueba rápida de antígenos", 
                      "resul_sars_covid_all" = "SARS-CoV-2 (confirmado por PCR o prueba rápida)",
                      "resul_virus_all" = "Todos viruses identificados"
                      )
    selected_virus_label <- virus_labels[[input$virus]]
    
    # Incidence Plot ---------------------
    # Add the conditional layer if the virus is not "resul_neg_all"
    if (input$virus != "resul_neg_all") {
    influenza_positivity_plot <- filtered %>%
      dplyr::mutate(
        epiweek_recolec_date = as.Date(epiweek_recolec),
        count_all_column_name = ifelse(is.na(count_all_column_name), 0, count_all_column_name)
      ) %>%
      ggplot(aes(x = epiweek_recolec_date)) +
      geom_bar(aes(y = .data[[count_all_column_name]], fill = "Total"), stat = "identity", color = "black") +
      geom_bar(aes(y = .data[[count_pos_column_name]], fill = "Prueba Positiva"), stat = "identity", color = "black")+
      scale_fill_manual(values = c("Total" = "grey", "Prueba Positiva" = "red")) +
      theme_classic() +
      labs(
        title = paste("Número de individuos con resultados de", selected_virus_label),
        x = "",
        y = "",
        fill = "Resultado"
      ) +
      scale_y_continuous(breaks = seq(0, max(filtered[[count_all_column_name]], na.rm = TRUE), by = 1)) +
      scale_x_date(
        labels = format_date_spanish,
        limits = as.Date(c(input$date_range_input_tab1[1], input$date_range_input_tab1[2]))
      )
    }else if(input$virus == "resul_neg_all"){
       filtered_wneg <- filtered %>%
        dplyr::mutate(
          epiweek_recolec_date = as.Date(epiweek_recolec),
          all_tested = ifelse(
            is.na(count_neg_resul_virus_all),
            0,
            rowSums(cbind(count_neg_resul_virus_all, count_pos_resul_virus_all), na.rm = TRUE)
          )
        )
      #colnames(influenza_positivity_plot)
      #print(influenza_positivity_plot%>%dplyr::select(all_tested, count_pos_resul_virus_all, count_neg_resul_virus_all))
       influenza_positivity_plot <- filtered_wneg%>% 
       ggplot(aes(x = epiweek_recolec_date)) +
        geom_bar(aes(y = all_tested, fill = "Total"), stat = "identity", color = "black") +
        geom_bar(aes(y = count_neg_resul_virus_all, fill = "Prueba Negativa"), stat = "identity", color = "black")+
        scale_fill_manual(values = c("Total" = "grey", "Prueba Negativa" = "blue")) +
        theme_classic() +
        labs(
          title = paste("Número de individuos con resultados de", selected_virus_label),
          x = "",
          y = "",
          fill = "Resultado"
        ) +
        scale_y_continuous(breaks = seq(0, max(filtered_wneg$all_tested, na.rm = TRUE), by = 1)) +
        scale_x_date(
          labels = format_date_spanish,
          limits = as.Date(c(input$date_range_input_tab1[1], input$date_range_input_tab1[2]))
        )
    }

    
    
    # Symptom plot------------
    influenza_symptom_plot <- influenza_symptoms_df %>%
    ggplot(aes(x = epiweek1, y = Síntoma)) +
    geom_tile(aes(fill = Count), color='black') +
    facet_wrap(~ visit_type, ncol = 1, scales = "free_y") +
    labs(
      title="Número de individuos experimentando un dado síntoma por semana después \n de una prueba de PCR",
      x = "Semana de recogida de muestra de PCR",
      y = "",
      fill= "Número de individuos"
    ) +
    theme_classic() +
    scale_fill_gradient(low = "#FFFFFF", high = "#FF0000")+
    scale_x_date(
      labels = format_date_spanish,
      limits = as.Date(c(input$date_range_input_tab1[1], input$date_range_input_tab1[2])),
      )  # Remove extra space at ends
  
  # Combine ----------------------
  influenza_combined_plot <- influenza_positivity_plot / influenza_symptom_plot + plot_layout(heights = c(1, 3), widths = c(1, 1))
  # Display the combined plot with specified width and height
    influenza_combined_plot
  
}, height = 900, width = 750) 
  
  
  # ----------------------------------------------------------------------------
  #                               AGRI CASA
  #----------------------------------------------------------------------------
  
  # Reactive text output for study information
  output$info_agricasa_text <- renderText({
    if (input$language_agricasa == "es") {
      Info_AgriCasa  # Spanish version
    } else {
      Info_AgriCasa_eng  # English version
    }
  })
  
  # Agri-Casa Incidence-----------------------------------------------------------------------
  
  # Reactive expression to filter data based on selected disease and date range
  filtered_agri_incidence <- reactive({
    # Filter data based on selected date range
    subset(agri_casa_incidence_summary, epiweek_muestra_funsalud >= input$date_range_input_tab2[1] & 
             epiweek_muestra_funsalud <= input$date_range_input_tab2[2])
  })
  
  # Render the plot based on filtered data
  output$incidence_plot_tab2 <- renderPlot({
    
    # Load dataset
    filtered_agri <- filtered_agri_incidence()
    
    # For code
    count_pos_column_name_agri <- paste0(input$virus_agri)

    # For labels
    virus_labels_agri <- c("sars_cov2_all" = "SARS-CoV-2",
                           "influenza_all" = "Influenza",
                           "vsr_all" = "VSR",
                           "virus_all" = "Todos")
    selected_virus_label_agri <- virus_labels_agri[[input$virus_agri]]
    
    filtered_agri %>%
      dplyr::mutate(epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud)) %>%
      ggplot(aes(x = epiweek_muestra_funsalud)) +
      geom_bar(aes(y = denominator, fill = "Total"), stat = "identity", color="black") +
      geom_bar(aes(y = total_ili_count, fill = "Experimentan tos, fiebre, \n o falta de aire"), stat = "identity", color="black") +
      geom_bar(aes(y = .data[[count_pos_column_name_agri]], fill = "Primera Prueba Positiva"), stat = "identity", color="black") +
      scale_fill_manual(values = c("Total" = "grey", "Primera Prueba Positiva" = "red", "Experimentan tos, fiebre, \n o falta de aire" = "orange")) +
      theme_classic() +
      theme(
        legend.position = "top",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      labs(title = paste("Número de individuos por semana en la Vigilancia de Rutina \n o Vigilancia Intensa con", selected_virus_label_agri),
           x = "Epiweek (Semana cuando se detectó la infección por primera vez)",
           y = "Número de individuos",
           fill = "") +
      scale_y_continuous(breaks = seq(0, max(filtered_agri$denominator, na.rm=TRUE), by = 25)) +
      scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
  }, width = 500, height = 400)
  
  
  # Agri-Casa Symptoms-----------------------------------------------------------------------
  # Reactive expression for filtered data
  filtered_data_tab2 <- reactive({
    
    selected_symptoms <- input$symptoms
    
    # Filter data based on selected symptoms
    filtered <- agri_casa_symptom_summary %>%
      filter(!is.na(epiweek_symptoms))  # Ensure epiweek_symptoms are not NA
    
    # Combine conditions for selected symptoms
    conditions_list <- lapply(selected_symptoms, function(symptom) {
      symptom_conditions <- symptom_map[[symptom]]
      paste(symptom_conditions, collapse = " | ")
    })
    
    conditions <- paste(unlist(conditions_list), collapse = " | ")
    
    # Count number of distinct anonymized_ids where any condition is met for the selected symptoms
    counts <- filtered %>%
      group_by(epiweek_symptoms) %>%
      summarise(count = sum(rowSums(sapply(conditions, function(cond) eval(parse(text = cond))), na.rm = TRUE) > 0, na.rm = TRUE)) %>%
      mutate(count = replace(count, is.na(count), 0))  # Replace NA with 0
    
    # Merge with original data to ensure all epiweek_symptoms are retained
    all_weeks <- agri_casa_symptom_summary %>%
      distinct(epiweek_symptoms)  # Get all unique epiweek_symptoms
    
    counts <- left_join(all_weeks, counts, by = "epiweek_symptoms") %>%
      mutate(count = replace(count, is.na(count), 0))  # Replace NA with 0
    
    # Merge with denominator from agri_casa_symptom_summary
    counts_w_denom <- left_join(counts, 
                                     (agri_casa_symptom_summary %>% 
                                        dplyr::select(epiweek_symptoms, denominator)%>%
                                        dplyr::distinct(epiweek_symptoms, denominator, .keep_all = TRUE)), by = "epiweek_symptoms")
    

    return(counts_w_denom)
  })
  
  # Render plot based on filtered data
  output$symptoms_plot_tab2 <- renderPlot({
    req(input$symptoms)  # Ensure symptoms are selected
    
    agri_casa_simptomas <- filtered_data_tab2()
    
    # Plot
    agri_casa_simptomas %>%
      mutate(epiweek_symptoms = as.Date(epiweek_symptoms)) %>%
      ggplot(aes(x = epiweek_symptoms)) +
      geom_bar(aes(y = denominator, fill = "Total"), stat = "identity", color="black") +
      geom_bar(aes(y = count, fill = "Experimentan Síntomas"), stat = "identity", color="black") +
      labs(
        title = "Individuos con los síntomas especificados por semana en la Vigilancia de Rutina \n o Vigilancia Intensa",
        x = "Semana",
        y = "Número de individuos", 
        fill=""
      ) +
      scale_fill_manual(values = c("Total" = "grey", "Experimentan Síntomas" = "gold")) +
      theme_classic() +
      theme(
        legend.position = "top",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
  }, width = 500, height = 300)
  
  
  # Create a percent positivity graph with everything overlaying each other----------------------------------
  
  
  # Render plot based on filtered data
  output$pct_plot_tab2 <- renderPlot({
    
    # Load dataset
    filtered_agri <- filtered_agri_incidence()
    
    # Find column
    count_pos_column_name_agri <- paste0(input$virus_agri)
    
    # For labels
    virus_labels_agri <- c("sars_cov2_all" = "SARS-CoV-2",
                           "influenza_all" = "Influenza",
                           "vsr_all" = "VSR",
                           "virus_all" = "Todos")
    selected_virus_label_agri <- virus_labels_agri[[input$virus_agri]]
    
    if(is.null(input$symptoms)){
      
      # Add new columns
      filtered_agri$pct_prueba_positiva <- ifelse(filtered_agri$denominator <=0, 0,
                                                     (filtered_agri[[count_pos_column_name_agri]] / filtered_agri$denominator)*100)
      
      filtered_agri$pct_ILI_symptoms <- ifelse(filtered_agri$denominator <=0, 0,
                                                  (filtered_agri$total_ili_count / filtered_agri$denominator)*100)
      
      filtered_agri %>%
        mutate(epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud)) %>%
        ggplot(aes(x = epiweek_muestra_funsalud)) +
        geom_bar(aes(y = pct_ILI_symptoms, fill = "Experimentan fiebre, tos, \n o falta de aire"), stat = "identity", color = "black", alpha = 0.5) +
        geom_bar(aes(y = pct_prueba_positiva, fill = "Primera Prueba Positiva"), stat = "identity", color = "black", alpha = 0.5) +
        labs(
          title = paste("Porcentaje de individuos por semana en la Vigilancia de Rutina \n o Vigilancia Intensa con", selected_virus_label_agri),
          x = "Semana",
          y = "% de individuos", 
          fill = ""
        ) +
        scale_fill_manual(
          values = c("Experimentan fiebre, tos, \n o falta de aire" = "orange", "Primera Prueba Positiva" = "red"),
          labels = c("Experimentan fiebre, tos, \n o falta de aire" = "Experimentan fiebre, tos, \n o falta de aire",
                     "Primera Prueba Positiva" = "Primera Prueba Positiva")
        ) +
        theme_classic() +
        theme(
          legend.position = "top",
          plot.margin = margin(10, 10, 10, 10)
        ) +
        scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
    }
    
    else{
      
   
    agri_casa_simptomas_2 <- filtered_data_tab2()
    
    
  # Merge dataset
    agri_casa_pct_df <- merge(x=agri_casa_simptomas_2, y=filtered_agri, by=c("epiweek_symptoms", "denominator"),
                             by.y=c("epiweek_muestra_funsalud", "denominator"), all=TRUE)
    
    # Add new columns
    agri_casa_pct_df$pct_prueba_positiva <- ifelse(agri_casa_pct_df$denominator <=0, 0,
                                                   (agri_casa_pct_df[[count_pos_column_name_agri]] / agri_casa_pct_df$denominator)*100)
    
    agri_casa_pct_df$pct_combo_symptoms <- ifelse(agri_casa_pct_df$denominator <=0, 0,
                                                  (agri_casa_pct_df$count / agri_casa_pct_df$denominator)*100)
    
    agri_casa_pct_df$pct_ILI_symptoms <- ifelse(agri_casa_pct_df$denominator <=0, 0,
                                                (agri_casa_pct_df$total_ili_count / agri_casa_pct_df$denominator)*100)

    agri_casa_pct_df %>%
      mutate(epiweek_symptoms = as.Date(epiweek_symptoms)) %>%
      ggplot(aes(x = epiweek_symptoms)) +
      geom_bar(aes(y = pct_combo_symptoms, fill = "Experimentan Síntomas"), stat = "identity", color = "black", alpha = 0.5) +
      geom_bar(aes(y = pct_prueba_positiva, fill = "Primera Prueba Positiva"), stat = "identity", color = "black", alpha = 0.5) +
      labs(
        title = paste("Porcentaje de individuos por semana en la Vigilancia de Rutina \n o Vigilancia Intensa con", selected_virus_label_agri),
        x = "Semana",
        y = "% de individuos", 
        fill = ""
      ) +
      scale_fill_manual(
        values = c("Experimentan Síntomas" = "gold", "Primera Prueba Positiva" = "red"),
        labels = c("Experimentan Síntomas" = "Experimentan Síntomas",
                   "Primera Prueba Positiva" = "Primera Prueba Positiva")
      ) +
      theme_classic() +
      theme(
        legend.position = "top",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
    
  }}, width = 500, height = 300)
  
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

# Create DENGUE plot -------------------------------------
# Generate the dengue plot
output$dengue_plot_tab3 <- renderPlot({
  # Get the selected date range from the input
  start_date <- input$date_range_input_tab3[1]  # This is the first value of the date range (start date)
  end_date <- input$date_range_input_tab3[2]    # This is the second value of the date range (end date)
  
  # Ensure `epiweek_recoleccion` is in Date format
  filtered_data <- namru_biofire_summary %>%
    mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion)) %>%
    filter(epiweek_recoleccion >= as.Date(start_date) & epiweek_recoleccion <= as.Date(end_date))
  
  # Check if there's any data in the selected date range
  if (nrow(filtered_data) == 0 || sum(!is.na(filtered_data$res_dengue)) == 0) {
    # If no data exists or no non-missing res_dengue, show the "No data available" message
    plot.new()
    grid::grid.text(
      "No hay datos disponibles para esta selección / No data available for this selection", 
      x = 0.5, y = 0.5, 
      gp = grid::gpar(fontsize = 16)  # Adjust font size dynamically if necessary
    )
  } else {
    # Summarize the data for the filtered date range
    dengue_summary <- filtered_data %>%
      group_by(epiweek_recoleccion) %>%
      summarise(
        total_tested = sum(!is.na(res_dengue)),  # Sum of non-missing `res_dengue` per epiweek
        total_pos_dengue = sum(res_dengue == 1, na.rm = TRUE)  # Count positives
      )
    
    # Generate the plot based on the filtered data
    ggplot(dengue_summary, aes(x = epiweek_recoleccion)) +
      geom_bar(aes(y = total_tested, fill = "Total Muestreados"), stat = "identity", alpha = 0.5) +  # Grey background
      geom_bar(aes(y = total_pos_dengue, fill = "Total Positivos"), stat = "identity") +  # Red foreground
      scale_fill_manual(values = c("Total Muestreados" = "grey", "Total Positivos" = "red")) +
      scale_x_date(
        labels = format_date_spanish,  # Use custom function for formatting
        limits = as.Date(c(start_date, end_date)),  # Dynamic x-axis range based on selected date
        expand = c(0, 0)  # Remove extra space at ends
      ) +
      scale_y_continuous(
        breaks = seq(0, ceiling(max(c(dengue_summary$total_tested, dengue_summary$total_pos_dengue))), by = 1),  # Integer steps on y-axis
        labels = scales::comma,  # Format y-axis labels as integers
        limits = c(0, NA)  # Ensure the y-axis starts at 0 and goes up dynamically
      ) +
      labs(x = "Epiweek", y = "# Muestreados", fill = "Resultado") +
      theme_minimal()
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
      )
    )
  
  # Convert epiweek & year into a proper date for x-axis
  filtered_data <- filtered_data %>%
    mutate(epiweek_date = as.Date(paste(year, epiweek, 1), format = "%Y %U %u"))

  
  # Generate the plot
  ggplot(filtered_data, aes(x = factor(paste(year, epiweek, sep = "-")))) +
    geom_bar(aes(y = total_tested, fill = "Total Muestreados"), stat = "identity", alpha = 0.5) +  # Grey background
    geom_bar(aes(y = total_pos_dynamic, fill = "Total Positivos"), stat = "identity") +  # Red foreground
    scale_fill_manual(values = c("Total Muestreados" = "grey", "Total Positivos" = "skyblue")) +
    scale_y_continuous(breaks = function(x) seq(0, ceiling(max(x)), by = 1)) +
    labs(x = "Epiweek", y = "# Muestreados", fill = "Resultado") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# --------------------------------------------------------------------------
#                             VIGILANCIA
# --------------------------------------------------------------------------


}

shinyApp(ui, server)

# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc
