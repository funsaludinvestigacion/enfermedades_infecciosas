#comentario
#library(shiny) #keep commented for app to be deployed on R Shiny
library(bslib)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(tidyr)
library(patchwork)

# ============================================================================
# ARCHIVE APP
# Contains: "Estudio AGRI" and "Estudio AGRI-CASA" tabs, split off from the
# main "Enfermedades Respiratorias y Febriles en Guatemala" dashboard.
# ============================================================================

# Load dataframes -----------------------------------------------------------------------
influenza_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/influenza_summary_updated.csv")
influenza_symptom_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/influenza_symptom_summary_updated.csv")
agri_casa_symptom_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/agri_casa_summary_updated.csv")
agri_casa_incidence_summary <- read.csv("https://raw.githubusercontent.com/funsaludinvestigacion/enfermedades_infecciosas/main/docs/agri_casa_incidence_summary_updated.csv")

# Information about each study ------------------------
Header_Agri <- "Estudio Agri: Síntomas de enfermedades respiratorias en trabajadores agrícolas"

Header_Agri_eng <- "Agri Study: Respiratory illness symptoms in farm workers"

Info_Agri <- "Este estudio es una vigilancia en las fincas de AgroAmerica con trabajadores Agrícolas del area de Banasa. Trabajadores con síntomas 
son captados en su lugar de trabajo y posteriormente cada semana entre 8-15 trabajadores agrícolas son contactados por teléfono quienes responden a 
preguntas sobre su salud. Si experimentan síntomas que indiquen una infección respiratoria, como tos seca, fiebre, y dificultad para respirar, el equipo de 
enfermería de investigación de FUNSALUD, procederá a tomar una muestra. Después de realizar pruebas de PCR, el equipo sigue a las personas con síntomas 
por un mes, haciendo visitas durante los 7 y 28 días después de la recolección de las muestras. El objetivo es entender cuales síntomas están asociados con que 
infección respiratoria y la carga clínica y económica."

Info_Agri_eng <- "This study is an agricultural worker disease surveillance in AgroAmerica's Banasa farms. Workers with symptoms are identified in their workplace and each week
between 8-15 agricultural workers are contacted by phone to answer questions about their health. If they experience symptoms indicating a respiratory infection, such as cough, fever, or difficulty breathing 
the research nursing team from FUNSALUD collects a nasal swab. After conducting PCR tests, the team follows up with symptomatic individuals for a month, making visits on days 7 and 28 after sample collection. 
The goal is to understand which symptoms are associated with specific respiratory infections and their clinical and economic burdens."

##
Header_AgriCasa <- "Estudio AgriCasa: síntomas y número de personas con resultados positivos por semana"

Header_AgriCasa_eng <- "AgriCasa study: symptoms and number of people with positive tests per week"

Info_AgriCasa <- "Ciento cincuentas casas están inscritas en este estudio. Cada semana en la visita de rutina, el equipo de campo de los enfermeros de investigación, 
preguntan a cada miembro de la familia si se sienten bien de salud. Si algún miembro refiere tener síntomas, se le hace una prueba para SARS-COV-2, VSR, e Influenza A/B. 
Si algún miembro de la familia tiene una prueba positiva toda la familia recibirá dos visitas intensivas cada semana para entender la transmisión de estas enfermedades 
respiratorias entre la casa y de la casa a la finca y viceversa."

Info_AgriCasa_eng <- "One hundred and fifty households are enrolled in this study. Each week, during the routine visit, the field team of research nurses asks each family member 
if they are feeling well. If any member reports having symptoms, they are tested for SARS-CoV-2, RSV, and Influenza A/B. If any family member tests positive, the entire household 
will receive two intensive visits each week to understand the transmission of these respiratory diseases within the home, between the home and the farm, and vice versa."

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

# ------------------------------------------------------
# Define UI for Tab 1 (Estudio AGRI) ----
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
        dateRangeInput("date_range_input_tab1", "Período del tiempo / Time period:",
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
          "Período del tiempo / Time period:",
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

# ------------------------------------------------------
# Main UI ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Enfermedades Respiratorias y Febriles en Guatemala - Archivo"),
  
  # Theme
  theme = shinytheme("united"),
  
  # Main panel content goes here
  tabsetPanel(
    tabPanel("Estudio AGRI", ui_tab1()),
    tabPanel("Estudio AGRI-CASA", ui_tab2())
  )
)

# ------------------------------------------------------
# Define server logic ----
server <- function(input, output) {
  
  # ----------------------------------------------------------------------------
  #                               HEADERS
  #----------------------------------------------------------------------------
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
  
}

shinyApp(ui, server)
