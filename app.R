library(shiny)
library(tidyverse)
library(DT)

dm <- read_csv('source_files/dm.csv')
lb <- read_csv('source_files/lb.csv')
vs <- read_csv('source_files/vs.csv')
eg <- read_csv('source_files/eg.csv')
ae <- read_csv('source_files/ae.csv')
mh <- read_csv('source_files/mh.csv')

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

    titlePanel("Sample SDTM visualisation"),

    fluidRow(
        column(width=3,
           selectInput("subject", "Select a subject", choices=unique(dm$USUBJID)),
           p("Important note: datasets used for this dashboard belong to the public domain. 
             To access the original source, please visit:"),
           tags$a(href="https://doi.org/10.7910/DVN/51B6NK", "https://doi.org/10.7910/DVN/51B6NK")
        ),

        column(width=9,
          tabsetPanel(
            tabPanel("Laboratory",
                     uiOutput("gen_choose_category_laboratory"),
                     plotOutput(outputId="laboratory_plot", brush="laboratory_brush"),
                     dataTableOutput(outputId="laboratory_table")),
            tabPanel("Vital Signs",
                     uiOutput("gen_choose_category_vitals"),
                     plotOutput(outputId="vitals_plot", brush="vitals_brush"),
                     dataTableOutput(outputId="vitals_table")),
            tabPanel("ECG",
                     uiOutput("gen_choose_category_ecg"),
                     plotOutput(outputId="ecg_plot", brush="ecg_brush"),
                     dataTableOutput(outputId="ecg_table")),
            tabPanel("Adverse Events",
                     dataTableOutput(outputId="ae_table")),
            tabPanel("Medical History",
                     dataTableOutput(outputId="mh_table")),
            
          )
        )
    )
)

server <- function(input, output) {
  
  theme_set(theme_bw(base_size=14))
  thematic::thematic_shiny()
  
  # LABORATORY
  
  laboratory_data <- reactive({
    lb %>% filter(USUBJID == input$subject) %>% mutate(LBTEST=paste0(LBTEST, " (", LBORRESU, ")"))
  })

  output$gen_choose_category_laboratory <- renderUI({
    selectInput("choose_category_laboratory", "Choose a lab test type", choices=unique(laboratory_data()$LBTEST))
  })
  
  laboratory_data_subset <- reactive({
    laboratory_data() %>% filter(LBTEST==input$choose_category_laboratory) %>%
      mutate(VISIT=reorder(VISIT, VISITDY))
  })

  output$laboratory_plot <- renderPlot({
    ggplot(laboratory_data_subset()) + geom_point(aes(x=VISIT, y=LBORRES), size=3)
  })

  output$laboratory_table <- renderDataTable({
    brushedPoints(laboratory_data_subset(), input$laboratory_brush) %>% select(LBTEST, LBORRES, LBORRESU, LBORNRLO, LBORNRHI, LBNRIND, LBSPEC, VISIT, VISITDY, LBDTC)
  })
  
  # VITALS
  
  vitals_data <- reactive({
    vs %>% filter(USUBJID == input$subject) %>% mutate(VSTEST=paste0(VSTEST, " (", VSORRESU, ")"))
  })
  
  output$gen_choose_category_vitals <- renderUI({
    selectInput("choose_category_vitals", "Choose a vital sign type", choices=unique(vitals_data()$VSTEST))
  })
  
  vitals_data_subset <- reactive({
    vitals_data() %>% filter(VSTEST==input$choose_category_vitals) %>%
      mutate(VISIT=reorder(VISIT, VISITDY))
  })
  
  output$vitals_plot <- renderPlot({
      ggplot(vitals_data_subset()) + geom_point(aes(x=VISIT, y=VSORRES), size=3)
  })
  
  output$vitals_table <- renderDataTable({
    brushedPoints(vitals_data_subset(), input$vitals_brush) %>% select(VSTEST, VSORRES, VSORRESU, VSPOS, VISIT, VISITDY, VSDTC)
  })
  
  # ECG
  
  ecg_data <- reactive({
    eg %>% filter(USUBJID == input$subject) %>% mutate(EGTEST=paste0(EGTEST, " (", EGSTRESU, ")"))
  })
  
  output$gen_choose_category_ecg <- renderUI({
    selectInput("choose_category_ecg", "Choose ECG measurement type", choices=unique(ecg_data()$EGTEST))
  })
  
  ecg_data_subset <- reactive({
    ecg_data() %>% filter(EGTEST==input$choose_category_ecg) %>%
      mutate(VISIT=reorder(VISIT, VISITDY))
  })
  
  output$ecg_plot <- renderPlot({
      ggplot(ecg_data_subset()) + geom_point(aes(x=VISIT, y=EGSTRESC), size=3)
  })
  
  output$ecg_table <- renderDataTable({
    brushedPoints(ecg_data_subset(), input$ecg_brush) %>% select(EGTEST, EGSTRESC, EGSTRESU, EGPOS, VISIT, VISITDY, EGDTC)
  })
  
  # AE
  
  output$ae_table <- renderDataTable({
    ae %>% filter(USUBJID==input$subject) %>% select(AETERM, AEDECOD, AEBODSYS, AESEV, AESER, AEACN, AEREL, AESTDTC, AEENDTC, AESTDY)
  })
  
  # Medical History
  
  output$mh_table <- renderDataTable({
    mh %>% filter(USUBJID==input$subject) %>% select(MHTERM, MHCAT, MHBODSYS, MHSTDTC)
  })
  
}

shinyApp(ui = ui, server = server)
