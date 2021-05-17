library(shiny) #  Shiny web app
library(shinydashboard)
library(data.table)    #  for data tables
library(plotly)
library(shinythemes)
library(ggplot2)
library(DT)    #  for data tables

ui <- dashboardPage(
    dashboardHeader(title = "Patient Lab Results"),
    dashboardSidebar(
        h4("Patient Age"),
        plotOutput("histplot", height = 250),
        h4("Patient Gender"),
        plotOutput("histplotgender", height = 250)
    ),
    dashboardBody(fluidPage(theme = shinytheme("cerulean"),
                
                h4("Patient Info"),
                fluidRow(column(12, div(dataTableOutput("patientTable")))),
                h4("Laboratory Measurements"),
                plotlyOutput("facetplot"),
                h4("Lab Info"),
                fluidRow(column(12, div(dataTableOutput("labTable")))))))
                

server <- function(input, output, session) {
    
    # Load patient metadata
    Patient_data <- reactive({
        setDT(read.csv("Random_PatientLevelInfo_2021.tsv", sep = "\t"))
    })
    
    # Reactive load lab data based on selected patient metadata
    Lab_data <- eventReactive(input$patientTable_rows_selected, {
        if (length(input$patientTable_rows_selected) == 0)
            return(NULL)
        
        labdata = setDT(read.csv("Random_LabValuesInfo_2021.tsv", sep = "\t"))
        selected_patientid = as.factor(unique(unlist(Patient_data()[input$patientTable_rows_selected,"USUBJID",with=FALSE])))
        print(selected_patientid)
        selected_labdata = labdata[USUBJID %in% selected_patientid,]
        
        return(selected_labdata)
    })
    
    # Reactive plot lab data based on selected genes from expression data
    biomarker_data <- eventReactive(input$patientTable_rows_selected,{
        if (length(input$patientTable_rows_selected) == 0)
            return(NULL)
        
        # construct set of unique gene names from selected expression data
        selected_patientid = as.factor(unique(unlist(Patient_data()[input$patientTable_rows_selected,"USUBJID",with=FALSE])))
        
        plotdata = Lab_data()[USUBJID %in% selected_patientid,]
        print(plotdata)
        return(plotdata)
    })
    
    output$histplot <- renderPlot({
        data <- Patient_data()
        hist(data$AGE,breaks = 50, col = "blue", main = "Age Distribution", xlab = "Age")
    })
    
    output$histplotgender <- renderPlot({
        data <- Patient_data()
        mydata = data[,.(.N),by = .(SEX)]
        barplot(mydata$N, main="Gender Distribution", horiz=TRUE,
                xlab="Gender", names.arg=mydata$SEX, color = "blue")
    })
    output$patientTable <- renderDT(
        Patient_data(), # data
        class = "display nowrap compact", 
        filter = "top"
    )
    
    output$labTable <- renderDT(
        Lab_data(), # data
        class = "display nowrap compact", 
        filter = "top"
    )
    
    output$facetplot <- renderPlotly({
        if (!is.null(biomarker_data())) {
            p <- ggplot(data = biomarker_data(), aes(x = AVISIT, y = AVAL, group = USUBJID)) + geom_line(aes(color = USUBJID)) + geom_point() + theme(legend.position = "right") + facet_grid(rows = vars(LBTESTCD))
            ggplotly(p)  
        }
    })
    
}

shinyApp(ui, server)

