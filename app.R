library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(stringr)
# code 
# Functie om de dagen sinds de startdatum te berekenen
convert_to_days_since_start <- function(date, start_date) {
  as.numeric(date - start_date) + 1
}
ui <- fluidPage(theme = shinytheme("united"), #themeSelector(),
                tags$head(
                  tags$style(HTML("
                    .action-buttons {
                      display: flex;
                      justify-content: space-between;
                    }
                    .action-message {
                      margin-top: 10px;
                    }
                  "))
                ),
                navbarPage("Shiny app",
                           tabPanel("Input files",
                                    titlePanel("Input files"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("file", "Upload the file", multiple = TRUE),
                                        helpText("Select the read.table parameters below"),
                                        checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                                        checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                        radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma = ',', Semicolon = ';', Tab = '\t', Space = ''), selected = ';')
                                      ),
                                      mainPanel(
                                        uiOutput("tb")
                                      )
                                    )
                           ),
                           tabPanel("Startdate",
                                    titlePanel("Startdate"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        dateInput("start_date", "Startdate ('Day 1')", value = NULL)
                                      ),
                                      mainPanel(
                                        h3("Enter the start date corresponding to 'Day 1' in your data here."),
                                      )
                                    )
                           ),
                           tabPanel("graphs",
                                    titlePanel("scatterplot"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        uiOutput("genotype_ui"),
                                        uiOutput("treatment_ui"),
                                        uiOutput("potnummers_ui")
                                      ),
                                      mainPanel(
                                        plotlyOutput("scatterPlot"),
                                        verbatimTextOutput("geno_treat_stats"),
                                        div(class = "action-buttons",
                                            actionButton("remove_outliers_btn", "Remove Outliers"),
                                            actionButton("keep_data_btn", "Keep Data")
                                        ),
                                        textOutput("action_message")
                                      )
                                    )
                           ),
                           tabPanel("Mean Lines",
                                    titlePanel("Mean Lines Plot"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        uiOutput("genotype_ui_mean"),
                                        uiOutput("treatment_ui_mean")
                                      ),
                                      mainPanel(
                                        plotlyOutput("meanLinesPlot")  
                                      )
                                    )
                           ),
                           tabPanel("Statistical tests",
                                    titlePanel("Statistical tests"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        numericInput("selected_day", "Select day for analysis", min = 1, max = max(data()$Day), value = 1),
                                        uiOutput("genotype_ui_test"),
                                        actionButton("run_statistical_tests_btn", "Run Statistical Tests")
                                      ),
                                      mainPanel(
                                        h3(textOutput("anova_title")),
                                        verbatimTextOutput("anova_summary"),
                                        h3("Pairwise comparisons - TukeyHSD test."),
                                        verbatimTextOutput("tukey_summary")
                                      )
                                    )
                           ),
                           tabPanel("Download",
                                    titlePanel("Here can you download the plots"),
                                    mainPanel(
                                      downloadButton("downloadPlot", "Download the first plot"),
                                      downloadButton("downloadmeanplot", "Download the mean plot")
                                    )
                           )
                           
    )
)

server <- function(input, output, session) {
  
  ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
  # this reactive output display the content of the input$file dataframe
  output$filedf <- renderTable({
    if(is.null(input$file)){return ()}
    input$file # the file input data frame object that contains the file attributes
  })
  
  # Extract the file path for file
  output$filedf2 <- renderTable({
    if(is.null(input$file)){return ()}
    input$file$datapath # the file input data frame object that contains the file attributes
  })
  
  ## Below code to display the structure of the input file object
  output$fileob <- renderPrint({
    if(is.null(input$file)){return ()}
    str(input$file)
  })
  
  ## Side bar select input widget coming through renderUI()
  # Following code displays the select input widget with the list of file loaded by the user
  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         helpText("Select the files for which you need to see data and summary stats"),
         selectInput("Select", "Select", choices=input$file$name)
    )
    
  })
  
  ## Summary Stats code ##
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$table <- renderTable({ 
    req(input$file)  # Controleer of het bestand is geüpload
    df <- read.csv(input$file$datapath[1], header = input$header, sep = input$sep, stringsAsFactors = input$stringAsFactors)  # Gebruik het eerste bestand in de lijst
    df
  })
  
  output$summ <- renderPrint({
    req(input$file)  # Controleer of het bestand is geüpload
    df <- read.csv(input$file$datapath[1], header = input$header, sep = input$sep, stringsAsFactors = input$stringAsFactors, dec = ',')  # Gebruik het eerste bestand in de lijst
    summary(df)
  })
  
  
  ## MainPanel tabset renderUI code ##
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
  # Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel("Input File Object DF ", tableOutput("filedf"), tableOutput("filedf2")),
        tabPanel("Input File Object Structure", verbatimTextOutput("fileob")),
        tabPanel("Dataset", tableOutput("table")),
        tabPanel("Summary Stats", verbatimTextOutput("summ")))
  })
  
  # Inladen van de dataset
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = TRUE, sep = ";")
    df$Date <- as.Date(df$date, format = "%d/%m/%Y")
    
    # Controleren of de startdatum is ingevoerd
    if (!is.null(input$start_date)) {
      start_date <- as.Date(input$start_date)
      
      # Toevoegen van een nieuwe kolom met dagen sinds de startdatum
      df$Day <- convert_to_days_since_start(df$Date, start_date)
    }
    
    df$DateNumber <- as.numeric(df$Date)
    df <- df %>% arrange(pot_number, Date)
    df <- df %>% group_by(pot_number) %>%
      mutate(DateNumber = row_number())
    df$shootArea2 <- as.numeric(gsub(",", ".", df$shootArea2))
    
    # Genereren van de nieuwe kolom "Geno_Treat"
    df$Geno_Treat <- paste(df$genotype, df$treatments, sep = "_")
    
    return(df)
  })
  
  output$genotype_ui <- renderUI({
    checkboxGroupInput("genotype", "Select Genotype", choices = unique(data()$genotype))
  })
  
  output$genotype_ui2 <- renderUI({
    checkboxGroupInput("genotype2", "Select Genotype", choices = unique(data()$genotype))
  })
  
  observe({
    if (!is.null(input$genotype) && length(input$genotype) > 0) {
      filtered_treatments <- unique(data()[data()$genotype %in% input$genotype, ]$treatments)
      updateCheckboxGroupInput(session, "treatments", choices = filtered_treatments)
      filtered_potnummers <- unique(data()[data()$genotype %in% input$genotype, ]$pot_number)
      updateCheckboxGroupInput(session, "potnummers", choices = filtered_potnummers)
    } else {
      updateCheckboxGroupInput(session, "treatments", choices = unique(data()$treatments))
      updateCheckboxGroupInput(session, "potnummers", choices = unique(data()$pot_number))
    }
  })
  
  output$treatment_ui <- renderUI({
    checkboxGroupInput("treatments", "Select treatments", choices = unique(data()$treatments))
  })
  
  output$treatment_ui2 <- renderUI({
    checkboxGroupInput("treatments2", "Select treatments", choices = unique(data()$treatments))
  })
  
  output$potnummers_ui <- renderUI({
    checkboxGroupInput("potnummers", "Selecteer Potnumbers", choices = unique(data()$pot_number))
  })
  
  output$potnummers_ui2 <- renderUI({
    checkboxGroupInput("potnummers2", "Selecteer Potnumbers", choices = unique(data()$pot_number))
  })
  
  scatter_data <- reactive({
    filtered_data <- data()
    
    if (!is.null(input$treatments) && length(input$treatments) > 0) {
      filtered_data <- filtered_data %>% filter(treatments %in% input$treatments)
    }
    
    if (!is.null(input$potnummers) && length(input$potnummers) > 0) {
      filtered_data <- filtered_data %>% filter(pot_number %in% input$potnummers)
    }
    
    if (!is.null(input$genotype) && length(input$genotype) > 0) {
      filtered_data <- filtered_data %>% filter(genotype %in% input$genotype)
    }
    
    filtered_data
  })
  # grpahs
  output$scatterPlot <- renderPlotly({
    num_colors <- length(unique(scatter_data()$Geno_Treat))
    color_palette <- scales::hue_pal()(num_colors)
    
    # Bereken het gemiddelde per genotype
    mean_data <- scatter_data() %>%
      group_by(Geno_Treat, Day) %>%
      summarise(mean_shootArea2 = mean(shootArea2, na.rm = TRUE))
    
    p <- ggplot() +
      geom_point(data = scatter_data(), aes(x = Day, y = shootArea2, color = factor(Geno_Treat), group = interaction(Geno_Treat, pot_number))) +
      geom_line(data = scatter_data(), aes(x = Day, y = shootArea2, color = factor(Geno_Treat), group = interaction(Geno_Treat, pot_number))) +
      geom_line(data = mean_data, aes(x = Day, y = mean_shootArea2, group = Geno_Treat), color = "black", linetype = "dashed") +
      labs(x = "Days since Start", y = "Shoot Area 2", color = "Genotype-Treatment") +
      theme_minimal() +
      scale_color_manual(values = color_palette) + # Geef kleuren op basis van genotype
      scale_x_continuous(breaks = seq(1, max(scatter_data()$Day), by = 5))  # X-as per 5 dagen
    
    ggplotly(p)  # Converteer ggplot naar plotly
  })
  # Mean Lines plot
  output$genotype_ui_mean <- renderUI({
    checkboxGroupInput("genotype_mean", "Select Genotype", choices = unique(data()$genotype))
  })
  
  observe({
    if (!is.null(input$genotype_mean) && length(input$genotype_mean) > 0) {
      filtered_treatments <- unique(data()[data()$genotype %in% input$genotype_mean, ]$treatments)
      updateCheckboxGroupInput(session, "treatments_mean", choices = filtered_treatments)
    } else {
      updateCheckboxGroupInput(session, "treatments_mean", choices = unique(data()$treatments))
    }
  })
  
  output$treatment_ui_mean <- renderUI({
    checkboxGroupInput("treatments_mean", "Select treatments", choices = unique(data()$treatments))
  })
  
  scatter_data_mean <- reactive({
    filtered_data <- data()
    
    if (!is.null(input$treatments_mean) && length(input$treatments_mean) > 0) {
      filtered_data <- filtered_data %>% filter(treatments %in% input$treatments_mean)
    }
    
    if (!is.null(input$genotype_mean) && length(input$genotype_mean) > 0) {
      filtered_data <- filtered_data %>% filter(genotype %in% input$genotype_mean)
    }
    
    filtered_data
  })
  
  output$meanLinesPlot <- renderPlotly({
    num_colors <- length(unique(scatter_data_mean()$Geno_Treat))
    color_palette <- scales::hue_pal()(num_colors)
    
    # Bereken het gemiddelde per genotype
    mean_data <- scatter_data_mean() %>%
      group_by(Geno_Treat, Day) %>%
      summarise(mean_shootArea2 = mean(shootArea2, na.rm = TRUE),
                sd_shootArea2 = sd(shootArea2, na.rm = TRUE))  # Bereken standaarddeviatie
    
    p <- ggplot() +
      geom_line(data = mean_data, aes(x = Day, y = mean_shootArea2, color = factor(Geno_Treat), group = Geno_Treat)) +
      geom_point(data = mean_data, aes(x = Day, y = mean_shootArea2, color = factor(Geno_Treat), group = Geno_Treat)) +
      geom_errorbar(data = mean_data, aes(x = Day, ymin = mean_shootArea2 - sd_shootArea2, ymax = mean_shootArea2 + sd_shootArea2), width = 0.25) + # Toevoegen van foutbalken
      labs(x = "Days since Start", y = "Mean Shoot Area 2", color = "Genotype-Treatment") +
      theme_minimal() +
      scale_color_manual(values = color_palette) +
      scale_x_continuous(breaks = seq(1, max(scatter_data_mean()$Day), by = 5))
    
    (p)  # Converteer ggplot naar plotly
  })
  # Download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("scatterplot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = {
        num_colors <- length(unique(scatter_data()$Geno_Treat))
        color_palette <- scales::hue_pal()(num_colors)
        
        # Bereken het gemiddelde per genotype
        mean_data <- scatter_data() %>%
          group_by(Geno_Treat, Day) %>%
          summarise(mean_shootArea2 = mean(shootArea2, na.rm = TRUE))
        
        p <- ggplot() +
          geom_point(data = scatter_data(), aes(x = Day, y = shootArea2, color = factor(Geno_Treat), group = interaction(Geno_Treat, pot_number))) +
          geom_line(data = scatter_data(), aes(x = Day, y = shootArea2, color = factor(Geno_Treat), group = interaction(Geno_Treat, pot_number))) +
          geom_line(data = mean_data, aes(x = Day, y = mean_shootArea2, group = Geno_Treat), color = "black", linetype = "dashed") +
          labs(x = "Days since Start", y = "Shoot Area 2", color = "Genotype-Treatment") +
          theme_minimal() +
          scale_color_manual(values = color_palette) + # Geef kleuren op basis van genotype
          scale_x_continuous(breaks = seq(1, max(scatter_data()$Day), by = 5))  # X-as per 5 dagen
        (p) 
      })
    })
  # download mean plot
  output$downloadmeanplot <- downloadHandler(
    filename = function() {
      paste("meanplot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = {
        num_colors <- length(unique(scatter_data()$Geno_Treat))
        color_palette <- scales::hue_pal()(num_colors)
        # Bereken het gemiddelde per genotype
        mean_data <- scatter_data_mean() %>%
          group_by(Geno_Treat, Day) %>%
          summarise(mean_shootArea2 = mean(shootArea2, na.rm = TRUE),
                    sd_shootArea2 = sd(shootArea2, na.rm = TRUE))  # Bereken standaarddeviatie
        
        p <- ggplot() +
          geom_line(data = mean_data, aes(x = Day, y = mean_shootArea2, color = factor(Geno_Treat), group = Geno_Treat)) +
          geom_point(data = mean_data, aes(x = Day, y = mean_shootArea2, color = factor(Geno_Treat), group = Geno_Treat)) +
          geom_errorbar(data = mean_data, aes(x = Day, ymin = mean_shootArea2 - sd_shootArea2, ymax = mean_shootArea2 + sd_shootArea2), width = 0.25) + # Toevoegen van foutbalken
          labs(x = "Days since Start", y = "Mean Shoot Area 2", color = "Genotype-Treatment") +
          theme_minimal() +
          scale_color_manual(values = color_palette) +
          scale_x_continuous(breaks = seq(1, max(scatter_data_mean()$Day), by = 5))
      })
    })
  
  # outliers + berekeningen
  outliers <- reactiveVal(NULL)
  # Bereken gemiddelde, standaardfout en outliers
  output$geno_treat_stats <- renderPrint({
    df <- scatter_data()
    # pijplijnoperator (%>%), die wordt gebruikt om de dataset df te bewerken. 
    # Hier wordt eerst gefilterd op de dag 24 (filter(Day == 24)), 
    # dan worden de gegevens gegroepeerd per Geno_Treat (group_by(Geno_Treat)), 
    # en vervolgens worden de samenvattende statistieken berekend, waaronder het gemiddelde (mean_shootArea2)
    # de standaarddeviatie (sd_shootArea2), en het aantal waarnemingen (n) voor elke groep Geno_Treat.
    stats <- df %>%
      filter(Day == 24) %>%
      group_by(Geno_Treat) %>%
      summarise(mean_shootArea2 = mean(shootArea2, na.rm = TRUE),
                sd_shootArea2 = sd(shootArea2, na.rm = TRUE),
                n = n())
    
    stats$SE <- stats$sd_shootArea2 / sqrt(stats$n)
    stats$outlier_lower_bound <- stats$mean_shootArea2 - 2 * stats$sd_shootArea2
    stats$outlier_upper_bound <- stats$mean_shootArea2 + 2 * stats$sd_shootArea2
    
    outliers(df %>%
               # indien de dag anders is verander hier de dag.
               filter(Day == 24) %>% # dag 24 tonen
               inner_join(stats, by = "Geno_Treat") %>% # samenvoegen van de stats en geno_treat data
               # Hier worden de rijen gefilterd waar de waarde van shootArea2 kleiner is dan de ondergrens van de outliers (gemiddelde - 2 * standaardfout)
               # of groter is dan de bovengrens van de outliers (gemiddelde + 2 * standaardfout).
               filter(shootArea2 < outlier_lower_bound | shootArea2 > outlier_upper_bound) %>%
               # zorgt ervoor dat alleen unieke potnummers worden behouden, zodat elk potnummer slechts één keer wordt vermeld
               distinct(pot_number))
    
    cat("The following pots should be considered as outliers:", paste(outliers(), collapse = ", "))
  })
  # remove button
  observeEvent(input$remove_outliers_btn, {
    df <- scatter_data()
    df <- df[!df$pot_number %in% outliers(), ]
    outliers(NULL)
    output$action_message <- renderText("Outliers removed successfully.")
  })
  # Event handler for keeping data
  observeEvent(input$keep_data_btn, {
    output$action_message <- renderText("Data kept successfully.")
  })
  
  
  ##################################################################
  ## statistical tests ##
  ##################################################################
  # Inlezen van de dataset voor statistische tests en aanpassen van de datum
  output$genotype_ui_test <- renderUI({
    checkboxGroupInput("selected_genotypes", "Select genotypes for analysis", choices = unique(data()$genotype))
  })
  
  observeEvent(input$run_statistical_tests_btn, {
    req(input$selected_day, input$selected_genotypes)
    
    # Controleer of minimaal twee genotypes zijn geselecteerd
    if (length(input$selected_genotypes) < 2) {
      showModal(modalDialog(
        title = "Error",
        "Please select at least two genotypes for analysis.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Subset van de data voor de geselecteerde dag en genotypes
    subset_data <- data() %>% filter(Day == input$selected_day + 1, genotype %in% input$selected_genotypes)
    
    # Voorbereiden ANOVA test
    Genotype <- as.factor(subset_data$genotype)
    Treatment <- as.factor(subset_data$treatments)
    anovatest <- aov(shootArea2 ~ Genotype + Treatment + Genotype*Treatment, data = subset_data)
    
    # Uitvoeren ANOVA test
    output$anova_title <- renderText({
      "\n\nANOVA tests for interaction\n"
    })
    output$anova_summary <- renderPrint({
      print(summary(anovatest))
    })
    
    # Uitvoeren TukeyHSD test
    output$tukey_summary <- renderPrint({
      cat("\nPairwise comparisons - TukeyHSD test.\n")
      cat("Genotype 1 vs. genotype 2, in control conditions. Adj. p-value =", TukeyHSD(anovatest)$'Genotype:Treatment'[6,4], "\n")
      cat("Genotype 1 vs. genotype 2, in treatment conditions. Adj. p-value =", TukeyHSD(anovatest)$'Genotype:Treatment'[1,4], "\n")
      cat("Genotype 1, control vs. treatment conditions. Adj. p-value =", TukeyHSD(anovatest)$'Genotype:Treatment'[2,4], "\n")
      cat("Genotype 2, control vs. treatment conditions. Adj. p-value =", TukeyHSD(anovatest)$'Genotype:Treatment'[3,4], "\n")
    })
  })
}

# Start de Shiny app
shinyApp(ui, server)
