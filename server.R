# multiGIviewer server
# Load packages
library(shiny)
library(dplyr)
library(purrr)
library(DT)
library(ggplot2)
library(ggthemes)
library(ggrepel)

# Define server function
server <- function(input, output) {

  ######
  # USER INPUTS
  ######

  # Available datasets
  datasets <- c("GIN_20210107", "CHEM_20210222")

  # Select dataset
  output$datasetOutput <- renderUI({
    selectInput("datasetInput", "Select dataset:",
                datasets, multiple = FALSE, selected = NULL)
  })

  # Load selected dataset
  dataset <- reactive({
    filename <- paste0("datasets/", input$datasetInput, ".Rdata")
    get(load(filename))
  })

  # Select queries
  output$queryOutput <- renderUI({
    if (is.null(input$datasetInput)) {
      return(NULL)
    }
    selectInput("queryInput", "Select screens (choose at least two):",
                sort(colnames(dataset()[["qgi"]])), multiple = TRUE,
                selected = NULL)
  })

  # Select media condition
  output$mediaOutput <- renderUI({
    if (is.null(input$datasetInput)) {
      return(NULL)
    }
    selectInput("mediaInput", "Select media condition(s):",
                colnames(dataset()[["fc_single"]]), multiple = TRUE,
                selected = NULL)
  })

  # Select FDR threshold
  output$fdrOutput <- renderUI({
    if (is.null(input$datasetInput)) {
      return(NULL)
    }
    sliderInput("fdrInput", "Select FDR threshold:",
                min = 0, max = 1, value = 0.2)
  })

  ######
  # DATA SETTING
  ######

  # Subset qGI data by selected queryInput
  qgi <- reactive({
    if (is.null(input$queryInput)) {
      return(NULL)
    }
    dataset()[["qgi"]][which(colnames(dataset()[["qgi"]]) %in% input$queryInput)]
  })

  # Subset FDR data by selected queryInput
  fdr <- reactive({
    if (is.null(input$queryInput)) {
      return(NULL)
    }
    dataset()[["fdr"]][which(colnames(dataset()[["fdr"]]) %in% input$queryInput)]
  })

  # Subset mutant LFC data by selected queryInput
  fc_double <- reactive({
    if (is.null(input$queryInput)) {
      return(NULL)
    }
    dataset()[["fc_double"]][which(colnames(dataset()[["fc_double"]]) %in% input$queryInput)]
  })

  # Subset wildtype LFC data by selected mediaInput
  fc_single <- reactive({
    if (is.null(input$mediaInput)) {
      return(NULL)
    }
    dataset()[["fc_single"]][which(colnames(dataset()[["fc_single"]]) %in% input$mediaInput)]
  })

  ######
  # DATA CONSTRUCTING
  ######

  # Subset for significant GIs
  qgi_in <- reactive({
    if (is.null(input$queryInput)) {
      return(NULL)
    }

    # Filter for consistent positive and negative GIs
    pos <- filter_at(qgi(), colnames(qgi()), all_vars(. > 0))
    neg <- filter_at(qgi(), colnames(qgi()), all_vars(. < 0))
    pn <- rbind(pos, neg)

    # Determine n screens GI is significant in
    fdr_pn <- subset(fdr(), rownames(fdr()) %in% rownames(pn))
    n_sig <- sapply(as.data.frame(t(fdr_pn)), function(x) {
      length(which(x < input$fdrInput))
    })

    # Subset FDR data for GIs significant in at least 2 screens
    fdr_sig <- fdr_pn %>%
      mutate(n_sig = n_sig) %>%
      subset(n_sig >= 2)

    # Subset for significant qGI data
    pn <- subset(pn, rownames(pn) %in% rownames(fdr_sig))
    pn_sig <- data.frame(
      gene = rownames(pn),
      mean_qGI = rowMeans(pn),
      min_FDR = apply(fdr_sig[,-ncol(fdr_sig)], 1, min),
      n_sig = fdr_sig$n_sig
    )
  })

  # Subset double mutant LFC data for significant GIs
  fc_double_in <- reactive({
    if (is.null(qgi_in())) {
      return(NULL)
    }
    fc_double <- subset(fc_double(), rownames(fc_double()) %in% qgi_in()$gene)
    fc_double <- data.frame(
      gene = rownames(fc_double),
      mean_koLFC = rowMeans(fc_double)
    )
  })

  # Subset single mutant (wildtype) LFC data for significant GIs
  fc_single_in <- reactive({
    if (is.null(input$mediaInput)) {
      return(NULL)
    }
    fc_single <- subset(fc_single(), colnames(fc_single()) %in% input$mediaInput)
    fc_single <- data.frame(
      gene = rownames(fc_single),
      mean_wtLFC = rowMeans(fc_single)
    )
  })

  # Combine data for plotting
  results <- reactive({
    if (is.null(fc_single_in()) || is.null(qgi_in())) {
      return(NULL)
    }
    res <- list(qgi_in(), fc_double_in(), fc_single_in()) %>%
      reduce(left_join, by = "gene") %>%
      select(gene, mean_qGI, min_FDR, mean_wtLFC, mean_koLFC, n_sig) %>%
      mutate(n_sig = ifelse(mean_qGI < 0, paste0("negative (", n_sig, ")"), paste0("positive (", n_sig, ")")))
  })

  ######
  # ADDITIONAL USER INPUTS (BASED ON CONSTRUCTED DATA)
  ######

  # Select plot labels
  output$labelOutput <- renderUI({
    if (is.null(results())) {
      return(NULL)
    }

    # Top 10 negative
    labs_neg <- results() %>%
      arrange(mean_qGI) %>%
      select(gene) %>%
      slice(1:10) %>%
      unlist() %>%
      as.character()

    # Top 10 positive
    labs_pos <- results() %>%
      arrange(desc(mean_qGI)) %>%
      select(gene) %>%
      slice(1:10) %>%
      unlist() %>%
      as.character()

    # Combine
    labels <- paste(c(labs_neg, labs_pos), collapse = ", ")
    textAreaInput("labelInput", "List plot labels (character sensitive):",
                  value = labels, height = "150px")
  })

  # Select label type
  output$typeOutput <- renderUI({
    if (is.null(results())) {
      return(NULL)
    }
    radioButtons("typeInput", "Select label type:",
                 list("Text", "Padded box"),
                 selected = "Text")
  })

  # Select plot reference lines
  output$lineOutput <- renderUI({
    if (is.null(results())) {
      return(NULL)
    }
    checkboxGroupInput("lineInput", "Select reference line(s):",
                       list("y=x", "x=0", "y=0"),
                       selected = NULL)
  })

  ######
  # OUTPUT RENDERING
  ######

  plotInput <- reactive({
    if (is.null(input$labelInput)) {
      return(NULL)
    }

    # Get plot labels
    labs_in <- unlist(strsplit(as.character(input$labelInput), ", "))
    labs_in <- data.frame(gene = labs_in, label = sprintf("italic('%s')", labs_in))

    if (is.null(results())) {
      return(NULL)
    }

    # Define colour gradients for positive and negative GIs
    negColFunc <- colorRampPalette(c("#61c2fa", "#014e7a"))
    posColFunc <- colorRampPalette(c("#fae057", "#826f03"))
    negCol <- negColFunc(length(unique(grep("negative", results()$n_sig, value = TRUE))))
    posCol <- posColFunc(length(unique(grep("positive", results()$n_sig, value = TRUE))))
    cols <- c(negCol, posCol)

    # Plot
    p <- results() %>%
      left_join(labs_in, by = "gene") %>%
      ggplot(aes(x = mean_wtLFC, y = mean_koLFC)) +
        geom_point(aes(size = abs(mean_qGI), fill = n_sig), shape = 21) +
        labs(x = "Fitness HAP1 wildtype [LFC]",
             y = "Fitness HAP1 knockout [LFC]",
             size = "Mean |qGI| score",
             fill = "Genetic interaction\nin n screens") +
        scale_fill_manual(values = cols,
                          guide = guide_legend(override.aes = list(size = 3))) +
        theme_linedraw(base_size = 14) +
        theme(panel.grid = element_blank(),
              legend.key.size = unit(0.5, "cm"))

    # Add plot labels
    if (input$typeInput == "Text") {
      p <- p + geom_text_repel(aes(label = label), parse = TRUE, na.rm = TRUE)
    }
    if (input$typeInput == "Padded box") {
      p <- p + geom_label_repel(aes(label = label), parse = TRUE, na.rm = TRUE)
    }

    # Add reference lines
    if (is.null(input$lineInput)) {
      p
    } else {
      if ("y=x" %in% input$lineInput) {
        p <- p + geom_abline(linetype = "dotted")
      }
      if ("x=0" %in% input$lineInput) {
        p <- p + geom_hline(yintercept = 0, linetype = "dotted")
      }
      if ("y=0" %in% input$lineInput) {
        p <- p + geom_vline(xintercept = 0, linetype = "dotted")
      }
    }
    p
  })

  # Output text
  output$text <- renderText({
    if (is.null(results())) {
      return(NULL)
    }
    paste("Total replicated interactions:", nrow(results()))
  })

  # Output plot
  output$plot <- renderPlot({
    print(plotInput())
  })

  # Output results table
  output$results <- renderDataTable({
    if (is.null(results())) {
      return(NULL)
    }
    results()
  })

  ######
  # OUTPUT DOWNLOADING
  ######

  output$downloadData <- downloadHandler(
   filename = function() {
     paste0(input$datasetInput, "_multiGItable", ".csv")
   },
   content = function(con) {
     write.csv(results(), con)
   }
  )

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$datasetInput, "_multiGIplot", ".png")
    },
    content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = 9, height = 5.5, res = 300, units = "in")
        ggsave(file, plot = plotInput(), device = device)
    }
  )
}
