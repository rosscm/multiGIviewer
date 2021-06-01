#' main UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny NS tagList
#'
#' @noRd
mod_main_ui <- function(id) {
  ns <- NS(id)
  tagList(
    col_2(uiOutput(ns("getData"))),
    col_2(uiOutput(ns("getPlot"))),
    rep_br(2),
    h3(textOutput(ns("text"))),
    rep_br(1),
    withSpinner(plotOutput(ns("plot"))),
    rep_br(2),
    DTOutput(ns("results"))
  )
}

#' main Server Functions
#'
#' @import dplyr
#' @import ggplot2
#' @import ggrepel
#' @importFrom purrr reduce
#' @importFrom DT renderDT DTOutput
#' @importFrom grDevices colorRampPalette
#' @importFrom utils write.csv
#'
#' @noRd
mod_main_server <- function(id, rvals, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({

      if (is.null(rvals$datasetInput) || is.null(rvals$queryInput) || is.null(rvals$mediaInput)) {
        return(NULL)
      }

      ######
      # DATA SUBSETTING
      ######

      # Selected dataset
      dat <- dataset[[rvals$datasetInput]]
      # Subset qGI data by selected queryInput
      qgi <- dat[["qGI"]][which(colnames(dat[["qGI"]]) %in% rvals$queryInput)]
      # Subset FDR data by selected queryInput
      fdr <- dat[["FDR"]][which(colnames(dat[["FDR"]]) %in% rvals$queryInput)]
      # Subset mutant LFC data by selected queryInput
      fc_double <- dat[["fc_doublePhenotype"]][which(colnames(dat[["fc_doublePhenotype"]]) %in% rvals$queryInput)]
      # Subset wildtype LFC data by selected mediaInput
      fc_single <- dat[["fc_singlePhenotype"]][which(colnames(dat[["fc_singlePhenotype"]]) %in% rvals$mediaInput)]


      #observe({ print(ncol(qgi)) })
      #observe({ print(ncol(fdr)) })
      #observe({ print(ncol(fc_double)) })
      #observe({ print(ncol(fc_single)) })

      if (!ncol(qgi) || !ncol(fc_single)) {
        output$text <- NULL
        output$plot <- NULL
        output$results <- NULL
        return(NULL)
      }

      ######
      # DATA CONSTRUCTING
      ######

      # Filter for consistent positive and negative GIs
      pos <- filter_at(qgi, colnames(qgi), all_vars(. > 0))
      neg <- filter_at(qgi, colnames(qgi), all_vars(. < 0))
      pn <- rbind(pos, neg)

      # Determine n screens where GI is significant
      fdr_pn <- subset(fdr, rownames(fdr) %in% rownames(pn))
      n_sig <- sapply(as.data.frame(t(fdr_pn)), function(x) {
        length(which(x < rvals$fdrInput))
      })

      screen_sig <- sapply(as.data.frame(t(fdr_pn)), function(x) {
        ind <- which(x < rvals$fdrInput)
        ind <- colnames(fdr_pn)[ind]
        ind <- paste(ind, collapse = ", ")
        return(ind)
      })

      # Subset FDR data for GIs significant in at least 2 screens
      fdr_sig <- fdr_pn %>%
        mutate(n_sig = n_sig) %>%
        mutate(screen_sig = screen_sig) %>%
        subset(n_sig >= 2)

      # Subset for significant qGI data
      pn <- subset(pn, rownames(pn) %in% rownames(fdr_sig))
      fdr_sig <- fdr_sig[rownames(pn),]
      qgi_in <- data.frame(
        gene = rownames(pn),
        mean_qGI = signif(rowMeans(pn), 3),
        min_FDR = signif(apply(fdr_sig[,-ncol(fdr_sig)], 1, min), 3),
        n_sig = fdr_sig$n_sig,
        screen_sig = fdr_sig$screen_sig
      )

      # Subset double mutant LFC data for significant GIs
      fc_double_in <- subset(fc_double, rownames(fc_double) %in% qgi_in$gene)
      fc_double_in <- data.frame(
        gene = rownames(fc_double_in),
        mean_koLFC = signif(rowMeans(fc_double_in), 3)
      )

      # Subset single mutant (wildtype) LFC data for significant GIs
      fc_single_in <- subset(fc_single, colnames(fc_single) %in% rvals$mediaInput)
      fc_single_in <- data.frame(
        gene = rownames(fc_single_in),
        mean_wtLFC = signif(rowMeans(fc_single_in), 3)
      )

      # Combine data for plotting
      results <- list(qgi_in, fc_double_in, fc_single_in) %>%
        reduce(left_join, by = "gene") %>%
        select(gene, mean_qGI, min_FDR, mean_wtLFC, mean_koLFC, n_sig, screen_sig) %>%
        mutate(n_sig = ifelse(mean_qGI < 0, paste0("negative (", n_sig, ")"), paste0("positive (", n_sig, ")")))

      # Define plot labels
      # Top 10 negative
      labs_neg <- results %>%
        arrange(mean_qGI) %>%
        select(gene) %>%
        slice(1:10) %>%
        unlist() %>%
        as.character()

      # Top 10 positive
      labs_pos <- results %>%
        arrange(desc(mean_qGI)) %>%
        select(gene) %>%
        slice(1:10) %>%
        unlist() %>%
        as.character()

      # Combine
      labels <- paste(c(labs_neg, labs_pos), collapse = ", ")

      # Prepare results for plotting
      labs_in <- unlist(strsplit(as.character(labels), ", "))
      labs_in <- data.frame(gene = labs_in, label = sprintf("italic('%s')", labs_in))

      # Define colour gradients for positive and negative GIs
      # using reactive colour inputs
      negColFunc <- colorRampPalette(c("#61C2FA", rvals$negColInput))
      posColFunc <- colorRampPalette(c("#FAE057", rvals$posColInput))
      negCol <- negColFunc(length(unique(grep("negative", results$n_sig, value = TRUE))))
      posCol <- posColFunc(length(unique(grep("positive", results$n_sig, value = TRUE))))
      cols <- c(negCol, posCol)

      # Plot
      p <- results %>%
        left_join(labs_in, by = "gene") %>%
        ggplot(aes(x = mean_wtLFC, y = mean_koLFC)) +
          geom_point(
            aes(size = abs(mean_qGI), fill = n_sig),
            shape = 21
          ) +
          labs(
            x = "Fitness HAP1 wildtype [LFC]",
            y = "Fitness HAP1 knockout [LFC]",
            size = "Mean |qGI| score",
            fill = "Genetic interaction\nin n screens"
          ) +
          scale_fill_manual(
            values = cols,
            guide = guide_legend(override.aes = list(size = 3))
          ) +
          theme_linedraw(base_size = 14) +
          theme(panel.grid = element_blank(),
                legend.key.size = unit(0.5, "cm"))

      # Add plot labels
      if (rvals$typeInput == "Text") {
        p <- p + geom_text_repel(aes_string(label = "label"), parse = TRUE, na.rm = TRUE)
      }
      if (rvals$typeInput == "Padded box") {
        p <- p + geom_label_repel(aes_string(label = "label"), parse = TRUE, na.rm = TRUE)
      }

      # Add reference lines
      if (is.null(rvals$lineInput)) {
        p <- p
      } else {
        if ("y=x" %in% rvals$lineInput) {
          p <- p + geom_abline(linetype = "dotted")
        }
        if ("x=0" %in% rvals$lineInput) {
          p <- p + geom_hline(yintercept = 0, linetype = "dotted")
        }
        if ("y=0" %in% rvals$lineInput) {
          p <- p + geom_vline(xintercept = 0, linetype = "dotted")
        }
      }

      # Store results in rvals object
      observeEvent(results, {
        rvals$resultsOutput = results
      })
      observeEvent(labels, {
        rvals$labelsInput = labels
      })

      # Get rid of check NOTEs
      gene=mean_qGI=min_FDR=mean_wtLFC=mean_koLFC=n_sig=screen_sig=NULL
      .="shut up"

      ######
      # OUTPUT RENDERING
      ######

      # Output text summary
      output$text <- renderText({
        paste("Total replicated interactions:", nrow(results))
      })

      # Output plot
      output$plot <- renderPlot({
        return(p)
      })

      # Output results table
      output$results <- renderDT({
        return(results)
      })

      ######
      # OUTPUT DOWNLOADING
      ######

      output$getData <- renderUI({
        req(results)
        downloadButton(ns("downloadData"), label = "Download table")
      })

      output$downloadData <- downloadHandler(
       filename = function() {
         paste0(rvals$datasetInput, "_multiGItable", ".csv")
       },
       content = function(con) {
         write.csv(results, con)
      })


      output$getPlot <- renderUI({
        req(results)
        downloadButton(ns("downloadPlot"), label = "Download plot")
      })

      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste0(rvals$datasetInput, "_multiGIplot", ".png")
        },
        content = function(file) {
          device <- function(..., width, height) grDevices::png(..., width = 9, height = 5.5, res = 300, units = "in")
          ggsave(file, plot = p, device = device)
      })

    })
  })
}
