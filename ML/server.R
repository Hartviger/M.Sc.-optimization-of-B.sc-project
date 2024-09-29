shinyServer(function(session, input, output) {
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  ## General
  rv <- reactiveValues(data = list(), sequence = list(), activeFile = NULL, 
                       tmpData = NULL, tmpSequence = NULL, 
                       choices = NULL, drift_plot_select = 1, info = vector("character"))
  
  ## Statistics
  st <- reactiveValues(stats = list(), sequence = list(), results = list(),
                       comparisons = list(), colcomp = list())
  
  userConfirmation <- reactiveVal(FALSE)
  
  rankings <- read.csv("./csvfiles/rankings.csv", stringsAsFactors = FALSE)
  
  observeEvent(list(c(input$sequence, input$example, input$submit)), {
    windowselect("sequence")
  }, ignoreInit = T
  )
  observeEvent(input$explore, {
    windowselect("datatable")
  })
  observeEvent(input$export, {
    windowselect("export")
  })
  observeEvent(input$statistics_button, {
    windowselect("statistics")
    updateSelectInput(session, "group1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "group2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "time1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
    updateSelectInput(session, "time2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
  })
  
  # Functions
  
  initializeVariables <- function() {
    st$stats[[length(st$stats) + 1]] <- data.frame()
    st$sequence[[length(st$stats) + 1]] <- data.frame()
    st$results[[length(st$results) + 1]] <- list()
    st$comparisons[[length(st$comparisons) + 1]] <- vector("character")
    st$colcomp[[length(st$colcomp) + 1]] <- vector("numeric")
  }
  
  observeEvent(input$submit, {
    shinyCatch({
      inputFile <- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
      if(input$fileType == "Samples in rows") {
        inputFile <- t(inputFile)
      }
    },
    blocking_level = 'message'
    )
    if(any(duplicated(names(inputFile)))) {
      sendSweetAlert(session, title = "Error", text = paste("Duplicate columns found."), type = "error")
    } else {
      labels <- identifyLabels(inputFile)
      #checkColumns(colnames(inputFile), labels)
      batch <- NA
      order <- NA
      class <- NA
      time <- NA
      paired <- NA
      initializeVariables()
      rv$sequence[[length(rv$sequence) + 1]] <- data.frame(labels, batch, order, class, time, paired)
      rv$data[[length(rv$data) + 1]] <- inputFile
      names(rv$data)[length(rv$data)] <- substr(input$inputFile$name, 1, nchar(input$inputFile$name) - 4)
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateTabItems(session, "tabs", selected = "Datainput")
      show("buttons")
    }
  })
  
  observeEvent(input$inputSequence, {
    shinyCatch({
      inputSequence <- read.csv(input$inputSequence$datapath, header = 1, stringsAsFactors = FALSE)
      colnames(inputSequence) <- tolower(colnames(inputSequence))
      inputSequence <- checkSequence(inputSequence)
    },
    blocking_level = 'message'
    )
    sequence <- rv$sequence[[rv$activeFile]]
    labeledSequence <- data.frame("sample" = row.names(sequence), sequence)
    inputSequence["sample"] <- lapply(inputSequence["sample"], as.character)
    sequence <- left_join(labeledSequence[, 1:2], inputSequence, by = "sample")
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[rv$activeFile]] <- sequence
  })
  
  observeEvent(input$reuseSequence, {
    inputSequence <- read.csv(input$inputSequence$datapath, header = 1, stringsAsFactors = FALSE)
    colnames(inputSequence) <- tolower(colnames(inputSequence))
    inputSequence <- checkSequence(inputSequence)
    sequence <- rv$sequence[[rv$activeFile]]
    labeledSequence <- data.frame("sample" = row.names(sequence), sequence)
    inputSequence["sample"] <- lapply(inputSequence["sample"], as.character)
    sequence <- left_join(labeledSequence[, 1:2], inputSequence, by = "sample")
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[rv$activeFile]] <- sequence
  })
  
  observeEvent(input$editSequence, {
    showModal(
      modalDialog(
        title = "Edit columns", size = "s", easyClose = TRUE,
        footer = list(actionButton("seq_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 9, h4("Column name")),
          column(width = 3, style = "text-align: left;", h4("Keep"))
        ),
        lapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
          fluidRow(
            column(
              width = 9,
              textInput(paste0("seq_edit_name", x), NULL, value = colnames(rv$data[[rv$activeFile]])[x])
            ),
            column(
              width = 3, style = "text-align: center;",
              prettyCheckbox(paste0("seq_edit_keep", x), NULL, status = "info", value = T)
            ),
          )
        })
      )
    )
  })
  
  observeEvent(input$editGroups, {
    showModal(
      modalDialog(
        title = "Edit Group Nicknames", size = "s", easyClose = TRUE,
        footer = list(actionButton("group_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 3, h4("Group")),
          column(width = 9, h4("Nickname"))
        ), 
        lapply(seq(unique(rv$sequence[[rv$activeFile]][, 4][!is.na(rv$sequence[[rv$activeFile]][, 4])])), function(x) {
          group <- sort(unique(rv$sequence[[rv$activeFile]][, 4][!is.na(rv$sequence[[rv$activeFile]][, 4])]))[x]
          fluidRow(
            column(width = 2, h5(stri_extract_first_regex(group, "[0-9]+"))),
            column(
              width = 10,
              textInput(paste0("edit_nickname", x), NULL, value = NULL)
            ),
          )
        }),
      )
    )
  })
  
  # Duplicates not allowed
  observeEvent(input$seq_edit_confirm, {
    # sapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
    #   isolate(colnames(rv$data[[rv$activeFile]])[x] <- input[[paste0("seq_edit_name", x)]])
    #   isolate(row.names(rv$sequence[[rv$activeFile]])[x] <- input[[paste0("seq_edit_name", x)]])
    # })
    keep <- sapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) input[[paste0("seq_edit_keep", x)]])
    rv$data[[rv$activeFile]] <- rv$data[[rv$activeFile]][, keep]
    rv$sequence[[rv$activeFile]] <- rv$sequence[[rv$activeFile]][keep, ]
    removeModal()
  })
  
  observeEvent(input$group_edit_confirm, {
    groups <- rv$sequence[[rv$activeFile]][, 4]
    sapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
      if(!is.na(groups[x])) {
        isolate(rv$sequence[[rv$activeFile]][, 4][x] <- paste(rv$sequence[[rv$activeFile]][, 4][x], input[[paste0("edit_nickname", groups[x])]], sep = ": "))
      }
    })
    removeModal()
    show("sequence_panel")
  })
  
  observeEvent(input$example, {
    # Lipidomics
    data <- read.csv("./csvfiles/Eva pos export from profinder.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./csvfiles/sequence_lipidomics_pos.csv", stringsAsFactors = FALSE)
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Lipidomics_pos"
    initializeVariables()
    
    # Metabolomics
    data <- read.csv("./csvfiles/Woz export from mzmine pos.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./csvfiles/sequence_metabolomics_pos.csv", stringsAsFactors = FALSE)
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Metabolomics_pos"
    initializeVariables()
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateTabItems(session, "tabs", selected = "Datainput")
    show("buttons")
    updateCollapse(session, "menu", close = "Data input")
    disable("example")
  })
  
  # Update selected data
  observeEvent(input$selectDataset, ignoreInit = TRUE, { 
    rv$activeFile <- which(rv$choices %in% input$selectDataset)
    rows <- nrow(rv$data[[rv$activeFile]])
    cols <- ncol(rv$data[[rv$activeFile]])
    if(rows > 50)
      rows <- 50
    if(cols > 30)
      cols <- 30
    forVisuals <- rv$data[[rv$activeFile]][1:rows, 1:cols]
    sequence <- rv$sequence[[rv$activeFile]]
    data <- rv$data[[rv$activeFile]]
    
    output$seq_table <- renderDT(rv$sequence[[rv$activeFile]], extensions = 'Responsive', server = F, 
                                 editable = T, selection = 'none', options = list(pageLength = nrow(rv$sequence[[rv$activeFile]]), 
                                                                                  fixedHeader = TRUE))
    output$diboxtitle <- renderText(names(rv$data[rv$activeFile]))
    output$dttable <- renderDT(data, rownames = FALSE, options = list(scrollX = TRUE, 
                                                                      scrollY = "700px"))
    output$dt_drift_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"], rownames = FALSE, 
                                      options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))
    output$dt_boxplot_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"], rownames = FALSE, 
                                        options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))
    
    output$histogram <- renderPlotly({
      samples <- data[, sequence[ , 'labels'] %in% "Sample"]
      samples[is.na(samples)] <- 0
      medians <- apply(samples, 2, median)
      median_data <- data.frame(
        Sample = names(medians),
        Median = medians
      )
      ggplot(median_data, aes(x = Sample, y = Median)) +
        geom_col(fill = "skyblue", color = "black") +
        labs(x = "Sample", y = "Median") +
        theme_minimal()
    })
    
    output$histogram_qc <- renderUI({
      QCs <- data[, sequence[ , 'labels'] %in% "QC"]
      if(ncol(QCs) > 0) {
        QCs[is.na(QCs)] <- 0
        medians <- apply(QCs, 2, median)
        median_QC <- data.frame(
          QC = names(medians),
          Median = medians
        )
        plotlyOutput("qc_distribution")
        output$qc_distribution <- renderPlotly({
          ggplot(median_QC, aes(x = QC, y = Median)) +
            geom_col(fill = "skyblue", color = "black") +
            labs(x = "Sample", y = "Median") +
            theme_minimal()
        })
      }
      else {
        textOutput("No columns labeled QC.")
      } 
    })
    
    if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") == 1) {
      internalStandards <- findInternalStandards(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"])
      updateCheckboxGroupInput(session, "isChoose", choices = internalStandards, selected = internalStandards)
      enable("normalizeIS"); enable("optimizeIS"); enable("removeIS"); enable("saveIS")
      if(length(internalStandards) == 0) {
        disable("normalizeIS"); disable("optimizeIS"); disable("removeIS"); disable("saveIS")
      } 
    }
    
    # Statistics
    updateSelectInput(session, "group1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "group2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "time1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
    updateSelectInput(session, "time2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
  })
  
  observeEvent(rv$choices, {
    output$downloadSequence <- downloadHandler(
      filename <- function() {
        paste0(names(rv$data[rv$activeFile]), "_seq.csv")
      },
      content = function(file) {
        write.csv(cbind("sample" = rownames(rv$sequence[[rv$activeFile]]), rv$sequence[[rv$activeFile]]), file, row.names = FALSE) # TODO
      }
    )
    
    # Export panel
    output$export_ui <- renderUI({
      lapply(1:length(rv$choices), function(x) {
        fluidRow(column(12, downloadLink(paste0("dwn", x), paste0(rv$choices[x], ".csv"))))
      })   
    })
    output$export_metabo <- renderUI({
      lapply(1:length(rv$choices), function(x) {
        fluidRow(column(12, downloadLink(paste0("dwn_metabo", x), paste0(rv$choices[x], "_metabo.csv"))))
      })
    })
    output$export_stats <- renderUI({
      lapply(1:length(rv$choices), function(x) {
        fluidRow(column(12, downloadLink(paste0("dwn_stats", x), paste0(rv$choices[x], "_results.xlsx"))))
      })
    })
    output$export_settings <- renderUI({
      lapply(1:length(rv$choices), function(x) {
        fluidRow(column(12, downloadLink(paste0("dwn_settings", x), paste0(rv$choices[x], ".txt"))))
      })
    })
    
    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_stats", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_results.xlsx")
        },
        content = function(file) {
          write_xlsx(st$results[[x]], file)
        }
      )
    })
    
    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), ".csv")
        },
        content = function(file) {
          write.csv(rv$data[[x]], file, row.names = FALSE)
        }
      )
    })
    
    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_settings", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), ".txt")
        },
        content = function(file) {
          write.csv(rv$info[x], file, row.names = FALSE)
        }
      )
    })
    
    lapply(1:length(rv$choices), function(x) {
      dat <- rv$data[[x]]
      seq <- rv$sequence[[x]]
      seq[seq[, 1] %in% "QC", 4] <- "QC"
      class <- c("", seq[seq[, 1] %in% c("Sample", "QC"), 4])
      outdat <- data.frame(dat[seq[, 1] %in% "Name"], dat[seq[, 1] %in% c("Sample", "QC")])
      outdat <- rbind(class, outdat)
      output[[paste0("dwn_metabo", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_metabo.csv")
        },
        content = function(file) {
          write.csv(outdat, file, row.names = FALSE)
        }
      )
    })
    
    updateCheckboxGroupInput(session, "export_xml_list", choices = rv$choices, selected = NULL)
    updateSelectInput(session, "mergeFile", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "drift_select", choices = c("None", rv$choices))
    updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca2", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateCheckboxGroupInput(session, "filesToRemove", choices = names(rv$data), selected = NULL)
  })
  
  observeEvent(input$removeFiles, {
    if (is.null(input$filesToRemove)) {
      showNotification("No files selected.", type = "error")
    } else if(length(input$filesToRemove) == length(rv$choices)) {
      showNotification("At least one file must be kept.", type = "error")
    } else {
      keep <- !names(rv$data) %in% input$filesToRemove      
      rv$data <- rv$data[keep]
      rv$sequence <- rv$sequence[keep]
      rv$info <- rv$info[keep]
      st$stats <- st$stats[keep]
      st$sequence <- st$sequence[keep]
      st$results <- st$results[keep]
      st$comparisons <- st$comparisons[keep]
      st$colcomp <- st$colcomp[keep]
      rv$activeFile <- names(rv$data)[length(rv$data)]
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      showNotification("Files removed.", type = "message")
    }
  })
  
  observeEvent(input$export_xml_list, {
    output$export_xml <- downloadHandler(
      filename = function() {
        paste0(names(rv$data[rv$choices %in% input$export_xml_list])[1], ".xlsx")
      },
      content = function(file) {
        write_xlsx(rv$data[rv$choices %in% input$export_xml_list], file)
      }
    )
  })
  
  observeEvent(input$seq_table_cell_edit, {
    sequence <- rv$sequence[[rv$activeFile]]
    info <- input$seq_table_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    if(j == 1) {
      sendSweetAlert(session, title = "Warning", text = "Column 'labels' cannot be edited", type = "warning")
    } else {
      sequence[i, j] <- v
      rv$sequence[[rv$activeFile]] <- sequence
    }
  })
  
  observeEvent(input$updateSequence, {
    if (!is.null(rv$activeFile) && !is.null(rv$tmpSequence)) {
      rv$sequence[[rv$activeFile]] <- rv$tmpSequence
      rv$tmpSequence <- NULL
    } else {
      showNotification("No changes to update", type = "message")
    }
  })
  
  # Blank filtration
  observeEvent(input$blankFiltrate, {
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (!"QC" %in% rv$sequence[[rv$activeFile]][, 1]) {
      showNotification("Data must have at least 1 QC", type = "error")
    } else if (!"Blank" %in% rv$sequence[[rv$activeFile]][, 1]) {
      showNotification("Data must have at least 1 Blank", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      filtered <- blankFiltration(data, sequence, input$signalStrength, input$keepIS)
      if(input$discardBlank) {
        filtered <- filtered[!sequence[, 1] %in% "Blank"]
        sequence <- sequence[!sequence[, 1] %in% "Blank", ]
      }
      rv$tmpData <- filtered
      rv$tmpSequence <- sequence
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste0(nrow(rv$data[[rv$activeFile]]) - nrow(rv$tmpData), " features removed"),
        type = "success"
      )
    }
  })
  
  observeEvent(input$saveBF, {
    if (is.null(rv$tmpData)) {
      showNotification("Blank filtrate first", type = "error")
    } else {
      if (input$newFileBF) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_", input$signalStrength, "xb")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_", input$signalStrength, "xb")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Blank filtrated with singal strength above blank =", input$signalStrength, "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })
  
  # IS normalization
  observeEvent(input$normalizeIS, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else if (is.null(input$isChoose)) {
      showNotification("No internal standards selected", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      
      normalized <- normalizationIS(data, sequence, input$isChoose, input$isMethod, input$normalizeQC)
      rv$tmpData <- normalized
      rv$tmpSequence <- sequence
      sendSweetAlert(session, title = "Success", text = paste0("Internal standards normalized with ", input$isMethod, " method"), type = "success")
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    }
  })
  
  observeEvent(input$optimizeIS, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      optimized <- optimizeIS(data, sequence, input$isChoose, input$isMethod, input$normalizeQC)
      updateCheckboxGroupInput(session, "isChoose", selected = optimized)
    }
  })
  
  observeEvent(input$saveIS, {
    if(is.null(rv$tmpData)) {
      showNotification("IS normalize first", type = "error")
    } else {
      if(input$newFileIS) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_is")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_is")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Internal standards normalized with", input$isMethod, "method\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  })
  
  observeEvent(input$removeIS, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      toRemove <- data[sequence[, 1] %in% "Name"]
      data <- data[!grepl("\\(IS\\)", toupper(toRemove[ , 1])), ]
      rv$data[[rv$activeFile]] <- data
      updateCheckboxGroupInput(session, "isChoose", choices = character(0), selected = NULL)
    }
  })
  
  # Missing value filtration
  observeEvent(input$runFilterNA, {
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      method <- input$filterNAmethod
      if(("in group" %in% method) & !any(complete.cases(sequence[, 4]))) {
        sendSweetAlert(session, "Error!", "Group information needed.", type = "error")
      }
      else if(is.null(method)) {
        sendSweetAlert(session, "Error!", "No method selected.", type = "error")
      }
      else {
        mvf_dat <- cutoffrm(rv$data[[rv$activeFile]], sequence, input$cutoffNAs, method)
        rv$tmpData <- mvf_dat
        rv$tmpSequence <- sequence
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
        sendSweetAlert(
          title = "Success",
          text = paste0(nrow(rv$data[[rv$activeFile]]) - nrow(rv$tmpData), " feature(s) removed"),
          type = "success"
        )
      }
    }
  })
  
  observeEvent(input$saveFilterNA, {
    if (is.null(rv$tmpData)) {
      showNotification("Filtrate first", type = "error")
    } else {
      if (input$mvf_newsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_mvr")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_mvr")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Missing value filtration using", 
                                        input$cutoffNAs, "% as threshold and method -", paste(input$filterNAmethod, collapse=", "), "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })
  
  # Imputation
  observeEvent(input$runImputation, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Sample") < 1) {
      showNotification("Data must have at least one Sample", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      imputed <- imputation(data, sequence,
                            method = input$imputationMethod,
                            minx = input$imputationMinX,
                            onlyqc = input$imp_onlyQC,
                            remaining = input$remainingNAs
      )
      rv$tmpData <- imputed
      rv$tmpSequence <- sequence
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        title = "Success",
        text = paste0(sum(is.na(rv$data[[rv$activeFile]]) | rv$data[[rv$activeFile]] == 0) - sum(is.na(rv$tmpData) | rv$tmpData == 0), " missing values were imputed."),
        type = "success"
      )
    }
  })
  
  observeEvent(list(input$imputationMethod, input$remainingNAs), {
    if (input$imputationMethod == "KNN") {
      hide("imp_minx_hide")
      hide("imp_remaining_hide")
    } else {
      show("imp_remaining_hide")
    }
    
    if (input$imputationMethod == "Min/X" || input$remainingNAs == "Min/X") {
      show("imp_minx_hide")
    } else {
      hide("imp_minx_hide")
    }
  })
  
  observeEvent(input$saveImputation, {
    if (is.null(rv$tmpData)) {
      showNotification("Imputate first", type = "error")
    } else {
      if(input$newFileImp) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_imp")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_imp")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Missing values imputation with", input$imputationMethod, "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })
  
  # Drift correction
  observeEvent(input$driftMethod, {
    if (input$driftMethod == "QC-RFSC (random forrest)") {
      hide("dc_qcspan_hide")
      hide("dc_degree_hide")
      show("dc_ntree_hide")
    } else {
      hide("dc_ntree_hide")
      show("dc_qcspan_hide")
      show("dc_degree_hide")
    }
  })
  
  observeEvent(input$runDrift, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]  
      dat_qc <- data[, sequence[, 1] %in% "QC"]
      
      if(any(colSums(!is.na(dat_qc)) != nrow(dat_qc))) {
        sendSweetAlert(session = session, title = "Error", text = "QCs cannot have missing values.", type = "error")
      }
      else {
        corrected <- driftcorrection(data, sequence,
                                     method = input$driftMethod,
                                     ntree = input$driftTrees,
                                     QCspan = input$driftQCspan
        )
        rv$tmpData <- corrected
        rv$tmpSequence <- sequence
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      }
    }
  })
  
  observeEvent(input$saveDrift, {
    if (is.null(rv$tmpData)) {
      showNotification("Drift correct first", type = "error")
    } else {
      if (input$newFileDrift) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_dc")
        initializeVariables()
        
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_dc")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Drift correction", input$driftMethod, "and", input$driftTrees, "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })
  
  # Merge datasets
  observeEvent(input$mergeRankings, {
    showModal(
      modalDialog(
        title = "Change the priority of annotations", size = "s", easyClose = TRUE,
        footer = list(actionButton("md_edit_rankings", "Save edits"), modalButton("Dismiss")),
        lapply(1:10, function(x) {
          fluidRow(
            column(
              width = 8,
              textInput(paste0("md_rankings_text", x), NULL, value = rankings[x, 1], placeholder = "Empty")
            ),
            column(
              width = 4,
              numericInput(paste0("md_rankings_prio", x), NULL, value = rankings[x, 2], min = 0, max = 10)
            ),
          )
        })
      )
    )
  })
  
  observeEvent(input$md_edit_rankings, {
    sapply(1:10, function(x) {
      rankings[x, 1] <<- toupper(input[[paste0("md_rankings_text", x)]])
      rankings[x, 2] <<- input[[paste0("md_rankings_prio", x)]]
    })
    removeModal()
  })
  
  observeEvent(input$mergeDatasets, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      activeSequence <- rv$sequence[[rv$activeFile]]
      activeDataset <- rv$data[[rv$activeFile]]
      selected <- which(rv$choices %in% input$mergeFile)
      if(is.null(selected)) {
        showNotification("No file selected", type = "error")
      } else {
        sequenceToMerge <- rv$sequence[[selected]]
        datasetToMerge <- rv$data[[selected]]
        
        if(names(rv$data)[rv$activeFile] == names(rv$data)[selected]) {
          showModal(
            modalDialog(
              title = "Do you want to merge a dataset with itself?", size = "m",
              footer = list(actionButton("mergeSameFile", "Yes"), modalButton("Cancel"))
            )
          )
        } else {
          userConfirmation(TRUE)
        }
      }
    }
  })
  
  observeEvent(input$mergeSameFile, {
    userConfirmation(TRUE)
    removeModal()
  })
  
  observeEvent(userConfirmation(), {
    if(userConfirmation()) {
      activeSequence <- rv$sequence[[rv$activeFile]]
      activeDataset <- rv$data[[rv$activeFile]]
      selected <- which(rv$choices %in% input$mergeFile)
      sequenceToMerge <- rv$sequence[[selected]]
      datasetToMerge <- rv$data[[selected]]
      if (sum(activeSequence[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1 || sum(sequenceToMerge[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1) {
        sendSweetAlert(session = session, title = "Error", text = "Each dataset must contain exactly one adduct column labeled in the sequence file.", type = "error")
      } else if (ncol(activeDataset) != ncol(datasetToMerge)) {
        sendSweetAlert(session = session, title = "Error", text = "Datasets must have the same number of columns", type = "error")
      } else {
        mergedDatasets <<- mergeDatasets(activeDataset, activeSequence,
                                         datasetToMerge, sequenceToMerge, input$merge_ppm, input$merge_rt)
        clustn <- data.frame(table(mergedDatasets$mergeID))
        dub_clust <- clustn[clustn$Freq > 1, ]
        dub_dat <- mergedDatasets[mergedDatasets$mergeID %in% dub_clust[, 1], ]
        dub_qc <- dub_dat[, activeSequence[, 1] %in% "QC"]
        cov <- cv(dub_qc)
        nclust <- sapply(dub_dat$mergeID, function(x) {
          table(dub_dat$mergeID)[names(table(dub_dat$mergeID)) == x]
        })
        
        out_dub <- data.frame(
          "nClust" = nclust,
          "Cluster_ID" = dub_dat$mergeID,
          "Ion_mode" = dub_dat$ionmode,
          "Adductor" = dub_dat$add,
          "Name" = dub_dat[, which(activeSequence[, 1] %in% "Name")],
          "RT" = dub_dat[, which(activeSequence[, 1] %in% "RT")],
          "Mass" = dub_dat[, which(activeSequence[, 1] %in% "Mass")],
          "CV" = cov
        )
        out_dub <- out_dub[order(out_dub[, 1], out_dub[, 2], decreasing = T), ]
        md_dup <<- out_dub
        cluster_ends <- which(!duplicated(out_dub[, 2]))
        output$md_modal_dt <- renderDataTable({
          datatable(out_dub,
                    rownames = F,
                    options = list(dom = "t", autowidth = T, paging = F),
                    selection = list(selected = finddup(out_dub, rankings))
          ) %>% formatStyle(1:8, `border-top` = styleRow(cluster_ends, "solid 2px"))
        },
        server = T
        )
        userConfirmation(FALSE)
        showModal(
          modalDialog(
            title = "Select features to keep", size = "l",
            p(paste0(length(unique(dub_dat$mergeID))), " duplicate clusters found, of those ", paste0(length(unique(out_dub[out_dub[, 1] > 2, ][, 2]))), " consists of more than 2 features."),
            DTOutput("md_modal_dt"),
            footer = list(actionButton("confirmMerging", "Remove duplicates"), modalButton("Dismiss"))
          )
        )
      }
    }
  })
  
  observeEvent(input$confirmMerging, {
    duplicates <- as.numeric(rownames(md_dup[-input$md_modal_dt_rows_selected, ]))
    merged <<- mergedDatasets[-duplicates, ]
    output$dttable <- renderDataTable(merged, rownames = F, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    removeModal()
    confirmSweetAlert(session, inputId = "newFileMerge", title = "Merge complete", text = "Save as new file?", btn_labels = c("No", "Yes"), type = "success")
  })
  
  observeEvent(input$newFileMerge, {
    if (isTRUE(input$newFileMerge)) {
      rv$data[[length(rv$data) + 1]] <- merged[, seq(ncol(merged) - 2)]
      rv$sequence[[length(rv$sequence) + 1]] <- rv$sequence[[rv$activeFile]]
      names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_merged")
      initializeVariables()
    } else if (isFALSE(input$newFileMerge)) {
      rv$data[[rv$activeFile]] <- merged[, seq(ncol(merged) - 2)]
      names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_merged")
    }
    rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Positive and negative mode merged: M/z tolerance ppm", input$merge_ppm, "and RT tolerance", input$merge_rt, "\n")
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
  })
  
  #TODO
  observeEvent(input$run_pca1, {
    if (!is.null(rv$activeFile)) {
      if (input$selectpca1 == "Unsaved data") {
        data <- rv$tmpData
        seq <- rv$tmpSequence
      } else {
        selectchoices <- paste(1:length(rv$data), ": ", names(rv$data))
        sd <- which(rv$choices %in% input$selectpca1)
        data <- rv$data[[sd]]
        seq <- rv$sequence[[sd]]
      }
      if ("Sample" %in% seq[, 1]) {
        if(any(seq[, 1] %in% "QC"))
          seq[seq[, 1] %in% "QC", ][, 4] <- "QC"
        
        sdata <- data[seq[, 1] %in% c("Sample", "QC")]
        sclass <- seq[seq[, 1] %in% c("Sample", "QC"), ][, 4]
        pca <- pcaplot(sdata, sclass, input$pca1_islog)
        output$plotpca1 <- renderPlotly(pca)
        
        if (sum(seq[, 1] %in% "QC") > 0) {
          qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
        } else {
          qccv <- "No QC in dataset </br>"
        }
        sclass <- sclass[sclass != "QC"]
        if (sum(!is.na(sclass)) > 0) {
          classcv <- sapply(sort(unique(sclass)), function(x) {
            round(cvmean(sdata[, sclass %in% x]), 2)
          })
          classcv <- sapply(seq_along(classcv), function(x) {
            paste0("CV in class ", x, ": ", classcv[x], "</br>")
          })
        } else {
          classcv <- NULL
        }
        text <- c(qccv, classcv)
        output$pca1Details <- renderUI({
          HTML(text)
        })
      }
    }
  })
  
  observeEvent(input$run_pca2, {
    selectchoices <- paste(1:length(rv$data), ": ", names(rv$data))
    sd <- which(rv$choices %in% input$selectpca2)
    if ("Sample" %in% rv$sequence[[sd]][, 1]) {
      data <- rv$data[[sd]]
      seq <- rv$sequence[[sd]]
      shinyCatch(
        seq[seq[, 1] %in% "QC", ][, 4] <- "QC",
        blocking_level = 'message',
        shiny = FALSE
      )
      
      sdata <- data[seq[, 1] %in% c("Sample", "QC")]
      sclass <- seq[seq[, 1] %in% c("Sample", "QC"), ][, 4]
      pca <- pcaplot(sdata, sclass, input$pca2_islog)
      output$plotpca2 <- renderPlotly(pca)
      
      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
      } else {
        qccv <- "No QC in dataset </br>"
      }
      sclass <- sclass[sclass != "QC"]
      if (sum(!is.na(sclass)) > 0) {
        classcv <- sapply(sort(unique(sclass)), function(x) {
          round(cvmean(sdata[sclass %in% x]), 2)
        })
        classcv <- sapply(seq_along(classcv), function(x) {
          paste0("CV in class ", x, ": ", classcv[x], "</br>")
        })
      } else {
        classcv <- NULL
      }
      text <- c(qccv, classcv)
      output$pca2Details <- renderUI({
        HTML(text)
      })
    }
  })
  
  
  
  # Lipid Heatmap
  # Values is used for HTML message display before and after data process
  values <- reactiveValues(runProcessClicked = FALSE)
  
  # When bottum clicked in interface, all the following will be processed
  observeEvent(input$run_process, {
    values$runProcessClicked <- TRUE
    
    # Accessing sequence and data from active files
    sequence <- rv$sequence[[rv$activeFile]]
    data <- rv$data[[rv$activeFile]]
    
    # Removes anything that are not part of the data of the samples and name. 
    data <- data[, sequence[ , 'labels'] %in% c("Name","Sample")]
    sequence <- sequence[sequence[ , 'labels'] %in% c("Name","Sample"), ]
    
    # Check if the sequence is uploaded before proceeding
    if (is.null(sequence)) {
      return(NULL)  # Stop the observeEvent if no file is uploaded
    }
    
    # Capture the number of rows before filtering
    number_of_rows_before <- nrow(data)
    
    
    # Cleaning the noise of the data, by calling functions
    # Apply the `extract_pattern` function to the first column of the dataset
    # Removes all noise from compound name, so name and length is the only left: eg. going from "CAR 14:1'CAR'[M+H]+" to "CAR 14:1"
    data[, 1] <- sapply(data[, 1], extract_pattern)
    
    # Puts the length and double bonds numbers into a (). Eg "CAR 14:1" to "CAR(14:1)"
    data[, 1] <- sapply(data[, 1], format_strings)
    
    # Function to filter rows based on the specified pattern, meaning removes any data that are not on X(C:D) format.
    data <- filter_data_by_pattern(data)
    
    
    
    
    # This will make it possible to switch between original data and merged data. OG data: using _1 _2 ... _n. Merged will sum the values of the "duplicated" data. 
    if (input$selected_dataset == "original") {
      # Call a function to process the original data
      data <- unique_compound_names(data)
    } else if (input$selected_dataset == "merged") {
      # Call a function to process the merged data
      data <- merge_duplicates(data)
    }
    
    # Some data are in NA, calculations cannot read this, therefore NA values are set low. 
    data[is.na(data)] <- 0.000001
    
    # For data counting, used in display of how many rows are removed. 
    # Capture the number of rows after filtering
    number_of_rows_after <- nrow(data)
    
    # Calculate the number of rows removed
    rows_removed <- number_of_rows_before - number_of_rows_after
    
    # Output the number of rows removed to the console
    print(paste("Rows removed:", rows_removed))
    
    # Alternatively, you can display this information in the UI using a textOutput
    output$rows_removed_text <- renderText({
      paste("Rows removed after data cleaning are:", rows_removed, ". The removal is be due to the names in the first column of the data file not being in the X(C:D) format. Keep in mind, that a merged data will also count as a removed row.")
    })
    

    
    
    
    # The following is used in the tab: 'Data of groups in heatmap'.
    # Call the process_data function and use the result directly within observeEvent
    processed_data <- process_data(sequence, data)
    
    output$groups_table <- renderTable({
      if (is.null(processed_data)) {
        return()
      }
      
      # Extract the 'Sample' labels and corresponding 'class' from 'sequence'
      sample_rows <- sequence[sequence$labels == "Sample", ]
      unique_groups <- unique(sample_rows$class)
      
      # Create the dataframe to be displayed as a table
      df_processed_data <- data.frame(
        Group = unique_groups,
        Samples = sapply(unique_groups, function(group) {
          sample_identifiers <- rownames(sample_rows)[sample_rows$class == group]
          paste(sample_identifiers, collapse = ", ")
        })
      )
      # Return the data frame to be rendered as a table
      df_processed_data
    })
    
    
    
    
    # Table output of the table in the tab: 'Heatmap' used for testing. 
    # Only testing, is not needed in the working app.
    observeEvent(input$run_process, {
      # Process your data here
      processed_results <- process_data(sequence, data)
      grouped_data_frames <- create_grouped_data_frames(sequence, data)
      
      # Add the first column of "data" to each grouped data frame
      compound_names <- data[[1]]  # Extract the first column which contains compound names
      
      # Assuming that each grouped data frame has rows in the same order as "data"
      for (i in seq_along(grouped_data_frames)) {
        grouped_data_frames[[i]] <- cbind(Compound_Name = compound_names, grouped_data_frames[[i]])
      }
      
      # Update the names of the grouped_data_frames if they're not already set
      names(grouped_data_frames) <- paste("Group", seq_along(grouped_data_frames))
      
      # Dynamically generate selectInput for group selection
      output$select_group_ui <- renderUI({
        selectInput("selected_group", "Select Group # THIS IS FOR TESTING:",
                    choices = names(grouped_data_frames))  # Use group names as choices
      })
      
      # Dynamically generate table output for the selected group
      output$selected_group_table <- renderTable({
        req(input$selected_group)  # Ensure a group is selected
        grouped_data_frames <- grouped_data_frames[[input$selected_group]]
        if (is.null(grouped_data_frames)) {
          return(data.frame())  # Return an empty data frame if group data is not available
        }
        grouped_data_frames
      })
    })
    
    
    
    # Heatmap input selection  
    observeEvent(input$run_process, {
      
      # Process data here
      processed_results <- process_data(sequence, data)
      grouped_data_frames <- create_grouped_data_frames(sequence, data)
      grouped_data_frames_with_means <- calculate_means_for_grouped_data(grouped_data_frames)
      #print(grouped_data_frames)
      #print(grouped_data_frames_with_means)
      

      
      # Add the first column of "data" to each grouped data frame (compound names)
      compound_names <- data[[1]]  # Extract the first column which contains compound names
      
      # Assuming that each grouped data frame has rows in the same order as "data"
      for (i in seq_along(grouped_data_frames)) {
        grouped_data_frames[[i]] <- cbind(Compound_Name = compound_names, grouped_data_frames[[i]])
      }
      
      # Extract unique group names from sequence
      unique_group_names <- unique(sequence[sequence$labels == "Sample", "class"])
      
      # Check if lengths of group names and grouped data frames match
      if (length(unique_group_names) == length(grouped_data_frames)) {
        # Apply the actual group names from the sequence file
        names(grouped_data_frames) <- unique_group_names
      } else {
        # If there's a mismatch, fallback to naming with numbers as before
        names(grouped_data_frames) <- paste("Group", seq_along(grouped_data_frames))
      }
      
      
      
      # Render the UI for group selection (same as before)
      output$select_group_ui_heatmap <- renderUI({
        column(
          title = "Select groups for Heatmap",
          width = 12,
          
          tagList(
            selectInput("selected_group_for_numerator", "Select Group for numerator:",
                        choices = names(grouped_data_frames)),
            selectInput("selected_group_for_denominator", "Select Group for denominator:",
                        choices = names(grouped_data_frames)),
          )
        )
      })
      
      
      
      # Create interactive table for selected numerator group with x-axis scrolling
      output$numerator_group_table <- DT::renderDataTable({
        req(input$selected_group_for_numerator)  # Ensure that a selection is made
        # Create a copy of the data for display purposes
        display_data <- grouped_data_frames[[input$selected_group_for_numerator]]
        
        
        # Render the data without the 'mean' column
        DT::datatable(
          display_data,  
          options = list(scrollX = TRUE)  # Enable horizontal scrolling
        )
      })
      
      # Create interactive table for selected denominator group with x-axis scrolling
      output$denominator_group_table <- DT::renderDataTable({
        req(input$selected_group_for_denominator)  # Ensure that a selection is made
        # Create a copy of the data for display purposes
        display_data <- grouped_data_frames[[input$selected_group_for_denominator]]
        

        
        # Render the data without the 'mean' column
        DT::datatable(
          display_data,  
          options = list(scrollX = TRUE)  # Enable horizontal scrolling
        )
      })
      
      
      
      
      
      
      
      
      # Message shown when hovering over Original data and merged data. # Remember to change this outside of the observe event, Search for addTooltip
      observe({
        addTooltip(session, "selected_dataset", 
                   "Choose 'Original Data' to work with the data as it was initially collected. Select 'Merged Data' for a combined and cleaned dataset.", 
                   placement = "bottem", 
                   trigger = "hover")
      })
      
      
      # Render UI for maximum p-value input
      output$p_value_max_ui <- renderUI({
        numericInput("p_value_max", 
                     "Maximum p-value:", 
                     value = 1, 
                     min = 0, 
                     step = 0.01)
      })
      
      # Render UI for logFC input
      output$logFC_input_ui <- renderUI({
        numericInput("logFC_input", 
                     "Enter logFC value:", 
                     value = 5)
      })
      
      # Dynamic p-values depended on interface
      reactiveFilteredData <- reactive({
        # Get the maximum p-value threshold from the input
        p_value_max <- input$p_value_max
        
        # Filter the data based on the maximum p-value
        filtered_data <- numerator_data %>%
          filter(P_Value <= p_value_max)
        
        # Now return the filtered data
        filtered_data
        print(filtered_data)
      })
      
      
      
      # Dynamic logFC depended on interface
      reactive_max_logFC <- reactive({
        input$logFC_input  # This will be the positive value
      })
      
      reactive_min_logFC <- reactive({
        -input$logFC_input  # This will be the negative value
      })
      
      
      
      
      reactiveLogFC <- reactive({
        # The required data input for the data handling. 
        req(input$selected_group_for_numerator, input$selected_group_for_denominator)
        req(reactive_max_logFC(), reactive_min_logFC())
        print(input$selected_group_for_numerator)
        print(input$selected_group_for_denominator)
        
        # Define data input, makes it more readable 
        numerator_data <- grouped_data_frames[[input$selected_group_for_numerator]]
        denominator_data <- grouped_data_frames[[input$selected_group_for_denominator]]
        print(numerator_data)
        
        # Ensure there is data to work with
        if (nrow(numerator_data) == 0 || nrow(denominator_data) == 0) {
          return(NULL)
        }
        
        numerator_data <- numerator_data[, -1]
        denominator_data <- denominator_data[, -1]
        #print(numerator_data)
        
        numerator_data_means <- rowMeans(numerator_data[, drop = FALSE], na.rm = TRUE)
        denominator_data_means <- rowMeans(denominator_data[, drop = FALSE], na.rm = TRUE)
        numerator_data_means <- data.frame(numerator_data_means)
        denominator_data_means <- data.frame(denominator_data_means)
        print(numerator_data_means)
        
        compound_names <- data[[1]]
        
        # Calculate logFC
        logFC_data <- log2((numerator_data_means + 1e-6) / (denominator_data_means + 1e-6))
        # Rename a single column
        colnames(logFC_data)[colnames(logFC_data) == "numerator_data_means"] <- "logFC"
        
        logFC <- data.frame(Compound_Name = compound_names, logFC = logFC_data)
        #print(logFC)
        

       

        # Continue filtering based on lipid selection
        if (!"All" %in% input$selected_lipid) {
          logFC <- logFC[logFC$Class %in% input$selected_lipid, ]
        }
        
        # Filter based on the input logFC range
        filtered_data <- logFC[logFC$logFC >= reactive_min_logFC() & logFC$logFC <= reactive_max_logFC(), ]
        #print(filtered_data)
        
        return(filtered_data) # Used for Heatmap display 
      })

      
      
      
      
      
      reactiveP_value <- reactive({
        req(input$selected_group_for_numerator, input$selected_group_for_denominator)
        
        numerator_data <- grouped_data_frames_with_means[[input$selected_group_for_numerator]]
        denominator_data <- grouped_data_frames_with_means[[input$selected_group_for_denominator]]
     

        
        # Ensure there is data to work with
        if (nrow(numerator_data) == 0 || nrow(denominator_data) == 0) {
          return(NULL)
        }
        
        # Initialize a vector to store the p-values
        p_values <- numeric(nrow(numerator_data))
        
        # Loop through each row to perform the t-test
        for (i in 1:nrow(numerator_data)) {
          num_values <- unlist(numerator_data[i, -1])
          denom_values <- unlist(denominator_data[i, -1])
          
          # Check if data is constant or contains NA values
          if (length(unique(num_values)) == 1 || length(unique(denom_values)) == 1 ||
              any(is.na(num_values)) || any(is.na(denom_values))) {
            p_values[i] <- NA  # Assign NA or another appropriate value
          } else {
            t_test_result <- t.test(num_values, denom_values)
            p_values[i] <- t_test_result$p.value
          }
        }
        
        # Store the data
        numerator_data$p_values <- p_values
        
        # Filter/store the data, so it is ready to display in table in 'Heatmap'.
        filtered_data <- numerator_data %>%
          mutate(p_value = p_values) %>%
          filter(p_value <= input$p_value_max)
        
        return(filtered_data[, c("Compound_Name", "p_values")])
        #print(filtered_data)
      })
      
      
      
      # Combine both logFC and p-values into one reactive expression
      reactiveFilteredData <- reactive({
        # Retrieve the filtered datasets based on logFC and p-values
        logFCData <- reactiveLogFC()
        pValuesData <- reactiveP_value()
        
        # Ensure both datasets are not NULL before proceeding
        req(logFCData, pValuesData)
        
        # Combine the datasets to have both logFC and p-value information
        # Assuming both datasets have a 'Compound_Name' column to join on
        combinedData <- merge(logFCData, pValuesData, by = "Compound_Name")
        
        # Now return the combined dataset
        combinedData
      })
      
      
      # Interface of selections of lipids to display
      output$select_lipid_ui <- renderUI({
        # Extract the lipid names from first column of the file 'data'
        lipid_names <<- group_lipids_by_class(data)
        
        selectizeInput("selected_lipid", "Select lipid(s) to display:",
                       choices = c("All", unique(lipid_names$Class)),
                       multiple = TRUE,
                       options = list(placeholder = 'Choose lipids...',
                                      onInitialize = I('function() { this.setValue(""); }')))
      })
      
      # Reactive expression to track the number of selected lipids or the "All" selection
      selected_lipid_count <- reactive({
        # If "All" is selected, we could set this to a value that causes the default text size to be used
        if ("All" %in% input$selected_lipid) {
          return(Inf)  # 'Inf' is used here as a flag for "All"
        } else {
          return(length(input$selected_lipid))
        }
      })
      
      
      
      
      
      
      
      # Reactive expression for bubble plot data and height
      bubble_plot_data <- reactive({
        # Step 1: Load the filtered data
        filtered_data <- reactiveLogFC()
        
        # Take input from user in interface and change p-value and logFC
        filtered_data <- reactiveFilteredData()
        
        # Ensure the data is not NULL and has rows to plot
        req(nrow(filtered_data) > 0)
        
        # Apply filtering based on logFC
        filtered_data <- filtered_data[filtered_data$logFC >= reactive_min_logFC() & filtered_data$logFC <= reactive_max_logFC(), ]
        
        # Map the lipid names (make sure it returns all necessary columns, including Lipid_Class)
        names.mapping <- map_lipid_names(x = filtered_data$Compound_Name)
        
        # Create a data frame containing the necessary lipid information
        lipid_data <- data.frame(
          Compound.Name = names.mapping$Name,
          Lipid_Class = names.mapping$Name.simple,  
          Carbon_Length = names.mapping$N.carbons,
          Double_Bonds = names.mapping$N.double.bonds,
          Concentration = filtered_data$logFC,
          p_value = filtered_data$p_value,
          Class = names.mapping$Class  # Ensure we are grouping by class
        )
        
        # Filter out rows with any NA values
        lipid_data <- lipid_data[complete.cases(lipid_data), ]
        
        # Define size bins for the p-values with appropriate labels
        lipid_data$size_category <- cut(
          lipid_data$p_value,
          breaks = c(-Inf, 0.00001, 0.0001, 0.001, 0.01, 0.05, 0.1, 0.2, Inf),
          labels = c("p ≤ 1e-5", "1e-5 < p ≤ 1e-4", "1e-4 < p ≤ 0.001", "0.001 < p ≤ 0.01",
                     "0.01 < p ≤ 0.05", "0.05 < p ≤ 0.1", "0.1 < p ≤ 0.2", "p > 0.2"),
          include.lowest = TRUE,
          right = TRUE  # Include the right end of intervals
        )
        
        # Remove any NA values in size_category
        lipid_data <- lipid_data[!is.na(lipid_data$size_category), ]
        
        # Define sizes for each category
        size_values <- c(20, 17, 15, 13, 11, 9, 8, 7)
        names(size_values) <- levels(lipid_data$size_category)
        
        # Calculate the number of unique classes
        num_classes <- length(unique(lipid_data$Class))
        
        # Calculate number of rows in the facets
        ncol_facets <- 3  # Adjust based on your facet_wrap setting
        num_rows <- ceiling(num_classes / ncol_facets)
        
        # Set base height per row
        height_per_row <- 300  # Adjust as needed to match heatmap scaling
        
        # Calculate total plot height
        total_plot_height <- num_rows * height_per_row
        
        # Ensure minimum and maximum height limits
        total_plot_height <- max(total_plot_height, 600)   # Minimum height
        total_plot_height <- min(total_plot_height, 4000)  # Maximum height
        
        
        
        
        bubble_plot <- ggplot(lipid_data, aes(x = Carbon_Length, y = Double_Bonds, size = size_category, 
                                              color = Concentration)) +
          geom_point(alpha = 0.7) +
          scale_color_gradient2(
            low = "#4575b4",         # Same as in heatmap
            mid = "white",           # Same as in heatmap
            high = "#d73027",        # Same as in heatmap
            midpoint = 0,            # Same as in heatmap
            limit = c(-2.5, 2.5),    # Same as in heatmap
            space = "Lab",           # Same as in heatmap
            name = "logFC"
          ) +
          scale_size_manual(
            values = size_values,
            name = "p-value range"
          ) +
          facet_wrap(~ Class, scales = "free", ncol = ncol_facets) +
          
          
          
          
          
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
            strip.text = element_text(color = "black", face = "bold"),
            strip.background = element_rect(fill = "#3483d1", color = "white"),
            panel.background = element_rect(fill = "#D3D3D3", color = "white"),
            panel.grid.major = element_line(color = "grey90"),
            panel.grid.minor = element_blank(),
            legend.position = "top",
            legend.box = "vertical",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            panel.spacing = unit(0.5, "lines")  # Increase spacing between facets
          )
        
        
        # Convert ggplot to plotly object with dynamic height
        p <- ggplotly(bubble_plot, width = 1000, height = total_plot_height)
        
        # Modify the colorbar position
        for (i in seq_along(p$x$data)) {
          if (!is.null(p$x$data[[i]]$marker$colorbar)) {
            p$x$data[[i]]$marker$colorbar$orientation <- "h"
            p$x$data[[i]]$marker$colorbar$x <- 0.5
            p$x$data[[i]]$marker$colorbar$y <- 1.05
            p$x$data[[i]]$marker$colorbar$xanchor <- "center"
            p$x$data[[i]]$marker$colorbar$yanchor <- "bottom"
            p$x$data[[i]]$marker$colorbar$lenmode <- "pixels"
            p$x$data[[i]]$marker$colorbar$len <- 300
            p$x$data[[i]]$marker$colorbar$thickness <- 15
          }
        }
        
        # Adjust the legend position
        p <- p %>%
          layout(
            legend = list(
              orientation = "h",
              x = 0.5,
              y = 1.15,
              xanchor = "center",
              yanchor = "bottom"
            ),
            margin = list(t = 150)
          )
        
        # Return the plot and height
        list(plot = p, height = total_plot_height)
      })
      
      # Render the bubble plot
      output$bubble_plot <- renderPlotly({
        bubble_plot_data()$plot
      })
      
      # UI for dynamic height
      output$bubble_plot_ui <- renderUI({
        plot_data <- bubble_plot_data()
        plot_height <- plot_data$height
        
        plotlyOutput("bubble_plot", width = "100%", height = paste0(plot_height, "px"))
      })
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      output$heatmapPlot <- renderPlot({
        filtered_data <- reactiveLogFC()
        print(filtered_data)
        print(filtered_data$Compound_Name)
        browser()
        # Take the input from user in interface and change p-value and logFC
        filtered_data <- reactiveFilteredData()
        
        
        # Ensure the data is not NULL and has rows to plot
        req(nrow(filtered_data) > 0)
        
        # Apply any necessary filtering based on logFC
        filtered_data <- filtered_data[filtered_data$logFC >= reactive_min_logFC() & filtered_data$logFC <= reactive_max_logFC(), ]
        
        # Get the count of selected lipids
        num_of_lipids <- selected_lipid_count()
        
        # Ensure compound_names are available
        names.mapping <- map_lipid_names(x = filtered_data$Compound_Name)
        
        heatmap_plot <- heatmap_lipidome(
          x = filtered_data[ , c("Compound_Name", "logFC")],
          names.mapping = names.mapping,
          class.facet = "wrap",
          x.names = "Compound_Name",
          fill.limits = c(-2.5, 2.5),
          fill.midpoint = 2.5,
          melt.value.name = "logFC",
          scales = "free"
        ) +
          scale_fill_gradient2(
            low = "#4575b4",
            mid = "white",
            high = "#d73027",
            midpoint = 0,
            limit = c(-2.5, 2.5),
            space = "Lab",
            name = "logFC"
          ) +
          facet_wrap(~ Class, scales = "free", ncol = 3) +
          
          # Use the named vector for lipid N.carbons labels
          
          theme(
            panel.background = element_rect(fill = "#D3D3D3", color = "white"),
            strip.background = element_rect(fill = "#3483d1", color = "white"),
            strip.text = element_text(color = "black", face = "bold"),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)  # Rotate lipid names for readability
          )
        
        # Return the heatmap plot
        heatmap_plot
        
      })
      
      
      
      
      
      
      output$heatmap_ui <- renderUI({
        # Use the reactive expression to get the number of selected lipids or if "All" is selected
        num_selected_lipids <- selected_lipid_count()
        
        # Dynamically adjust the height based on the number of selected lipids using nested ifelse
        height <- ifelse(num_selected_lipids > 0 & num_selected_lipids <= 3, "800px", 
                         ifelse(num_selected_lipids > 3 & num_selected_lipids <= 6, "1200px", "2000px"))
        
        plotOutput("heatmapPlot", width = "100%", height = height)
      })
      
      
      
      output$lipid_class_count <- DT::renderDataTable({
        lipid_names <<- group_lipids_by_class(data)
        lipid_class_count <- table(lipid_names$Class)  # Note the use of names.mapping() to access the reactive value
        lipid_class_df <- as.data.frame(lipid_class_count)
        colnames(lipid_class_df) <- c("Lipid Class", "Count")
        
        # Return the data frame as a datatable
        DT::datatable(lipid_class_df, options = list(pageLength = 5, autoWidth = TRUE))
      })
      
      
      
      
      
      
      
      
      
      
      
      #Display af p_Values and logFC values under Heatmap
      output$pValueTable <- renderDataTable({
        # Access the logFC and p_values data
        logFCData <- reactiveLogFC()
        pValuesData <- reactiveP_value()
        
        
        # Ensure both are not NULL before attempting to merge
        req(logFCData, pValuesData)
        
        # Merge the dataframes based on the common "Compound_Name" column
        combinedData <- merge(logFCData, pValuesData, by = "Compound_Name")
        
        # Select only the columns you want to display
        dataTableToShow <<- combinedData[, c("Compound_Name", "logFC", "p_values")]
        
        
        # Round 'logFC' and 'p_values' to the desired number of decimal places
        dataTableToShow$logFC <- round(dataTableToShow$logFC, 5) # 2 decimal places for logFC
        dataTableToShow$p_values <- round(dataTableToShow$p_values, 5) # 4 decimal places for p-values
        
        # Render the selected data in a DataTable
        datatable(dataTableToShow, options = list(pageLength = 10, scrollX = TRUE))
      })
    })
    
    
    
    
  }) # This finishes the first 'observeEvent' when 'Run data processing' is clicked
  
  # Outside of the observeEvent, based on whether runProcessClicked is TRUE or FALSE, the message display will be placed on this: 
  output$table_message <- renderUI({
    if (!values$runProcessClicked) {
      HTML('<p>Make sure sequences file is uploaded, when uploaded: Press "Run Data Processing" to get a display of data</p>')
    }
  })
  
  # Outside of the observeEvent, so the message both are shown before and after runProcessClicked is clicked. 
  observe({
    addTooltip(session, "selected_dataset", 
               "Choose 'Original Data' to work with the data as it was initially collected. Select 'Merged Data' for a combined and cleaned dataset.", 
               placement = "bottom", 
               trigger = "hover")
  })
  
  
  ##############################
  # User guide inside 'Heatmap'
  observeEvent(input$show_lipid_info, {
    showModal(modalDialog(
      title = "Lipid summeray",
      textOutput("rows_removed_text"),
      textOutput(" "),
      dataTableOutput("lipid_class_count"),
      
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })  
  
  
  
  
  
  observeEvent(input$drift_1, {
    rv$drift_plot_select <- 1
  })
  
  observeEvent(input$drift_2, {
    rv$drift_plot_select <- 2
  })
  
  observeEvent(input$drift_3, {
    rv$drift_plot_select <- 3
  })
  
  output$drift_ui <- renderUI({
    box(
      width = NULL,
      if (is.null(rv$activeFile)) {
        p("No data")
      } else if (input$drift_select != "None" && nrow(rv$data[[rv$activeFile]]) != nrow(rv$data[[which(rv$choices %in% input$drift_select)]])) {
        p("Not able to compare the selected datasets")
      } else if (input$drift_select == "None" && rv$drift_plot_select == 2) {
        p("Need dataset to compare with")
      } else if (is.null(input$dt_drift_panel_rows_selected) && rv$drift_plot_select == 1) {
        p("Select feature to plot")
      } else if (rv$drift_plot_select == 1) {
        lapply(1:length(input$dt_drift_panel_rows_selected), function(i) {
          fluidRow(
            column(6, plotOutput(paste0("driftplotoutput", i), height = 280, width = "100%")),
            column(6, plotOutput(paste0("driftplotoutput2", i), height = 280, width = "100%"))
          )
        })
      } else if (rv$drift_plot_select == 2) {
        fluidRow(column(12, plotOutput("cvscatterplot", height = 600, width = "100%")))
      } else {
        p("Nothing here yet")
      }
    )
  })
  
  output$boxplot_ui <- renderUI({
    box(
      width = NULL,
      if (is.null(rv$activeFile)) {
        p("No data")
      } else if (is.null(input$dt_boxplot_panel_rows_selected)) {
        p("Select feature to plot")
      } else {
        lapply(1:length(input$dt_boxplot_panel_rows_selected), function(i) {
          fluidRow(column(12, plotOutput(paste0("boxplotoutput", i), height = 280, width = "100%")))
        })
      }
    )
  })
  
  observe({
    if (length(input$dt_drift_panel_rows_selected) == 0 && rv$drift_plot_select == 1) {
      output$driftplotoutput1 <- renderPlot({
        NULL
      })
      output$driftplotoutput21 <- renderPlot({
        NULL
      })
    } else if (rv$drift_plot_select == 1) {
      for (i in 1:(length(input$dt_drift_panel_rows_selected) + 1)) {
        output[[paste0("driftplotoutput", i)]] <- renderPlot({
          NULL
        })
        output[[paste0("driftplotoutput2", i)]] <- renderPlot({
          NULL
        })
      }
      for (i in 1:length(input$dt_drift_panel_rows_selected)) {
        local({
          my_i <- i
          output[[paste0("driftplotoutput", my_i)]] <- renderPlot({
            driftplot(
              data = rv$data[[rv$activeFile]][input$dt_drift_panel_rows_selected[my_i], ],
              seq = rv$sequence[[rv$activeFile]]
            )
          })
        })
        if (input$drift_select != "None") {
          local({
            my_i <- i
            output[[paste0("driftplotoutput2", my_i)]] <- renderPlot({
              driftplot(
                data = rv$data[[which(rv$choices %in% input$drift_select)]][input$dt_drift_panel_rows_selected[my_i], ],
                seq = rv$sequence[[rv$activeFile]]
              )
            })
          })
        }
      }
    } else if (rv$drift_plot_select == 2) {
      output$cvscatterplot <- renderPlot({
        cvscatterplot(
          data = rv$data[[rv$activeFile]],
          data2 = rv$data[[which(rv$choices %in% input$drift_select)]],
          seq = rv$sequence[[rv$activeFile]],
          name1 = names(rv$data)[rv$activeFile],
          name2 = names(rv$data)[which(rv$choices %in% input$drift_select)]
        )
      })
    }
  })
  
  observe({
    if (length(input$dt_boxplot_panel_rows_selected > 0)) {
      for (i in 1:length(input$dt_boxplot_panel_rows_selected)) {
        local({
          my_i <- i
          output[[paste0("boxplotoutput", my_i)]] <- renderPlot({
            myboxplot(
              data = rv$data[[rv$activeFile]][input$dt_boxplot_panel_rows_selected[my_i], ],
              seq = rv$sequence[[rv$activeFile]],
              log = input$bloxplot_log,
              ylog = input$bloxplot_ylog
            )
          })
        })
      }
    }
  })
  
  observe({ 
    if (!is.null(rv$activeFile)) {
      seq <- rv$sequence[[rv$activeFile]]
      dat <- rv$data[[rv$activeFile]]
      blank_mv <- sum(is.na(dat[seq[, 1] %in% "Blank"])) +
        sum(dat[seq[, 1] %in% "Blank"] == 0, na.rm = TRUE)
      qc_mv <- sum(is.na(dat[seq[, 1] %in% "QC"])) +
        sum(dat[seq[, 1] %in% "QC"] == 0, na.rm = TRUE)
      sample_mv <- sum(is.na(dat[seq[, 1] %in% "Sample"])) +
        sum(dat[seq[, 1] %in% "Sample"] == 0, na.rm = TRUE)
      
      sdata <- dat[seq[, 1] %in% "Sample"]
      sclass <- seq[seq[, 1] %in% "Sample", ][, 4]
      
      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ",
                       round(cvmean(dat[seq[, 1] %in% "QC"]), 2), "</br>")
      } else {
        qccv <- "No QC in dataset </br>"
      }
      
      if (sum(!is.na(sclass)) > 0) {
        classcv <- sapply(sort(unique(sclass)), function(x) {
          round(cvmean(sdata[sclass %in% x]), 2)
        })
        classcv <- sapply(seq_along(classcv), function(x) {
          paste0("CV in class ", x, ": ", classcv[x], "</br>")
        })
      } else {
        classcv <- NULL
      }
      text <- c(qccv, classcv)
      output$title <- renderText({
        HTML("<h3>", names(rv$data)[rv$activeFile], "</h3>")
      })
      output$info_ui <- renderUI({
        HTML(nrow(dat) - 1, " features.<br>", ncol(dat[seq[, 1] %in% "Sample"]), " samples.<br>", ncol(dat[seq[, 1] %in% "QC"]), " QC samples.<br>", ncol(dat[seq[, 1] %in% "Blank"]), " Blank samples.<br>", "<br>", sample_mv, " missing values in Samples<br>", qc_mv, " missing values in QC samples<br>", blank_mv, " missing values in Blank samples<br><br>")
      })
      output$cvinfo_ui <- renderUI({
        HTML(text)
      })
    }
  })
  
  observeEvent(input$normalize, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if(input$normMethod == "QC (PQN)" & sum(rv$sequence[[rv$activeFile]][, 1] %in% "QC") == 0) {
      sendSweetAlert(session = session, title = "Error", text = "No QC samples in dataset.", type = "error")
    } else if(input$normMethod == "Sample amount" & sum(complete.cases(rv$sequence[[rv$activeFile]][, 'amount'])) == 0) {
      sendSweetAlert(session = session, title = "Error", text = "No sample amount information in dataset.", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      qualityControls <- data[, sequence[, 1] %in% "QC"] 
      normalizedData <- normalization(data, sequence, qualityControls, input$normMethod)
      data[, sequence[, 1] %in% c("QC", "Sample")] <- normalizedData
      normalizedQCs <- data[, sequence[, 1] %in% "QC"]
      rv$tmpData <- data
      rv$tmpSequence <- sequence
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", 
                        choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = 
                                          list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(title = "Success", text = paste0("Data normalized using ", input$normMethod), type = "success")
      # Plot variance in QC samples before and after normalization
      # output$beforeNormalization <- renderPlot({
      #   boxplot(log2(qualityControls), main = "Before Normalization", xlab = "Metabolite", ylab = "Intensity")
      # })
      # output$afterNormalization <- renderPlot({
      #   boxplot(log2(normalizedQCs), main = "After Normalization", xlab = "Metabolite", ylab = "Intensity")
      # })
      
      # showModal(
      #   modalDialog(
      #     title = "Assess data quality", size = "m",
      #     fluidRow(
      #         column(6, plotOutput("beforeNormalization", height = 280, width = "100%")),
      #         column(6, plotOutput("afterNormalization", height = 280, width = "100%"))
      #     ),
      #     footer = list(actionButton("saveNormalization", "Save changes"), modalButton("Dismiss"))
      #   )
      # )
    }
  })
  
  observeEvent(input$saveNormalization, {
    if(is.null(rv$tmpData)) {
      showNotification("Normalize first", type = "error")
    } else {
      if (input$newFileNorm) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_normalized")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_normalized")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Normalized with", input$normMethod, " method\n")
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })
  
  observeEvent(input$transform, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if(input$logTransform == "None" & input$scaling == "None") {
      sendSweetAlert(session = session, title = "Warning", text = "No method selected.", type = "warning")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      transformed <- transformation(data, sequence, input$logTransform, input$scaling)
      data[, sequence[, 1] %in% c("QC", "Sample")] <- transformed
      rv$tmpData <- data
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(session, title = "Success", text = "Data transformed.", type = "success")
    }
  })
  
  observeEvent(input$saveTransform, {
    if(is.null(rv$tmpData)) {
      showNotification("Transform first", type = "error")
    } else {
      rv$data[[length(rv$data) + 1]] <- rv$tmpData
      rv$sequence[[length(rv$sequence) + 1]] <- rv$sequence[[rv$activeFile]]
      names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_transformed")
      initializeVariables()
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  })
  
  # Statistics
  observeEvent(input$testType, { #TODO move this to functions file? 
    sequence <- rv$sequence[[rv$activeFile]]
    enable("selectTest")
    switch(input$testType,
           GroupsUnpaired = {
             if(!any(complete.cases(sequence[, 4]))) {
               sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
               disable("selectTest")
             }
           },
           GroupsMultipleTime = {
             if(any(complete.cases(sequence[, 5])) & any(complete.cases(sequence[, 6]))) {
               sequence <- sequence[sequence[, 1] %in% "Sample" & complete.cases(sequence[, 4]), ]
               group_time <- getGroupTime(sequence)
               unique_values <- unique(group_time)
               combinations <- combn(unique_values, 2)
               valid_combinations <- combinations[, apply(combinations, 2, function(cols) is_valid_combination(cols[1], cols[2]))]
               contrasts <- generate_contrasts(matrix(valid_combinations)) # matrix bc if it's only 1 combination, valid_combinations is not a matrix and generate_contrasts fails
               
               updateCheckboxGroupInput(session, "contrasts", choices = contrasts, selected = NULL)
             } else {
               sendSweetAlert(session, "Oops!", "Invalid test. No paired samples or time points in dataset.", type = "error")
               disable("selectTest")
             }
           },
           CompareToReference = {
             if(!any(complete.cases(sequence[, 4]))) { # or if only one group
               sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
               disable("selectTest")
             } else {
               updateSelectInput(session, "referenceGroup", label = NULL, choices = na.omit(sequence[, 'class']))
             }
           },
           {
             print('default')
           }
    )
  }, ignoreInit = TRUE
  )
  
  observeEvent(input$selectTest, { #TODO move this to functions file?
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    switch(input$testType, 
           GroupsUnpaired = {
             if(input$group1 == input$group2) {
               sendSweetAlert(session, "Oops!", "Choose different groups to compare.", type="error")
             } else {
               results <- groupComparison(data, sequence, c(input$group1, input$group2))
               st$results[[rv$activeFile]][[length(st$results[[rv$activeFile]])+1]] <- results
               names(st$results[[rv$activeFile]])[length(st$results[[rv$activeFile]])] <- paste0(input$group1, "_vs_", input$group2)
             }
           },
           GroupsMultipleTime = { # multi-level in limma 
             data <- data[sequence[, 1] %in% c("Name", "Sample")]
             sequence <- sequence[sequence[, 1] %in% c("Sample"), ]
             
             group_time <- getGroupTime(sequence)
             group_time <- factor(group_time, exclude = NA)
             paired <- factor(sequence[, 'paired'],  exclude = NA)
             results <- pairedAnalysis(data, group_time, input$contrasts, paired)
             
             st$results[[rv$activeFile]] <- results
           },
           CompareToReference = {
             data <- data[sequence[, 1] %in% c("Name", "Sample")]
             groups <- sequence[complete.cases(sequence[, 4]), 4]
             results <- referenceGroupComparison(data, as.numeric(input$referenceGroup), groups)
             st$results[[rv$activeFile]][[length(st$results[[rv$activeFile]])+1]] <- results
           },
           {
             print('default')
           }
    )
    # Render one table for each contrast
    output$results_ui <- renderUI({
      lapply(seq_along(st$results[[rv$activeFile]]), function(i) {
        fluidRow(
          column(12, strong(names(st$results[[rv$activeFile]])[i])),
          column(12, box(width = NULL, DTOutput(paste0("results", i))))
        )
      })
    })
    lapply(seq_along(st$results[[rv$activeFile]]), function(i) {
      output[[paste0("results", i)]] <- renderDT({
        st$results[[rv$activeFile]][[i]]
      })
    })
    #enable("runTest")
  })
  
  observeEvent(input$send_polystest, {
    # Select Name and Samples (no QCs)
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    tseq <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
    groups <- factor(tseq[, 4], exclude = NA)
    NumReps <- max(table(groups))
    NumCond <- length(levels(groups))
    groups <- levels(groups)
    
    tdata <- addEmptyCols(tdata, tseq, groups, NumReps)
    PolySTestMessage <- toJSON(list(
      numrep=NumReps, numcond=NumCond, grouped=F,
      firstquantcol=2, expr_matrix=as.list(as.data.frame(tdata))
    ))
    js$send_message(url="http://computproteomics.bmb.sdu.dk:443/app_direct/PolySTest/", 
                    dat=PolySTestMessage, tool="PolySTest")
  })
  
  observeEvent(input$send_vsclust, {
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    tseq <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
    groups <- factor(tseq[, 4], exclude = NA)
    NumReps <- max(table(groups))
    NumCond <- length(levels(groups))
    groups <- levels(groups)
    
    tdata <- addEmptyCols(tdata, tseq, groups, NumReps)
    
    VSClustMessage <- toJSON(list(
      numrep=NumReps, numcond=NumCond, grouped=F, 
      modsandprots=F, expr_matrix=as.list(as.data.frame(tdata))
    ))
    js$send_message(url="http://computproteomics.bmb.sdu.dk/app_direct/VSClust/",
                    dat=VSClustMessage, tool="VSClust")
  })
  
  
  #
  
  data_processed <- eventReactive(input$process, {
    req(input$dataFile)  # Ensure a file is uploaded
    
    # Read the data from the uploaded file
    Data_in <- read.csv(input$dataFile$datapath, stringsAsFactors = FALSE)
    # Set the number of eqQCs, MSMSs, and QCs you need
    num_eqQCs <- input$num_eqQCs # Adjust the number of eqQC rows as needed
    num_MSMSs <- input$num_MSMSs # Adjust the number of MSMS rows as needed
    num_QCs <- input$num_QCs # Adjust the number of QC rows as needed
    insert_after_samples <- input$insert_after_samples # Change this number as needed
    
    
    
    
    # Split the data into subsets: blanks, QCs, and the rest
    blanks <- Data_in[tolower(Data_in$Sample.type) == "blank", ]
    qcs <- Data_in[tolower(Data_in$Sample.type) == "qc", ]
    samples <- Data_in[tolower(Data_in$Sample.type) == "sample", ]
    non_samples <- Data_in[!(Data_in$Sample.type %in% c("Blank", "QC", "Sample")), ]
    existing_qcs <- Data_in[tolower(Data_in$Sample.type) == "qc", ]
    
    # Name to the   # Generate new eqQC names... 
    sample_type_eqQC_names <- paste0(existing_qcs$name, "_eq_QC")
    sample_type_MSMS_names <- paste0(existing_qcs$name, "_MSMS")
    new_QC_names <- paste0(existing_qcs$name, "_QC")
    new_QC_samples_names <- paste0(existing_qcs$name, "_QC")
    
    new_eqQCs <- qcs[rep(1, num_eqQCs), ]
    
    
    # Check if there are existing QCs to duplicate
    if(nrow(qcs) > 0) {
      # Create new eqQC rows by replicating the existing ones
      new_eqQCs <- qcs[rep(1, num_eqQCs), ]
      
      # Generate new eqQC names
      new_eqQC_names <- paste(sample_type_eqQC_names, sprintf("%02d", seq_len(num_eqQCs)), sep="")
      new_eqQCs$name <- new_eqQC_names
      
      # Create new MSMS rows by replicating the existing QCs
      new_MSMSs <- qcs[rep(1, num_MSMSs), ]
      
      # Generate new MSMS names
      new_MSMS_names <- paste(sample_type_MSMS_names, sprintf("%02d", seq_len(num_MSMSs)), sep="")
      new_MSMSs$name <- new_MSMS_names
      
      # Create new QC rows
      new_QCs <- qcs[rep(1, num_QCs), ]
      
      # Generate new QC names
      new_QC_names <- paste(new_QC_names, sprintf("%02d", seq_len(num_QCs)), sep="")
      new_QCs$name <- new_QC_names
      
      # Combine the new eqQCs, MSMSs, and QCs with the blanks
      qcs_combined <- rbind(new_eqQCs, new_MSMSs, new_QCs)
      
    } else {
      stop("No QC samples available to duplicate.")
    }
    
    
    # Randomize the sample rows (true randomization)
    samples_randomized <- samples[sample(nrow(samples)), ]
    
    # Initialize an empty data frame for the samples with interspersed QCs
    samples_with_qcs <- data.frame(Position = character(0),
                                   name = character(0),
                                   Sample.type = character(0),
                                   stringsAsFactors = FALSE)
    
    # Start numbering new QC entries from num_QCs + 1
    qc_counter <- num_QCs + 1
    
    # Add the samples and intersperse new QCs
    for (i in 1:nrow(samples_randomized)) {
      # Add a sample row
      samples_with_qcs <- rbind(samples_with_qcs, samples_randomized[i, ])
      
      # After every insert_after_samples, insert a new QC row
      if (i %% insert_after_samples == 0) {
        new_qc_row <- data.frame(Position = qcs$Position,
                                 name = paste(new_QC_samples_names, sprintf("%02d", qc_counter)),
                                 Sample.type = "QC",
                                 stringsAsFactors = FALSE)
        samples_with_qcs <- rbind(samples_with_qcs, new_qc_row)
        qc_counter <- qc_counter + 1
      }
    }
    
    Data_out <- rbind(blanks, qcs_combined, samples_with_qcs, new_qc_row)
    
  })
  
  output$table <- renderDT({
    req(data_processed())  # Ensure data has been processed
    data_processed()
  }, options = list(pageLength = 50))
  
  
  
  
  # Handle file download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-output-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(data_processed())  # Ensure data has been processed
      write.csv(data_processed(), file, row.names = FALSE)
    }
  )
  
  
  
  
})
