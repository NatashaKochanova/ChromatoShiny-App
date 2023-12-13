library(colourpicker)
library(ggplot2)
library(shiny)
library(Cairo)
library(shinyWidgets)
library(shinydashboard)
library(baseline)
library(ggrepel)
library(png)
library(readxl)
library(writexl)


transformFile <- function(data, background, fraction260, fraction280, ufraction, plot260, plot280, baseline260, baseline280, fraction1, fraction2, fraction3, fraction4, ml280, ml260, ml_min, ml_max, colour1, colour2, colour3, colour4, colour6, x_min, x_max, y_min, y_max) {
  if (!is.null(data)) {
    options(stringsAsFactors = TRUE)
    names(data) <- as.matrix(data[1, ])
    data <- data[-c(1, 2), ]

    names(data)[is.na(names(data))] <- 0


    # Rename columns of the file

    for (i in 1:length(names(data))) {
      if (names(data[i]) == "UV 1_280") {
        names(data)[i] <- "UV280_ml"
        names(data)[i + 1] <- "UV_280nm_mAU"
      }
    }


    for (i in 1:length(names(data))) {
      if (names(data[i]) == "UV 2_260") {
        names(data)[i] <- "UV260_ml"
        names(data)[i + 1] <- "UV_260nm_mAU"
      }
    }

    for (i in 1:length(names(data))) {
      if (names(data[i]) == "UV") {
        names(data)[i] <- "UV280_ml"
        names(data)[i + 1] <- "UV_280nm_mAU"
      }
    }

    for (i in 1:length(names(data))) {
      if (names(data[i]) == "Fraction") {
        names(data)[i] <- "Fraction_ml"
        names(data)[i + 1] <- "Fraction_number"
      }
    }

    for (i in 1:length(names(data))) {
      if (names(data[i]) == "Cond") {
        names(data)[i] <- "Cond_ml"
        names(data)[i + 1] <- "Cond_mS/cm"
      }
    }

    for (i in 1:length(names(data))) {
      if (names(data[i]) == "Conc B") {
        names(data)[i] <- "Conc_B_ml"
        names(data)[i + 1] <- "Conc_B_%"
      }
    }

    for (i in 1:length(names(data))) {
      if (names(data[i]) == "Run Log") {
        names(data)[i] <- "Run_Log_ml"
        names(data)[i + 1] <- "Run_Log_Logbook"
      }
    }

    for (i in 1:length(names(data))) {
      if (names(data[i]) == "UV_CUT_TEMP@100,BASEM") {
        names(data)[i] <- "UV_CUT_TEMP@100,BASEM_ml"
        names(data)[i + 1] <- "UV_CUT_TEMP@100,BASEM_mAU"
      }
    }

    data2 <- data
    data_fract <- data2
    data_fract$Fraction_ml2 <- as.numeric(data_fract$Fraction_ml)
    data2 <- as.data.frame(sapply(data2, function(x) as.numeric(x)))



    # Subtraction UV mAU
    data2$UV_280nm_mAU <- data2$UV_280nm_mAU - min(data2$UV_280nm_mAU)
    data2$UV_260nm_mAU <- data2$UV_260nm_mAU - min(data2$UV_260nm_mAU, na.rm = T)
    fract_line_length <- -max(data2$UV_280nm_mAU, data2$UV_260nm_mAU, na.rm = T) / 15



    # Baseline for 280
    spectra <- t(as.matrix(data2$UV_280nm_mAU))
    baseline <- baseline.peakDetection(spectra, left = 300, right = 300, lwin = 1000, rwin = 1000)
    baseline_t <- as.vector(t(as.matrix(baseline$baseline)))
    data2 <- cbind(data2, baseline_t)

    # Baseline for 260
    spectra2 <- t(as.matrix(data2$UV_260nm_mAU))
    spectra2 <- t(na.omit(t(spectra2)))
    baseline2 <- baseline.peakDetection(spectra2, left = 300, right = 300, lwin = 1000, rwin = 1000)
    baseline_t2 <- as.vector(t(as.matrix(baseline2$baseline)))
    data260 <- cbind(data2$UV260_ml[c(1:length(baseline_t2))], baseline_t2)
    data260 <- as.data.frame(data260, stringsAsFactors = FALSE)

    # Making numeric minumim and maximum fractions, ml, x and y for zoom:

    fract_min <- levels(droplevels(as.factor(data[data$Fraction_number == fraction1, "Fraction_ml"])))
    fract_max <- levels(droplevels(as.factor(data[data$Fraction_number == fraction2, "Fraction_ml"])))

    fract_min2 <- levels(droplevels(as.factor(data[data$Fraction_number == fraction3, "Fraction_ml"])))
    fract_max2 <- levels(droplevels(as.factor(data[data$Fraction_number == fraction4, "Fraction_ml"])))

    fract_min <- as.numeric(fract_min)
    fract_min2 <- as.numeric(fract_min2)
    fract_max <- as.numeric(fract_max)
    fract_max2 <- as.numeric(fract_max2)
    ml_min <- as.numeric(ml_min)
    ml_max <- as.numeric(ml_max)
    x_min <- as.numeric(x_min)
    x_max <- as.numeric(x_max)
    y_min <- as.numeric(y_min)
    y_max <- as.numeric(y_max)

    # White or grey

    if (background == "Grey") {
      plot <- ggplot() +
        labs(x = "ml", y = "UV mAU enrichment") +
        theme(axis.line = element_line(colour = "black")) +
        theme(text = element_text(size = 25, colour = "black"), axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black"), axis.text.y = element_text(angle = 0, hjust = 0.5, colour = "black"))
    } else {
      if (background == "White") {
        plot <- ggplot() +
          labs(x = "ml", y = "UV mAU enrichment") +
          theme_bw() +
          theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
          theme(text = element_text(size = 25, colour = "black"), axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black"), axis.text.y = element_text(angle = 0, hjust = 0.5, colour = "black"))
      }
    }



    # Fraction
    if (fraction280 == T) {
      plot <- plot + geom_ribbon(data = subset(data2, UV280_ml >= fract_min & UV280_ml <= fract_max), aes(x = UV280_ml, ymin = 0, ymax = UV_280nm_mAU), fill = colour6)
    }
    if (fraction260 == T) {
      plot <- plot + geom_ribbon(data = subset(data2, UV260_ml >= fract_min & UV260_ml <= fract_max), aes(x = UV260_ml, ymin = 0, ymax = UV_260nm_mAU), fill = colour6)
    }


    # Milliliters

    if (ml280 == T) {
      plot <- plot + geom_ribbon(data = subset(data2, UV280_ml >= ml_min & UV280_ml <= ml_max), aes(x = UV280_ml, ymin = 0, ymax = UV_280nm_mAU), fill = colour6)
    }
    if (ml260 == T) {
      plot <- plot + geom_ribbon(data = subset(data2, UV260_ml >= ml_min & UV260_ml <= ml_max), aes(x = UV260_ml, ymin = 0, ymax = UV_260nm_mAU), fill = colour6)
    }

    # 260 or 280

    if (plot260 == T & plot280 == F) {
      plot <- plot +
        geom_ribbon(data = data2, aes(x = UV260_ml, ymin = 0, ymax = UV_260nm_mAU), fill = colour1) +
        geom_line(data = data2, aes(x = UV260_ml, y = UV_260nm_mAU), color = colour3)
    } else {
      if (plot280 == T & plot260 == F) {
        plot <- plot +
          geom_ribbon(data = data2, aes(x = UV280_ml, ymin = 0, ymax = UV_280nm_mAU), fill = colour2) +
          geom_line(data = data2, aes(x = UV280_ml, y = UV_280nm_mAU), color = colour4)
      } else {
        if (plot280 == T & plot260 == T) {
          plot <- plot +
            geom_ribbon(data = data2, aes(x = UV280_ml, ymin = 0, ymax = UV_280nm_mAU), fill = colour2) +
            geom_line(data = data2, aes(x = UV280_ml, y = UV_280nm_mAU), color = colour4) +
            geom_ribbon(data = data2, aes(x = UV260_ml, ymin = 0, ymax = UV_260nm_mAU), fill = colour1) +
            geom_line(data = data2, aes(x = UV260_ml, y = UV_260nm_mAU), color = colour3)
        }
      }
    }

    # 260 or 280 baseline

    if (plot260 == T & plot280 == F & baseline260 == T) {
      plot <- plot + geom_line(data = data260, aes(x = V1, y = baseline_t2), color = colour3)
    } else {
      if (plot280 == T & plot260 == F & baseline280 == T) {
        plot <- plot + geom_line(data = data2, aes(x = UV280_ml, y = baseline_t), color = colour4)
      } else {
        if (plot280 == T & plot260 == T & baseline260 == T & baseline280 == F) {
          plot <- plot + geom_line(data = data260, aes(x = V1, y = baseline_t2), color = colour3)
        } else {
          if (plot280 == T & plot260 == T & baseline260 == F & baseline280 == T) {
            plot <- plot + geom_line(data = data2, aes(x = UV280_ml, y = baseline_t), color = colour4)
          } else {
            if (plot280 == T & plot260 == T & baseline260 == T & baseline280 == T) {
              plot <- plot + geom_line(data = data260, aes(x = V1, y = baseline_t2), color = colour3) +
                geom_line(data = data2, aes(x = UV280_ml, y = baseline_t), color = colour4)
            }
          }
        }
      }
    }

    # Underfraction without zoom
    data_fract2 <- subset(data_fract, Fraction_ml2 >= fract_min2 & Fraction_ml2 <= fract_max2)



    if (ufraction == T & is.na(x_min) == T & is.na(x_max) == T & is.na(y_min) == T & is.na(y_max) == T) {
      plot <- plot +
        geom_rug(data = data_fract2, aes(x = Fraction_ml2, ymax = fract_line_length / 2, ymin = fract_line_length)) +
        geom_text_repel(data = data_fract2[c(1, max(nrow(data_fract2))), ], mapping = aes(x = seq(min(data_fract2$Fraction_ml2), max(data_fract2$Fraction_ml2), length.out = 2), y = fract_line_length / 2 + 2, label = Fraction_number, angle = 90), hjust = 0, size = 5)
    }

    # Underfraction with zoom
    if (ufraction == T & is.na(x_min) == F & is.na(x_max) == F & is.na(y_min) == F & is.na(y_max) == F) {
      data_fract2 <- subset(data_fract, Fraction_ml2 >= fract_min2 & Fraction_ml2 <= fract_max2)
      data_fract3 <- subset(data_fract2, Fraction_ml2 >= x_min & Fraction_ml2 <= x_max)
      plot <- plot +
        geom_rug(data = data_fract2, aes(x = Fraction_ml2, ymax = fract_line_length / 2, ymin = fract_line_length)) +
        geom_text_repel(data = data_fract3[c(1, max(nrow(data_fract3))), ], mapping = aes(x = seq(min(data_fract3$Fraction_ml2), max(data_fract3$Fraction_ml2), length.out = 2), y = y_min + 2, label = Fraction_number, angle = 90), hjust = 0, size = 5) +
        coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max))
    }

    # Zoom in

    if (is.na(x_min) == F & is.na(x_max) == F & is.na(y_min) == F & is.na(y_max) == F) {
      plot <- plot + coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max))
    }



    return(plot)
  }
}

# A function to create a dropdown menu

createdropdown <- function(data) {
  if (!is.null(data)) {
    options(stringsAsFactors = TRUE)
    names(data) <- as.matrix(data[1, ])
    data <- data[-c(1, 2), ]

    names(data)[is.na(names(data))] <- 0


    # Rename fraction columns

    for (i in 1:length(names(data))) {
      if (names(data[i]) == "Fraction") {
        names(data)[i] <- "Fraction_ml"
        names(data)[i + 1] <- "Fraction_number"
      }
    }

    dropdown <- data$Fraction_number

    return(dropdown[dropdown != ""])
  }
}

ui <- fluidPage(
  titlePanel("ChromatoShiny: an app for plotting chromatography profiles"),
  fluidRow(
    column(
      4,
      tabsetPanel(
        tabPanel(
          "App description", br(),
          tags$div(HTML(paste("This is an app for plotting TXT files, exported from the UNICORN", tags$sup("TM"), " software, used to output intensity values from column chromatography.", sep = ""))), br(),
          "The info about how to plot can be found in respective tabs.", br(), br(),
          tags$a(
            href = "https://pubmed.ncbi.nlm.nih.gov/37692131/",
            HTML(paste("The app was published in", tags$i("Wellcome Open Research,")), "08 Aug 2023, 8:332.")
          ), br(), br(),
          "Happy plotting!"
        ),
        tabPanel(
          "General",
          fileInput("file1", "Load .txt file (exported from the UNICORN TM software)", accept = c(".txt")),
          #    fileInput('file1.csv', 'Load .csv file (exported from "Unicorn" software)',accept = c(".csv")),

          selectInput("background", "Choose background", c("Grey", "White"), selected = "Grey"),
          checkboxInput("plot280", "Plot 280 nm", TRUE),
          checkboxInput("plot260", "Plot 260 nm", TRUE),
          checkboxInput("baseline280", "Fit a baseline 280 nm", FALSE),
          checkboxInput("baseline260", "Fit a baseline 260 nm", FALSE),
          downloadButton("downloadPlot", "Download .pdf"),
          downloadButton("downloadPlot2", "Download .eps"),
          downloadButton("downloadPlot3", "Download .tiff"),
          tags$hr(style = "border-color: black;"),
          column(
            width = 12,
            infoBox("Info for",
              width = NULL, value = "tab “General”: ",
              subtitle = tags$p(
                "Begin by loading a TXT file exported from Äkta's UNICORN", tags$sup("TM"), " software to visualise a chromatogram. Choose the plot background color, whether to display 260 and/or 280 nm curves, and fit a baseline if needed. Download your plots as PDF, TIFF, or EPS files using the provided 'Download' options. For curve and area color customisation, visit the 'Colors' tab.",
                tags$br(), tags$br(), "This app is specifically designed for chromatogram files from Äkta's UNICORN", tags$sup("TM"), " software. To visualise chromatograms from other LC manufacturers, reformat them to align with UNICORN", tags$sup("TM"), "'s output. This involves saving them as TXT files in UTF-16 format and adjusting the file header and column names. An example header suitable for input files can be downloaded below ('Table_header.png'). Additionally, a template file ('Template.xlsx') is available for manual data entry of elution times, intensities, and fraction numbers. Ensure fractions in this file have unique names."
              ),
              icon = icon("info-circle")
            ),
            downloadButton("downloadTableHeader", "Download Table_header.png"),
            downloadButton("downloadTemplate", "Download Template.xlsx")
          )
        ),
        tabPanel(
          "Plot fractions and ml", br(),
          pickerInput("fraction1", "Minimum fraction", choices = c()),
          pickerInput("fraction2", "Maximum fraction", choices = c()),
          checkboxInput("fraction280", "Highlight fractions on the 280 nm peak", FALSE),
          checkboxInput("fraction260", "Highlight fractions on the 260 nm peak", FALSE),
          tags$hr(style = "border-color: black;"),
          pickerInput("fraction3", "Minimum fraction", choices = c()),
          pickerInput("fraction4", "Maximum fraction", choices = c()),
          checkboxInput("ufraction", "Mark fractions under the peak", FALSE),
          tags$hr(style = "border-color: black;"),
          textInput("ml_min", "Minimum ml", value = "", width = NULL, placeholder = NULL),
          textInput("ml_max", "Maximum ml", value = "", width = NULL, placeholder = NULL),
          checkboxInput("ml280", "Highlight ml on the 280 nm peak", FALSE),
          checkboxInput("ml260", "Highlight ml on the 260 nm peak", FALSE),
          tags$hr(style = "border-color: black;"),
          column(
            width = 12,
            infoBox("Info for",
              width = NULL, value = "tab “Plot fractions and ml”: ",
              subtitle = tags$p("You can fill a restricted area under the 260/280 nm curves, corresponding to particular fractions or elution volume. It is also possible to plot selected fractions under the curve. To do this, choose min/max fraction (max > min) or ml (decimals should be typed with a decimal point, not with a comma, e.i. 1.1) and tick one of the boxes “Highlight fractions/ml on/under the peak”.  You can also select the color of the highlighted area under the peak in the “Colors” tab."),
              icon = icon("info-circle")
            )
          )
        ),
        tabPanel(
          "Zoom in", br(),
          textInput("x_min", "Minimum x", value = "", width = NULL, placeholder = NULL),
          textInput("x_max", "Maximum x", value = "", width = NULL, placeholder = NULL),
          textInput("y_min", "Minimum y", value = "", width = NULL, placeholder = NULL),
          textInput("y_max", "Maximum y", value = "", width = NULL, placeholder = NULL),
          tags$hr(style = "border-color: black;"),
          column(
            width = 12,
            infoBox("Info for",
              width = NULL, value = "tab “Zoom in”: ",
              subtitle = tags$p("It allows you to change the labels on the axes of the plot and zoom in. Type in minimum and maximum x and y (max > min)."),
              icon = icon("info-circle")
            )
          )
        ),
        tabPanel(
          "Colors", br(), colourpicker::colourInput("colour4", "Select line color for 280 nm", "blue", allowTransparent = T),
          colourpicker::colourInput("colour3", "Select line color for 260 nm", "red", allowTransparent = T),
          colourpicker::colourInput("colour2", "Select fill color for 280 nm", "transparent", allowTransparent = T),
          colourpicker::colourInput("colour1", "Select fill color for 260 nm", "transparent", allowTransparent = T),
          colourpicker::colourInput("colour6", "Select fill color for fractions or ml area", "#9e6c9e", allowTransparent = T)
        )
      )
    ),
    column(8, plotOutput("plot1", width = "800px", height = "400px"))
  )
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2)

  get.data <- reactive({
    inFile <- input$file1
    if (!is.null(inFile)) {
      read.table(file = inFile$datapath, sep = "\t", fileEncoding = "utf-16", header = T)
    }
  })

  # One could adapt the code for reading .csv files:
  #  get.data <- reactive({
  #   inFile <- input$file1.csv
  #  if(!is.null(inFile)){
  #   read.csv(file=inFile$datapath, header=T)
  #  }
  #  })



  observeEvent(input$file1, {
    updatePickerInput(session, "fraction1", choices = createdropdown(get.data()))
  })

  observeEvent(input$file1, {
    updatePickerInput(session, "fraction2", choices = createdropdown(get.data()))
  })

  observeEvent(input$file1, {
    updatePickerInput(session, "fraction3", choices = createdropdown(get.data()))
  })

  observeEvent(input$file1, {
    updatePickerInput(session, "fraction4", choices = createdropdown(get.data()))
  })



  output$plot1 <- renderPlot({
    my.data2 <- get.data()
    background <- input$background
    fraction280 <- input$fraction280
    fraction260 <- input$fraction260
    ml280 <- input$ml280
    ml260 <- input$ml260
    ml_min <- input$ml_min
    ml_max <- input$ml_max
    ufraction <- input$ufraction
    plot260 <- input$plot260
    plot280 <- input$plot280
    baseline260 <- input$baseline260
    baseline280 <- input$baseline280
    fraction1 <- input$fraction1
    fraction2 <- input$fraction2
    fraction3 <- input$fraction3
    fraction4 <- input$fraction4
    colour1 <- input$colour1
    colour2 <- input$colour2
    colour3 <- input$colour3
    colour4 <- input$colour4
    colour6 <- input$colour6
    x_min <- input$x_min
    x_max <- input$x_max
    y_min <- input$y_min
    y_max <- input$y_max
    transformFile(my.data2, background, fraction260, fraction280, ufraction, plot260, plot280, baseline260, baseline280, fraction1, fraction2, fraction3, fraction4, ml280, ml260, ml_min, ml_max, colour1, colour2, colour3, colour4, colour6, x_min, x_max, y_min, y_max)
  })


  # The code for downloadHandler is adapted from https://community.rstudio.com/t/using-downloadhandler-and-r-studio-export-as-pdf-cairo-pdf/12881/2

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$file1, ".pdf", sep = "")
    },
    content = function(file) {
      cairo_pdf(
        filename = file,
        width = 18, height = 10, pointsize = 12, family = "sans", bg = "transparent",
        antialias = "subpixel", fallback_resolution = 300
      )
      my.data <- get.data()
      my.data2 <- get.data()
      background <- input$background
      fraction280 <- input$fraction280
      fraction260 <- input$fraction260
      ml280 <- input$ml280
      ml260 <- input$ml260
      ml_min <- input$ml_min
      ml_max <- input$ml_max
      ufraction <- input$ufraction
      plot260 <- input$plot260
      plot280 <- input$plot280
      baseline260 <- input$baseline260
      baseline280 <- input$baseline280
      fraction1 <- input$fraction1
      fraction2 <- input$fraction2
      fraction3 <- input$fraction3
      fraction4 <- input$fraction4
      colour1 <- input$colour1
      colour2 <- input$colour2
      colour3 <- input$colour3
      colour4 <- input$colour4
      colour6 <- input$colour6
      x_min <- input$x_min
      x_max <- input$x_max
      y_min <- input$y_min
      y_max <- input$y_max

      plot(transformFile(my.data2, background, fraction260, fraction280, ufraction, plot260, plot280, baseline260, baseline280, fraction1, fraction2, fraction3, fraction4, ml280, ml260, ml_min, ml_max, colour1, colour2, colour3, colour4, colour6, x_min, x_max, y_min, y_max))
      dev.off()
    },
    contentType = "application/pdf"
  )

  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste(input$file1, ".eps", sep = "")
    },
    content = function(file) {
      cairo_ps(
        filename = file,
        width = 18, height = 10, pointsize = 12, family = "sans", bg = "transparent",
        antialias = "subpixel", fallback_resolution = 300
      )
      my.data <- get.data()
      my.data2 <- get.data()
      background <- input$background
      fraction280 <- input$fraction280
      fraction260 <- input$fraction260
      ml280 <- input$ml280
      ml260 <- input$ml260
      ml_min <- input$ml_min
      ml_max <- input$ml_max
      ufraction <- input$ufraction
      plot260 <- input$plot260
      plot280 <- input$plot280
      baseline260 <- input$baseline260
      baseline280 <- input$baseline280
      fraction1 <- input$fraction1
      fraction2 <- input$fraction2
      fraction3 <- input$fraction3
      fraction4 <- input$fraction4
      colour1 <- input$colour1
      colour2 <- input$colour2
      colour3 <- input$colour3
      colour4 <- input$colour4
      colour6 <- input$colour6
      x_min <- input$x_min
      x_max <- input$x_max
      y_min <- input$y_min
      y_max <- input$y_max

      plot(transformFile(my.data2, background, fraction260, fraction280, ufraction, plot260, plot280, baseline260, baseline280, fraction1, fraction2, fraction3, fraction4, ml280, ml260, ml_min, ml_max, colour1, colour2, colour3, colour4, colour6, x_min, x_max, y_min, y_max))
      dev.off()
    },
    contentType = "application/eps"
  )


  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      paste(input$file1, ".tiff", sep = "")
    },
    content = function(file) {
      CairoTIFF(
        filename = file, width = 3456, height = 1920, pointsize = 12, family = "sans", bg = "transparent",
        dpi = 400, fallback_resolution = 400
      )
      my.data <- get.data()
      my.data2 <- get.data()
      background <- input$background
      fraction280 <- input$fraction280
      fraction260 <- input$fraction260
      ml280 <- input$ml280
      ml260 <- input$ml260
      ml_min <- input$ml_min
      ml_max <- input$ml_max
      ufraction <- input$ufraction
      plot260 <- input$plot260
      plot280 <- input$plot280
      baseline260 <- input$baseline260
      baseline280 <- input$baseline280
      fraction1 <- input$fraction1
      fraction2 <- input$fraction2
      fraction3 <- input$fraction3
      fraction4 <- input$fraction4
      colour1 <- input$colour1
      colour2 <- input$colour2
      colour3 <- input$colour3
      colour4 <- input$colour4
      colour6 <- input$colour6
      x_min <- input$x_min
      x_max <- input$x_max
      y_min <- input$y_min
      y_max <- input$y_max

      plot(transformFile(my.data2, background, fraction260, fraction280, ufraction, plot260, plot280, baseline260, baseline280, fraction1, fraction2, fraction3, fraction4, ml280, ml260, ml_min, ml_max, colour1, colour2, colour3, colour4, colour6, x_min, x_max, y_min, y_max))
      dev.off()
    },
    contentType = "application/tiff"
  )

  # The code for downloading the image and excel table was adapted from https://stackoverflow.com/questions/63483105/error-with-downloadhandler-downloading-an-image-with-r-shiny

  output$downloadTableHeader <- downloadHandler(
    filename = "Table_header.png",
    contentType = "image/png",
    content = function(file) {
      outfile <- tempfile(fileext = ".png")
      image <- png::readPNG(file.path(src = "www/Table_header.png"))
      png(outfile, width = 6.58, height = 1.21, units = "in", res = 600)
      par(mai = c(0, 0, 0, 0))
      plot.new()
      rasterImage(image, 0, 0, 1, 1)
      dev.off()
      file.copy(outfile, file)
      file.remove(outfile)
    }
  )

  output$downloadTemplate <- downloadHandler(
    filename = "Template.xlsx",
    contentType = "excel file",
    content = function(file) {
      outfile <- tempfile(fileext = ".xlsx")
      excel_table <- readxl::read_excel(file.path(src = "www/Template.xlsx"))
      write_xlsx(excel_table, outfile)
      file.copy(outfile, file)
    }
  )
}

shinyApp(ui, server)
