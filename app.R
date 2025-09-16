# app.R - front-end that calls daymet2Raven_nc() with lat/lon=NULL and outdir hardcoded to "out"
library(shiny)
library(shinyjs)
library(sf)
library(zip)
options(shiny.session.timeout = 300)

# Raw URL where daymet2Raven_nc() is stored
DAYMET_FUNC_URL <- "https://raw.githubusercontent.com/rarabzad/daymet2Raven/refs/heads/main/daymet2Raven_nc.R"

# Source the daymet2Raven_nc function at startup
tryCatch({
  source(DAYMET_FUNC_URL)
}, error = function(e) {
  message("Failed to source daymet2Raven_nc from URL: ", e$message)
})

# Helper label with popover
label_with_help <- function(id, label_text, help_text) {
  tags$label(
    style = "display: inline-flex; align-items: center;",
    label_text,
    tags$span(icon("question-circle", style = "color:#337ab7; cursor:pointer; margin-left: 5px;",
                   id = id, tabindex = "0", `data-toggle` = "popover", `data-trigger` = "hover focus",
                   `data-content` = help_text, `data-html` = "true", title = "")),
    tags$script(HTML(sprintf("$(function(){ $('#%s').popover({placement:'right',container:'body'}); });", id)))
  )
}


ui <- fluidPage(
  tags$div(
    style = "display: flex; align-items: center; gap: 15px;",
    tags$img(src = "logo.png", width = "100px", style = "border-radius: 20px;"),
    tags$h3("Daymet2Raven")
  ),
  tags$p(
    "This app is designed to process spatial climate data from the Daymet database and prepare it for use in the Raven Hydrological Modelling Framework. It generates a NetCDF file and grid weight information, integrating spatial data from a shapefile with gridded climatic variables."
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$p(
        "For more information and sample data ",
        tags$a(href = "https://github.com/rarabzad/daymet2Raven/tree/main",
               "click here", target = "_blank")
      ),
      fileInput("hru_zip", label_with_help("help_hru", "Upload a zipped shapefile", "Upload a zipped HRU/subbasin/basin shapefile (.zip) containing .shp/.dbf/.shx/.prj etc."), accept = ".zip"),
      dateInput(
        "start_date",
        label_with_help("help_start_date", "Start Date", "Start date for data (YYYY-MM-DD)."),
        value = as.Date(paste0(as.numeric(format(Sys.Date(), "%Y"))-1,"-01-01")),
        max = as.Date(paste0(as.numeric(format(Sys.Date(), "%Y"))-1,"-12-31"))
      ),
      dateInput(
        "end_date",
        label_with_help("help_end_date", "End Date", "End date for data (YYYY-MM-DD)."),
        value = as.Date(paste0(as.numeric(format(Sys.Date(), "%Y"))-1,"-12-31")),
        max = as.Date(paste0(as.numeric(format(Sys.Date(), "%Y"))-1,"-12-31"))
      ),
      textInput("grid_size_txt", label_with_help("help_grid_size", "NetCDF Cell Size", "Grid resolution in degrees (default 0.1)."), placeholder = "e.g. 0.1"),
      uiOutput("hru_id_ui"),
      checkboxInput("plot_flag", label_with_help("help_plot_flag", "Plot", "Produce diagnostic PDFs."), value = TRUE),
      actionButton("run_btn", label_with_help("help_run_btn", "Fetch Data and Process", "Start data fetch and processing."), class = "btn-primary"),
      uiOutput("download_ui_sidebar")
    ),
    
    mainPanel(
      verbatimTextOutput("log_text"),
      hr(),
      uiOutput("produced_files_ui")
    )
  )
)


server <- function(input, output, session) {
  # Hardcoded outdir (in app working directory)
  outdir <- normalizePath(file.path(getwd(), "out"), mustWork = FALSE)
  
  # reactive values
  shp_tmpdir <- reactiveVal(NULL)
  shp_path <- reactiveVal(NULL)
  shp_sf <- reactiveVal(NULL)
  produced_files <- reactiveVal(character(0))
  log_msgs <- reactiveVal(character(0))
  
  observeEvent(input$start_date, {
    updateDateInput(session, "end_date", min = input$start_date)
  })
  
  # Ensure start_date cannot be after end_date
  observeEvent(input$end_date, {
    updateDateInput(session, "start_date", max = input$end_date)
  })
  
  
  append_log <- function(msg) {
    t <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_msgs(head(c(paste0("[", t, "] ", msg), log_msgs()), 400))
  }
  
  output$log_text <- renderText({
    paste(rev(log_msgs()), collapse = "\n")
  })
  
  # Unpack shapefile when uploaded and populate HRU_ID choices
  observeEvent(input$hru_zip, {
    req(input$hru_zip)
    td <- tempfile("hruzip_")
    dir.create(td)
    unzip(input$hru_zip$datapath, exdir = td)
    shp_files <- list.files(td, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp_files) == 0) {
      append_log("ERROR: No .shp found inside the uploaded zip.")
      return()
    }
    shp_tmpdir(td)
    shp_path(shp_files[1])
    append_log(paste("Shapefile unpacked:", basename(shp_files[1])))
    # read sf attributes
    res <- tryCatch({
      s <- sf::st_read(shp_files[1], quiet = TRUE)
      shp_sf(s)
      append_log(paste("Shapefile read; features:", nrow(s)))
      TRUE
    }, error = function(e) {
      append_log(paste("ERROR reading shapefile:", e$message))
      FALSE
    })
  })
  
  output$hru_id_ui <- renderUI({
    s <- shp_sf()
    if (is.null(s)) {
      tagList(
        #helpText("Upload HRU shapefile (.zip) to select HRU_ID field.")
      )
    } else {
      selectInput("HRU_ID", "HRU_ID (field name from shapefile)", choices = names(s), selected = names(s)[1])
    }
  })
  
  # parse grid_size input
  parse_grid_size <- function(txt) {
    if (is.null(txt) || txt == "" || grepl("^\\s*$", txt)) return(NULL)
    v <- unlist(strsplit(txt, "\\s*,\\s*"))
    nv <- as.numeric(v)
    if (any(is.na(nv))) return(NULL)
    return(nv)
  }
  
  # Run button: validate and call daymet2Raven_nc()
  observeEvent(input$run_btn, {
    append_log("Run requested.")
    if (!exists("daymet2Raven_nc", mode = "function")) {
      append_log("ERROR: daymet2Raven_nc() not found in the app environment. Check DAYMET_FUNC_URL or paste function locally.")
      return()
    }
    if (is.null(shp_path())) {
      append_log("ERROR: please upload a zipped HRU shapefile (.zip) and wait until it is processed.")
      return()
    }
    # HRU_ID must be selected
    if (is.null(input$HRU_ID) || input$HRU_ID == "") {
      append_log("ERROR: HRU_ID must be selected from shapefile fields.")
      return()
    }
    # dates validation
    sd <- input$start_date
    ed <- input$end_date
    if (is.null(sd) || is.null(ed) || sd > ed) {
      append_log("ERROR: start_date and end_date must be valid and start_date <= end_date.")
      return()
    }
    
    # Read shapefile sf (we stored it earlier; re-read if needed)
    s <- NULL
    try({
      s <- sf::st_read(shp_path(), quiet = TRUE)
    }, silent = TRUE)
    if (is.null(s)) {
      append_log("ERROR: failed to read shapefile for validation.")
      return()
    }
    
    # 1) Attempt to repair invalid geometries and drop empty geometries
    s_fixed <- tryCatch({
      s2 <- sf::st_make_valid(s)
      # some drivers return GEOMETRYCOLLECTIONs; extract polygons if needed
      # ensure single geometry column
      geom_empty <- sf::st_is_empty(s2)
      if (any(geom_empty)) {
        append_log(paste0("Warning: ", sum(geom_empty), " empty geometry(ies) found and will be removed."))
        s2 <- s2[!geom_empty, , drop = FALSE]
      }
      # drop if zero rows after cleaning
      if (nrow(s2) == 0) stop("All geometries are empty after cleaning.")
      s2
    }, error = function(e) {
      append_log(paste("ERROR validating shapefile geometry:", e$message))
      NULL
    })
    if (is.null(s_fixed)) return()
    
    # 2) Determine grid_size: parse user input; if NULL compute sensible default from bbox
    parse_grid_size <- function(txt) {
      if (is.null(txt) || txt == "" || grepl("^\\s*$", txt)) return(NULL)
      v <- unlist(strsplit(txt, "\\s*,\\s*"))
      nv <- as.numeric(v)
      if (any(is.na(nv))) return(NULL)
      return(nv)
    }
    gs_user <- parse_grid_size(input$grid_size_txt)
    
    if (is.null(gs_user)) {
      # compute bbox span in degrees (first transform to lon/lat if necessary)
      s_ll <- tryCatch({
        sf::st_transform(s_fixed, 4326)
      }, error = function(e) {
        append_log(paste("Warning: could not transform shapefile to EPSG:4326 for bbox; using original CRS."))
        s_fixed
      })
      bb <- sf::st_bbox(s_ll)
      dx <- bb$xmax - bb$xmin
      dy <- bb$ymax - bb$ymin
      # avoid zero spans (single point) — fall back to small defaults
      if (dx <= 0 || is.na(dx)) dx <- 0.5
      if (dy <= 0 || is.na(dy)) dy <- 0.5
      # choose grid cell about 1/20 of span (adjustable)
      gs_auto <- c(dx / 20, dy / 20)
      # ensure positive non-zero
      gs_auto[gs_auto <= 0] <- 0.01
      append_log(paste0("No grid_size provided. Using automatic grid_size = ", paste(round(gs_auto, 6), collapse = ", ")))
      grid_size_arg <- gs_auto
    } else {
      grid_size_arg <- gs_user
      append_log(paste0("Using user grid_size = ", paste(grid_size_arg, collapse = ", ")))
    }
    
    # Ensure outdir exists and is empty (hardcoded "out")
    if (dir.exists(outdir)) {
      unlink(file.path(outdir, "*"), recursive = TRUE, force = TRUE)
    } else {
      dir.create(outdir, recursive = TRUE)
    }
    append_log(paste("Using hardcoded outdir:", outdir))
    
    # Call function with lat = NULL, lon = NULL
    append_log(sprintf("Calling daymet2Raven_nc(hru_shp_file=%s, start_date=%s, end_date=%s, grid_size=%s, lat=NULL, lon=NULL, HRU_ID=%s, outdir=%s, plot=%s)",
                       basename(shp_path()), as.character(sd), as.character(ed),
                       if (is.null(grid_size_arg)) "NULL" else paste(round(grid_size_arg,6), collapse = ","),
                       input$HRU_ID, outdir, ifelse(isTRUE(input$plot_flag), "TRUE", "FALSE")))
    
    # run
    withProgress(message = "Running daymet2Raven_nc()", value = 0, {
      incProgress(0.02)
      tryCatch({
        daymet2Raven_nc(
          hru_shp_file = shp_path(),
          start_date = as.character(sd),
          end_date = as.character(ed),
          grid_size = grid_size_arg,
          lat = NULL,
          lon = NULL,
          HRU_ID = input$HRU_ID,
          outdir = outdir,
          plot = isTRUE(input$plot_flag)
        )
        incProgress(0.9)
        # collect outdir files
        files <- list.files(outdir, full.names = TRUE, recursive = TRUE)
        produced_files(files)
        # register the out directory as a static resource path so we can embed files in the UI
        addResourcePath("out_files", outdir)
        append_log(paste("Finished. Files produced:", paste(basename(files), collapse = ", ")))
      }, error = function(e) {
        append_log(paste("ERROR running daymet2Raven_nc():", e$message))
        # If there is an sf-to-sp conversion problem we can add a helpful hint:
        if (grepl("empty geometries|conversion failed|empty geometries are not supported", e$message, ignore.case = TRUE)) {
          append_log("Hint: the shapefile geometry or buffer likely produced empty geometry. Try simplifying the shapefile, or provide a small grid_size (e.g. 0.1).")
        }
      }, finally = {
        incProgress(1)
      })
    })
  })
  
  # UI for produced files and download
  # Server: Show produced files list in main panel (no download button here)
  # Render a tabset with embedded PDFs (one tab per pdf in out folder)
  output$produced_files_ui <- renderUI({
    files <- produced_files()
    if (length(files) == 0) {
      return(helpText(""))
    }
    
    # find PDFs only (use basenames)
    pdfs <- basename(list.files(outdir, pattern = "\\.pdf$", full.names = FALSE))
    if (length(pdfs) == 0) {
      return(helpText("No PDF outputs found in 'out'. Available files: ", paste(basename(files), collapse = ", ")))
    }
    
    tabs <- lapply(pdfs, function(pf) {
      tabPanel(
        title = pf,
        # embed the PDF via the resource path we registered earlier (out_files)
        tags$div(
          style = "width:100%;",
          tags$iframe(
            src = file.path("out_files", pf),
            style = "width:100%; height:700px; border:none;"
          )
        )
      )
    })
    
    # Return the tabsetPanel (default to the first tab)
    do.call(tabsetPanel, c(id = "pdfTabs", tabs))
  })
  output$download_ui_sidebar <- renderUI({
    files <- produced_files()
    if (length(files) == 0) return(NULL)
    downloadButton("download_zip", "     Download    ")
  })
  # Download handler: zip the hardcoded out folder
  # robust download handler: copy out contents to temp dir, zip using relative paths
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("out_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      append_log("Download requested: preparing zip...")
      
      # ensure zip package available
      if (!requireNamespace("zip", quietly = TRUE)) {
        append_log("ERROR: 'zip' package not installed.")
        stop("Server missing required package 'zip'.")
      }
      
      # 1) collect files in outdir
      if (!dir.exists(outdir)) stop("out directory does not exist.")
      all_files_abs <- list.files(outdir, all.files = TRUE, recursive = TRUE, full.names = TRUE, no.. = TRUE)
      if (length(all_files_abs) == 0) stop("No files to include in the zip (out directory empty).")
      append_log(paste0("Found ", length(all_files_abs), " file(s) in out directory."))
      
      # log first few files and their sizes
      nf <- head(all_files_abs, 10)
      for (f in nf) {
        fi <- tryCatch(file.info(f), error = function(e) NULL)
        append_log(paste0(" - ", basename(f), " (", ifelse(is.null(fi$size), "size-NA", paste0(fi$size, " bytes")), ")"))
      }
      if (length(all_files_abs) > length(nf)) append_log(paste0("... and ", length(all_files_abs) - length(nf), " more"))
      
      # 2) create temp working dir and copy files preserving relative structure
      tmp_root <- tempfile("out_zip_contents_")
      dir.create(tmp_root, recursive = TRUE)
      append_log(paste0("Created temporary folder for zipping: ", tmp_root))
      
      # compute relative paths and copy files
      outdir_norm <- normalizePath(outdir, winslash = "/", mustWork = TRUE)
      copied <- 0L
      for (absf in all_files_abs) {
        absf_norm <- normalizePath(absf, winslash = "/", mustWork = TRUE)
        # relative to outdir
        rel_path <- substring(absf_norm, nchar(outdir_norm) + 2) # +2 to remove the slash
        target_dir <- file.path(tmp_root, dirname(rel_path))
        if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
        ok <- file.copy(absf_norm, file.path(tmp_root, rel_path), overwrite = TRUE, copy.mode = TRUE)
        if (!ok) {
          append_log(paste0("WARNING: failed to copy file for zipping: ", absf_norm))
        } else {
          copied <- copied + 1L
        }
      }
      append_log(paste0("Copied ", copied, " files to temporary folder (for zipping)."))
      
      # 3) create temporary zip inside system tempdir
      tmpzip <- tempfile(fileext = ".zip")
      cwd <- getwd()
      on.exit({
        # cleanup
        try(setwd(cwd), silent = TRUE)
        # try remove tmp dir (best-effort)
        try(unlink(tmp_root, recursive = TRUE), silent = TRUE)
      }, add = TRUE)
      
      # use relative paths from tmp_root
      wd_ok <- tryCatch({
        setwd(tmp_root)
        TRUE
      }, error = function(e) {
        append_log(paste0("ERROR: could not setwd to temp folder: ", e$message))
        FALSE
      })
      if (!wd_ok) stop("Failed to set working dir for zipping.")
      
      files_to_zip <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
      if (length(files_to_zip) == 0) stop("No files found in temporary zipping folder (unexpected).")
      
      append_log(paste0("Zipping ", length(files_to_zip), " file(s) into ", tmpzip, " ..."))
      zres <- tryCatch({
        zip::zipr(zipfile = tmpzip, files = files_to_zip)
        TRUE
      }, error = function(e) {
        append_log(paste0("ERROR: zip::zipr failed: ", e$message))
        FALSE
      })
      
      if (!isTRUE(zres) || !file.exists(tmpzip)) {
        append_log("ERROR: Zip creation failed (tmpzip not found).")
        stop("Zip creation failed; see app log for details.")
      }
      
      # 4) copy tmp zip to Shiny's expected 'file' path
      copied_ok <- tryCatch({
        file.copy(tmpzip, file, overwrite = TRUE)
      }, error = function(e) {
        append_log(paste0("ERROR copying zip to Shiny download path: ", e$message))
        FALSE
      })
      if (!isTRUE(copied_ok)) {
        append_log("ERROR: failed to copy zip to download location.")
        stop("Failed to copy zip to download location.")
      }
      append_log(paste0("Zip created and copied to download location: ", basename(file), " (", format(file.info(file)$size, big.mark = ","), " bytes)"))
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)
