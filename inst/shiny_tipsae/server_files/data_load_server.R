### Create Reactive objects -----

# creation reactive object with loaded data
loaded_data <- shiny::reactive({
  if(input$load_emilia_cs > 0) {
    read.csv(
      file = system.file("extdata","emilia_cs.csv", package = "tipsae")
    )
  }else{
  if (is.null(input$file1$datapath)) {
    return(NULL)
  } else {
    extension <- tools::file_ext(input$file1$datapath)
    check_data <- !(extension %in% c("csv", "txt"))
    shinyFeedback::feedbackDanger(
      "file1", check_data, "A '.csv' or '.txt' file must be uploaded"
    )
    if (check_data) {
      NULL
    }else{
      read.csv(
        file = input$file1$datapath,
        header = as.logical(input$header_pres),
        sep = input$sep_file1,
        dec = input$dec_file1
      )
    }
  }}
})

# Freeze the input metadata: reaction to the 'update_data1' button
organized_data <- shiny::eventReactive(input$update_data1, {
  if (is.null(input$choice_cov)) {
    cov_names <- "1"
  } else {
    cov_names <- input$choice_cov
  }
  formula_input <- as.formula(paste0(input$choice_resp, "~", paste(cov_names, collapse = "+")))
  terms_input <- terms.formula(formula_input)
  data1_obj <-  create_data_shiny(
    formula_fixed = formula_input,
    terms_formula = terms_input,
    data = loaded_data(),
    domains = input$domain_col,
    domain_size = input$size_col,
    disp_direct = input$choice_var)
  # check response
  check_resp <- !all(data1_obj$y_is >= 0 & data1_obj$y_is <= 1)
  shinyFeedback::feedbackDanger("choice_resp", check_resp,
                                "The response must be included in the [0,1] interval")
  # check covariates
  check_cov <- !all(complete.cases(data1_obj$X_scal))
  check_cov2 <- ncol(data1_obj$X_scal) == 0
  shinyFeedback::feedbackDanger("choice_cov", check_cov || check_cov2,
                                "At least one covariate must be considered and
                                  the covariates must not have NAs.")
  # check var 1
  check_var1 <- !all(!is.na(data1_obj$dispersion[data1_obj$y_is != 0 &
                                                   data1_obj$y_is != 1]))
  # check var 2
  check_var2 <- sum(data1_obj$dispersion <= 0, na.rm = T) != 0
  shinyFeedback::feedbackDanger("choice_var", check_var2 || check_var1,
                                "The dispersion parameters must not be negative and not have NAs.")
  # check size domains
  check_size <- FALSE
  if (!is.null(data1_obj$domain_size_n)) {
    check_size <- sum(data1_obj$domain_size_n <= 0, na.rm = T) != 0
    shinyFeedback::feedbackDanger("size_col", check_size,
                                  "The domains sizes must not be negative.")
  }
  # check names domains
  check_names <- FALSE
  if (input$domain_col == "Unspecified") {
    if (input$time_eff == "Yes") {
      check_names <- TRUE
    }
    shinyFeedback::feedbackDanger("domain_col", check_names,
                                  "When the indicator is repeatedly measured in time,
                                  the domains names must be specified.")
  }
  # check time
  check_time <- FALSE
  time <- NULL
  if (input$time_eff == "Yes") {
    time_col_name <- input$time_col
    time <- loaded_data()[, time_col_name]
    TP <- length(unique(time)) # time periods
    D <- nrow(loaded_data()) / TP # areas
    check_time <- D %% 1 != 0
    shinyFeedback::feedbackDanger("time_col", check_time,
                                  "Each area should have the same observed time periods.")
  }
  # check smooth
  check_smooth <- FALSE
  if(input$need_smooth == "Yes"){
    if(input$type_disp == "neff"){
      check_smooth <- TRUE
    }
    if(input$size_col == "Unspecified"){
      check_smooth <- TRUE
    }
  }
  if (check_resp || check_cov || check_cov2 || check_var1 || check_var2 ||
      check_size || check_time || check_smooth || check_names) {
    # with an error return NULL
    return(NULL)
  } else {
    return(list(
      formula = formula_input,
      data = loaded_data(),
      domain_colname = input$domain_col,
      id_domains = data1_obj$domains_names,
      size = input$size_col,
      disp = input$choice_var,
      type_disp = input$type_disp,
      need_smooth = input$need_smooth,
      time_eff = input$time_eff,
      check_smooth = check_smooth,
      time = time,
      all = data1_obj
    )
    )
  }
})

### renderUI for Inputs -----

# Input: choose the response
output$resp <- shiny::renderUI({
  shiny::selectInput("choice_resp",
                     "Response variable",
                     choices = colnames(loaded_data()),
                     width = '300px')
})

# Input: choose the covariates
output$cov <- shiny::renderUI({
  shiny::selectInput("choice_cov",
                     "Covariates",
                     choices = colnames(loaded_data())[colnames(loaded_data()) != input$choice_resp],
                     multiple = TRUE)
})

# Input: choose the dispersion parameter
output$var <- shiny::renderUI({
  shiny::selectInput("choice_var",
                     "Dispersion parameter",
                     choices = colnames(loaded_data())[!(colnames(loaded_data()) %in% c(input$choice_resp, input$choice_cov)) ],
                     width = '300px')
})

# Input: choose the time variable
output$time_name <- shiny::renderUI({
  shiny::selectInput("time_col",
                     "Time variable:",
                     choices = colnames(loaded_data())[!(colnames(loaded_data()) %in% c(input$choice_resp,input$choice_cov, input$choice_var))],
                     width = '300px')
})
# Input: choose the names of the domains
output$domain_name <- shiny::renderUI({
  shiny::selectInput("domain_col",
                     "Domains names (required to include a time variable and useful to load a map):",
                     choices = c("Unspecified", colnames(loaded_data())[!(colnames(loaded_data()) %in% c(input$choice_resp,input$choice_cov,
                                                                                                         input$choice_var, input$time_name))]),
                     width = '300px')
})
# Input: choose the sample size
output$size_name <- shiny::renderUI({
  shiny::selectInput("size_col",
                     "Domains sample sizes (required for smoothing procedures and to evaluate design consistency):",
                     choices = c("Unspecified", colnames(loaded_data())[!(colnames(loaded_data()) %in% c(input$choice_resp,input$choice_cov,
                                                                                                         input$choice_var, input$time_name,
                                                                                                         input$domain_name))]),
                     width = '300px')
})

## Output: logical statements ------

# condition: TRUE if example data is loaded
output$example_data <- shiny::reactive({
  input$load_emilia_cs > 0
})
shiny::outputOptions(output, 'example_data', suspendWhenHidden = FALSE)


# condition: TRUE if data is loaded
output$cond_data <- shiny::reactive({
  is.null(loaded_data())
})
shiny::outputOptions(output, 'cond_data', suspendWhenHidden = FALSE)

# Condition: true if a time variable is requested
output$time_present <- shiny::reactive({
  input$time_eff == "Yes"
})
shiny::outputOptions(output, 'time_present', suspendWhenHidden = FALSE)

# Condition: TRUE if the area sample sizes are specified
output$presence_sample_size <- shiny::reactive({
  if(is.null(input$size_col)){
    FALSE
  }else{
    input$size_col != "Unspecified"
  }
})
shiny::outputOptions(output, 'presence_sample_size', suspendWhenHidden = FALSE)

## Condition: TRUE if metadata are wrong
output$show_wrong_load <- shiny::reactive({
  if (input$update_data1 > 0 ) {
    is.null(organized_data())
  }
})
shiny::outputOptions(output, 'show_wrong_load', suspendWhenHidden = FALSE)

## Condition: TRUE if metadata are correctly loaded
output$show_correct_load <- shiny::eventReactive(input$update_data1, {
  if (input$update_data1 > 0) {
    !is.null(organized_data())
  }
})
shiny::outputOptions(output, 'show_correct_load', suspendWhenHidden = FALSE)

## Condition: TRUE if an error in the specification of the smoothing procedure occurred
output$cond_error_smoothing <- shiny::eventReactive(input$update_data1,{
  check_smooth <- FALSE
  if(input$need_smooth == "Yes"){
    if(input$type_disp == "neff"){
      check_smooth <- TRUE
    }
    if(input$size_col == "Unspecified"){
      check_smooth <- TRUE
    }
  }
})
shiny::outputOptions(output, 'cond_error_smoothing', suspendWhenHidden = FALSE)


## Output: visual/data outcomes -------

# Output: data table
output$data_preview <- shiny::renderDataTable(
  options = list(pageLength = 10, scrollX = TRUE), {
    loaded_data()
  })

## Output: messages after clicking the button
output$correct_load <- shiny::renderText({"Metadata correctly saved"})
output$wrong_load <- shiny::renderText({"An Error in the input metadata occurred: see the error messages"})
output$error_smoothing <- shiny::renderText({"If a variance smoothing process is required, the input must be a
                                      raw variance and the domains sample sizes must be provided."})


## Observe expressions ------

# Allow to open a new window to explore data
shiny::observeEvent(input$opendata, {
  shiny::showModal(shiny::modalDialog(
    shiny::tags$head(shiny::tags$style("#showdata{overflow-y:scroll; max-height: 500px;overflow-x:scroll;}")),
    shiny::dataTableOutput("data_preview"),
    size = "l",
    footer = NULL,
    easyClose = TRUE
  ))
})


shiny::observeEvent(input$type_disp, {
  if(input$type_disp == "neff"){
    mychoices <- c("No")

  }else{
    mychoices <- c("No", "Yes")
  }
  shiny::updateRadioButtons(session, "need_smooth", choices = mychoices)
})


