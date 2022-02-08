shiny::tagList(
  shiny::conditionalPanel(
    condition = "output.example_data==false",
    shiny::wellPanel(
     shiny::fluidRow(# row with options on the read.csv function
       shiny::column(width = 2,
                     shiny::radioButtons(# header?
                        "header_pres",
                        "A header is present:",
                        c("Yes" = TRUE, "No" = FALSE),
                        inline = T
                      )
        ),
        shiny::column(width = 2,
                      shiny::selectizeInput(# sep
                        inputId = "sep_file1",
                        label = "Choose the column separator:",
                        multiple = FALSE,
                        choices = c("," = ",", ";" = ";", "." = ".",
                                    ":" = ":", "Space" = " ", "Tab" = "\t")
                      )
        ),
        shiny::column(width = 2,
                      shiny::selectInput(# dec
                        inputId = "dec_file1",
                        label = "Choose the decimal separator:",
                        multiple = FALSE,
                        choices = c(",","."),
                        selected = "."
                      )
      ),
      shiny::column(width = 5,offset = 1,
                    shiny::fileInput(buttonLabel = "Browse",## input .csv file with data
                      inputId = "file1",
                      label = "Search for the data file (.csv or .txt files are requested)",
                      accept = c(".csv", ".txt")
                      )
        )
      )
    )
  ),
  shiny::conditionalPanel(## See data: datatable
    condition = "output.cond_data==false",
    shiny::br(),
    shiny::actionButton("opendata", "View loaded dataset"),
    shiny::br(),shiny::br(),
    ## command to use feedbacks
    shinyFeedback::useShinyFeedback(),
    shiny::h3(shiny::strong("Information about the data")),
    shiny::wellPanel(## Information about data
      shiny::h4(shiny::strong("Variables included in the model")),
      ## Response and covariates
      shiny::fluidRow(
        shiny::column(width = 5, shiny::uiOutput("resp")),
        shiny::column(width = 5, offset = 2, shiny::uiOutput("cov"))
      )
    ),
    shiny::wellPanel(## About the dispersion parameter
      shiny::h4(shiny::strong("Dispersion parameter")),
      shiny::fluidRow(
        shiny::column(width = 5, shiny::uiOutput("var")),
        shiny::column(width = 5, offset = 2, shiny::radioButtons(
          "type_disp",
          "Kind of dispersion parameter provided:",
          c("Variance" = "var", "Effective Sample Size" = "neff"),
          inline = T)
        )
      ),
      ## Do we need smooothing
      shiny::fluidRow(
        shiny::column(width = 5,
                      shiny::radioButtons(
                        "need_smooth",
                        "Is the smoothing procedure required for the dispersion parameter?
                        It requires variances as dispersion parameters.",
                        c("No", "Yes")
                      )),shiny::column(width = 5,offset = 2,
                                       shiny::conditionalPanel(condition = "output.cond_error_smoothing==true",
                                                               shiny::strong(style = "color: #d9534f;",
                                                                             shiny::textOutput("error_smoothing"))))
      )
    ),
    shiny::wellPanel(## Time structure
      shiny::h4(shiny::strong("Time variable")),
      shiny::fluidRow(shiny::column(width = 5,
                                    shiny::radioButtons(
                                      "time_eff",
                                      "Is the indicator repeatedly measured in time?",
                                      c("No", "Yes"),
                                      inline = T)
      ),
      shiny::column(width = 5,offset = 2,
                    shiny::conditionalPanel(condition = "output.time_present==true",
                                            shiny::uiOutput("time_name"))
      )
      )
    ),
    ## Further information: sample size and names
    shiny::wellPanel(
      shiny::h4(shiny::strong("Other information")),
      shiny::fluidRow(shiny::column(width = 5, style = "margin-top: 25px;", shiny::uiOutput("domain_name")),
                      shiny::column(width = 5, offset = 2, shiny::uiOutput("size_name"))
      )
    ),
    shiny::fluidRow(shiny::column(width = 3, shiny::actionButton("update_data1", label = "Update data information")),
                    shiny::column(width = 7, offset = 2, style = "margin-top: 10px;",
                                  shiny::conditionalPanel(condition = "output.show_wrong_load==true",
                                                          shiny::strong(style = "color: #d9534f;",
                                                                        shiny::textOutput("wrong_load"))),
                                  shiny::conditionalPanel(condition = "output.show_correct_load==true",
                                                          shiny::strong(style = "color: #5cb85c;",
                                                                        shiny::textOutput("correct_load")))
                    )
    )
  )
)
