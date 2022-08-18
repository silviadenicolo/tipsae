shiny::tagList(
  shiny::h3("Hi there, here is the tipsae shiny app!"),
  shiny::br(),
  shiny::h4("I'm an interactive application for mapping proportions and indicators defined on the unit interval through small area estimation, relying on ",
            shiny::tags$a(href = "https://cran.r-project.org/", "R"), ", ",
            shiny::tags$a(href = "https://mc-stan.org/", "Stan"), " and ",
            shiny::tags$a(href = "https://shiny.rstudio.com/", "Shiny"), "."),
  shiny::h4("I will allow you to perform a complete SAE analysis via area-level models fitted in the Bayesian framework I implement:"),
  shiny::h4(shiny::tags$ul(style = "line-height:150%",
              shiny::tags$li("the Beta regression model (see ",
                             shiny::tags$a(href = "https://www.tandfonline.com/doi/abs/10.1080/03610926.2019.1570266", "Janicki 2020"),
                             ")"),
              shiny::tags$li("the zero and/or one inflated Beta model (see ",
                             shiny::tags$a(href = "https://www.census.gov/content/dam/Census/library/working-papers/2012/demo/wieczoreknugenthawalajsm2012.pdf", "Wieczorek et al 2012"),
                             ")"),
              shiny::tags$li("the Flexible Beta model (i.e. a Beta mixture model by",
                             shiny::HTML("De Nicol&ograve")," et al., to appear) "
            ))
            ),
  shiny::h4("Moreover, the models may account for possibly spatial and/or temporal dependency structures. For further details see the package ",
            shiny::tags$a(href = "https://cran.r-project.org/web/packages/tipsae/vignettes/tipsae_vignette.pdf", "vignette"),"."),
  shiny::br(),
  shiny::h4("Offered methods include:"),
  shiny::h4(
  shiny::tags$ul(style = "line-height:150%",
    shiny::tags$li("Load your own dataset in CSV format in the",
                   shiny::strong(style = "color: #bbd9ec;", "Data"), "tab."),
    shiny::tags$li("Perform a variance smoothing procedure in the",
                   shiny::strong(style = "color: #bbd9ec;", "Data"), "tab."),
    shiny::tags$li("Account for the possibly spatial dimension of your data by including a shapefile in the",
                   shiny::strong(style = "color: #bbd9ec;", "Data"), "tab."),
    shiny::tags$li("Fit a small area model on your data by means of an MCMC algorithm and check the estimation progress in the",
                   shiny::strong(style = "color: #bbd9ec;", "Model Fitting"), "tab."),
    shiny::tags$li("Check the MCMC diagnostics such as chain mixing and convergence via graphical tools within the",
                   shiny::strong(style = "color: #bbd9ec;", "Check Convergence"), "tab."),
    shiny::tags$li("Visualise model summaries, estimates and small area specific diagnostics via graphical tools in the",
                   shiny::strong(style = "color: #bbd9ec;", "Results"), "tab."),
    shiny::tags$li("Perform a Posterior Predictive Check to validate the model in the",
                   shiny::strong(style = "color: #bbd9ec;", "Results"), "tab."),
    shiny::tags$li("Easily export the produced estimates in the",
                   shiny::strong(style = "color: #bbd9ec;", "Results"), "tab.")
  )
  ),
  shiny::h4("If you want to start analysizing your data move to the", shiny::strong(style = "color: #bbd9ec;", "Data"), "tab. Otherwise press the following button to load the example data (see the documentation of the emilia_cs data):"),
  shiny::fluidRow(style = 'padding-left:15px', shiny::actionButton("load_emilia_cs", label = "Load dataset"))


)
