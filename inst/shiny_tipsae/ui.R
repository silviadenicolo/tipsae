source("utils_ui.R", local = TRUE)
options(shiny.maxRequestSize = 140*1024^2)

ui <- navbarPage(
  theme = shinythemes::shinytheme("superhero"),
  collapsible = TRUE,
  position = "fixed-top",
  title = "tipsae Shiny app",
  # 1) Home: description of the workflow
  tabPanel(
    shiny::includeCSS("www/complement_theme.css"),
    title = shiny::strong(style = "color: #bbd9ec;", "Home"),
    value = "home",
    source_ui("main_home.R")
  ),
  # 2) Data input and exploration
  tabPanel(
    shiny::includeCSS("www/complement_theme.css"),
    title = shiny::strong(style = "color: #bbd9ec;", "Data"),
    value = "data",
    source_ui("main_data.R")
  ),
  # 3) Fit model
  tabPanel(
    shiny::includeCSS("www/complement_theme.css"),
    title = shiny::strong(style = "color: #bbd9ec;", "Model Fitting"),
    value = "fit",
    source_ui("main_fit.R")
  ),
  # 4) Check convergence
  tabPanel(
    shiny::includeCSS("www/complement_theme.css"),
    title = shiny::strong(style = "color: #bbd9ec;", "Check Convergence"),
    value = "conv",
    source_ui("main_convergence.R")
  ),
  # 5) Results
  tabPanel(
    shiny::includeCSS("www/complement_theme.css"),
    title = shiny::strong(style = "color: #bbd9ec;", "Results"),
    value = "results",
    source_ui("main_results.R")
  )
)
