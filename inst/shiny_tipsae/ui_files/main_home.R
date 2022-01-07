shiny::tagList(
  shiny::h4("Qui semplicemente una descrizione dell'app e dei diversi passaggi"),
  shiny::tags$ol(
    shiny::tags$li("caricare i dati e breve esplorazione"),
    shiny::tags$li("Stima del modello"),
    shiny::tags$li("check MCMCM"),
    shiny::tags$li("risultati")
  ),
  shiny::h4("varie references")
)
