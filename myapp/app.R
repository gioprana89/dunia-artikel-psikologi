

#library(webr)


#library(plyr)
library(readxl)
#library(openxlsx)
#library(data.table)
library(shiny)
#library(shinyAce)
#source("chooser.R")

#library(lavaan)

#library(mnormt)
#library(curl)
#library(plspm)


########################################
########UI (User Interface)#############
########################################

modul_literature_review_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
    
    
    uiOutput(ns("radio_button_pilih_topik")),
    
    
    br(),
    
    uiOutput(ns("buka_pemilihan_informasi_bebankerja_terhadap_kepuasankerja")),
    uiOutput(ns("buka_pemilihan_informasi_bebankerja_terhadap_turnoverintention")),
    uiOutput(ns("buka_pemilihan_informasi_kepuasankerja_terhadap_turnoverintention")),
    uiOutput(ns("buka_pemilihan_informasi_motivasikerja_terhadap_kepuasankerja")),
    uiOutput(ns("buka_pemilihan_informasi_motivasikerja_terhadap_turnoverintention")),
    #uiOutput(ns("buka_pemilihan_informasi")),
    
  
    br(),
    
    
    #DT::DTOutput(ns("buka_data")),
    DT::DTOutput(ns("buka_data_bebankerja_terhadap_kepuasankerja")),
    DT::DTOutput(ns("buka_data_bebankerja_terhadap_turnoverintention")),           
    DT::DTOutput(ns("buka_data_kepuasankerja_terhadap_turnoverintention")),           
    DT::DTOutput(ns("buka_data_motivasikerja_terhadap_kepuasankerja")),
    DT::DTOutput(ns("buka_data_motivasikerja_terhadap_turnoverintention")),             
               
               
    
    
    
    
    
    br(),
    
    
    #DT::DTOutput(ns("buka_data")),

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    br()
    
  ) #Akhir Fluidpage
  
  
} #Akhir dari modul_literature_review_ui

#Akhir dari modul_literature_review_ui
#Akhir dari modul_literature_review_ui
#Akhir dari modul_literature_review_ui
#Akhir dari modul_literature_review_ui











































































########################################
################Server##################
########################################



modul_literature_review_server <- function(input, output, session) {
  
  
  
  nama_topik <- function()
  {
    
    
    nama_topik <- c("Beban Kerja terhadap Kepuasan Kerja",
                    "Beban Kerja terhadap Turnover Intention",
                    "Kepuasan Kerja terhadap Turnover Intention",
                    "Motivasi Kerja terhadap Kepuasan Kerja",
                    "Motivasi Kerja terhadap Turnover Intention")
    
    
    
  }
  
  
  output$radio_button_pilih_topik <- renderUI({
    
    
    
    
    
    
    radioButtons(session$ns("terpilih_topik_paper"), 
                       label="Pilih Topik:", choices = c(nama_topik()), 
                       selected=c("Beban Kerja terhadap Kepuasan Kerja"), inline = TRUE)
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #uiOutput(ns("buka_pemilihan_informasi")),
  
  
  
  nama_variabel <- function()
  {
    
    dat <- read_xlsx("Beban Kerja terhadap Kepuasan Kerja.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  
  
  
  output$buka_pemilihan_informasi_bebankerja_terhadap_kepuasankerja <- renderUI({
    
    
      
      if(input$terpilih_topik_paper == "Beban Kerja terhadap Kepuasan Kerja")
      {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel"), 
                         label="Pilih Variabel:", choices = c(nama_variabel()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
        
        
      }
      
      
  
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_bebankerja_terhadap_kepuasankerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Beban Kerja terhadap Kepuasan Kerja")
    {
   
    dat <- read_xlsx("Beban Kerja terhadap Kepuasan Kerja.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    
    terpilih_variabel <- input$terpilih_variabel
    
    dat_baru <- dat[c(terpilih_variabel)]
    
    print(dat_baru)
    
    
    }

    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_variabel_bebankerja_terhadap_turnoverintention <- function()
  {
    
    dat <- read_xlsx("Beban Kerja terhadap Turnover Intention.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_bebankerja_terhadap_turnoverintention <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Beban Kerja terhadap Turnover Intention")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_bebankerja_terhadap_turnoverintention"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_bebankerja_terhadap_turnoverintention()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_bebankerja_terhadap_turnoverintention <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Beban Kerja terhadap Turnover Intention")
    {
      
      dat <- read_xlsx("Beban Kerja terhadap Turnover Intention.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_variabel_bebankerja_terhadap_turnoverintention
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_variabel_kepuasankerja_terhadap_turnoverintention <- function()
  {
    
    dat <- read_xlsx("Kepuasan Kerja terhadap Turnover Intention.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_kepuasankerja_terhadap_turnoverintention <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Kepuasan Kerja terhadap Turnover Intention")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_kepuasankerja_terhadap_turnoverintention"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_kepuasankerja_terhadap_turnoverintention()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_kepuasankerja_terhadap_turnoverintention <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Kepuasan Kerja terhadap Turnover Intention")
    {
      
      dat <- read_xlsx("Kepuasan Kerja terhadap Turnover Intention.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_variabel_kepuasankerja_terhadap_turnoverintention
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_variabel_motivasikerja_terhadap_kepuasankerja <- function()
  {
    
    dat <- read_xlsx("Motivasi Kerja terhadap Kepuasan Kerja.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_motivasikerja_terhadap_kepuasankerja <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Motivasi Kerja terhadap Kepuasan Kerja")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_motivasikerja_terhadap_kepuasankerja"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_motivasikerja_terhadap_kepuasankerja()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_motivasikerja_terhadap_kepuasankerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Motivasi Kerja terhadap Kepuasan Kerja")
    {
      
      dat <- read_xlsx("Motivasi Kerja terhadap Kepuasan Kerja.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_variabel_motivasikerja_terhadap_kepuasankerja
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_variabel_motivasikerja_terhadap_turnoverintention <- function()
  {
    
    dat <- read_xlsx("Motivasi Kerja terhadap Turnover Intention.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_motivasikerja_terhadap_turnoverintention <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Motivasi Kerja terhadap Turnover Intention")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_motivasikerja_terhadap_turnoverintention"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_motivasikerja_terhadap_turnoverintention()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_motivasikerja_terhadap_turnoverintention <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Motivasi Kerja terhadap Turnover Intention")
    {
      
      dat <- read_xlsx("Motivasi Kerja terhadap Turnover Intention.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_variabel_motivasikerja_terhadap_turnoverintention
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari modul_literature_review_server

#akhir dari modul_literature_review_server
#akhir dari modul_literature_review_server
#akhir dari modul_literature_review_server

















































































ui <- fluidPage(
  
  
  #includeHTML("intro_home.html"),
  
  
  uiOutput("modul_literature_review"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_literature_review <- renderUI({
    
    
    
    #source("module//modul_literature_review.R")
    callModule(module = modul_literature_review_server, id = "modul_literature_review")
    modul_literature_review_ui(id = "modul_literature_review")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














