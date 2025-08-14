

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
    
    
    
    tabsetPanel(
      
      tabPanel(title = tags$h5( tags$img(src = "icon_select.gif", width = "30px"), 'Pilih Topik'),
               
               
               uiOutput(ns("radio_button_pilih_topik")),
               
               br()
               
               
      ),
      
      
      tabPanel(title = tags$h5( tags$img(src = "icon_literatur.gif", width = "30px"), 'Data Referensi'),
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               uiOutput(ns("buka_pemilihan_informasi_bebankerja_terhadap_kepuasankerja")),
               DT::DTOutput(ns("buka_data_bebankerja_terhadap_kepuasankerja")),
               
               uiOutput(ns("buka_pemilihan_informasi_motivasikerja_terhadap_kepuasankerja")),
               DT::DTOutput(ns("buka_data_motivasikerja_terhadap_kepuasankerja")),
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               uiOutput(ns("buka_pemilihan_informasi_bebankerja_terhadap_turnoverintention")),
               DT::DTOutput(ns("buka_data_bebankerja_terhadap_turnoverintention")), 
               
               
               uiOutput(ns("buka_pemilihan_informasi_kepuasankerja_terhadap_turnoverintention")),
               DT::DTOutput(ns("buka_data_kepuasankerja_terhadap_turnoverintention")),   
               
               
               uiOutput(ns("buka_pemilihan_informasi_motivasikerja_terhadap_turnoverintention")),
               DT::DTOutput(ns("buka_data_motivasikerja_terhadap_turnoverintention")),  
               
               
               
               
               
               
               
               
               
               
               
               
               uiOutput(ns("buka_pemilihan_informasi_dukungansosial_terhadap_kesiapankerja")),
               DT::DTOutput(ns("buka_data_dukungansosial_terhadap_kesiapankerja")),   
               
               
               uiOutput(ns("buka_pemilihan_informasi_efikasidiri_terhadap_kesiapankerja")),
               DT::DTOutput(ns("buka_data_efikasidiri_terhadap_kesiapankerja")),   
               
               
               
               
               
               
               
               
               
               
               
               
               
               uiOutput(ns("buka_pemilihan_informasi_kontroldiri_terhadap_cyberloafing")),
               DT::DTOutput(ns("buka_data_kontroldiri_terhadap_cyberloafing")),    
               
               
               uiOutput(ns("buka_pemilihan_informasi_streskerja_terhadap_cyberloafing")),
               DT::DTOutput(ns("buka_data_streskerja_terhadap_cyberloafing")),   
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               uiOutput(ns("buka_pemilihan_informasi_kontroldiri_terhadap_kinerja")),
               DT::DTOutput(ns("buka_data_kontroldiri_terhadap_kinerja")),   
               
               
               uiOutput(ns("buka_pemilihan_informasi_streskerja_terhadap_kinerja")),
               DT::DTOutput(ns("buka_data_streskerja_terhadap_kinerja")),   
               
               
               uiOutput(ns("buka_pemilihan_informasi_cyberloafing_terhadap_kinerja")),
               DT::DTOutput(ns("buka_data_cyberloafing_terhadap_kinerja")),
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               uiOutput(ns("buka_pemilihan_informasi_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM")),
               DT::DTOutput(ns("buka_data_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM")),  
               
               
               #uiOutput(ns("buka_pemilihan_informasi")),
               
               
               br(),
               
               
               #DT::DTOutput(ns("buka_data")),

          
        

           
               
               
          

               
               
               
               
         

               
               

 

               
               
               
               
 
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               br()
               
               
      ) #Akhir tabpanel Data Referensi
      
      
      
      
    ), #Akhir tabsetpanel
    
    
    
    
    
    br(),
    

    
    
    
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
    
    
    nama_topik <- c(
      
      "Beban Kerja terhadap Kepuasan Kerja (PLS-SEM, SmartPLS) (5 Artikel)",
      "Motivasi Kerja terhadap Kepuasan Kerja (PLS-SEM, SmartPLS) (6 Artikel)",
      

      
                    "Beban Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (6 Artikel)",
                    "Kepuasan Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (3 Artikel)",
                    "Motivasi Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (6 Artikel)",
      
      
      
                    
                    "Dukungan Sosial terhadap Kesiapan Kerja (PLS-SEM, SmartPLS) (1 Artikel)",
                    "Efikasi Diri terhadap Kesiapan Kerja (PLS-SEM, SmartPLS) (1 Artikel)",
                    
                    
                    "Kontrol Diri terhadap Cyberloafing (PLS-SEM, SmartPLS) (1 Artikel)",
                    "Stres Kerja terhadap Cyberloafing (PLS-SEM, SmartPLS) (1 Artikel)",
                    
                    
                    "Kontrol Diri terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)",
                    "Stres Kerja terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)",
                    "Cyberloafing terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)",
                    
                    
                    "Kumpulan Artikel di Jurnal dengan Metode Analisis Data PLS-SEM (70 Artikel)")
    
    
    
  }
  
  
  output$radio_button_pilih_topik <- renderUI({
    
    
    
    
    
    
    radioButtons(session$ns("terpilih_topik_paper"), 
                       label="Pilih Topik:", choices = c(nama_topik()), 
                       selected=c("Beban Kerja terhadap Kepuasan Kerja (PLS-SEM, SmartPLS) (5 Artikel)"), inline = FALSE)
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$pilihan_hasil <- renderUI({
    

    
    
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
    
    
      
      if(input$terpilih_topik_paper == "Beban Kerja terhadap Kepuasan Kerja (PLS-SEM, SmartPLS) (5 Artikel)")
      {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel"), 
                         label="Pilih Variabel:", choices = c(nama_variabel()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
        
        
      }
      
      
  
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_bebankerja_terhadap_kepuasankerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Beban Kerja terhadap Kepuasan Kerja (PLS-SEM, SmartPLS) (5 Artikel)")
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
    
    
    
    if(input$terpilih_topik_paper == "Beban Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (6 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_bebankerja_terhadap_turnoverintention"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_bebankerja_terhadap_turnoverintention()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_bebankerja_terhadap_turnoverintention <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Beban Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (6 Artikel)")
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
    
    
    
    if(input$terpilih_topik_paper == "Kepuasan Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (3 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_kepuasankerja_terhadap_turnoverintention"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_kepuasankerja_terhadap_turnoverintention()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_kepuasankerja_terhadap_turnoverintention <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Kepuasan Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (3 Artikel)")
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
    
    
    
    if(input$terpilih_topik_paper == "Motivasi Kerja terhadap Kepuasan Kerja (PLS-SEM, SmartPLS) (6 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_motivasikerja_terhadap_kepuasankerja"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_motivasikerja_terhadap_kepuasankerja()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_motivasikerja_terhadap_kepuasankerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Motivasi Kerja terhadap Kepuasan Kerja (PLS-SEM, SmartPLS) (6 Artikel)")
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
    
    
    
    if(input$terpilih_topik_paper == "Motivasi Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (6 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_motivasikerja_terhadap_turnoverintention"), 
                         label="Pilih Variabel:", choices = c(nama_variabel_motivasikerja_terhadap_turnoverintention()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_motivasikerja_terhadap_turnoverintention <- DT::renderDT({
    
    
    
    if(input$terpilih_topik_paper == "Motivasi Kerja terhadap Turnover Intention (PLS-SEM, SmartPLS) (6 Artikel)")
    {
      

      dat <- read_xlsx("Motivasi Kerja terhadap Turnover Intention.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_variabel_motivasikerja_terhadap_turnoverintention
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM <- function()
  {
    
    dat <- read_xlsx("Kumpulan Artikel Jurnal Nasional dan Internasional Bidang Psikologi dengan Metode PLS-SEM.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Kumpulan Artikel di Jurnal dengan Metode Analisis Data PLS-SEM (70 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM"), 
                         label="Pilih Variabel:", choices = c(nama_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM()), 
                         selected=c("Tahun", "Judul", "Link Artikel", "Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Kumpulan Artikel di Jurnal dengan Metode Analisis Data PLS-SEM (70 Artikel)")
    {
      
      dat <- read_xlsx("Kumpulan Artikel Jurnal Nasional dan Internasional Bidang Psikologi dengan Metode PLS-SEM.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_Kumpulan_Artikel_di_Jurnal_dengan_Metode_Analisis_Data_PLSSEM
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_dukungansosial_terhadap_kesiapankerja <- function()
  {
    
    dat <- read_xlsx("Dukungan Sosial terhadap Kesiapan Kerja.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_dukungansosial_terhadap_kesiapankerja <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Dukungan Sosial terhadap Kesiapan Kerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_dukungansosial_terhadap_kesiapankerja"), 
                         label="Pilih Variabel:", choices = c(nama_dukungansosial_terhadap_kesiapankerja()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_dukungansosial_terhadap_kesiapankerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Dukungan Sosial terhadap Kesiapan Kerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      dat <- read_xlsx("Dukungan Sosial terhadap Kesiapan Kerja.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_dukungansosial_terhadap_kesiapankerja
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_efikasidiri_terhadap_kesiapankerja <- function()
  {
    
    dat <- read_xlsx("Efikasi Diri terhadap Kesiapan Kerja.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_efikasidiri_terhadap_kesiapankerja <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Efikasi Diri terhadap Kesiapan Kerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_efikasidiri_terhadap_kesiapankerja"), 
                         label="Pilih Variabel:", choices = c(nama_efikasidiri_terhadap_kesiapankerja()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_efikasidiri_terhadap_kesiapankerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Efikasi Diri terhadap Kesiapan Kerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      dat <- read_xlsx("Efikasi Diri terhadap Kesiapan Kerja.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_efikasidiri_terhadap_kesiapankerja
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_kontroldiri_terhadap_cyberloafing <- function()
  {
    
    dat <- read_xlsx("Kontrol Diri terhadap Cyberloafing.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_kontroldiri_terhadap_cyberloafing <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Kontrol Diri terhadap Cyberloafing (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_kontroldiri_terhadap_cyberloafing"), 
                         label="Pilih Variabel:", choices = c(nama_kontroldiri_terhadap_cyberloafing()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_kontroldiri_terhadap_cyberloafing <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Kontrol Diri terhadap Cyberloafing (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      dat <- read_xlsx("Kontrol Diri terhadap Cyberloafing.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_kontroldiri_terhadap_cyberloafing
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_streskerja_terhadap_cyberloafing <- function()
  {
    
    dat <- read_xlsx("Stres Kerja terhadap Cyberloafing.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_streskerja_terhadap_cyberloafing <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Stres Kerja terhadap Cyberloafing (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_streskerja_terhadap_cyberloafing"), 
                         label="Pilih Variabel:", choices = c(nama_streskerja_terhadap_cyberloafing()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_streskerja_terhadap_cyberloafing <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Stres Kerja terhadap Cyberloafing (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      dat <- read_xlsx("Stres Kerja terhadap Cyberloafing.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_streskerja_terhadap_cyberloafing
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_kontroldiri_terhadap_kinerja <- function()
  {
    
    dat <- read_xlsx("Kontrol Diri terhadap Kinerja.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_kontroldiri_terhadap_kinerja <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Kontrol Diri terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_kontroldiri_terhadap_kinerja"), 
                         label="Pilih Variabel:", choices = c(nama_kontroldiri_terhadap_kinerja()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_kontroldiri_terhadap_kinerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Kontrol Diri terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      dat <- read_xlsx("Kontrol Diri terhadap Kinerja.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_kontroldiri_terhadap_kinerja
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_streskerja_terhadap_kinerja <- function()
  {
    
    dat <- read_xlsx("Stres Kerja terhadap Kinerja.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_streskerja_terhadap_kinerja <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Stres Kerja terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_streskerja_terhadap_kinerja"), 
                         label="Pilih Variabel:", choices = c(nama_streskerja_terhadap_kinerja()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_streskerja_terhadap_kinerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Stres Kerja terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      dat <- read_xlsx("Stres Kerja terhadap Kinerja.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_streskerja_terhadap_kinerja
      
      dat_baru <- dat[c(terpilih_variabel)]
      
      print(dat_baru)
      
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###############
  
  
  
  nama_cyberloafing_terhadap_kinerja <- function()
  {
    
    dat <- read_xlsx("Cyberloafing terhadap Kinerja.xlsx")
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  
  output$buka_pemilihan_informasi_cyberloafing_terhadap_kinerja <- renderUI({
    
    
    
    if(input$terpilih_topik_paper == "Cyberloafing terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      
      checkboxGroupInput(session$ns("terpilih_cyberloafing_terhadap_kinerja"), 
                         label="Pilih Variabel:", choices = c(nama_cyberloafing_terhadap_kinerja()), 
                         selected=c("Judul Artikel", "Author", "Link Artikel", "Nama Jurnal" ), inline = TRUE)
      
      
    }
    
    
    
  })
  
  
  
  
  
  ##################
  
  
  
  
  
  output$buka_data_cyberloafing_terhadap_kinerja <- DT::renderDT({
    
    
    if(input$terpilih_topik_paper == "Cyberloafing terhadap Kinerja (PLS-SEM, SmartPLS) (1 Artikel)")
    {
      
      dat <- read_xlsx("Cyberloafing terhadap Kinerja.xlsx")
      dat <- as.data.frame(dat)
      
      nama <- colnames(dat)
      
      
      terpilih_variabel <- input$terpilih_cyberloafing_terhadap_kinerja
      
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














