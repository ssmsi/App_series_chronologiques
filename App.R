library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(readxl) 
library(tidyverse)
library(DT)
library(highcharter)
library(lubridate, warn.conflicts = FALSE) 
library(shinyjs)
library(shinycssloaders)
library(openxlsx)
library(writexl)
###########################################################################################################################################
################################# Chargement des données ##################################################################################
###########################################################################################################################################
source("Functions.R", encoding = 'UTF-8')
base<-readRDS("base.rds")
DataGouv<-readRDS("DataGouv.rds")
annuelle<-readRDS("annuelle.rds")
mensuelle<-readRDS("mensuelle.rds")
trimestrielle<-readRDS("trimestrielle.rds")
ListeIndicateurs<-sort(unique(subset(base$Indicateur,base$Indicateur!="NonRenseigne")))

###########################################################################################################################################
################################# Interface Utilisateur ##################################################################################
###########################################################################################################################################
ui<-fluidPage(
  tags$head(
    tags$script(HTML("
function ScreenGraphique(identifiant) {
  if (identifiant.requestFullscreen) {
    identifiant.requestFullscreen();
  } else if (identifiant.mozRequestFullScreen) { 
    identifiant.mozRequestFullScreen();
  } else if (identifiant.webkitRequestFullscreen) { 
    identifiant.webkitRequestFullscreen();
  } else if (identifiant.msRequestFullscreen) { 
    identifiant.msRequestFullscreen();
  }
} "))),
  
######################################lescture des fichiers css #####################################  
tags$head(includeScript("google-analytics.js")),
tags$head(includeHTML("google-analytics.html")),
useShinyjs(), 
includeCSS("css/print.css"),
includeCSS("css/footer.css"),
includeCSS("css/App.css"),
includeCSS("css/nav.css"),
###########################################################################################################################################
################################################################ HEADER ###################################################################
###########################################################################################################################################
div(class="nav",
      div(class="items",
          div(class="img",
              tags$ul(
                tags$li(tags$a(href="https://www.interieur.gouv.fr/",target="_blank",
                               tags$img(src = 'logoministere.png',style="height:80px;"))),
                tags$li(tags$a(href="https://www.interieur.gouv.fr/Interstats/Actualites",target="_blank",
                               tags$img(src = 'logo_SSMSI.jpg',style="height:80px;") 
                ))
              )
          )
      ),
      div(class="items",
          div(class="titre",
              p("SÉRIES CHRONOLOGIQUES SUR LA DÉLINQUANCE ET L'INSÉCURITÉ")
          ))),
  
div(class="row",style="margin-left:2%;margin-right:2%;margin-top:2%;",   
      div(class="col-md-4",
          div(class="col-md-12",id="sidebarpanel",
              textInputIcon(
                inputId = "Inputsearch",
                label = "",
                placeholder = "Trouver un indicateur...",
                icon = icon("search", class ="far fa-search", lib = "font-awesome")
              ),
              h4("Indicateurs ","(",textOutput("nb_element_indic",inline = TRUE),")",style="margin-bottom:25px;margin-top:25px;"),
              radioButtons(inputId = "InputIndicateurs",label=NULL,
                           choices=ListeIndicateurs,
                           selected=ListeIndicateurs[1]), 
              uiOutput("BoutonsSources"),
              uiOutput("BoutonsSousIndic"),
              uiOutput("BoutonsStatistiques"),
              uiOutput("BoutonsZonesGeo"),
              uiOutput("BoutonsPeriodicite"),
              uiOutput("BoutonsRequalification"),
              uiOutput("BoutonsUniteCompte"),
              br(),
              br(),
              h4("Télécharger la/les série(s) sélectionnée(s):"),
              downloadButton(".xlsx",outputId='telechargementDoubleExcel',class = "DownloadData",style="margin-bottom:8px;"),
              br(),
              downloadButton(".csv",outputId='telechargementDoubleCSV',class = "DownloadData"),
              br(),
              br(),
              h4("Télécharger toutes les séries :"),
              downloadButton(".csv",outputId='TelechargementCSV',class="DownloadSeries",style="margin-bottom:8px;"),
              br(),
              downloadButton(".xlsx",outputId='TelechargementXLSX',class="DownloadSeries"))
          
          
      ),
    

    div(class="col-md-8",id="container",
          div(class="col-md-12",
              uiOutput("SelectInputAnnee"),
              withSpinner(tagList(
                actionButton("",inputId="bouton_screen",icon =icon('external-link-alt'),onclick = "ScreenGraphique(document.getElementById('graphique'));"),
                highchartOutput('graphique',height="600px")
              ),type=5,hide.ui=TRUE),
              style="border:1px solid #C0C0C0;background-color:#FFFFFF;margin-bottom:3%"),
          br(),
          br(),
          DT::dataTableOutput('table'),
          br(),
          div(class="col-md-12",htmlOutput("descriptif"))
      )
  ),
  
###########################################################################################################################################
################################################################ FOOTER ###################################################################
###########################################################################################################################################
includeHTML("css/SSMSIFooter.html"))
server<-function(input,output,session){
  observeEvent(input$Inputsearch,{
    if(input$Inputsearch==""){
      updateRadioButtons(session,"InputIndicateurs",
                         label=NULL,
                         choices=sort(unique(subset(base$Indicateur,base$Indicateur!="NonRenseigne"))),
                         selected=sort(unique(subset(base$Indicateur,base$Indicateur!="NonRenseigne")))[1])
    } else if (!is.null(input$Inputsearch)) {
      data <- filter(base, grepl(input$Inputsearch, Indicateur,ignore.case = TRUE) & Indicateur!="NonRenseigne") 
      updateRadioButtons(session,"InputIndicateurs","",choices=unique(data$Indicateur),selected=unique(data$Indicateur)[1]) 
    }
  })
  
########################################################################
########################### Boutons source ############################
ReactiveSource<-reactive({
  
    liste_source<-sort(unique((base %>%  dplyr::filter(base$Indicateur %in% req(input$InputIndicateurs) ))$Source))
    
    }) 
  
# créer le bouton "source" et s'assurer que celui-ci propose plusieurs choix à l'utilisateur sinon ne pas l'afficher 
# et si le bouton n'est pas affiché, effectuer une correction de telle sorte que le filtre sur la base de données permettant de créer la série ne
# porte pas sur l'input du bouton, faire ceci pour les boutons 
output$BoutonsSources<-renderUI({
    req(ReactiveSource())
    if (length(ReactiveSource())==1)
      return(NULL)
    radioButtons(inputId = "InputSources",label=h4("Source(s) ","(",textOutput("NbElementSource",inline = TRUE),")"),choices=ReactiveSource(),selected=input$InputSources) 
    
  })


  CorrectionSource<-reactive({
    if(length(ReactiveSource())==1){
      selected<-ReactiveSource()
    } else {
      selected<-input$InputSources
    }
    
  })
  
########################### Boutons Sous-Indicateurs ###########################################################
ReactiveSousIndic<-reactive({
    liste_sous_indicateurs=sort(unique((base %>% dplyr::filter(base$Indicateur %in% input$InputIndicateurs &
                                                                 base$Source %in% CorrectionSource()))$Sous_indicateur)) 
  })
  
output$BoutonsSousIndic<-renderUI({
    req(ReactiveSousIndic())
    if(length(ReactiveSousIndic())==1)
      return(NULL)
    radioButtons("InputSousIndic",label=h4("Sous-Indicateurs ","(",textOutput("NbElementSousIndic",inline = TRUE),")"),choices=ReactiveSousIndic(),
                 selected=ifelse(("Ensemble" %in% ReactiveSousIndic()),"Ensemble",ReactiveSousIndic()[1])
    )
  })
  
CorrectionSousIndic<-reactive({
    if("NonRenseigne" %in% ReactiveSousIndic()&length(ReactiveSousIndic())==1) {
      element_selected<-c("NonRenseigne")
    } else if (!("NonRenseigne" %in% ReactiveSousIndic())&length(ReactiveSousIndic())==1) {
      element_selected<-ReactiveSousIndic()
    } else {
      element_selected<-input$InputSousIndic
    }
  })
  
  
########################### Boutons Statistique ##############################################################################
ReactiveStatistique<-reactive({
    liste_statistique=sort(unique((base %>% filter(base$Indicateur %in% input$InputIndicateurs &
                                                     base$Source %in% CorrectionSource() &
                                                     base$Sous_indicateur %in% CorrectionSousIndic() ))$Statistique))})
output$BoutonsStatistiques<-renderUI({
    req(ReactiveStatistique())
    if(length(ReactiveStatistique())==1)
      return(NULL)
    radioButtons("InputStatistique",label=h4("Statistique","(",textOutput("NbElementStatistique",inline = TRUE),")"),choices=ReactiveStatistique(),selected=ReactiveStatistique()[1])
  })
  
CorrectionStatistique<-reactive({
    if(length(ReactiveStatistique())==1){
      selected<-ReactiveStatistique()
    } else {
      selected<-input$InputStatistique
    }
  })
  
########################### Boutons Z.G ###########################################################################
ReactiveZoneGeo<-reactive({
    liste_zone_geographique<-sort(unique((base %>% filter(base$Indicateur %in% input$InputIndicateurs &
                                                            base$Source %in% CorrectionSource() &
                                                            base$Sous_indicateur %in% CorrectionSousIndic() &
                                                            base$Statistique %in% CorrectionStatistique()))$Zone_geographique))})
output$BoutonsZonesGeo<-renderUI({
    req(ReactiveZoneGeo())
    if(length(ReactiveZoneGeo())==1)
      return(NULL)
    radioButtons("InputZoneGeo",label=h4("Zone Géographique","(",textOutput("NbElementZoneGeo",inline = TRUE),")"),choices=ReactiveZoneGeo(),selected=input$InputZoneGeo) # input$InputZoneGeo
  })
  
CorrectionZoneGeo<-reactive({
    if(length(ReactiveZoneGeo())==1){
      selected<-ReactiveZoneGeo()
    } else {
      selected<-input$InputZoneGeo
    }
  })
  
########################### Boutons Périodicité #####################################################################################################
ReactivePeriodicite<-reactive({
    liste_periodicite<-sort(unique((base %>% filter(base$Indicateur %in% input$InputIndicateurs &
                                                      base$Source %in% CorrectionSource() &
                                                      base$Sous_indicateur %in% CorrectionSousIndic() &
                                                      base$Statistique %in% CorrectionStatistique() &
                                                      base$Zone_geographique %in% CorrectionZoneGeo()))$Periodicite))})
output$BoutonsPeriodicite<-renderUI({
    req(ReactivePeriodicite())
    if(length(ReactivePeriodicite())==1)
      return(NULL)
    radioButtons("InputPeriodicite",label=h4("Périodicité","(",textOutput("NbElementPeriodicite",inline = TRUE),")"),choices=ReactivePeriodicite(),selected=input$InputPeriodicite) # input$InputPeriodicite
  })
CorrectionPeriodicite<-reactive({
    if(length(ReactivePeriodicite())==1){
      selected<-ReactivePeriodicite()
    } else {
      selected<-input$InputPeriodicite
    }
  })
  
########################### Boutons Requalification ###########################################################
ReactiveRequalification<-reactive({
    liste_periodicite<-sort(unique((base %>% filter(base$Indicateur %in% input$InputIndicateurs &
                                                      base$Source %in% CorrectionSource() &
                                                      base$Sous_indicateur %in% CorrectionSousIndic() &
                                                      base$Statistique %in% CorrectionStatistique() &
                                                      base$Zone_geographique %in% CorrectionZoneGeo() & 
                                                      base$Periodicite %in% CorrectionPeriodicite() ))$Donnees_requalifiees))})
output$BoutonsRequalification<-renderUI({
    req(ReactiveRequalification())
    if(length(ReactiveRequalification())==1)
      return(NULL)
    radioButtons("InputRequalification",label=h4("Requalification:"),choices=ReactiveRequalification(),selected=ReactiveRequalification()[1])
  })
  
CorrectionRequalification<-reactive({
    if("NonRenseigne" %in% ReactiveRequalification() & length(ReactiveRequalification())==1) {
      element_selected<-c("NonRenseigne")
    } else if (!("NonRenseigne" %in% ReactiveRequalification() ) & length(ReactiveRequalification())==1) {
      element_selected<-ReactiveRequalification()
    } else {
      element_selected<-input$InputRequalification
    }
    
  })
  
########################### Boutons Unite de compte #########################################################################################
ReactiveUniteCompte<-reactive({
    liste_unite_de_compte=sort(unique((base %>% filter(base$Indicateur %in% input$InputIndicateurs &
                                                         base$Source %in% CorrectionSource() &
                                                         base$Sous_indicateur %in% CorrectionSousIndic() &
                                                         base$Statistique %in% CorrectionStatistique() &
                                                         base$Zone_geographique %in% CorrectionZoneGeo() &
                                                         base$Periodicite %in% CorrectionPeriodicite() &
                                                         base$Donnees_requalifiees %in% CorrectionRequalification() ))$Unite_de_compte))})
output$BoutonsUniteCompte<-renderUI({
    req(ReactiveUniteCompte())
    if(length(ReactiveUniteCompte())==1)
      return(NULL)
    radioButtons("InputUniteCompte",label=h4("Unités de compte :","(",textOutput("NbElementUnitedeCompte",inline = TRUE),")"),choices= ReactiveUniteCompte(),
                 selected=input$InputUniteCompte)})
  
CorrectionUniteCompte<-reactive({
    if(length(ReactiveUniteCompte())==1){
      selected<-ReactiveUniteCompte()
    } else {
      selected<-input$InputUniteCompte
    }
  })
#################################################################################################
DataInformations<-reactive({
    serie_chronologique<-base %>% filter (
      base$Indicateur %in% input$InputIndicateurs &
        base$Source %in% CorrectionSource() &
        base$Sous_indicateur %in% CorrectionSousIndic() &
        base$Statistique %in% CorrectionStatistique() &
        base$Zone_geographique %in% CorrectionZoneGeo() &
        base$Periodicite %in% CorrectionPeriodicite() &
        base$Donnees_requalifiees %in% CorrectionRequalification() &
        base$Unite_de_compte %in% CorrectionUniteCompte() )
    DataInformations<-data.frame(serie_chronologique)})
  
DataTemporalite<-reactive({
    DataInformations<-DataInformations()
    if ("Annuelle" %in% DataInformations$Periodicite){
      DTA<-t(DataInformations) 
      data<-cbind(annuelle,DTA)
    } else if ("Mensuelle" %in% DataInformations$Periodicite ) {
      DTM<-t(DataInformations) 
      data<-cbind(mensuelle,DTM)
    } else if ("Trimestrielle" %in% DataInformations$Periodicite) {
      DTT<-t(DataInformations) 
      data<-cbind(trimestrielle,DTT)
    }
    
    if (length(data)==2){
      colnames(data)<-c(paste0("Série"," ","(",DataInformations()$Periodicite,')'),unique(DataInformations()$Titre))
      data<-data[rowSums(data=="NonRenseigne")==0, ,drop = FALSE]
      data<-data %>% drop_na()
    } else if (length(data)==3) {
      colnames(data)<-c(paste0("Série"," ","(",unique(DataInformations()$Periodicite),')'),paste0(unique(DataInformations()$Titre)," ","(",unique(DataInformations()$Correction)[1],")"),paste0(unique(DataInformations()$Titre)," ","(",unique(DataInformations()$Correction)[2],")"))
      data<-data[rowSums(data=="NonRenseigne")==0, ,drop = FALSE]
      data<-data %>% drop_na()
    }
  })
 
####################################################### Graphique ####################################################################################################
ReactivePlot<-reactive({
    data_graphique <- DataTemporalite()
    hc<-highchart() %>%
      hc_chart(
        backgroundColor = "#FFFFFF",
        marginBottom = 120
      ) %>%
      hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
                   buttons=list(
                     contextButton=list(
                       text= "Télécharger",
                       menuItems=telechargement_graphique,
                       symbol='',y=10))) %>%
      hc_title(text=unique(DataInformations()$Titre),
               margin = 20, align = "center",
               style = list(color = "#000000", fontSize='15px',fontWeight = "normal",useHTML = TRUE)) %>%
      hc_yAxis(title = list(text=unique(DataInformations()$Ordonnees)),
               style = list(color = "#000000", fontSize='15px',fontWeight = "bold",useHTML = TRUE),
               gridLineWidth=0.2,gridLineColor='black',
               labels = list(format = "{value:,.0f}")
      ) %>%
      hc_plotOptions(series = list(
        animation=FALSE,
        showInLegend = FALSE,
        dataLabels = list(enabled =FALSE,style=list(color="#000000")),marker=list(enabled=FALSE,lineWidth=1,fillColor='#1a2980',lineColor='#1a2980'))) %>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} "),
                 headerFormat = '<span style="font-size: 13px"> Date : {point.key}</span>'
      ) %>%
      hc_subtitle(
        text = str_c("Champ : ",unique(DataInformations()$Zone_geographique),"<br/> Source :",unique(DataInformations()$Source),sep = " "),
        style = list(fontWeight = "bold"),
        align = "left",verticalAlign = 'bottom'
      ) %>%
      hc_caption(
        text = str_c(paste0(tags$b("Série :"),unique(DataInformations()$Periodicite)),"<br/>",
                     ifelse(unique(DataInformations()$Donnees_requalifiees)=="NonRenseigne","",paste0("Données requalifiées : ",unique(DataInformations()$Donnees_requalifiees))),sep = " "),
        style = list(fontWeight = "bold"),
        align = "right",verticalAlign = 'bottom'
      ) %>%
      hc_legend(layout = 'vertical', align = 'center', verticalAlign = 'top', floating = T, x = 60, y =40)
    ###################################################################################### Chart for plotLine##################################
    chartLine <- hc %>% hc_add_series(
      type = "line", 
      color="#1a2980",
      name=unique(DataInformations()$Titre),
      data=as.numeric(data_graphique[,2]),
      dataLabels = list(
        enabled = TRUE,
        # si return this.y; pour avoir les données sur la série sinon return null;
        # Retourner un point + valeur après la rupture de la série === Math.abs(this.y) + '<span>●</span>'   <span>●</span>
        # Retourner uniquement un point après la rupture de la série === '<span>●</span>' 
        formatter = JS(
          "function() {
            if(this.x=='2020'| this.x=='2021'){   
             return '<span>\u25CF</span>' ;
             } else {
            return null;}}")))      
    ###################################################################################### Chart for plotBands###########################################################
    chartplotBands<- hc %>%
      hc_add_series(
        type = "line", 
        name=unique(DataInformations()$Titre),
        data=as.numeric(data_graphique[,2]),
        color="#1a2980",            
        marker=list(enabled=FALSE),
        zoneAxis="x",
        zones=list(
          list(value=0,dashStyle='Solid'),
          list(value=7,dashStyle='Solid'),
          list(value =10,dashStyle='Dot'))
      ) %>% 
      hc_xAxis(categories=data_graphique[,1],
               gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
               plotBands = list(
                 list(
                   label = list(text = ""),
                   color = "#d6dbdf", #d6dbdf
                   from =7,
                   to = 10
                 ))) 
    ##################################################################################################################################################
    if(!("Mensuelle" %in% DataInformations()$Periodicite) & length(data_graphique)==2 & ("0000" %in% DataInformations()$Identifiant) ){ 
      hc %>%
        hc_add_series(name=unique(DataInformations()$Titre),
                      type = "line",
                      color = '#1a2980',
                      showInLegend = FALSE,
                      data = as.numeric(data_graphique[,2])  ) %>% 
        hc_xAxis(categories=data_graphique[,1],gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on') 
    } else if (!("Mensuelle" %in% DataInformations()$Periodicite) & length(data_graphique)==3) {
      hc %>%
        hc_add_series(name=unique(DataInformations()$Correction)[1],
                      type = "line",
                      color = '#1a2980',
                      showInLegend = TRUE,
                      data = as.numeric(data_graphique[,2]) ) %>%
        hc_add_series(name=unique(DataInformations()$Correction)[2],
                      type = "line",
                      color = 'red',
                      showInLegend = TRUE,
                      data = as.numeric(data_graphique[,3]) ) %>%
        hc_xAxis(categories=data_graphique[,1],gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on') 
    } else if ("Mensuelle" %in% DataInformations()$Periodicite & length(data_graphique)==3) {
      hc %>%
        hc_add_series(name=unique(DataInformations()$Correction)[1],
                      type = "line",
                      color = '#1a2980',
                      showInLegend = TRUE,
                      data = as.numeric(SelectinputReactive()$brute)) %>%
        hc_add_series(name=unique(DataInformations()$Correction)[2],
                      type = "line",
                      color = 'red',
                      showInLegend = TRUE,
                      data = as.numeric(SelectinputReactive()$cvs)) %>%
        hc_xAxis(categories=SelectinputReactive()$serie,gridLineWidth=0.1,gridLineColor='black',tickmarkPlacement='on') %>%
        hc_plotOptions(line = list(
          animation=FALSE,
          dataLabels = list(enabled =FALSE,style=list(color="#000000")),marker=list(enabled=FALSE,lineWidth=4))) 
    } else if ("Mensuelle" %in% DataInformations()$Periodicite & length(data_graphique)==2) {
      hc %>%
        hc_add_series(name=unique(DataInformations()$Titre),
                      type = "line",
                      color = '#1a2980',
                      showInLegend = FALSE,
                      data = as.numeric(SelectinputReactive()$brute)) %>%
        hc_xAxis(categories=SelectinputReactive()$serie,gridLineWidth=0.1,gridLineColor='black',tickmarkPlacement='on') %>%
        hc_plotOptions(line = list(
          animation=FALSE,
          dataLabels = list(enabled =FALSE,style=list(color="#000000")),marker=list(enabled=FALSE,lineWidth=4))) 
    } else if("0001" %in% DataInformations()$Identifiant) { 
      chartLine %>%
        hc_xAxis(
          categories=data_graphique[,1],
          gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
          plotLines = list(
            list(
              label = list(text = ""),
              color = "red",
              dashStyle='Dot',
              value=13,
              
              width=2))) 
      
    } else if("0002" %in% DataInformations()$Identifiant) { 
      chartLine %>%
        hc_xAxis(
          categories=data_graphique[,1],
          gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
          plotLines = list(
            list(
              label = list(text = ""),
              color = "red",
              dashStyle='Dot',
              value=11,
              width=2)))  
    } else if("0003" %in% DataInformations()$Identifiant) { 
      chartLine %>%
        hc_xAxis(
          categories=data_graphique[,1],
          gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
          plotLines = list(
            list(
              label = list(text = ""),
              color = "red",
              dashStyle='Dot',
              value=9,
              width=2))) 
    } else if("0004" %in% DataInformations()$Identifiant) { 
      chartLine %>%
        hc_xAxis(
          categories=data_graphique[,1],
          gridLineWidth=0.2,gridLineColor='black',tickmarkPlacement='on',tickInterval=1,
          plotLines = list(
            list(
              label = list(text = ""),
              color = "red",
              dashStyle='Dot',
              value=13,
              width=2)))  
      
    } else if("0005" %in% DataInformations()$Identifiant) {
      chartplotBands %>%
        hc_annotations(
          list(
            labelOptions=list(
              backgroundColor='rgba(255,255,255,0.5)'),
            labels =  
              list(
                list(
                  point = list(x=8,y=200000,xAxis=0,yAxis=0), 
                  text = "Réformulation <br> des questions relatives aux <br> violences sexuelles"
                ),
                
                list(
                  point = list(x=9,y=276000,xAxis =0, yAxis =0),
                  text = "Première enquête <br> post-affaire <br> weinstein"
                ))))
      
    } else if("0006" %in% DataInformations()$Identifiant) {
      chartplotBands %>%
        hc_annotations(
          list(
            labelOptions=list(
              backgroundColor='rgba(255,255,255,0.5)'),
            labels =  
              list(
                list(
                  point = list(x=8,y=285000,xAxis=0,yAxis=0),
                  text = "Réformulation <br> des questions relatives aux <br> violences sexuelles"
                ),
                
                list(
                  point = list(x=9,y=408000,xAxis=0,yAxis =0),
                  text = "Première enquête <br> post-affaire <br> weinstein"
                ))))}
    
  })
  
output$graphique<-renderHighchart({ 

    ReactivePlot()  
    
}) #%>% bindCache(DataTemporalite())


output$table <-DT::renderDataTable({
  if(is.null(ReactivePlot()))
    return(NULL)
  DataTemporalite<-DataTemporalite() 
  if(length(DataTemporalite)==2){
    DataTemporalite[,2]<-as.numeric(DataTemporalite[,2])
  } else if (length(DataTemporalite)==3){
    DataTemporalite[,2]<-as.numeric(DataTemporalite[,2])
    DataTemporalite[,3]<-as.numeric(DataTemporalite[,3])
  }
  downloadTitle<-unique(DataInformations()$Titre)
  downloadFilename<-unique(DataInformations()$Titre)
  DataformatCurrency(DataTemporalite,downloadTitle,downloadFilename)
})
  
output$descriptif<-renderUI({
  if(is.null(ReactivePlot()))
    return(NULL)
fluidRow(class="desc",HTML(paste0("<p> <h4> Pour en savoir plus : </h4> </p> ",as.character(unique(DataInformations()$description)))))})  
################################################################### TEXTE #################################################################
output$nb_element_indic<-renderText({
    if(input$Inputsearch==""){
      length(sort(unique(subset(base$Indicateur,base$Indicateur!="NonRenseigne"))))
    } else if (!is.null(input$Inputsearch)) {
      length(unique(filter(base, grepl(input$Inputsearch, Indicateur,ignore.case = TRUE) & Indicateur!="NonRenseigne")$Indicateur))
    }})
  
output$NbElementSource<-renderText({length(ReactiveSource())})
output$NbElementSousIndic<-renderText({length(ReactiveSousIndic() )})
output$NbElementStatistique<-renderText({length(ReactiveStatistique())})
output$NbElementPeriodicite<-renderText({length(ReactivePeriodicite())})
output$NbElementZoneGeo<-renderText({length(ReactiveZoneGeo())})
output$NbElementUnitedeCompte<-renderText({length(ReactiveUniteCompte())})
################################################################################ selectInput ############################################
selectInputData<-reactive({
    data<-DataTemporalite()
    if("Mensuelle" %in% DataInformations()$Periodicite & length(data)==3){
      colnames(data)<-c("serie","brute","cvs")
      data<-data %>% separate(serie, into = c("Annees", "Mois"), sep = "M",remove=FALSE)
    }else if("Mensuelle" %in% DataInformations()$Periodicite & length(data)==2){
      colnames(data)<-c("serie","brute")
      data<-data %>% separate(serie, into = c("Annees", "Mois"), sep = "M",remove=FALSE)
    }
    
  })
  
output$SelectInputAnnee<-renderUI({
    req(selectInputData())
    DataSplit<-selectInputData()
    tagList(
      fluidRow(
        column(4,selectInput('DebutAnnee',"Début :",choices=c(unique(DataSplit$Annees)),selected=input$DebutAnnee),style="display:inline-block;"),
        column(4,selectInput('FinAnnee',"Fin :",choices=sort(c(unique(DataSplit$Annees)),decreasing=TRUE),selected=input$FinAnnee),style="display:inline-block;")
        
      )
      
    )})
  
SelectinputReactive<-reactive({
    req(selectInputData())
    DataAnneeUnique<-subset(selectInputData(),(selectInputData()$Annees>=input$DebutAnnee & selectInputData()$Annees<=input$FinAnnee))
})
  
##########################################################################################################################################  
################################################Telecharegemnt############################################################################
########################################################################################################################################## 
output$telechargementDoubleCSV <- downloadHandler(
    filename = function(){paste0(DataInformations()$Indicateur,'.csv')},
    content = function(file) {
      data<-DataTemporalite()
      write.table(data,
                  col.names=T,
                  row.names=FALSE,
                  sep=";",file)}
    
  )  
  
output$telechargementDoubleExcel<- downloadHandler(
    filename = function() {
      paste0(DataInformations()$Indicateur, ".xlsx")
    },
    content = function(file){
      feuille<-DataTemporalite()
      feuille2<-DataInformations() %>% select(Indicateur,Source,Sous_indicateur,Statistique,
                                               Zone_geographique,Periodicite,
                                               Donnees_requalifiees,Unite_de_compte) %>%
        rename("Indicateurs"=Indicateur,
               "Sous-Indicateur" = Sous_indicateur ,
               "Zone Géographique" = Zone_geographique,
               "Périodicité" = Periodicite,
               "Requalification" = Donnees_requalifiees,
               "Unité de compte" = Unite_de_compte)
      sheets <- mget(ls(pattern = "feuille")) 
      names(sheets) <- c("Série","Informations") 
      writexl::write_xlsx(sheets, path = file) 
    }
    
  ) 
  
#################################################################################################################################### 
output$TelechargementCSV <- downloadHandler(
    filename = function(){paste0("ToutesLesSeries",'.csv')},
    content = function(file) {
      write.csv2(DataGouv,
                 col.names=T,
                 row.names=FALSE,
                 sep=";",file)}
    
  )
  
################################################################################################################################### 
output$TelechargementXLSX <- downloadHandler(
    filename = function(){paste0("ToutesLesSeries",'.xlsx')},
    content = function(file) {
      write.xlsx(DataGouv,
                 file,
                 col.names=T,
                 row.names=FALSE)}
    
  )
  
}

shinyApp(ui=ui,server=server)

