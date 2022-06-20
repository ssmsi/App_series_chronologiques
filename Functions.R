
DataformatCurrency<-function(df,downloadTitle,downloadFilename){

TableBody<-datatable(df,
                   
                   rownames=FALSE,
                   
                   extensions = c('Buttons'),
                   
                   # caption = informations,
                   
                   callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                   
                   options = list(
                     
                     initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background': '#1a2980', 'color': '#fff'});","}"),
                     
                     columnDefs = list(list(targets='_all', orderable=FALSE)),
                     
                     pageLength = nrow(df),
                     
                     dom = 'Bt',
                     
                     deferRender = TRUE,
                     
                     scrollY = 400,
                     
                     scrollCollapse = TRUE,
                     
                     paging = FALSE,
                     
                     scroller = TRUE,
                     
                     buttons = list(
                       
                       list(extend = 'print',text='Imprimer la table',
                            
                            title=downloadTitle,
                            
                            filename=downloadFilename),
                       
                       
                       list(extend = 'pdf',text='Télécharger au format PDF',
                            
                            title=downloadTitle,
                            
                            filename=downloadFilename)
                       
                       
                     )))
  
  if(length(df)==2){
    
    TableBody %>% formatCurrency(c(2), currency='',mark=" ",digits=0)
    
    
  }else if(length(df)==3){
    
    TableBody %>% formatCurrency(c(2,3), currency='',mark=" ",digits=0)
    
  }
  
}



telechargement_graphique <- list(
  
  list(
    text = "PNG",
    onclick = JS("function () {
                   this.exportChart({ type: 'image/png' });}")
  ),
  list(
    text = "JPEG",
    onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
  ),
  
  list(
    text = "PDF",
    onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf'}); }")
  ),
  
  list(text="CSV",
       onclick=JS("function () {this.downloadCSV();}")), 
  
  list(text="XLS",
       onclick=JS("function () {this.downloadXLS();}")),  # 
  
  
  list(text="Imprimer",
       onclick=JS("function () {this.print();}")),
  
  list(text="Full",
       onclick=JS("function () {this.fullscreenElement();}"))  

)




















