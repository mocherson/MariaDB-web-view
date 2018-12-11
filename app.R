library(shiny)
library(RMariaDB)
library(DBI)
library(rlist)
library(dplyr)

con <- dbConnect(RMariaDB::MariaDB(), 
                 dbname = "pcancer", 
                 username = 'webuser',
                 password = '123')

tbs <- dbListTables(con);
tabset_par <- list(id = 'dataset');
sidebar_par <- tagList(
      div(tableOutput("query_summary"), style = 'max-height: 700px; overflow-y: auto')
    )

for (x in tbs) {
    tabset_par <- list.append(tabset_par, tabPanel(x));
    fd <- dbListFields(con,x);
    cpnl <- tagList();
    col_info <- dbGetQuery(con,paste('show columns from ',x));
    rownames(col_info) <- col_info$Field;
    for (f in fd) {
        if (col_info[[f,'Type']] %in% c('double','float')) {
            tpinput <- textInput(paste('range',x,f,sep='_'),label=NULL,placeholder="input the search range, eg. 0,1");
        } else {
            value <- dbGetQuery(con,sprintf('select distinct(%s) from %s', f,x));
            tpinput <- selectizeInput(paste('range',x,f,sep='_'),label=NULL, choices=c(value[[1]]),
                                      multiple=TRUE)
        }
        
        cpnl <- list.append(cpnl,fluidRow( 
            column(3, align="left", checkboxInput(paste('check',x,f,sep='_'), f, value = TRUE)),
            column(8, tpinput) )
                   )}
    sidebar_par <- list.prepend(sidebar_par, 
                                conditionalPanel(
                                    sprintf('input.dataset == "%s"', x ),  
                                    cpnl
                                ))
}

# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Cancer Database"),    

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel( sidebar_par),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: HTML table with requested number of observations ----
       do.call(tabsetPanel,tabset_par),
       
       dataTableOutput("datashow")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
     
#    datasetquery <- eventReactive(input$query, {
#        dbGetQuery(con,input$sql)
#  })
     
  # Return the sql requested ----
  datasetquery <- reactive({
      fd <- dbListFields(con,input$dataset);
      sql=sprintf("SELECT * FROM %s where 1 ",input$dataset);
      for (f in fd) {
          cond = input[[paste('range',input$dataset,f,sep='_')]];
          if ( is.null(cond) || cond=='' ) {
              sql <- paste(sql, ' and 1');
          } else {
              col_info <- dbGetQuery(con,paste('show columns from ',input$dataset));
              rownames(col_info) <- col_info$Field;
              if (col_info[[f,'Type']] %in% c('double','float')) {
                  rg <- as.numeric(unlist(strsplit(cond,',')))[1:2];
                  if (is.na(rg[1])) { 
                      st = 'and 1';
                  } else if (is.na(rg[2])) {
                      st = sprintf('and %s=%d',f,rg[1]);
                  } else { st = sprintf('and %s>=%d and %s<=%d',f,rg[1],f,rg[2]); }
                  sql <- paste(sql,st);
                  
              } else {
                  sql <- sprintf('%s and %s in ("%s")',sql, f, paste(cond,collapse='","'));
              }              
          }
      }
      
    dbGetQuery(con,sql)
  })  
    
    
  output$query_summary <- renderTable({
	if(input$dataset=='feature') {
    	dataset <- datasetquery()[c('feature','value')];
    	dataset %>% group_by(feature) %>% summarise(max=max(value),min=min(value),mean=mean(value));
	}
  }, options=list(className = 'dt-left', targets = 0:4))           

  # Show the observations ----
  output$datashow <- renderDataTable({
      fd <- names(datasetquery());
      v=vector();
        for (x in paste("check",input$dataset,fd,sep='_') ) {
            v=c(v,input[[x]])
        }
    datasetquery()[,v]
  } )                               

    output$test <- renderPrint({
       
        #input[paste("check",input$dataset,fd,sep='_')]
        #input[c('check_feature_V3','check_feature_V4')]
        
        #mode(input$range_feature_V4)
      
    })

}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
