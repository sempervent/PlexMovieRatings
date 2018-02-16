#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages <- c('shiny','DT','ggplot2')
#packages <- c('shiny','DT','DBI','ggplot2','RSQLite')
lapply(packages, require, character.only=TRUE)
# play nice with shinyapps.io
library(shiny)
library(DT)
library(ggplot2)

get_plex_metadata <- function() {
  # plexLibrary = paste0("/var/lib/plexmediaserver/Library/Application",
  #                      "\ Support/Plex\ Media\ Server/Plug-in\ Support",
  #                      "/Databases/com.plexapp.plugins.library.db")) {
  # if (file.exists(plexLibrary) & !file.exists('dat.rds')) {
  #   mydb <- dbConnect(SQLite(),dbname=plexLibrary)
  #   rs=DBI::dbGetQuery(mydb, paste0(
  #     'select ',
  #     'title,rating,tags_genre,year,studio,audience_rating,library_section_id',
  #     ' from metadata_items'))
  #   
  #   DBI::dbDisconnect(mydb)
  #   
  #   rs = rs[which(is.na(rs$library_section_id)),1:5]
  #   rs = rs[which(!is.na(rs$rating)),]
  #   # rs = rs[which(rs$tags_genre!=''),]
  #   rs=data.frame(titles=rs$title,rating=rs$rating,genre=rs$tags_genre,
  #                 year=rs$year,studio=rs$studio,audience_rating=rs$audience_rating,
  #                 stringsAsFactors = FALSE)
  #   saveRDS(rs, 'dat.rds')
  # } else 
  if (file.exists('dat.rds')) {
    rs = readRDS('dat.rds')
  } else {
    rs = NULL
    showNotification(fluidPage('File not Found!'))
    stop(paste0('File\n\t',plexLibrary,'\nNot Found!'))
  }
  
  return(rs)
}

freqfunc <- function(x, n=20){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}



# ui ----------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
    title="Movie Rating System",
   
  # Show a plot of the generated distribution
      fluidRow(
        selectInput('plot',label='Plot to Show',choices=c('Bar','Tile','Facet'))
      ),
      fluidRow(
         plotOutput("ratings_plot"),
         dataTableOutput("rating_table")
   )
)

# server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
   dat <- reactive({
     dat = get_plex_metadata()
     dat
     })
   dd <- reactive({
     dat = dat()
     s = strsplit(dat$genre, split='|', fixed=TRUE)
     ratings=rep(dat$rating, sapply(s, length))
     dd = data.frame(rating = ratings, 
                     genre = unlist(s),
                     stringsAsFactors=FALSE)
    dd$factor = as.character(round(dd$rating,digits=1))
    dd
   })

   dt <- reactive({
     dd = isolate(dd())
     dd = dd[dd$genre %in% names(freqfunc(dd$genre,20)),]
     dd$factor= round(dd$rating, digits=0)

     dt = as.data.frame(table(dd[,2:3]))
     dt
   })
   
   dm <- reactive({
     dt = dt()
     dd = dd()
     dm = merge(dd, dt, by=c('genre','factor'))
     dm$factor = factor(dm$factor, levels=paste(0:10))
     dm
   })
   
   # get top 20 of the genres

   
   
   plot_choice <- reactive({
     input$plot
   })
   
   output$ratings_plot <- renderPlot({
    dd  = dd()
    dt  = dt()
    dm  = dm()
     if (plot_choice()=='Bar') {

       colfunc <- colorRampPalette(c("red4","goldenrod","green3"))

       dd$factor= round(dd$rating, digits=0)
       ggplot(dd, aes(x=genre, group=factor, fill=as.factor(factor))) +
         geom_hline(aes(yintercept=500), color='goldenrod') +
         geom_hline(aes(yintercept=1000), color='red4') +
         geom_bar() + coord_flip() +
         scale_y_continuous(expand=c(0,0), name='', breaks=NULL) +
         scale_x_discrete(name="") +
         scale_fill_manual(name='Rating',
                           values=colfunc(11)) +
         theme(axis.ticks=element_blank(),
               panel.background=element_blank(),
               axis.text = element_text(color='black', size=15))
       
     } else if (plot_choice()=='Tile') {

       ggplot(dm, aes(x=genre, y=factor,fill=Freq)) +
         geom_raster(aes(fill=Freq),hjust=0,vjust=0) + coord_flip() +
         scale_y_discrete(expand=c(0,0), name='', breaks=1:10, labels=1:10) +
         # scale_x_discrete(name='') +
         theme(axis.ticks=element_blank(),
               panel.background=element_blank(),
               axis.text = element_text(color='black', size=15),
               legend.position='none')
       
     } else if (plot_choice()=='Facet') {
      ggplot(dm, aes(x=factor)) +
         facet_wrap(~ genre) +
         geom_histogram(stat='count') +
         theme(panel.background = element_blank(),
               strip.background = element_blank(),
               strip.text = element_text(size=16, color='black'),
               axis.ticks = element_blank(),
               axis.text.y = element_blank())
     }
     
   })


   output$rating_table <- renderDT(dat())
}

# Run the application 
shinyApp(ui = ui, server = server)

