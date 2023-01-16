#to deploy on shinnyapps.io:
#library(rsconnect)
#rsconnect::deployApp("~\\a_ABSys\\divers\\Incendie\\plans_pdf\\surface_brulee") or file>publish... on Rstudio

library(dplyr) #pour gestion des donnees
library(shiny) #pour interactivite
library(leaflet) #pour carte interactive
library(sf) #pour donnees spatiales
library(raster) #pour carto spatiale
#library(terra) #remplace raster
library(DT) #pour affichage tableau dans shiny
library(ggplot2) #pour graphes sur pdf
library(gridExtra) #pour mise en page des graphes
library(ggspatial) #pour affichage des donnees spatiales dans ggplot
#library(gmailr) #pour envoyer l email d alerte

load("data_surface_brulee.Rdata")

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Export de carte de zone brulee (patientez une dizaine de secondes au debut)"),
  
  
  verticalLayout(
    # Top panel with selected zones
    wellPanel(
      p("1. selectionnez une section en cliquant sur un point rouge de la carte"),
      textOutput("section"),
      textInput("parcelles", label="2. entrez les numeros de parcelles (separes par , ou ;)"),
      actionButton("calculsurfaces", label="Calculer les surfaces brulees"),
      p("3. telechargez le pdf en cliquant sur le bouton en bas de page")
    ),
    
    # the map itself
    mainPanel(fluidRow(
      column(width=6, leafletOutput("map")),
      column(width=6, DT::dataTableOutput("table"))
      
    ),
    wellPanel(
      p("Je suis en train d essayer de construire un projet participatif pour la reconstruction du territoire apres l'incendie"),
      p("Pour plus d'informations, merci d'entrer votre email ci-dessous"),
      p("vous recevrez un mail de confirmation de votre inscription a ma liste de diffusion"),
      p("Vous pouvez egalement me contacter a marie.gosme@inrae.fr"),
      textInput("email", label="envoie d email ne marche pas encore, donc pas la peine d entrer votre adresse email"),
      #actionButton("chargepdf", label="Telecharger pdf"))
      downloadButton("chargepdf",label="Telecharger pdf"))
    )
  )
)

# Define server logic       
server <- function(input, output, session) {
  
  react<-reactiveValues(section="", 
                        parcelles="", 
                        df_brule=data.frame(section="", parcelle="", superficie="", surfacebrulee=""))
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      #addProviderTiles("Stamen.Toner") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Photo") %>% 
      #addGeotiff(file="sentinel256couleurs.tif", group = "SWIR") %>% 
      addRasterImage(raster, group = "SWIR",
                     project=FALSE,
                     colors=colorNumeric(raster@legend@colortable,0:255,na.color = "transparent")) %>% 
      # addPolygons(data = cadastre, 
      #             fillColor = "aliceblue", 
      #             color = "grey",
      #             layerId = ~geo_parcel) %>%  # unique id for polygons
      addPolygons(data = sections, group="Sections",
                  color = rainbow(10)[as.numeric(as.factor(sections$geo_sectio))],
                  layerId = ~geo_sectio) %>%  # unique id for polygons
      addCircleMarkers(data = points, 
                       fillColor = "red", 
                       color = NA,
                       radius = 10,
                       fillOpacity = .75,
                       layerId = ~geo_sectio) %>%   # unique id for points
    # addLabelOnlyMarkers(data = etiqparcelles, group= "Num parcelles",
    #                   label = ~tex,
    #                   labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>% 
    #addLabelOnlyMarkers(data = points,
    #                     label = ~geo_sectio,
    #                     labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE))
    # Layers control
    addLayersControl(
      baseGroups = c("Photo", "SWIR"),
      overlayGroups = c("Sections", "Num parcelles", "Bords parcelles"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup("Num parcelles")
  })
  
  output$table = DT::renderDataTable(react$df_brule)
  
  # click on polygon
  # observe({ 
  #   
  #   event <- input$map_shape_click
  #   
  #   message <- paste("parcelle selectionnee:", cadastre$tex[cadastre$geo_parcel == event$id])
  #   message <- paste("section selectionnee:", cadastre$tex[cadastre$geo_sectio == event$id])
  #   
  #   output$section <- renderText(message)
  #   
  #   
  #   
  #   
  # })
  
  # click on a marker
  observeEvent(input$map_marker_click, ignoreInit =TRUE, handlerExpr={ 
    print("click point rouge")
    event <- input$map_marker_click
    
    message <- paste("section selectionnee", points$geo_sectio[points$geo_sectio == event$id]) 
    
    output$section <- renderText(message)
    react$section<-event$id
    
    #get extent of the section
    bounding<-extent(sections[sections$geo_sectio == event$id,])
    
    #get labels of parcels in the section
    etiq<-etiqparcelles[etiqparcelles$geo_sectio== event$id,]
    bords<-cadastre[cadastre$geo_sectio==event$id,]
    leafletProxy("map") %>%
      clearGroup(group="Num parcelles") %>% 
      addLabelOnlyMarkers(data = etiq, group= "Num parcelles",
                          label = ~tex,
                          labelOptions = labelOptions(noHide = TRUE, 
                                                      direction = 'top', 
                                                      textOnly = TRUE,
                                                      style = list("color" = "lightblue")))    %>% 
      clearGroup(group="Bords parcelles") %>% 
      addPolygons(data = bords, group="Bords parcelles",
                  color = "lightblue",
                  layerId = ~geo_parcel) %>%  # unique id for polygons
      
      #input$map_zoom
    flyToBounds( lng1 = unname(bounding@xmin)
                    , lat1 = unname(bounding@ymin)
                    , lng2 = unname(bounding@xmax)
                    , lat2 = unname(bounding@ymax) )# %>% 
     #showGroup("Num parcelles")

  })
  
  #click on button calculate
  observeEvent(input$calculsurfaces, ignoreInit =TRUE, handlerExpr={
    print("click calculesurfaces")
    parcelles<-unlist(strsplit(x=gsub(pattern=",", replacement=";", gsub(pattern=" ", replacement="", x=input$parcelles)),
                               split=";", fixed=TRUE))
    react$parcelles<<-parcelles
    monbrule<- cadastrebrule %>% 
      filter(geo_sectio==react$section & tex %in% react$parcelles) %>% 
      dplyr::select(tex, geo_parcel, surface_ge, area, pctbrule)  #there is a conflict for select between raster and dplyr
      
    react$df_brule<<- st_drop_geometry(monbrule %>% 
                                         rename(numparcelle=tex, idparcelle=geo_parcel, sfce_totale=surface_ge, sfce_brulee=area, pourcentage_brule=pctbrule))

    #get extent of the section
    bounding<-extent(monbrule)
    
    #get labels of parcels in the section
    etiq<-etiqparcelles[etiqparcelles$geo_sectio== react$section & etiqparcelles$tex %in% react$parcelles,]
    bords<-monbrule
    leafletProxy("map") %>%
      clearGroup(group="Num parcelles") %>% 
      addLabelOnlyMarkers(data = etiq, group= "Num parcelles",
                          label = ~tex,
                          labelOptions = labelOptions(noHide = TRUE, 
                                                      direction = 'top', 
                                                      textOnly = TRUE,
                                                      style = list("color" = "lightblue")))    %>% 
      clearGroup(group="Bords parcelles") %>% 
      addPolygons(data = bords, group="Bords parcelles",
                  color = "lightblue",
                  layerId = ~geo_parcel) %>%  # unique id for polygons
      
      #input$map_zoom
      flyToBounds( lng1 = unname(bounding@xmin)
                   , lat1 = unname(bounding@ymin)
                   , lng2 = unname(bounding@xmax)
                   , lat2 = unname(bounding@ymax) )
    
      
  })
  
  #click on button pdf
  # Downloadable csv of selected dataset ----
  output$chargepdf <- downloadHandler(
    filename = function() {
      paste("zonesbrulees.pdf")
    },
    content = function(file) {
      #observeEvent(input$chargepdf,{
      print("click chargepdf")
      monlcad<- lcad %>% 
        filter(geo_sectio==react$section & tex %in% react$parcelles)
      monlcadbrul<-lcadbrul %>% 
        filter(geo_sectio==react$section & tex %in% react$parcelles) %>% 
        dplyr::select(tex, geo_parcel, surface_ge, area, pctbrule)  #there is a conflict for select between raster and dplyr
        
      
      bounding<-extent(monlcadbrul)
      
      monlcad <- crop(as_Spatial(lcad), c(xmin=bounding@xmin, xmax=bounding@xmax,
                                          ymin=bounding@ymin, ymax=bounding@ymax))
      monlcad <- st_as_sf(monlcad) 
      
      
      monlbrul <- crop(as_Spatial(lbrul), c(xmin=bounding@xmin, xmax=bounding@xmax,
                                            ymin=bounding@ymin, ymax=bounding@ymax))
      monlbrul <- st_as_sf(monlbrul) 
      
      monraster<-crop(sentiLamb, bounding)
      
      coul<-raster::colortable(monraster); names(coul)<-0:255
      dfraster<-as.data.frame(monraster, xy=TRUE)%>%  #Convert raster to data.frame 
        mutate(couleur=coul[as.character(sentinel256couleurs)])
      dfcadbru<-df_spatial(monlcadbrul)
      dfcadastre<-df_spatial(monlcad)
      dfbru<-df_spatial(monlbrul)
      
      print("debut ggplot")
      g1<-ggplot()+
        geom_raster(data = dfraster, mapping=aes(x=x, y=y, fill=couleur))+
        scale_fill_identity()+ 
        theme(legend.position = "none")+
        geom_polypath(data=dfcadastre, mapping=aes(x=x, y=y, group = feature_id), colour="black", fill=NA)+
        geom_polypath(data=dfbru, mapping=aes(x=x, y=y, group = feature_id), colour="red", fill=NA)+
        geom_polypath(data=dfcadbru, mapping=aes(x=x, y=y, group = tex), colour="yellow", fill=NA)+
        coord_equal()
      
      g2<-grid.table(react$df_brule) #, show.rownames=FALSE) #tableau
      
      g2 <- tableGrob(react$df_brule)
      print("fin ggplot")
      
      
      pdf(file=file.path(tempdir(), "essaipdffromshinyapps.pdf"))
      grid.arrange(g1, g2, nrow = 2,top = grid::textGrob(paste("Section : ", react$section, "Parcelles : ", paste(react$parcelles, collapse=","))))
      dev.off()
      print(paste("pdf saved to", file.path(tempdir(), "essaipdffromshinyapps.pdf")))
       
      file.copy(file.path(tempdir(), "essaipdffromshinyapps.pdf"), file)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)  
