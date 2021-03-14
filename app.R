#REDAP_UN 1.2



#######################################################################################################################
# Inicialmente se descarga de "https://drive.google.com/drive/folders/18oV5-_Gw5NXT6Duyuhf6_bOOC7dHE_JF?usp=sharing"  #
# el archivo denominado "Datos_Finales.csv y se incluye en la carpeta "data", con el fin de poder tener los datos     #
# brutos en la carpeta de la aplicacion. Recuerde ademas guardar el archivo en File/Save Encoding/UTF-8               #
#######################################################################################################################



#Anexo de librerias 
library(shinydashboard)
library(rgdal)
library(leaflet)
library(raster) 
library(ggplot2) 
library(gstat) 
library(sp) 
library(dismo) 
library(ggvoronoi)
library(zoo)
library(DT)
library(lubridate)

#Datos
datos_estaciones <- read.csv("data/Datos_Finales.csv", header=TRUE)
x=as.POSIXct(datos_estaciones$Fecha,format="%Y-%m-%d %H:%M",origin=min(datos_estaciones$Fecha))
datos_estaciones$Fecha<-x





#ui object
ui <- dashboardPage(
    
    skin = "blue",
    dashboardHeader(title= "Red Pluviografica", titleWidth="300px", disable=TRUE),
    
    dashboardSidebar(width = "300px",
                     
        sidebarMenu(
            #Logo REDAP
            fluidRow(
                column(width=12,offset =2,
                       tags$img(src="redap_un.png",width="160px",height="160px")
                       )
            ),
            
            br(),
            

            # Diseno de la barra de inputs de los pluviografos 
            
            menuItem("Busqueda y descarga", tabName = "busqueda", icon=icon("search",lib="glyphicon")),
            
            selectInput(
                inputId = "var",
                label= "Pluviografo",
                choices= c("P01 (Hemeroteca)"=1,
                           "P02 (Hemeroteca)"=2,
                           "P03 (Concha A.)"=3,
                           "P04 (Concha A.)"=4,
                           "P05 (Posgrado CH.)"=5,
                           "P06 (Posgrado CH.)"=6,
                           "P07 (Medicina)"=7,
                           "P08 (Medicina)"=8,
                           "P09 (Capilla)"=9,
                           "P10 (Capilla)"=10,
                           "P11 (Hidraulica)"=11,
                           "P12 (Hidraulica)"=12,
                           "P13 (Genetica)"=13,
                           "P14 (Genetica)"=14),
                selected=11
            ),
            
            #Diseno de la barra de inputs asociada a las fechas
            dateRangeInput(inputId = "date",
                           label ="Fecha",
                           start = "2007-10-08 00:00:00",
                           end = "2007-10-09 00:00:00",
                           format = "yyyy/mm/dd",
                           separator = " - ",
                           min = min(datos_estaciones$Fecha),
                           max = max(datos_estaciones$Fecha)
                           
            ),
            
            #Diseno de el boton de descargas de la aplicacion
            
            br(),
            
            fluidRow(
                column(width=12,offset =3,
                       downloadButton("downloadData","Download", label ="Descargar Datos")
                )
            ),
            
            br(),
            
            menuItem("Comparacion de estaciones", tabName = "analisis", icon=icon("object-align-bottom",lib="glyphicon")),
            menuItem("Interpolacion espacial", tabName = "datos_espa", icon=icon("globe",lib="glyphicon")),
            menuItem("Contador de tormentas", tabName="tormenta",icon=icon("tint",lib="glyphicon")),
            
            br(),br(),br(),br(),br(),br(),br(),br(),br(),
            
            #Diseno de la autoria de la aplicacion
            
            #Logo GIREH Y SIHU
            
            fluidRow(
                column(width=12,offset =0,
                       tags$img(src="logo_gireh_gri.png",width="63px",height="50px"),
                       tags$img(src="logo_sihu.png",width="63px",height="50px")
                )
            ),
            
            p("Hecho por:", a("GIREH",href = "http://gireh.unal.edu.co/" ),"y",a("SIHU",href = "http://gireh.unal.edu.co/sihu/" ), ".")
            
            
        )
        
    ),
    
    dashboardBody(
        
        tabItems(
            
            tabItem(
                
                tabName = "busqueda",
                
                fluidRow(width=12,
                         column(width=12,offset =3,
                                box(width=6,
                                    title="Mapa Universidad Nacional",
                                    status="primary",
                                    solidHeader = TRUE,
                                    leafletOutput(outputId = "map")
                                    )
                         )
                         
                ),
                
                
                fluidRow(width=12,
                         column(width = 12, offset = 2,
                                box(width=8,
                                    title="Hietograma",
                                    status="primary",
                                    solidHeader = TRUE,
                                    plotOutput("plot1"),
                                    align="center"
                                    )
                         )
                )
                    
            ),
            
            tabItem(
                
                tabName = "analisis",
                
                
                fluidRow(width =12,
                         column(width = 12, offset=3,
                                box(title="Ingreso de datos",status="primary",solidHeader = TRUE,
                                    dateRangeInput(
                                        inputId = "date2",
                                        label ="Fecha",
                                        start = "2007-10-08 00:00:00",
                                        end = "2007-11-08 00:00:00",
                                        format = "yyyy/mm/dd",
                                        separator = " - ",
                                        min = min(datos_estaciones$Fecha),
                                        max = max(datos_estaciones$Fecha)
                                        
                                    ),
                                    
                                    selectInput(
                                        inputId = "var2",
                                        label= "Pluviografo 1",
                                        choices= c("P01 (Hemeroteca)"=1,
                                                   "P02 (Hemeroteca)"=2,
                                                   "P03 (Concha A.)"=3,
                                                   "P04 (Concha A.)"=4,
                                                   "P05 (Posgrado CH.)"=5,
                                                   "P06 (Posgrado CH.)"=6,
                                                   "P07 (Medicina)"=7,
                                                   "P08 (Medicina)"=8,
                                                   "P09 (Capilla)"=9,
                                                   "P10 (Capilla)"=10,
                                                   "P11 (Hidraulica)"=11,
                                                   "P12 (Hidraulica)"=12,
                                                   "P13 (Genetica)"=13,
                                                   "P14 (Genetica)"=14),
                                        selected=1
                                    ),
                                    
                                    selectInput(
                                        inputId = "var3",
                                        label= "Pluviografo 2",
                                        choices= c("Seleccione"=0,
                                                   "P01 (Hemeroteca)"=1,
                                                   "P02 (Hemeroteca)"=2,
                                                   "P03 (Concha A.)"=3,
                                                   "P04 (Concha A.)"=4,
                                                   "P05 (Posgrado CH.)"=5,
                                                   "P06 (Posgrado CH.)"=6,
                                                   "P07 (Medicina)"=7,
                                                   "P08 (Medicina)"=8,
                                                   "P09 (Capilla)"=9,
                                                   "P10 (Capilla)"=10,
                                                   "P11 (Hidraulica)"=11,
                                                   "P12 (Hidraulica)"=12,
                                                   "P13 (Genetica)"=13,
                                                   "P14 (Genetica)"=14),
                                        selected=0
                                    ),
                                    
                                    selectInput(
                                        inputId = "var4",
                                        label= "Pluviografo 3",
                                        choices= c("Seleccione"=0,
                                                   "P01 (Hemeroteca)"=1,
                                                   "P02 (Hemeroteca)"=2,
                                                   "P03 (Concha A.)"=3,
                                                   "P04 (Concha A.)"=4,
                                                   "P05 (Posgrado CH.)"=5,
                                                   "P06 (Posgrado CH.)"=6,
                                                   "P07 (Medicina)"=7,
                                                   "P08 (Medicina)"=8,
                                                   "P09 (Capilla)"=9,
                                                   "P10 (Capilla)"=10,
                                                   "P11 (Hidraulica)"=11,
                                                   "P12 (Hidraulica)"=12,
                                                   "P13 (Genetica)"=13,
                                                   "P14 (Genetica)"=14),
                                        selected=0
                                    )
                                )
                                )
                ),
                
                
                
                fluidRow(width=12,
                         column(width=12, offset=1,
                                tabBox(width=5,
                                       title="Curvas de masa",
                                       id="tabset1",
                                       tabPanel("Curva de masa",plotOutput("plot2")),
                                       tabPanel("Curva adimensional de masa",plotOutput("plot3"))),
                                
                                box(width=5,
                                    status = "primary",
                                    title="Hietogramas",
                                    plotOutput("plot4"))
                            
                                )
                )
            ),
            
            tabItem(
                
                tabName = "datos_espa",
                
                br(),
                
                fluidRow(width=12,
                         column(width = 8,offset=4,
                                box(title="Ingreso de fechas",status="primary",solidHeader = TRUE,
                                    dateRangeInput(
                                        inputId = "date3",
                                        label ="Fecha",
                                        start = "2007-10-08 00:00:00",
                                        end = "2007-11-08 00:00:00",
                                        format = "yyyy/mm/dd",
                                        separator = " - ",
                                        min = min(datos_estaciones$Fecha),
                                        max = max(datos_estaciones$Fecha)
                                    )
                                )
                          )
                         
                ),
                
                br(),br(),br(),
                
                fluidRow(width=12,
                         column(width = 12,offset=3,
                                tabBox(width=6,height="500px",
                                       title="Interpolaciones",
                                       id="tabset2",
                                       tabPanel("IDW",plotOutput("idw",height = "500px")),
                                       tabPanel("Kriging",plotOutput("kriging",height = "500px")),
                                       tabPanel("Thiessen",plotOutput("thiessen",height = "500px"))
                                )
                         )
                )
            ),
            
            tabItem(
                
                tabName = "tormenta",
                
                fluidRow(width=12,
                         column(width=12,offset=1,
                                box(width=5,
                                    title="Ingreso de datos",status="primary",solidHeader = TRUE,
                                    dateRangeInput(
                                        inputId = "date4",
                                        label ="Fecha",
                                        start = "2007-10-08 00:00:00",
                                        end = "2007-10-10 00:00:00",
                                        format = "yyyy/mm/dd",
                                        separator = " - ",
                                        min = min(datos_estaciones$Fecha),
                                        max = max(datos_estaciones$Fecha)
                                        ),
                                    
                                    selectInput(
                                        inputId = "var5",
                                        label= "Pluviografo ",
                                        choices= c("Seleccione"=0,
                                                   "P01 (Hemeroteca)"=1,
                                                   "P02 (Hemeroteca)"=2,
                                                   "P03 (Concha A.)"=3,
                                                   "P04 (Concha A.)"=4,
                                                   "P05 (Posgrado CH.)"=5,
                                                   "P06 (Posgrado CH.)"=6,
                                                   "P07 (Medicina)"=7,
                                                   "P08 (Medicina)"=8,
                                                   "P09 (Capilla)"=9,
                                                   "P10 (Capilla)"=10,
                                                   "P11 (Hidraulica)"=11,
                                                   "P12 (Hidraulica)"=12,
                                                   "P13 (Genetica)"=13,
                                                   "P14 (Genetica)"=14),
                                        selected=11
                                    ),
                                    
                                    selectInput(
                                        inputId = "var6",
                                        label= "Duracion de tormenta (minutos)",
                                        choices= c("15 minutos"=15,
                                                   "30 Minutos"=30,
                                                   "45 minutos"=45,
                                                   "60 minutos"=60,
                                                   "90 minutos"=90),
                                        selected=15
                                        
                                    ),
                                    numericInput("num", 
                                                 label = "Precipitacion minima (mm/min)", 
                                                 value = 0.01
                                    ),
                                    
                                    selectInput(
                                        inputId = "inter",
                                        label= "Periodo intertormenta (minutos)",
                                        choices= c("15 minutos"=15,                                                  
                                                   "30 minutos"=30,
                                                   "45 minutos"=45,
                                                   "60 minutos"=60,
                                                   "90 minutos"=90),
                                        selected=30

                                    )
                                    
                                    
                                ),
                                
                                box(width=5,
                                    title="Tabla de tormentas",status="primary",solidHeader = TRUE,
                                    DTOutput(outputId = "table")
                                )
                            )
                ),
                
                
                fluidRow(width=12,
                         column(width = 12, offset = 2,
                                box(width=8,
                                    title="Hietograma",
                                    status="primary",
                                    solidHeader = TRUE,
                                    plotOutput("plot5"),
                                    align="center"
                                )
                         )
                )
            
            )
            
        )
    )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## BUSQUEDA Y DESCARGA ##
    
    # Diseno del mapa en leaflet
    output$map <- renderLeaflet({
        
        #Shapefile de las estaciones
        shape_estaciones<-readOGR("data/Coordenadas_Fi.shp")
        # Lugar de la estacion
        lugar_estacion <- shape_estaciones$Ubicacion %>% unique() %>% length()
        # Nombre de la estacion
        nombre_estacion <- shape_estaciones$Ubicacion %>% unique() 
        # Colores de la leyenda
        # se sustraen de "colorbrewer2.org" 
        colores <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')
        ## Paleta de colores
        pal <- colorFactor(colores,domain=nombre_estacion)
        # Crear el Mapa
        m <- leaflet() %>%
            addProviderTiles(providers$Stamen.Toner) %>%
            setView(lng = -74.083777, lat = 4.63756, zoom = 15.3) %>%
            addCircleMarkers(data = shape_estaciones,
                             radius = 5,
                             color= ~pal(Ubicacion),
                             fillOpacity = 1,
                             popup = ~Ubicacion,
                             label= ~Pluviograf)
        
        # Generar una leyenda
        m <- m %>% leaflet::addLegend(data=shape_estaciones, 
                                      "bottomright", 
                                      pal = pal,
                                      values=~Ubicacion,
                                      title = "Estacion",
                                      opacity = 2,
                                      group= "Leyenda")
        
        # Control de grupos
        m <-m %>% addLayersControl(overlayGroups = c("Leyenda"),
                                   options = layersControlOptions(collapsed = TRUE))
        
        
        
        
        
        
    })
    
    # Diseno de los hietogramas
    output$plot1 <- renderPlot({
        
        x1=as.POSIXct(input$date[1])
        x2=as.POSIXct(input$date[2])
        
        colm<-as.numeric(input$var)
        
        limi=match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims=match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        xs=datos_estaciones$Fecha[(limi+300):(lims+300)]
        ys=datos_estaciones[,colm+1][(limi+300):(lims+300)]
        
        datos_ter<-data.frame(xs,ys)
        

        plot(datos_estaciones$Fecha[(limi+300):(lims+300)],
             datos_estaciones[,colm+1][(limi+300):(lims+300)],
             type="h",
             xlab="Fecha",
             ylab="Precipitacion (mm)",
             col="cornflowerblue") 
    })
    
    # Diseno de el boton de descarga de datos
    
    data_react<-reactive({
        
        data.frame(fechas_prec=(datos_estaciones$Fecha[((match(as.POSIXct(input$date[1]),datos_estaciones$Fecha))+300):((match(as.POSIXct(input$date[2]),datos_estaciones$Fecha))+300)]),
                   precip_mm=(datos_estaciones[,(as.numeric(input$var))+1][((match(as.POSIXct(input$date[1]),datos_estaciones$Fecha))+300):((match(as.POSIXct(input$date[2]),datos_estaciones$Fecha))+300)]))
        
        
    })
    
    
    output$downloadData <- downloadHandler(
        
        
        filename=function() {
            
            paste("datos",".csv",sep = "")
            
        },
        
        content = function(file) {
            
            write.csv(data_react(),file,row.names = FALSE)
            
        }
        
        
    )
    
    
    ## COMPARACION DE ESTACIONES ##
    
    #Curva de masa
    output$plot2 <- renderPlot({
        
        color1=list("cornflowerblue","red","darkgreen")
        
        x1<-as.POSIXct(input$date2[1])
        x2<-as.POSIXct(input$date2[2])
        
        limi=match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims=match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        colm1<-as.numeric(input$var2)
        sumacu1<-cumsum(datos_estaciones[((limi+300):(lims+300)),colm1+1])
        
        if(input$var3==0){sumacu2=0}
        else{
            colm2<-as.numeric(input$var3)
            sumacu2<-cumsum(datos_estaciones[((limi+300):(lims+300)),colm2+1])}
        
        if(input$var4==0){sumacu3=0}
        else{
        colm3<-as.numeric(input$var4)
        sumacu3<-cumsum(datos_estaciones[((limi+300):(lims+300)),colm3+1])}
        
        leyenda=list(paste0("P",input$var2))
        
        plot(datos_estaciones$Fecha[(limi+300):(lims+300)],sumacu1,
             type="l",
             col=color1[[1]],
             lwd=3,
             xlab="Fecha",
             ylab="Precipitacion Acumulada (mm)",
             ylim=c(0,max(sumacu1,sumacu2, sumacu3)))
        
        if(sumacu2[1]==0){color1[[3]]<-"red"}
        else {
        lines(datos_estaciones$Fecha[(limi+300):(lims+300)],sumacu2,
                type="l",
                col=color1[[2]],
                lwd=3)
        leyenda[[length(leyenda)+1]]<-paste0("P",input$var3)
            }
        
        if(sumacu3[1]==0){}
        else {
        lines(datos_estaciones$Fecha[(limi+300):(lims+300)],
              sumacu3,
              type="l",
              col=color1[[3]],
              lwd=3)
        leyenda[[length(leyenda)+1]]<-paste0("P",input$var4)
        
            }
        
        title(main="Curva de masa")
        
        legend("topleft",inset=0.05, legend=leyenda,
               col=c("cornflowerblue", "red","darkgreen"), lty=1:1, cex=0.8,lwd=2)
        
    })
    
    #Curva de masa adimensional
    output$plot3 <- renderPlot({
        
        color1=list("cornflowerblue","red","darkgreen")
        
        x1<-as.POSIXct(input$date2[1])
        x2<-as.POSIXct(input$date2[2])
        
        limi=match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims=match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        colm1<-as.numeric(input$var2)
        sumacu1<-cumsum(datos_estaciones[((limi+300):(lims+300)),colm1+1])
        
        if(input$var3==0){sumacu2=0}
        else{
        colm2<-as.numeric(input$var3)
        sumacu2<-cumsum(datos_estaciones[((limi+300):(lims+300)),colm2+1])}
        
        if(input$var4==0){sumacu3=0}
        else{
        colm3<-as.numeric(input$var4)
        sumacu3<-cumsum(datos_estaciones[((limi+300):(lims+300)),colm3+1])}

        cont<-1:((lims+1)-limi)
        leyenda=list(paste0("P",input$var2))
        
        plot((cont/max(cont))*100,
             sumacu1/max(sumacu1)*100,
             type="l",
             xlab="Tiempo (%)",
             ylab="Precipitacion Acumulada (%)",
             col=color1[[1]],
             ylim=c(0,100),
             lwd=3)
        title(main="Curva adimensional de masa")
        
        if(sumacu2[1]==0){color1[[3]]<-"red"}
        else{
        lines((cont/max(cont))*100,
              sumacu2/max(sumacu2)*100,
              type="l",
              col=color1[[2]],
              lwd=3)
            leyenda[[length(leyenda)+1]]<-paste0("P",input$var3)    
            }
        
        if(sumacu3[1]==0){}
        else{
        lines((cont/max(cont))*100,
              sumacu3/max(sumacu3)*100,
              type="l",
              col=color1[[3]],
              lwd=3)
            leyenda[[length(leyenda)+1]]<-paste0("P",input$var4)
            }
        
        legend("topleft",inset=0.05, legend=leyenda,
               col=c("cornflowerblue", "red","darkgreen"),lty=1:1, cex=0.8,lwd=2)
        
        
    })
    
    #Hietograma comparativo
    output$plot4 <- renderPlot({
        
        color1=list("cornflowerblue","red","darkgreen")
        
        x1=as.POSIXct(input$date2[1])
        x2=as.POSIXct(input$date2[2])
        
        limi=match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims=match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        #Precipitacion interpolada
        
        colm1<-as.numeric(input$var2)
        ys1=datos_estaciones[,colm1+1][(limi+300):(lims+300)]
        
        if(input$var3==0){ys2=0}
        else{
        colm2<-as.numeric(input$var3)
        ys2=datos_estaciones[,colm2+1][(limi+300):(lims+300)]}
        
        if(input$var4==0){ys3=0}
        else{
        colm3<-as.numeric(input$var4)
        ys3=datos_estaciones[,colm3+1][(limi+300):(lims+300)]}
        
        #Fechas interpoladas
        xs=datos_estaciones$Fecha[(limi+300):(lims+300)]
        
        data_ys<-data.frame(p1=ys1,p2=ys2,p3=ys3)
        
        leyenda=list(paste0("P",input$var2))
        
        plot(xs,
             ys1,
             type="h",
             xlab="Fecha",
             ylab="Precipitacion (mm)",
             col=color1[[1]],
             ylim=c(0,max(data_ys))
             )
        
        if(ys2[1]==0){color1[[3]]<-"red"}
        else{
        lines(xs,
              ys2,
              type="h",
              col=color1[[2]])
            leyenda[[length(leyenda)+1]]<-paste0("P",input$var3)
            }
        
        if(ys3[1]==0){}
        else{
        lines(xs,
              ys3,
              type="h",
              col=color1[[3]])
            leyenda[[length(leyenda)+1]]<-paste0("P",input$var4)
            }
        
        legend("topleft",inset=0.05, legend=leyenda,
               col=c("cornflowerblue", "red","darkgreen"), lty=1:1, cex=0.8,lwd=2)

    })
    
    
    
    #INTERPOLACION ESPACIAL#
    
    #Interpolacion IDW
    output$idw <- renderPlot({
        
        #Insertar Shp y Raster requeridos 
        shape_estaciones<-readOGR("data/Coordenadas_Fi.shp")
        raster_un<-raster("data/raster_un.tif")
        shape_un<-readOGR("data/Poligono_unal.shp")
        
        #Crear dataframes del raster suministrado
        raster_un<-aggregate(raster_un,fact=1)
        raster_undf<- as.data.frame(raster_un, xy=T)
        
        #Fechas Input
        x1=as.POSIXct(input$date3[1])
        x2=as.POSIXct(input$date3[2])
        
        # Ubicacion en el vector Fechas de las fechas 
        limi<-match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims<-match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        # Nuevo dataframe 
        datos_filtrados <- datos_estaciones[((limi+300):(lims+300)),]
        
        # Promedio de cada estacion
        datos_promedio<-c()
        for (i in 1:14){
            datos_promedio[i]<-c(sum(datos_filtrados[,i+1]))
        }
        
        # Agregar valores de promedio al dataframe de el shape de los puntos
        shape_estaciones$promedios<-datos_promedio 
        
        # Volver un dataframe 
        data_shape<-shape_estaciones@data
        data_shape$alias<-c("Htca",
                            "Htca",
                            "Concha",
                            "Concha",
                            "P.CH",
                            "P.CH",
                            "Medici",
                            "Medici",
                            "Cplla",
                            "Cplla",
                            "Hidra",
                            "Hidra",
                            "Gene",
                            "Gene")
        
        ## Generacion de la interpolacion ##
        
        # Creacion de un grid
        grid<-as(raster_un,"SpatialPixels")
        griddf<-as.data.frame(grid)
        
        #Se crea un dataframe con coordenadas y promedio
        puntos_espa <- data.frame(x_cord=data_shape$x_cord,y_cord=data_shape$y_cord,promedios=data_shape$promedios)
        pts=puntos_espa
        coordinates(pts)<- ~x_cord + y_cord
        proj4string(pts)<- proj4string(grid)
        
        
        #Espacializacion por medio de el metodo de IDW
        idw = idw(formula = puntos_espa$promedios~1,
                  locations = pts,
                  newdata =grid)
        
        idwdf <- as.data.frame(idw)
        
        pidwdf<- mean(idwdf$var1.pred)
        ggplot()+
            geom_tile(data=idwdf,aes(x=x, y=y, fill=var1.pred))+
            geom_point(data=data_shape, aes(x=x_cord,y=y_cord),
                       shape=4)+
            geom_text(data=data_shape,aes(x=x_cord,y=y_cord,label=data_shape$alias),hjust=0, vjust=0,size=4,check_overlap = TRUE)+
            scale_fill_gradient(low="skyblue", high = "darkblue", name="Precipitacion (mm)", position="right")+
            labs(x="Longitud",y="Latitud", title ="Interpolacion IDW")+
            theme_classic()+
            theme(plot.title=element_text(face = "bold", size = 20, hjust = 0.5))+
            annotate("text", x = min(data_shape$x_cord), 
                     y = min(data_shape$y_cord), 
                     label = paste0("P Promedio= ",round(pidwdf,2),"mm"), 
                     hjust = -0.08, size = 4) 
            
        
    })
    
    #Interpolacion Kriging
    output$kriging <- renderPlot({
        
        #Insertar Shp y Raster requeridos 
        shape_estaciones<-readOGR("data/Coordenadas_Fi.shp")
        raster_un<-raster("data/raster_un.tif")
        shape_un<-readOGR("data/Poligono_unal.shp")
        
        #Crear dataframes del raster suministrado
        raster_un<-aggregate(raster_un,fact=1)
        raster_undf<- as.data.frame(raster_un, xy=T)
        
        #Fechas Input
        x1=as.POSIXct(input$date3[1])
        x2=as.POSIXct(input$date3[2])
        
        # Ubicacion en el vector Fechas de las fechas 
        limi<-match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims<-match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        # Nuevo dataframe 
        datos_filtrados <- datos_estaciones[((limi+300):(lims+300)),]
        
        # Promedio de cada estacion
        datos_promedio<-c()
        for (i in 1:14){
            datos_promedio[i]<-c(sum(datos_filtrados[,i+1]))
        }
        
        # Agregar valores de promedio al dataframe de el shape de los puntos
        shape_estaciones$promedios<-datos_promedio 
        
        # Volver un dataframe 
        data_shape<-shape_estaciones@data
        data_shape$alias<-c("Htca",
                            "Htca",
                            "Concha",
                            "Concha",
                            "P.CH",
                            "P.CH",
                            "Medici",
                            "Medici",
                            "Cplla",
                            "Cplla",
                            "Hidra",
                            "Hidra",
                            "Gene",
                            "Gene")
        
        ## Generacion de la interpolacion ##
        
        # Creacion de un grid
        grid<-as(raster_un,"SpatialPixels")
        griddf<-as.data.frame(grid)
        
        #Se crea un dataframe con coordenadas y promedio
        puntos_espa <- data.frame(x_cord=data_shape$x_cord,y_cord=data_shape$y_cord,promedios=data_shape$promedios)
        pts=puntos_espa
        coordinates(pts)<- ~x_cord + y_cord
        proj4string(pts)<- proj4string(grid)
        
        
        # Se crea el variograma de la variable requerida, y los puntos notables del diagrama
        var <- variogram(object=puntos_espa$promedios~1,location=pts)

        nugget_var = var$gamma[1]
        sill_var = max(var$gamma)
        range_var =	var[match(max(var$gamma),var$gamma),2]
        
        
        # Variograma aproximado para el calculo de Kriging  
        fit_var <- fit.variogram(object = var, model=gstat::vgm(psill=sill_var,nugget=nugget_var,range=range_var,model="Sph"))

        # Calculo de kriging y data.frame de el metodo kriging 
        
        kriging <- krige(formula = puntos_espa$promedios~1,
                         locations = pts,
                         newdata=grid,
                         model=fit_var)
        
        krigingdf <- as.data.frame(kriging)
        
        #Grafica de Kriging
        
        pkrigingdf <- mean(krigingdf$var1.pred)
        ggplot()+
            geom_tile(data=krigingdf,aes(x=x, y=y, fill=var1.pred))+
            geom_point(data=data_shape, aes(x=x_cord,y=y_cord),
                       shape=4)+
            geom_text(data=data_shape,aes(x=x_cord,y=y_cord,label=data_shape$alias),hjust=0, vjust=0,size=4,check_overlap = TRUE)+
            scale_fill_gradient(low="skyblue", high = "darkblue", name="Precipitacion (mm)", position="right")+
            labs(x="Longitud",y="Latitud", title ="Interpolacion Kriging")+
            theme_classic()+
            theme(plot.title=element_text(face = "bold", size = 20, hjust = 0.5))+
            annotate("text", x = min(data_shape$x_cord), 
                     y = min(data_shape$y_cord), 
                     label = paste0("P Promedio= ",round(pkrigingdf,2),"mm"), 
                     hjust = -0.08, size = 4) 
        
    })
    
    #Interpolacion Thiessen
    output$thiessen <- renderPlot({
        
        #Insertar Shp y Raster requeridos 
        shape_estaciones<-readOGR("data/Coordenadas_Fi.shp")
        raster_un<-raster("data/raster_un.tif")
        shape_un<-readOGR("data/Poligono_unal.shp")
        
        #Crear dataframes del raster suministrado
        raster_un<-aggregate(raster_un,fact=1)
        raster_undf<- as.data.frame(raster_un, xy=T)
        
        #Fechas Input
        x1=as.POSIXct(input$date3[1])
        x2=as.POSIXct(input$date3[2])
        
        # Ubicacion en el vector Fechas de las fechas 
        limi<-match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims<-match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        # Nuevo dataframe 
        datos_filtrados <- datos_estaciones[((limi+300):(lims+300)),]
        
        # Promedio de cada estacion
        datos_promedio<-c()
        for (i in 1:14){
            datos_promedio[i]<-c(sum(datos_filtrados[,i+1]))
        }
        
        # Agregar valores de promedio al dataframe de el shape de los puntos
        shape_estaciones$promedios<-datos_promedio 
        
        # Volver un dataframe 
        data_shape<-shape_estaciones@data
        data_shape$alias<-c("Htca",
                            "Htca",
                            "Concha",
                            "Concha",
                            "P.CH",
                            "P.CH",
                            "Medici",
                            "Medici",
                            "Cplla",
                            "Cplla",
                            "Hidra",
                            "Hidra",
                            "Gene",
                            "Gene")
        
        ## Generacion de la interpolacion ##
        
        # Creacion de un grid
        grid<-as(raster_un,"SpatialPixels")
        griddf<-as.data.frame(grid)
        
        #Se crea un dataframe con coordenadas y promedio
        puntos_espa <- data.frame(x_cord=data_shape$x_cord,y_cord=data_shape$y_cord,promedios=data_shape$promedios)
        pts=puntos_espa
        coordinates(pts)<- ~x_cord + y_cord
        proj4string(pts)<- proj4string(grid)
        
        #Creacion de los poligonos de voronoi
        voronoi_map <- voronoi(pts,grid)
        
        #Intersecto entre el vector de la UNAL, con el generado de los poligonos de Voronoi
        mapa_voronoi=intersect(shape_un,voronoi_map)
        
        #Creacion de los poligonos de voronoi
        voronoi_map <- voronoi(pts,grid)
        voronoi_mapdf <-voronoi_map@data
        pvoronoi_mapdf <- mean(voronoi_mapdf$promedios)
        #Intersecto entre el vector de la UNAL, con el generado de los poligonos de Voronoi
        mapa_voronoi=intersect(shape_un,voronoi_map)
        #grafico del mapa de Voronoi
        
        ggplot(data=data_shape, aes(x=x_cord,y=y_cord,fill=promedios)) +
            scale_fill_gradient(low="skyblue", high = "darkblue", name="Precipitación (mm)", position="right")+
            geom_voronoi(outline=shape_un)+
            geom_point(data=data_shape, aes(x=x_cord,y=y_cord),shape=4)+
            geom_text(aes(label=data_shape$alias),hjust=0, vjust=0, size=3,check_overlap = TRUE)+
            labs(x="Longitud",y="Latitud", title ="Polígonos de Thiessen")+
            theme_classic()+
            theme(plot.title=element_text(face = "bold", size = 15, hjust = 0.5))+
            annotate("text", x = min(data_shape$x_cord), 
                     y = min(data_shape$y_cord), 
                     label = paste0("P Promedio= ",round(pvoronoi_mapdf,2),"mm"), 
                     hjust = -0.08, size = 3)  
        
        
    
    })
    

    #DETECTOR DE TORMENTAS#
    
    # Diseno de los hietogramas
    
    output$plot5 <- renderPlot({
        
        x1=as.POSIXct(input$date4[1])
        x2=as.POSIXct(input$date4[2])
        
        colm1<-as.numeric(input$var5)
        colm2<-as.numeric(input$var6)
        colm3<-as.numeric(input$num)
        colm4<-as.numeric(input$inter)
        
        
        limi=match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims=match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        prueba <- datos_estaciones[(limi+300):(lims+300),c(1,(1+colm1))]
        
        # Duracion
        t<-colm2
        d<-colm3
        e<-colm4
        
        # Devuelve en el vector index la posición de los elementos que satisfacen la condición
        index <- which(prueba[,2] > d)
        
        # Cuenta el número de elementos que cumple la condicion anterior (longitud del vector)
        vindex <- length(index)
        
        # Definición de variable que determina cuantas tormentas tienen una duración por encima de x tiempo (x=15)
        
        matches<-c()
        a<-0
        # Conteo de la primera secuencia del vector index
        if (vindex[1]!=0){
            a=a+1
            matches[a]<-c(1)
        }
        
        # Para cada reinicio de secuencia de numeros evalúa si los x siguientes numeros son una secuencia o no
        
        for (i in (2:(vindex-t))){
            if ((index[i]>(index[i-1]+e))){
                a=a+1
                matches[a]<-c(i) 
            }
        }
        
        a2<-0
        matches2<-c()
        for (i in (1:(vindex-1))){
            if ((index[i]<(index[i+1]-e))){
                a2 = a2+1
                matches2[a2]<-c(i)
            }
        }
        
        if (index[vindex]==(index[vindex-t]+t)){
            matches2[length(matches2)+1]<-vindex
        }
        
        matches_index<-index[c(matches)]
        matches2_index<-index[c(matches2)]
        tormenta_inicio<-prueba[c(matches_index),]
        tormenta_final<-prueba[c(matches2_index),]
        vtormenta<-nrow(tormenta_inicio)
        fechas<-prueba[c(matches_index),1]
        
        tormenta<-as.POSIXct(c())
        
        c2<-0
        tormenta_cumple<-c()
        for (i in 1:vtormenta) {
            if (matches2[i]-matches[i]>t){
                tormenta[matches_index[i]:matches2_index[i]]<-prueba[matches_index[i]:matches2_index[i],1]
                c2=c2+1
                tormenta_cumple[c2]<-c(i)
            }
        }
        
        tormenta<- na.omit(tormenta)
        
        graf_tormentas<-prueba[c(match(tormenta,prueba$Fecha)),]
        
        plot(prueba$Fecha,
             prueba[,2],
             type="h",
             xlab="Fecha",
             ylab="Precipitacion (mm)",
             col="cornflowerblue")
        lines(graf_tormentas$Fecha,
              graf_tormentas[,2],
              type="h",
              col="red")
        legend("topright",inset=0.05, legend=c("Precipitacion(mm)","Tormenta(mm)"),
               col=c("cornflowerblue", "red"), lty=1:1, cex=0.8,lwd=2,box.lty=0)
        
        
    })
    
    # Tormentas
    output$table <- renderDT({
        
        x1t=as.POSIXct(input$date4[1])
        x2t=as.POSIXct(input$date4[2])
        
        colm1t<-as.numeric(input$var5)
        colm2t<-as.numeric(input$var6)
        colm3t<-as.numeric(input$num)
        colm4t<-as.numeric(input$inter)
        
        
        limit=match(as.POSIXct(x1t),datos_estaciones$Fecha)
        limst=match(as.POSIXct(x2t),datos_estaciones$Fecha)
        
        
        
        prueba <- datos_estaciones[(limit):(limst),c(1,(1+colm1t))]
        
        # Duracion
        t<-colm2t
        d<-colm3t
        e<-colm4t
        
        
        # Devuelve en el vector index la posición de los elementos que satisfacen la condición
        index <- which(prueba[,2] > d)
        
        # Cuenta el número de elementos que cumple la condicion anterior (longitud del vector)
        vindex <- length(index)
        
        # Definición de variable que determina cuantas tormentas tienen una duración por encima de x tiempo (x=15)
        
        matches<-c()
        a<-0
        # Conteo de la primera secuencia del vector index
        if (vindex[1]!=0){
            a=a+1
            matches[a]<-c(1)
        }
        
        # Para cada reinicio de secuencia de numeros evalúa si los x siguientes numeros son una secuencia o no
        
        for (i in (2:(vindex-t))){
            if ((index[i]>(index[i-1]+e))){
                a=a+1
                matches[a]<-c(i) 
            }
        }
        
        a2<-0
        matches2<-c()
        for (i in (1:(vindex-1))){
            if ((index[i]<(index[i+1]-e))){
                a2 = a2+1
                matches2[a2]<-c(i)
            }
        }
        
        if (index[vindex]==(index[vindex-t]+t)){
            matches2[length(matches2)+1]<-vindex
        }
        
        matches_index<-index[c(matches)]
        matches2_index<-index[c(matches2)]
        tormenta_inicio<-prueba[c(matches_index),]
        tormenta_final<-prueba[c(matches2_index),]
        vtormenta<-nrow(tormenta_inicio)
        fechas<-prueba[c(matches_index),1]
        
        tormenta<-as.POSIXct(c())
        
        c2<-0
        tormenta_cumple<-c()
        for (i in 1:vtormenta) {
            if (matches2[i]-matches[i]>t){
                tormenta[matches_index[i]:matches2_index[i]]<-prueba[matches_index[i]:matches2_index[i],1]
                c2=c2+1
                tormenta_cumple[c2]<-c(i)
            }
        }
        
        tormenta<- na.omit(tormenta)
        
        graf_tormentas<-prueba[c(match(tormenta,prueba$Fecha)),]
        
        
        fechas<-fechas[c(tormenta_cumple)]
        
        df_tormentas<-data.frame("No.Tormenta"=(1:length(fechas)),
                                 "Dia"=as_date(fechas),
                                 "Hora"=strftime(fechas, format="%H:%M"))
        
        
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)

