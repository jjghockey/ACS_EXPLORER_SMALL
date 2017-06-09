#####################################################################################################
#Engagement		-	UCLA MAS - STAT 405 - Project													#
#FileName		-	server.r								  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	5/30/2017									  									#
#																	  								#
#Purpose:		-	Shiny App - ACS EXPLORER SMALL 													#
#Notes:			- 																					#
#																									#
#####################################################################################################

#server.R

require(stringr)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(data.table)
require(dtplyr)
require(shiny)
require(gridExtra)
require(scales)
require(forcats)	
require(curl)
require(RCurl)

options(scipen=20)	

options(warn= -1)

shinyServer(function(input, output, session) {

	rm(tmp, tbl1a, tbl1b, tbl3a, tbl3b, tbl4a, tbl4b)
	gc(reset=TRUE)

  # Load the MAP data
  MAPInput <- reactive({
    isolate({
      withProgress({

		#Map File 1
        setProgress(message = "Downloading Map Files...")	  
		temp <- tempfile()
		download.file("https://raw.githubusercontent.com/jjghockey/ACS_EXPLORER_SMALL/master/1DataProcessing/output/stateDF.zip", paste(temp, ".zip", sep=""), method="curl")
		incProgress(1)
        setProgress(message = "Processing Mapping Data...")
		stateDF <- fread(unzip(paste(temp, ".zip", sep="")), showProgress=FALSE) 
		unlink(temp)
		unlink("stateDF.csv")
		rm(temp)
		incProgress(1)
		gc(reset=TRUE)
		
		stateDF[, pop:=factor(pop, levels=c("0-0.9", "1-19.9", "20-89.9", "90-499.9", "500-1999.9", ">=2000"))]
		stateDF[, unemp_den:=factor(unemp_den, levels=c("0-0.9", "1-19.9", "20-89.9", "90-499.9", "500-1999.9", ">=2000"))]
					
		stateDF           #Something weird keeps appearing at the bottom right of the maps.  This will 
						  #force the map to only produce the continental US.			
	  })
    })
  })
  
  # Load the MAP Table data
  TBLInput <- reactive({
    isolate({
      withProgress({

        setProgress(message = "Downloading Map Summary Files...")	  
		temp <- tempfile()
		download.file("https://raw.githubusercontent.com/jjghockey/ACS_EXPLORER_SMALL/master/1DataProcessing/output/tblDF.zip", paste(temp, ".zip", sep=""), method="curl")
		incProgress(1)

        setProgress(message = "Processing Map Summary Data...")
		tblDF <- fread(unzip(paste(temp, ".zip", sep="")), showProgress=FALSE) 
		unlink(temp)
		unlink("tblDF.csv")
		rm(temp)
		incProgress(1)
		as.data.table(tblDF)
      })
    })
  })  
  
  # Load the other data for tables and graphs
  AGEInput <- reactive({
    isolate({
      withProgress({

        setProgress(message = "Downloading Age and Population Files...")	  
		temp <- tempfile()
		download.file("https://raw.githubusercontent.com/jjghockey/ACS_EXPLORER_SMALL/master/1DataProcessing/output/ageDF.zip", paste(temp, ".zip", sep=""), method="curl")

		incProgress(1)
		
        setProgress(message = "Processing Age and Population Summary Data...")	
		ageDF <- fread(unzip(paste(temp, ".zip", sep="")), showProgress=FALSE) 
		unlink(temp)
		unlink("ageDF.csv")
		
		rm(temp)
		incProgress(1)
		as.data.table(ageDF)
      })
    })
  })  
    
  AGEUREDUCInput <- reactive({
    isolate({
      withProgress({

        setProgress(message = "Downloading Age and Population Files...")	  
		temp <- tempfile()
		download.file("https://raw.githubusercontent.com/jjghockey/ACS_EXPLORER_SMALL/master/1DataProcessing/output/ageureducDF.zip", paste(temp, ".zip", sep=""), method="curl")
		incProgress(1)
		
        setProgress(message = "Processing Age and Population Summary Data...")
		ageureducDF <- fread(unzip(paste(temp, ".zip", sep="")), showProgress=FALSE) 
		unlink(temp)
		unlink("ageureducDF.csv")
		
		rm(temp)
		incProgress(1)
		as.data.table(ageureducDF)
      })
    })
  })  
  
  OCCPInput <- reactive({
    isolate({
      withProgress({

        setProgress(message = "Downloading Occupation Files...")	  
		temp <- tempfile()
		download.file("https://raw.githubusercontent.com/jjghockey/ACS_EXPLORER_SMALL/master/1DataProcessing/output/top10occpDF.zip", paste(temp, ".zip", sep=""), method="curl")
		incProgress(1)
		
        setProgress(message = "Processing Occupation Summary Data...")
		occpDF <- fread(unzip(paste(temp, ".zip", sep="")), showProgress=FALSE) 
		unlink(temp)
		unlink("top10occpDF.csv")
		rm(temp)
		incProgress(1)
		as.data.table(occpDF)
      })
    })
  })  
  
  INDInput <- reactive({
    isolate({
      withProgress({

        setProgress(message = "Downloading Industry Files...")	  
		temp <- tempfile()
		download.file("https://raw.githubusercontent.com/jjghockey/ACS_EXPLORER_SMALL/master/1DataProcessing/output/top10indDF.zip", paste(temp, ".zip", sep=""), method="curl")
		incProgress(1)

        setProgress(message = "Processing Industry Summary Data...")
		indDF <- fread(unzip(paste(temp, ".zip", sep="")), showProgress=FALSE) 
		unlink(temp)
		unlink("top10indDF.csv")
		
		rm(temp)
		incProgress(1)
		as.data.table(indDF)
      })
    })
  })  

  WAGEInput <- reactive({
    isolate({
      withProgress({

        setProgress(message = "Downloading Wage Files...")	  
		temp <- tempfile()
		download.file("https://raw.githubusercontent.com/jjghockey/ACS_EXPLORER_SMALL/master/1DataProcessing/output/wagedistDF.zip", paste(temp, ".zip", sep=""), method="curl")
		incProgress(1)

        setProgress(message = "Processing Wage Summary Data...")
		wageDF <- fread(unzip(paste(temp, ".zip", sep="")), showProgress=FALSE) 
		unlink(temp)
		unlink("wagedistDF.csv")
		
		rm(temp)
		incProgress(1)
		as.data.table(wageDF)
      })
    })
  })   
  
  observe({
    # This will change the value of input$State
    updateSelectInput(session, "State", 
                    label = "State:", 
                    choices = unique(MAPInput()[is.na(stab)==FALSE, .(stab)], by=c("stab"))
				)
  })
  
  # This should set progress bars when switching tabs
  output$activeTab <- reactive({
	return(input$tab)
  })
  outputOptions(output, 'activeTab', suspendWhenHidden=FALSE)
  
  gc(reset=TRUE)
  
#MAPS
	  
	#1. Population

		#State Graph
		ranges <- reactiveValues(x = NULL, y = NULL)

		output$plot1a <- renderPlot({
			tmp<-MAPInput()
			tmp<-tmp[stab==input$State,.(stab, long,lat, zpop_grwth, group)] 
					
			ggplot(tmp, aes(long, lat, group=group, fill=zpop_grwth))+geom_polygon()+
			coord_fixed()+theme_bw()+theme(panel.grid.major = element_line(color="white"))+
			scale_fill_gradient(low="#FFFFB2", high="#BD0026")+
			labs(colour = "Population Growth", fill="Population Growth", x="", y="", title="Relative Population Growth", subtitle="Since 2010 Census")+
			labs(xlab="", ylab="")+theme(legend.position="none")+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+
			coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
		})

		# When a double-click happens, check if there's a brush on the plot.
		# If so, zoom to the brush bounds; if not, reset the zoom.
			observeEvent(input$plot1a_dblclick, {
				brush <- input$plot1a_brush
				if (!is.null(brush)) {
				  ranges$x <- c(brush$xmin, brush$xmax)
				  ranges$y <- c(brush$ymin, brush$ymax)

				} else {
				  ranges$x <- NULL
				  ranges$y <- NULL
				}
			})
			
		output$plot2a <- renderPlot({
			tmp<-MAPInput()
			tmp<-tmp[stab==input$State,.(stab, long,lat, pop, group)] 
			
			ggplot(tmp, aes(long, lat, group=group, fill=pop))+geom_polygon()+
			coord_fixed()+theme_bw()+theme(panel.grid.major = element_line(color="white"))+
			scale_fill_manual(values=c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026"))+
			labs(colour = "Population", fill="Population", x="", y="", title="Population Density", subtitle="")+
			labs(xlab="", ylab="")+theme(legend.position="none")+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+
			coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)

		})
			
		# When a double-click happens, check if there's a brush on the plot.
		# If so, zoom to the brush bounds; if not, reset the zoom.
			observeEvent(input$plot2a_dblclick, {
				brush <- input$plot2a_brush
				if (!is.null(brush)) {
				  ranges$x <- c(brush$xmin, brush$xmax)
				  ranges$y <- c(brush$ymin, brush$ymax)

				} else {
				  ranges$x <- NULL
				  ranges$y <- NULL
				}
			})		
			
		output$tbl1a<-renderTable({
			tbl1a<-TBLInput()
			tbl1a<-tbl1a[is.na(pop_grwth)==FALSE & stab==input$State, .(PUMAname, pop_grwth, tot_pop, cen_pop, pop_den)][order(pop_grwth)]
			tbl1a<-unique(rbind(head(tbl1a,5), tail(tbl1a,5)))
			tbl1a<-as.data.table(tbl1a)
		}, 
			caption = "Top 5 and Bottom 5 Population Growth by PUMA",
			caption.placement = getOption("xtable.caption.placement", "bottom"), 
			caption.width = getOption("xtable.caption.width", NULL)
		
		)
		
	#2. Income

		#State Graph
		ranges <- reactiveValues(x = NULL, y = NULL)

		output$plot3a <- renderPlot({
			tmp<-MAPInput()
			tmp<-tmp[stab==input$State,.(stab, long,lat,zavg_inc,group)] 
			
			ggplot(tmp, aes(long, lat, group=group, fill=zavg_inc))+geom_polygon()+
			coord_fixed()+theme_bw()+theme(panel.grid.major = element_line(color="white"))+
			scale_fill_gradient(low="#FFFFB2", high="#BD0026")+
			labs(colour = "Average Income", fill="Average Income", x="", y="", title="Average Income")+
			labs(xlab="", ylab="")+theme(legend.position="none")+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+
			coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)

		})

		# When a double-click happens, check if there's a brush on the plot.
		# If so, zoom to the brush bounds; if not, reset the zoom.
			observeEvent(input$plot3a_dblclick, {
				brush <- input$plot3a_brush
				if (!is.null(brush)) {
				  ranges$x <- c(brush$xmin, brush$xmax)
				  ranges$y <- c(brush$ymin, brush$ymax)

				} else {
				  ranges$x <- NULL
				  ranges$y <- NULL
				}
			})
					
		output$tbl3a<-renderTable({
			tbl3a<-TBLInput()
			tbl3a<-tbl3a[is.na(avg_inc)==FALSE & stab==input$State, .(PUMAname, avg_inc)][order(avg_inc)]
			tbl3a<-unique(rbind(head(tbl3a,5), tail(tbl3a,5)))
			tbl3a<-as.data.table(tbl3a)
		}, 
			caption = "Top 5 and Bottom 5 Average Income by PUMA",
			caption.placement = getOption("xtable.caption.placement", "bottom"), 
			caption.width = getOption("xtable.caption.width", NULL)

		)
		
	#3. Unemployment

		#State Graph
		ranges <- reactiveValues(x = NULL, y = NULL)

		output$plot4a <- renderPlot({
			tmp<-MAPInput()
			tmp<-tmp[stab==input$State,.(stab, long,lat, unemp_den, group)] 
			
			ggplot(tmp, aes(long, lat, group=group, fill=unemp_den))+geom_polygon()+
			coord_fixed()+theme_bw()+theme(panel.grid.major = element_line(color="white"))+
			scale_fill_manual(values=c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026"))+
			labs(colour = "Unemployment Density", fill="Unemployment Density", x="", y="", title="Unemployment Density")+		
			labs(xlab="", ylab="")+theme(legend.position="none")+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+
			coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
			
		})

		# When a double-click happens, check if there's a brush on the plot.
		# If so, zoom to the brush bounds; if not, reset the zoom.
			observeEvent(input$plot4a_dblclick, {
				brush <- input$plot4a_brush
				if (!is.null(brush)) {
				  ranges$x <- c(brush$xmin, brush$xmax)
				  ranges$y <- c(brush$ymin, brush$ymax)

				} else {
				  ranges$x <- NULL
				  ranges$y <- NULL
				}
			})
			
		output$plot5a <- renderPlot({
			tmp<-MAPInput()
			tmp<-tmp[stab==input$State,.(stab, long,lat, zur, group)] 
			
			ggplot(tmp, aes(long, lat, group=group, fill=zur))+geom_polygon()+
			coord_fixed()+theme_bw()+theme(panel.grid.major = element_line(color="white"))+
			scale_fill_gradient(low="#FFFFB2", high="#BD0026")+
			labs(colour = "Unemployment Rate", fill="Unemployment Rate", x="", y="", title="Unemployment Rate")+
			labs(xlab="", ylab="")+theme(legend.position="none")+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+
			coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
	
		})

		# When a double-click happens, check if there's a brush on the plot.
		# If so, zoom to the brush bounds; if not, reset the zoom.
			observeEvent(input$plot5a_dblclick, {
				brush <- input$plot5a_brush
				if (!is.null(brush)) {
				  ranges$x <- c(brush$xmin, brush$xmax)
				  ranges$y <- c(brush$ymin, brush$ymax)

				} else {
				  ranges$x <- NULL
				  ranges$y <- NULL
				}
			})		
			
		output$tbl4a<-renderTable({
			tbl4a<-TBLInput()
			tbl4a<-tbl4a[is.na(ur)==FALSE & stab==input$State, .(PUMAname, ur, unemp)][order(ur)]
			tbl4a<-unique(rbind(head(tbl4a,5), tail(tbl4a,5)))
			tbl4a<-as.data.table(tbl4a)
		}, 
			caption = "Top 5 and Bottom 5 Unemployment Rate by PUMA",
			caption.placement = getOption("xtable.caption.placement", "bottom"), 
			caption.width = getOption("xtable.caption.width", NULL)

		)
			
#Summary Graphs

#A. Pyramids

	#1. Population
	
		#State
		output$plot6a <- renderPlot({
			tmp<-AGEInput()
			tmp<-tmp[stab==input$State,.(age_cat,stab, variable, value)] 
			
			ggplot(tmp, aes(age_cat, value, fill=variable))+theme_bw()+
			geom_bar(data=subset(tmp, variable=="female"), stat="identity")+
			geom_bar(data=subset(tmp, variable=="male"), stat="identity", position="identity", mapping=aes(y= -value))+
			scale_y_continuous(labels = abs, limits = max(tmp$value) * c(-1,1)) +
			theme(legend.position="right") +
			scale_fill_manual(values=c('#6495ED','#800000')	)	+
			labs(colour = "Gender", fill="Gender", x="Age", y="Population", title="Age and Population Pyramid")+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+coord_flip()

		})
		
	#2. Population / UR / Education 
	
		#State
		output$plot7a <- renderPlot({
			tmp<-AGEUREDUCInput()
			tmp<-tmp[stab==input$State,.(age_cat,stab, variable, value, educ, ur)] 
			
			ggplot(tmp, aes(age_cat, ur, fill=variable))+theme_bw()+
			facet_wrap(~educ)+
			geom_bar(data=subset(tmp, variable=="female", fill=variable), stat="identity", position="stack")+
			geom_bar(data=subset(tmp, variable=="male", fill=variable), stat="identity", position="stack", mapping=aes(y= -ur))+
			theme(legend.position="right")+
			scale_fill_manual(values=c("#6495ED", "#800000", "#C90E17", "#691b14", "#08519c","#800000"))+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+			
			labs(colour = "Gender", fill="Gender", x="Age", y="Unemployment Rate", title="Age, Education, Unemployment, and Population Pyramid")+coord_flip()
		
		})

#B. Occupation Bar Charts
	#1. Wages
		#State
		output$plot8a <- renderPlot({
			tmp<-OCCPInput()
			tmp<-tmp[stab==input$State & tp=="Wage",]
			tmp<-tmp[, occp_descr:=fct_reorder(occp_descr, value)]
			
			ggplot(tmp, aes(occp_descr, value))+geom_bar(fill="#6495ED", stat="identity")+theme_bw()+
			labs(x="Occupation", y="Average Wage", title="Top 10 Occupations - Average Wages")+coord_flip()	

		})

	#2. Total Employment
		#State
		output$plot9a <- renderPlot({
			tmp<-OCCPInput()
			tmp<-tmp[stab==input$State & tp=="Total",]
			tmp<-tmp[, occp_descr:=fct_reorder(occp_descr, value)]
			
			ggplot(tmp, aes(occp_descr, value))+geom_bar(fill="#6495ED", stat="identity")+theme_bw()+
			labs(x="Occupation", y="Total Employment", title="Top 10 Occupations - Employment")+coord_flip()	

		})

#C. Industry Bar Charts
	#1. Wages
		#State
		output$plot10a <- renderPlot({
			tmp<-INDInput()
			tmp<-tmp[stab==input$State & tp=="Wage",]
			tmp<-tmp[, ind:=fct_reorder(ind, value)]
			
			ggplot(tmp, aes(ind, value))+geom_bar(fill="#6495ED", stat="identity")+theme_bw()+
			labs(x="Industry", y="Average Wage", title="Top 10 Industries - Average Wages")+coord_flip()	

		})

	#2. Total Employment
		#State
		output$plot11a <- renderPlot({
			tmp<-INDInput()
			tmp<-tmp[stab==input$State & tp=="Total",]
			tmp<-tmp[, ind:=fct_reorder(ind, value)]
			
			ggplot(tmp, aes(ind, value))+geom_bar(fill="#6495ED", stat="identity")+theme_bw()+
			labs(x="Industry", y="Total Employment", title="Top 10 Industries - Employment")+coord_flip()	


		})
	
#D. Wage Distribution

		output$plot12a <- renderPlot({
			tmp<-WAGEInput()
			tmp<-tmp[stab==input$State & is.na(logwage)==FALSE & sex==1,]
		
			ggplot(tmp) + 
			geom_histogram(aes(x=logwage, y=..density..), position="identity", bins=20, fill="#6495ED") + 
			geom_density(aes(x=logwage,y=..density.., color="#800000"), size=1.0, bw="nrd", kernel="gaussian", n=20)+theme_bw()+
			theme(legend.position="none")+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+
			xlim(7,14)+
			labs(y="Density", x="Log Wages", title="Distribution of Wages (Male)", subtitle="")

		})
		

		output$plot13a <- renderPlot({
			tmp<-WAGEInput()
			tmp<-tmp[stab==input$State & is.na(logwage)==FALSE & sex==2,]
		
			ggplot(tmp) + 
			geom_histogram(aes(x=logwage, y=..density..), position="identity", bins=20, fill="#6495ED") + 
			geom_density(aes(x=logwage,y=..density.., color="#800000"), size=1.0, bw="nrd", kernel="gaussian", n=20)+theme_bw()+
			theme(legend.position="none")+
			theme(plot.title = element_text(hjust = 0.5))+
			theme(plot.subtitle = element_text(hjust = 0.5))+
			xlim(7,14)+
			labs(y="Density", x="Log Wages", title="Distribution of Wages (Female)", subtitle="")

		})
		
	rm(tmp, tbl1a, tbl1b, tbl3a, tbl3b, tbl4a, tbl4b)
	gc(reset=TRUE)
		
}) #End of ShinyServer
