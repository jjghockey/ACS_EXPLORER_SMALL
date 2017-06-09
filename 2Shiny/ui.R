#####################################################################################################
#Engagement		-	UCLA MAS - STAT 405 - Project													#
#FileName		-	ui.r								  											#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	5/30/2017									  									#
#																	  								#
#Purpose:		-	Shiny App																		#
#Notes:			- 																					#
#																									#
#####################################################################################################

#ui.R

require(data.table)
require(shiny)

options(scipen=20)	
options(warn= -1)

fluidPage(

# Application title
titlePanel("American Community Survey - Statistical Snapshot For the Pacific - 2015"),

    # ACS Viewer - Pacific
	
	#State Layout
	sidebarLayout(
		sidebarPanel(width = 2,
			helpText("Select State"),	
			selectInput("State", "State:", 
                  choice = list("State")	
			)

		),
		mainPanel(
			tabsetPanel(
				#Income
				tabPanel("Income",
					splitLayout(cellWidths = c("40%", "30%", "30%"),
						 plotOutput("plot3a", height="400px", width="80%",
							dblclick = "plot3a_dblclick",
							brush = brushOpts(
								id = "plot3a_brush",
								resetOnNew = TRUE
							)
						),
						plotOutput("plot12a", height="400px", width="100%") #Wage Distribution
					),
				fluidRow(title="Top 5 and Bottom 5 Average Wages by PUMA",
					 tableOutput("tbl3a")
					) 
				),
				#Population
				tabPanel("Population",
					splitLayout(cellWidths = c("50%", "50%")
						, plotOutput("plot1a", height="400px", width="80%",
							dblclick = "plot1a_dblclick",
							brush = brushOpts(
								id = "plot1a_brush",
								resetOnNew = TRUE
							)
						)
						,plotOutput("plot2a", height="400px", width="80%",
							dblclick = "plot2a_dblclick",
							brush = brushOpts(
								id = "plot2a_brush",
								resetOnNew = TRUE
							)
						)
					),
				fluidRow(title="Top 5 and Bottom 5 Population Growth Rates by PUMA",
					 tableOutput("tbl1a")
					) 
				),	
				#Unemployment
				tabPanel("Unemployment",
					splitLayout(cellWidths = c("50%", "50%")
						, plotOutput("plot4a", height="400px", width="80%",
							dblclick = "plot4a_dblclick",
							brush = brushOpts(
								id = "plot4a_brush",
								resetOnNew = TRUE
							)
						)
						,plotOutput("plot5a", height="400px", width="80%",
							dblclick = "plot5a_dblclick",
							brush = brushOpts(
								id = "plot5a_brush",
								resetOnNew = TRUE
							)
						)
					),
				fluidRow(title="Top 5 and Bottom 5 Unemployment Rates by PUMA",
					 tableOutput("tbl4a")
					) 
				),
				#Other Graphics
				tabPanel("Age",
					fluidRow(
						plotOutput("plot6a", height="300px", width="500px")  #Age Pyramid
					),
					fluidRow(
					    plotOutput("plot7a", height="300px", width="500px")  #Age Unemployment Pyramid
					)
				),
				tabPanel("Occupation", 
					fluidRow(
						plotOutput("plot8a", height="300px", width="500px")  #Occupation Wages
					),
					fluidRow(
						plotOutput("plot9a", height="300px", width="500px")  #Occupation Employment				
					)
				),
				tabPanel("Industry",
					fluidRow(
						plotOutput("plot10a", height="300px", width="500px")  #Industry Wages
					),
					fluidRow(
						plotOutput("plot11a", height="300px", width="500px")  #Industry Employment					
					)					
				)
			)
		)
	)
)  #End Fluid Page
