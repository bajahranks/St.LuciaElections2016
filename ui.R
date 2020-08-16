## ui.R ##
library(shiny)
library(bs4Dash)
library(plotly)
library(BBmisc)
library(tractor.base)

candidatesDF = read.csv(file = "2016_Candidates.csv")

menu = bs4SidebarMenu(
   bs4SidebarMenuItem("Home", tabName = "home", icon = "building"),
   bs4SidebarMenuItem("Analysis", tabName = "analysis", icon = "eye"),
   
   bs4SidebarMenuItem("Extras", tabName = "extras", icon = "asterisk" ,                  
     bs4SidebarMenuSubItem("Candidates", tabName = "candidates", icon = "user-tie"),
     bs4SidebarMenuSubItem("Police Votes", tabName = "police", icon = "user-secret"),
     bs4SidebarMenuSubItem("Rejected Votes", tabName = "rejected_votes", icon = "times-circle")
   ),
    
   bs4SidebarMenuItem("Anse La Raye", tabName = "anse_la_raye_canaries", icon = "location-arrow"),
   bs4SidebarMenuItem("Babonneau", tabName = "babonneau", icon = "location-arrow"),
   bs4SidebarMenuItem("Castries Central", tabName = "castries_central", icon = "location-arrow"),
   bs4SidebarMenuItem("Castries East", tabName = "castries_east", icon = "location-arrow"),
   bs4SidebarMenuItem("Castries North", tabName = "castries_north", icon = "location-arrow"),
   bs4SidebarMenuItem("Castries South", tabName = "castries_south", icon = "location-arrow"),
   bs4SidebarMenuItem("Castries South East", tabName = "castries_south_east", icon = "location-arrow"),
   bs4SidebarMenuItem("Choiseul", tabName = "choiseul", icon = "location-arrow"),
   bs4SidebarMenuItem("Dennery North", tabName = "dennery_north", icon = "location-arrow"),
   bs4SidebarMenuItem("Dennery South", tabName = "dennery_south", icon = "location-arrow"),
   bs4SidebarMenuItem("Gros Islet", tabName = "gros_islet", icon = "location-arrow"),
   bs4SidebarMenuItem("Laborie", tabName = "laborie", icon = "location-arrow"),
   bs4SidebarMenuItem("Micoud North", tabName = "micoud_north", icon = "location-arrow"),
   bs4SidebarMenuItem("Micoud South", tabName = "micoud_south", icon = "location-arrow"),
   bs4SidebarMenuItem("Soufriere", tabName = "soufriere", icon = "location-arrow"),
   bs4SidebarMenuItem("Vieux-Fort North", tabName = "vieux_fort_north", icon = "location-arrow"),
   bs4SidebarMenuItem("Vieux-Fort South", tabName = "vieux_fort_south", icon = "location-arrow"),
   id = "left-menu"
)

home =  bs4TabItem(tabName = "home",
  fluidRow(
     bs4Card(
      title = "St. Lucia General Election Results 2016",
      p("This site provides visual representation of the 2016 elections in
        St. Lucia. The data was collected from the", 
        a(href="https://data.govt.lc/dataset/election-results", "St. Lucia Open Data") ,"website.
        The data had some inconsistencies and data verification and sanitization
        was needed before the data could be stuctured for visualization. A more
        detail report of my observations can be found in the Analysis section."),
      p("It was a joy building this site and was intrigued by the detailed
        results. I hope you find it enjoyable and can be put to use for further
        research."),
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      maximizable = TRUE,
      elevation = 4
    )
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("generalResults"), width = 8, status = "success",
        title = "Total votes by party",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4,
        height = 620
    ),
    column(width = 4, img(src ="img/st_lucia_elections_map.png", 
        alt = "Election results map of St. Lucia 2016", width = 450
      )
    )
  )
)

analysis = bs4TabItem(tabName = "analysis",
  bs4Jumbotron(
    title = "Analysis of St. Lucia Election - 2016",
    lead = div(class = "lead",
      p("I decided to work on this project to reinforced my newly gained 
        knowledge in R programming. I wanted to work on something unique to St. Lucia
        and found the election results of 2016 from the St. Lucia Open Data website."
      ),
      p("As of this writing the dataset from St. Lucia Open Data website had some
        noticable inconsistencies. At first glance one would notice that the 
        percentage total vote given to the SLP is 49.32% and the UWP is 61.25%.
        Mathematically this amounts to 110.57% which can not be accurate. 
        Immediately I began to dig deeper to see what else is inaccurate."
      ),
      p("You would also notice that LPM votes are not visible on the 2016_summary sheet
        and the number of rejected votes are also missing. The total number of votes
        cast is also inaccurate. Their dataset shows 75,785, however when I added the 
        LPM votes and the rejected votes I got 86,855. I was able to track down the
        error to Gros-Islet. It should have been 11,195 but 157 was entered. The number
        of votes not cast was also affected by Gros-Islet, where it should have been
        9,309 but 11,195 was entered. That would mean that the actual votes not cast 
        is 74,745 instead of the 76,613 that is shown. Another wierd observation 
        was that the number of police votes were added to the total for Gros-Islet
        to Soufriere but was left out from Choiseul to Castries South East." 
      ),
      p("Despite my efforts to clean up the data and make it as accurate as possible
        base on the raw data from the other sheets the numbers still didn't add up 
        when compared to the data from the Caribbean Elections website. According 
        to the dataset there where 162,222 registered voters, however the", 
        a(href = "http://www.caribbeanelections.com/lc/elections/lc_results_2016.asp",
        "Caribbean Elections"), "website has it at 161,883. My total votes cast 
        came to 86855, while their site has 86525. The total votes for districts 
        and parties in certain districts don't add up with the dataset made public
        on the St. Lucia Open Data website."
      ),
      p("I'm sure there may be reasons for the difference in data, such as a 
        possible recount, but hoping someone at the electoral department will
        see this and respond accordingly. If and when the data has been cleaned
        I will update the charts to reflect it."
        
      ),
    ),
  status = "info",
  btn_name = "Say Hi!",
  href = "https://www.facebook.com/RaviLamontagne"
  )
)                    

# Get number of rows from dataset
records = nrow(candidatesDF)

# Each row will have max. 3 items
numRows = ceiling(nrow(candidatesDF) / 3)
numRows = c(1:numRows)

count = 0
offset = 3

checkOffset = function(records, offset) {
  if (records < offset) {
    offset = records
  }
  
  return(offset) 
}


candidates =  bs4TabItem(tabName = "candidates",
 lapply(numRows, function(r) {
   fluidRow(
     lapply(1:(checkOffset(records, offset)), function(c) {
       count <<- count + 1
    
       # Convert the names to a more legible format
       name = explode(candidatesDF[count, "Candidate"], sep = ", ")
       name = paste0(name[2], ' ', name[1])
       name = capitalizeStrings(name, all.words = TRUE, lower.back = TRUE)

       # Convert the names to the img name format
       imgName = explode(name, sep = " ")
       imgName = tolower(implode(imgName, "_"))
       imgUrl = paste0("img/", imgName, ".png")
       
       # Get the colour
       partyColour = switch(candidatesDF[count, "Colour"], 
         "red" = "danger",
         "yellow" = "warning",
         "green" = "success",
         "blue" = "primary",
         "lightblue" = "info"
      ) 
       
       records <<- records - 1
       
       # Create a user card on each iteration.
       bs4UserCard(
         title = name,
         subtitle = candidatesDF[count, "Party"],
         type = NULL,
         width = 4,
         src = imgUrl,
         status = partyColour,
         closable = TRUE,
         elevation = 4,
         imageElevation = 4,
         fluidRow(
           column(
             width = 4,
             descriptionBlock(header = "District",
              text = capitalizeStrings(candidatesDF[count, "District"],
                     all.words = TRUE, lower.back = TRUE ))
           ),
           column(
             width = 4,
             descriptionBlock(header = "Votes",
              text = candidatesDF[count, "Votes"])
           ),
           column(
             width = 4,
             descriptionBlock(header = "Result",
              text = candidatesDF[count, "Result"], right_border = FALSE)
           )
         )
       )
     })
   )
 })                        
)

police =  bs4TabItem(tabName = "police",
  fluidRow(
     bs4Card(plotlyOutput("police"), width = 12, status = "success",
        title = "Votes by police per district",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

rejectedVotes =  bs4TabItem(tabName = "rejected_votes",
  fluidRow(
     bs4Card(plotlyOutput("rejectedVotes"), width = 12, status = "success",
        title = "Rejected votes by district",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

anseLaRayeCanaries =  bs4TabItem(tabName = "anse_la_raye_canaries", 
  fluidRow(
     bs4Card(plotlyOutput("anseLaRayeCanaries"), status = "success",
        title = "Results for Anse La Raye / Canaries",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("anseLaRayeCanariesPS"), width = 12, status = "success",
        title = "Polling station results for Anse La Raye / Canaries",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

babonneau =  bs4TabItem(tabName = "babonneau",
  fluidRow(
     bs4Card(plotlyOutput("babonneau"), status = "success",
        title = "Results for Babonneau",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("babonneauPS"), width = 12, status = "success",
        title = "Polling station results for Babonneau",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

castriesCentral =  bs4TabItem(tabName = "castries_central",
  fluidRow(
     bs4Card(plotlyOutput("castriesCentral"), status = "success",
        title = "Results for Castries Central",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("castriesCentralPS"), width = 12, status = "success",
        title = "Polling station results for Castries Central",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
  
)

castriesEast =  bs4TabItem(tabName = "castries_east",
  fluidRow(
     bs4Card(plotlyOutput("castriesEast"), status = "success",
        title = "Results for Castries East",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("castriesEastPS"), width = 12, status = "success",
        title = "Polling station results for Castries East",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
  
)
castriesNorth =  bs4TabItem(tabName = "castries_north",
  fluidRow(
     bs4Card(plotlyOutput("castriesNorth"), status = "success",
        title = "Results for Castries North",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("castriesNorthPS"), width = 12, status = "success",
        title = "Polling station results for Castries North",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

castriesSouth =  bs4TabItem(tabName = "castries_south",
  fluidRow(
     bs4Card(plotlyOutput("castriesSouth"), status = "success",
        title = "Results for Castries South",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("castriesSouthPS"), width = 12, status = "success",
        title = "Polling station results for Castries South",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

castriesSouthEast =  bs4TabItem(tabName = "castries_south_east",
  fluidRow(
     bs4Card(plotlyOutput("castriesSouthEast"), status = "success",
        title = "Results for Castries South East",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("castriesSouthEastPS"), width = 12, status = "success",
        title = "Polling station results for Castries South East",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

choiseul =  bs4TabItem(tabName = "choiseul",
  fluidRow(
     bs4Card(plotlyOutput("choiseul"), status = "success",
        title = "Results for Choiseul",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("choiseulPS"), width = 12, status = "success",
        title = "Polling station results for Choiseul",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

denneryNorth =  bs4TabItem(tabName = "dennery_north",
  fluidRow(
     bs4Card(plotlyOutput("denneryNorth"), status = "success",
        title = "Results for Dennery North",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("denneryNorthPS"), width = 12, status = "success",
        title = "Polling station results for Dennery North",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

dennerySouth =  bs4TabItem(tabName = "dennery_south", 
  fluidRow(
     bs4Card(plotlyOutput("dennerySouth"), status = "success",
        title = "Results for Dennery South",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("dennerySouthPS"), width = 12, status = "success",
        title = "Polling station results for Dennery South",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

grosIslet =  bs4TabItem(tabName = "gros_islet",
  fluidRow(
     bs4Card(plotlyOutput("grosIslet"), status = "success",
        title = "Results for Gros-Islet",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("grosIsletPS"), width = 12, status = "success",
        title = "Polling station results for Gros-Islet",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

laborie =  bs4TabItem(tabName = "laborie",
  fluidRow(
     bs4Card(plotlyOutput("laborie"), status = "success",
        title = "Results for Laborie",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("laboriePS"), width = 12, status = "success",
        title = "Polling station results for Laborie",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

micoudNorth =  bs4TabItem(tabName = "micoud_north",
  fluidRow(
     bs4Card(plotlyOutput("micoudNorth"), status = "success",
        title = "Results for Micoud North",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("micoudNorthPS"), width = 12, status = "success",
        title = "Polling station results for Micoud North",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

micoudSouth =  bs4TabItem(tabName = "micoud_south",
  fluidRow(
     bs4Card(plotlyOutput("micoudSouth"), status = "success",
        title = "Results for Micoud South",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("micoudSouthPS"), width = 12, status = "success",
        title = "Polling station results for Micoud South",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

soufriere =  bs4TabItem(tabName = "soufriere",
  fluidRow(
     bs4Card(plotlyOutput("soufriere"), status = "success",
        title = "Results for Soufriere",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("soufrierePS"), width = 12, status = "success",
        title = "Polling station results for Soufriere",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)

vieuxFortNorth =  bs4TabItem(tabName = "vieux_fort_north",
  fluidRow(
     bs4Card(plotlyOutput("vieuxFortNorth"), status = "success",
        title = "Results for Vieux-Fort North",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("vieuxFortNorthPS"), width = 12, status = "success",
        title = "Polling station results for Vieux-Fort North",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )
)
vieuxFortSouth =  bs4TabItem(tabName = "vieux_fort_south",
  fluidRow(
     bs4Card(plotlyOutput("vieuxFortSouth"), status = "success",
        title = "Results for Vieux-Fort South",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  ),
  
  fluidRow(
     bs4Card(plotlyOutput("vieuxFortSouthPS"), width = 12, status = "success",
        title = "Polling station results for Vieux-Fort South",
        solidHeader = TRUE, 
        collapsible = TRUE,
        closable = TRUE,
        maximizable = TRUE,
        elevation = 4)
  )  
)

navbar = bs4DashNavbar(
  a(class = "nav-link", 
    href = "https://data.govt.lc/dataset/election-results", "St. Lucia Open Data"
  ),
  a(class = "nav-link", href = "http://www.caribbeanelections.com/lc/elections/lc_results_2016.asp",
    "Caribbean Elections"
  ),
  fixed = TRUE
)

sideBar = bs4DashSidebar(menu,
  inputId = "brand",
  title = "St. Lucia Elections 2016",
  src = "img/ravi-lamontagne.jpg",
  brandColor = "primary",
  skin = "light"
)

body = bs4DashBody(
   bs4TabItems(
    home,
    analysis,
    police,
    rejectedVotes,
    candidates,
    anseLaRayeCanaries,
    babonneau,
    castriesCentral,
    castriesEast,
    castriesNorth,
    castriesSouth,
    castriesSouthEast,
    choiseul,
    denneryNorth,
    dennerySouth,
    grosIslet,
    laborie,
    micoudNorth,
    micoudSouth,
    soufriere,
    vieuxFortNorth,
    vieuxFortSouth
  )
)

footer = bs4DashFooter(p("Developed by Ravi Lamontagne"), 
 copyrights = "Free Use", fixed = TRUE,
 right_text = tags$span(
   tags$a(id="facebook", class = "", href = "https://www.facebook.com/RaviLamontagne", 
     tags$i(class = "fab fa-facebook fa-3x")
   ),
   tags$a(id="twitter", class = "", href = "https://twitter.com/RaviLamontagne", 
     tags$i(class = "fab fa-twitter fa-3x")
   ),
   tags$a(id="github", class = "", href = "https://github.com/bajahranks", 
     tags$i(class = "fab fa-github fa-3x")
   ),
   tags$a(id="drupal", class = "", href = "https://www.drupal.org/u/bajah1701", 
     tags$i(class = "fab fa-drupal fa-3x")
   ),
   tags$a(id="linkedin", class = "", href = "https://www.linkedin.com/in/ravilamontagne/", 
     tags$i(class = "fab fa-linkedin fa-3x")
   ),
  )
 )

ui = bs4DashPage(navbar, sideBar, body, footer, title = "Election Results")
