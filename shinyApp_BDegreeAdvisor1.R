library(shiny)
library(tibble)
library(DT)
library(devtools)
devtools::install_github("PHP2560-Statistical-Programming-R/r-package-courses-brown/BDegreeAdvisor")
library(BDegreeAdvisor)

# Compile a list of undergraduate concentrations available at Brown from the website, so 
# that if the concentrations are updated on the website, the list is also updated
library(rvest)
link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
conc_list <- link %>% 
  html_nodes("#textcontainer li") %>% # css selector for the entire list of concentration
  html_text() # select only the text 
conc_list <- as.vector(conc_list)
names(conc_list) <- conc_list

# Load package to use a predetermined theme
library(shinythemes)

ui <- navbarPage("BDegreeAdvisor", theme = shinytheme("united"),
                 tabPanel("Welcome",
                  # Create A header panel
                 em(headerPanel(h1("Welcome to BDegreeAdvisor",style= {"font-family:Georgia;color:#ad1d28"}))),
                 sidebarLayout(
                   #add the brown logo
                   sidebarPanel(img(src ='https://www.edx.org/sites/default/files/school/image/logo/brown_200x101.png', aligh = "right")),
                          mainPanel(
                            # Add the text for the main panel
                            
                            h3("How to use the app",style="font-family:Georgia"),
                            tags$p(" Welcome to BDegree Advisor, the one stop app to help students figure out their concentration, create their schedule, and search for jobs all in one place!",style="font-family:Georgia"),
                            tags$br(),
                            tags$p("In order to use this app, simply navigate to the different tabs above. Each tab consists of drop down menus where you can choose a concentration and get an up to date table of the requirements, or cross reference and compare requirements between concentrations. You can also find out what courses are being offered in the upcoming semesters and customize your schedule. Lastly you can populate a search query to find up to date job postings that fit your specifications!",style="font-family:Georgia"),
                            tags$br(),
                            tags$p("If you would like to save the output onto your device, you can simply click the 'Download' button featured in the tab and save the file onto your computer.",style="font-family:Georgia"),
                            tags$br(),
                            tags$p(" We hope you enjoy this app and that it can help guide you during your time at Brown!",style="font-family:Georgia")
                          )
                          
                 )),
                 tabPanel("Concentration Requirements",
                          sidebarLayout(
                            sidebarPanel(
                              # Add Brown Logo
                              img(src ='https://www.edx.org/sites/default/files/school/image/logo/brown_200x101.png', aligh = "left"),
                              # Select concentration
                              selectInput("selected_conc", label = h5("Select a concentration"), 
                                          choices = names(conc_list), selected = conc_list[29]), 
                              # Submit button
                              actionButton("submit", "See table of requirements!"),
                              # Download button
                              h5("Download the selected table"),
                              downloadButton("downloadData", "Download",class="btn btn-primary btn-sm")
                              
                            ),
                            mainPanel(
                              # Display table of concentration requirements 
                              DT::dataTableOutput("conc_table")
                            )
                          )
                 ),
                 tabPanel("Compare Concentration Requirements",  sidebarLayout(
                   sidebarPanel(
                     # Add Brown Logo
                     img(src ='https://www.edx.org/sites/default/files/school/image/logo/brown_200x101.png', aligh = "left"),
                     # Select concentration
                     selectInput("selected_conc1", label = h5("Select concentration 1"), 
                                 choices = names(conc_list), selected = conc_list[29]), 
                     selectInput("selected_conc2", label = h5("Select concentration 2"), 
                                 choices = names(conc_list), selected = conc_list[78]), 
                     # Submit button
                     actionButton("submit2", label = h5("See table of requirements!"))
                     
                   ),
                   mainPanel(
                     # Display table of concentration requirements 
                     DT::dataTableOutput("conc_comp") 
                   )
                 )
                 ),
                 tabPanel("Classes available per semester",
                          sidebarLayout(
                            sidebarPanel(
                              # Add Brown Logo
                              img(src ='https://www.edx.org/sites/default/files/school/image/logo/brown_200x101.png', aligh = "left"),
                              selectInput("conc_name", "Concentration", choices = names(conc_list), selected = conc_list[29]),
                              selectInput("term", "Semester", choices = c("fall", "winter", "spring"), selected = "fall")
                            ),
                            mainPanel(
                              h4("Courses", style= {"font-family:Georgia;color:#ad1d28"}),
                              dataTableOutput("course_table")
                            )
                          )
                          ),
                 tabPanel("Find Your Next Job",
                          sidebarLayout(
                            sidebarPanel(
                              # Add Brown Logo
                              img(src ='https://www.edx.org/sites/default/files/school/image/logo/brown_200x101.png', aligh = "left"),
                              textInput(inputId = "query",label="Job Title",value="",placeholder = "eg: data analyst"),
                              textInput(inputId = "loc",label="Location",value="",placeholder = "eg: Providence,RI or 02912"),
                              actionButton("submit3",label=h5("Go!")),
                              downloadButton('downloadJobs','Download',class="btn btn-primary btn-sm")
                            ),
                            mainPanel(
                              #Display table of job postings
                              DT::dataTableOutput(outputId = "jobtable")
                            )))
)


server <- function(input, output) {
  ################################################Welcome Tab ###################################################

  ################################################# Tab 1   #################################################
  conc_name <- eventReactive(input$submit, {
    ## Code used to change values of choices in switch function
    #numbering <- vector(list, length=length(conc_list))
    #for (i in 1:length(conc_list)) {
    #numbering[i] <- paste0(conc_list[[i]], " = ", i)
    #}
    
    switch(input$selected_conc,
           "Africana Studies" = 1, 
           "American Studies" = 2, 
           "Anthropology" = 3, 
           "Applied Mathematics" = 4,                         
           "Applied Mathematics-Biology" = 5,
           "Applied Mathematics-Computer Science" = 6,          
           "Applied Mathematics-Economics" = 7,
           "Archaeology and the Ancient World" = 8,  
           "Architecture" = 9,
           "Astronomy" = 10, 
           "Behavioral Decision Sciences" = 11, 
           "Biochemistry & Molecular Biology" = 12, 
           "Biology" = 13, 
           "Biomedical Engineering" = 14, 
           "Biophysics" = 15, 
           "Business, Entrepreneurship and Organizations" = 16,
           "Chemical Physics" = 17,                              
           "Chemistry" = 18,                                    
           "Classics" = 19,                                      
           "Cognitive Neuroscience" = 20,                     
           "Cognitive Science" = 21,                          
           "Comparative Literature" = 22,                       
           "Computational Biology" = 23,                         
           "Computer Science" = 24,                            
           "Computer Science-Economics" = 25,                    
           "Contemplative Studies" = 26,                        
           "Development Studies" = 27,                           
           "East Asian Studies" = 28,                           
           "Economics" = 29,                                     
           "Education Studies" = 30,                            
           "Egyptology and Assyriology" = 31,                    
           "Engineering" = 32,                                  
           "Engineering and Physics" = 33,                      
           "English" = 34,                                     
           "Environmental Studies" = 35,                        
           "Ethnic Studies" = 36,                               
           "French and Francophone Studies" = 37,                
           "Gender and Sexuality Studies" = 38,                 
           "Geological Sciences" = 39, 
           "Geology-Biology" = 40, 
           "Geology-Chemistry" = 41, 
           "Geology-Physics/Mathematics" = 42, 
           "German Studies" = 43, 
           "Health & Human Biology" = 44, 
           "Hispanic Literatures and Culture" = 45, 
           "History" = 46, 
           "History of Art and Architecture" = 47, 
           "Independent Concentration" = 48, 
           "International Relations" = 49, 
           "Italian Studies" = 50,                              
           "Judaic Studies" = 51, 
           "Latin American and Caribbean Studies" = 52, 
           "Linguistics" = 53,                                   
           "Literary Arts" = 54,                                
           "Mathematics" = 55,
           "Mathematics-Computer Science" = 56,                 
           "Mathematics-Economics" = 57, 
           "Medieval Cultures" = 58,                            
           "Middle East Studies" = 59, 
           "Modern Culture and Media" = 60, 
           "Music" = 61, 
           "Neuroscience" = 62, 
           "Philosophy" = 63, 
           "Physics" = 64, 
           "Physics and Philosophy" = 65, 
           "Political Science" = 66,                            
           "Portuguese and Brazilian Studies" = 67, 
           "Psychology" = 68,                                   
           "Public Health" = 69, 
           "Public Policy" = 70,                                
           "Religious Studies" = 71,
           "Renaissance and Early Modern Studies" = 72, 
           "Science and Society" = 73, 
           "Slavic Studies" = 74,                               
           "Social Analysis and Research" = 75,                  
           "Sociology" = 76, 
           "South Asian Studies" = 77, 
           "Statistics" = 78,                                   
           "Theatre Arts and Performance Studies" = 79, 
           "Urban Studies" = 80, 
           "Visual Art" = 81)                                  
  }, ignoreNULL = FALSE) 
  
  table_req <- reactive({
    library(dplyr)
    # Pull up the website that has a list of all the undergraduate concentrations
    link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
    # Select the concentration of interest
    link_conc <- link %>% follow_link(conc_name()+36)      
    # Read the content of the link
    content <- read_html(link_conc)
    # Scrape the table
    link_table <- html_nodes(content, 'table')
    # If the department doesn't display a table, an error "subscript out of bounds" appears. tryCatch will 
    # ignore this error and allow the function to keep working
    scrape_table <- tryCatch(html_table(link_table)[[1]], error=function(e) matrix(nrow=2, ncol=2))
    
    # Create a table only if the table exists (i.e. if scrape table is not NA)
    if (is.na(scrape_table[1,1]) == FALSE) {
      # Convert the table into a dataframe  
      classes <- scrape_table$X1
      class_name <- scrape_table$X2
      number_classes <- scrape_table$X3
      
      table_req1 <- data_frame(classes, class_name, number_classes)
      table_req1$number_classes[table_req1$number_classes == ""] <- " "
      
      explain <- list("", "", "If the Class Number cell is empty or has a NA, refer to the category the class belongs to.")
      table_req2 <- rbind(table_req1, explain)
      table_req1re <- rename(table_req2, "Class Code" = classes, "Class Name" = class_name, "Number of Classes" = number_classes)
    } else {stop('This department does not have a table of requirements')}
  })
  
  # Create the table as a tibble
  output$conc_table <-
    DT::renderDataTable({table_req()},escape=FALSE)
  
  
  # Download Table of Requirements
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$selected_conc,"Requirements", ".csv", sep = "")
    },
    content = function(file) {
      write.csv({
        # Pull up the website that has a list of all the undergraduate concentrations
        link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
        # Select the concentration of interest
        link_conc <- link %>% follow_link(conc_name()+36)      
        # Read the content of the link
        content <- read_html(link_conc)
        # Scrape the table
        link_table <- html_nodes(content, 'table')
        # If the department doesn't display a table, an error "subscript out of bounds" appears. tryCatch will 
        # ignore this error and allow the function to keep working
        scrape_table <- tryCatch(html_table(link_table)[[1]], error=function(e) print(NA))
        # Create a table only if the table exists (i.e. if scrape table ??? NA)
        if (is.na(scrape_table) == FALSE) {
          # Convert the table into a dataframe  
          classes <- scrape_table$X1
          class_name <- scrape_table$X2
          number_classes <- scrape_table$X3
          
          table_req1 <- data_frame(classes, class_name, number_classes)
          table_req1$number_classes[table_req1$number_classes == ""] <- " "
          
          explain <- list("", "", "If the Class Number cell is empty or has a NA, refer to the category the class belongs to.")
          table_req2 <- rbind(table_req1, explain)
          table_req1re<- rename(table_req2, "Class Code" = classes, "Class Name" = class_name, "Number of Classes" = number_classes)
        } else {stop('This department does not have a table of requirements')}
      }, file, row.names = FALSE)
    }
  )
  
  
  ################################################# Tab 2   ################################################# 
  conc_name1 <- eventReactive(input$submit2, {
    switch(input$selected_conc1,
           "Africana Studies" = 1, 
           "American Studies" = 2, 
           "Anthropology" = 3, 
           "Applied Mathematics" = 4,                         
           "Applied Mathematics-Biology" = 5,
           "Applied Mathematics-Computer Science" = 6,          
           "Applied Mathematics-Economics" = 7,
           "Archaeology and the Ancient World" = 8,  
           "Architecture" = 9,
           "Astronomy" = 10, 
           "Behavioral Decision Sciences" = 11, 
           "Biochemistry & Molecular Biology" = 12, 
           "Biology" = 13, 
           "Biomedical Engineering" = 14, 
           "Biophysics" = 15, 
           "Business, Entrepreneurship and Organizations" = 16,
           "Chemical Physics" = 17,                              
           "Chemistry" = 18,                                    
           "Classics" = 19,                                      
           "Cognitive Neuroscience" = 20,                     
           "Cognitive Science" = 21,                          
           "Comparative Literature" = 22,                       
           "Computational Biology" = 23,                         
           "Computer Science" = 24,                            
           "Computer Science-Economics" = 25,                    
           "Contemplative Studies" = 26,                        
           "Development Studies" = 27,                           
           "East Asian Studies" = 28,                           
           "Economics" = 29,                                     
           "Education Studies" = 30,                            
           "Egyptology and Assyriology" = 31,                    
           "Engineering" = 32,                                  
           "Engineering and Physics" = 33,                      
           "English" = 34,                                     
           "Environmental Studies" = 35,                        
           "Ethnic Studies" = 36,                               
           "French and Francophone Studies" = 37,                
           "Gender and Sexuality Studies" = 38,                 
           "Geological Sciences" = 39, 
           "Geology-Biology" = 40, 
           "Geology-Chemistry" = 41, 
           "Geology-Physics/Mathematics" = 42, 
           "German Studies" = 43, 
           "Health & Human Biology" = 44, 
           "Hispanic Literatures and Culture" = 45, 
           "History" = 46, 
           "History of Art and Architecture" = 47, 
           "Independent Concentration" = 48, 
           "International Relations" = 49, 
           "Italian Studies" = 50,                              
           "Judaic Studies" = 51, 
           "Latin American and Caribbean Studies" = 52, 
           "Linguistics" = 53,                                   
           "Literary Arts" = 54,                                
           "Mathematics" = 55,
           "Mathematics-Computer Science" = 56,                 
           "Mathematics-Economics" = 57, 
           "Medieval Cultures" = 58,                            
           "Middle East Studies" = 59, 
           "Modern Culture and Media" = 60, 
           "Music" = 61, 
           "Neuroscience" = 62, 
           "Philosophy" = 63, 
           "Physics" = 64, 
           "Physics and Philosophy" = 65, 
           "Political Science" = 66,                            
           "Portuguese and Brazilian Studies" = 67, 
           "Psychology" = 68,                                   
           "Public Health" = 69, 
           "Public Policy" = 70,                                
           "Religious Studies" = 71,
           "Renaissance and Early Modern Studies" = 72, 
           "Science and Society" = 73, 
           "Slavic Studies" = 74,                               
           "Social Analysis and Research" = 75,                  
           "Sociology" = 76, 
           "South Asian Studies" = 77, 
           "Statistics" = 78,                                   
           "Theatre Arts and Performance Studies" = 79, 
           "Urban Studies" = 80, 
           "Visual Art" = 81)                                  
  }, ignoreNULL = FALSE)  
  
  conc_name2 <- eventReactive(input$submit2, {
    switch(input$selected_conc2,
           "Africana Studies" = 1, 
           "American Studies" = 2, 
           "Anthropology" = 3, 
           "Applied Mathematics" = 4,                         
           "Applied Mathematics-Biology" = 5,
           "Applied Mathematics-Computer Science" = 6,          
           "Applied Mathematics-Economics" = 7,
           "Archaeology and the Ancient World" = 8,  
           "Architecture" = 9,
           "Astronomy" = 10, 
           "Behavioral Decision Sciences" = 11, 
           "Biochemistry & Molecular Biology" = 12, 
           "Biology" = 13, 
           "Biomedical Engineering" = 14, 
           "Biophysics" = 15, 
           "Business, Entrepreneurship and Organizations" = 16,
           "Chemical Physics" = 17,                              
           "Chemistry" = 18,                                    
           "Classics" = 19,                                      
           "Cognitive Neuroscience" = 20,                     
           "Cognitive Science" = 21,                          
           "Comparative Literature" = 22,                       
           "Computational Biology" = 23,                         
           "Computer Science" = 24,                            
           "Computer Science-Economics" = 25,                    
           "Contemplative Studies" = 26,                        
           "Development Studies" = 27,                           
           "East Asian Studies" = 28,                           
           "Economics" = 29,                                     
           "Education Studies" = 30,                            
           "Egyptology and Assyriology" = 31,                    
           "Engineering" = 32,                                  
           "Engineering and Physics" = 33,                      
           "English" = 34,                                     
           "Environmental Studies" = 35,                        
           "Ethnic Studies" = 36,                               
           "French and Francophone Studies" = 37,                
           "Gender and Sexuality Studies" = 38,                 
           "Geological Sciences" = 39, 
           "Geology-Biology" = 40, 
           "Geology-Chemistry" = 41, 
           "Geology-Physics/Mathematics" = 42, 
           "German Studies" = 43, 
           "Health & Human Biology" = 44, 
           "Hispanic Literatures and Culture" = 45, 
           "History" = 46, 
           "History of Art and Architecture" = 47, 
           "Independent Concentration" = 48, 
           "International Relations" = 49, 
           "Italian Studies" = 50,                              
           "Judaic Studies" = 51, 
           "Latin American and Caribbean Studies" = 52, 
           "Linguistics" = 53,                                   
           "Literary Arts" = 54,                                
           "Mathematics" = 55,
           "Mathematics-Computer Science" = 56,                 
           "Mathematics-Economics" = 57, 
           "Medieval Cultures" = 58,                            
           "Middle East Studies" = 59, 
           "Modern Culture and Media" = 60, 
           "Music" = 61, 
           "Neuroscience" = 62, 
           "Philosophy" = 63, 
           "Physics" = 64, 
           "Physics and Philosophy" = 65, 
           "Political Science" = 66,                            
           "Portuguese and Brazilian Studies" = 67, 
           "Psychology" = 68,                                   
           "Public Health" = 69, 
           "Public Policy" = 70,                                
           "Religious Studies" = 71,
           "Renaissance and Early Modern Studies" = 72, 
           "Science and Society" = 73, 
           "Slavic Studies" = 74,                               
           "Social Analysis and Research" = 75,                  
           "Sociology" = 76, 
           "South Asian Studies" = 77, 
           "Statistics" = 78,                                   
           "Theatre Arts and Performance Studies" = 79, 
           "Urban Studies" = 80, 
           "Visual Art" = 81)                                  
  }, ignoreNULL = FALSE)   
  
  
  
  output$conc_comp <- DT::renderDataTable({
    # Pull up the website that has a list of all the undergraduate concentrations
    link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
    # Select the concentration of interest
    link_conc1 <- link %>% follow_link(conc_name1()+36)
    link_conc2 <- link %>% follow_link(conc_name2()+36)
    # Read the content of the link
    content1 <- read_html(link_conc1)
    content2 <- read_html(link_conc2)
    # Scrape the table
    link_table1 <- html_nodes(content1, 'table')
    link_table2 <- html_nodes(content2, 'table')
    # If the department doesn't display a table, an error "subscript out of bounds" appears. tryCatch will 
    # ignore this error and allow the function to keep working
    scrape_table1 <- tryCatch(html_table(link_table1)[[1]], error=function(e)  matrix(nrow=2, ncol=2))
    scrape_table2 <- tryCatch(html_table(link_table2)[[1]], error=function(e)  matrix(nrow=2, ncol=2))
    
    # Create a table only if the table exists (i.e. if scrape table ??? NA)
    if ((is.na(scrape_table1[1,1])== FALSE) && (is.na(scrape_table2[1,1]) == FALSE)) {
      # This code chunk puts the tables for each concentration together, one after the other 
      ## Table 1 
      classes <- scrape_table1$X1
      class_name <- scrape_table1$X2
      number_classes <- scrape_table1$X3
      
      table_req1 <- tibble(classes, class_name, number_classes)
      table_req1$number_classes[table_req1$number_classes == ""] <- " "
      
      # Add spaces between the two tables
      space1 <- list("-------------------------------------------------", "-------------------------------------------------", "---------------------------------------")
      space2 <- list("-------------------------------------------------", "-------------------------------------------------", "---------------------------------------")
      
      #Add a title for each table
      name1 <- list("Concentration 1: ", "", "")
      name2 <- list("Concentration 2: ", "", "")
      
      table_req1s <- rbind(name1, table_req1)
      table_req1s <- rbind(table_req1s, space1)
      table_req1s <- rbind(table_req1s,space2)
      
      ## Table 2
      classes <- scrape_table2$X1
      class_name <- scrape_table2$X2
      number_classes <- scrape_table2$X3
      table_req2 <- tibble(classes, class_name, number_classes)
      table_req2 <- rbind(name2, table_req2)
      
      # Join the two tables
      total <- rbind(table_req1s, table_req2)
      
      # Get rid of the NAs in the number of classes
      total$number_classes[total$number_classes == ""] <- " "
      
      # Rename the columns
      table_req_2<-rename(total, "Class Code" = classes, "Class Name" = class_name, "Number of Classes" = number_classes)
      
      # Add an explanation about empty cell or NA in the number of classes
      explain <- list("", "", "If the Class Number cell is empty or has a NA, refer to the category the class belongs to.")
      
      rbind(table_req_2, explain)
      
    } else {stop('One of the concentrations does not have a table presented')}
  }, escape = FALSE)  

  
  ################################################# Tab 3   ################################################# 
  
  courses <- reactive({
    # read in json and store info to variable
    url <- "https://cab.brown.edu/asoc-api/?output=json&page=asoc.rjs&route=search&term=999999"
    all_courses <- jsonlite::fromJSON(url, flatten=TRUE)[["courses"]]
    all_courses <- all_courses[, c(1:3, 8)]
    
    # get required courses for concentration & check that concentration is valid
    tryCatch(suppressMessages(required <- conc_req(input$conc_name)), error=function(e) return(stop("Concentration not found.")))
    
    # pull out course codes
    required <- unlist(stringr::str_match_all(required[, 1:2],"[A-Z]{3,4}[:space:][0-9]{4}[A-Z]*"))
    required <- as.character(required)
    
    # split up course code into department, course code, and letter
    required <- data.frame(code = required, 
                           subj = stringr::str_extract(required, "^[A-Z]{3,4}"),
                           course_code = as.numeric(stringr::str_extract(required, "[0-9]{4}")), 
                           letter = stringr::str_extract(required, "[A-Z]*$"),
                           stringsAsFactors = FALSE)
    
    # make columns for up to three possible terms listed that the course runs in 
    all_courses <- dplyr::mutate(all_courses, term1 = as.numeric(sapply(all_courses$terms, function(x) x[1])), 
                                 term2 = as.numeric(sapply(all_courses$terms, function(x) x[2])), 
                                 term3 = as.numeric(sapply(all_courses$terms, function(x) x[3])))
    # remove unneeded column
    all_courses <- all_courses[,-4]
    
    # set semester 
    if(input$term == "fall"){
      sem <- 201710
    } else if (input$term == "spring") {
      sem <- 201720
    } else if (input$term == "winter") {
      sem <- 201715
    }
    
    # filter courses by selected semester
    current_term <- dplyr::filter(all_courses, term1 == sem | term2 == sem | term3 == sem)  
    # split course code into subject, course code, and letter and remove any course lists
    current_term <- dplyr::filter(current_term, !stringr::str_detect(current_term$code, 'XLIST'))
    current_term <- dplyr::mutate(current_term, course_code = stringr::str_extract(current_term$code, "[0-9]{4}"))
    current_term <- dplyr::mutate(current_term, letter = stringr::str_extract(current_term$code, "[A-Z]*$"))
    # change type of course code from chr to num
    current_term$course_code <- as.numeric(current_term$course_code)
    
    #filter for rows in current term courses that are in the respective fields of required courses
    final <- dplyr::filter(current_term, subj %in% required$subj &
                             course_code %in% required$course_code &
                             # subset only course code and title
                             letter %in% required$letter)[,1:2] 
    final <- as.data.frame(final)
    names(final) <- c("Course Code", "Course Title")
    final
  })
  output$course_table <- renderDataTable({courses()})
  
  
  ################################################# Tab 4   ################################################# 
 
  

  
job_finder<-reactive({
    input$submit3

    # isolate the inputs 
    querys<-isolate(as.character(input$query))
    locs<-isolate(as.character(input$loc))
    
    # Create an empty tibble to store results in
    indeed_job_compiled<-tibble("Hiring Company"=character(),
                                "Job Title"=character(),
                                "Description"=character(),
                                "Location"=character(),
                                "Job Link"=character()) 
    
    # Divide the url into parts and update the URL based on the user's inputs
    b= seq(from= 10, to= 50, by=10)
    urls=vector(length=length(b)+1)
    url_part1<-"https://www.indeed.com/jobs?q="
    url_part2<-"&l="
    url_part3<-"&start="  
    
    # The first page of search results has a slightly different URL structure and so we account for that here
    first_page<-paste0(url_part1,querys,url_part2,locs)
    
    urls[1]<-gsub(pattern=" ",replacement = "+",first_page)
    
    # This loops runs through all pages of search results and pastes together the appropriate URLS
    for (i in 1:length(b)){
      
      url<-paste0(url_part1,querys,url_part2,locs,url_part3,b[i])  
      
      urls[i+1]<-gsub(pattern=" ",replacement = "+",url) 
    }
    
    # This loop runs through each page of search results and scrapes the desired information
    for ( j in 1:length(urls)){
      
      session1<-html_session(urls[j])
      
      # grab the titles of the jobs
      job_titles<-session1 %>%
        html_nodes("[data-tn-element=jobTitle]") %>%
        html_text() 
      
      # The name of the Organization that is hiring
      company_names<-session1 %>%
        html_nodes(".company") %>%
        html_text()
      
      # Where is the job located ?
      location<-session1 %>%
        html_nodes(".location") %>%
        html_text()
      # Job Description
      description <- session1 %>%
        html_nodes(".summary") %>%
        html_text()
      
      # Give the user a link to the job so that they can apply to it later
      job_link<- session1%>%
        html_nodes(css= "[data-tn-element=jobTitle]") %>%
        html_attr("href")
      
      job_link<-paste('https://www.indeed.com',job_link,sep = '')
      job_link<-paste(job_link, ')', sep='')
      
      # Create a label for the url so that a user can click on the words "Apply Here!" and be taken to the appropriate website
      label<- "Apply Here!"
      job_link<-paste0("<a href='",job_link,"'>",label,"</a>")
      
      # create a tibble with all the scraped information and rename the column names
      df_new<-tibble(company_names,job_titles,description,location,job_link)
      df_new<- df_new %>%
              dplyr::rename("Hiring Company"=company_names,"Job Title"=job_titles,"Description"=description,"Location"=location,"Job Link"=job_link)
      
      # bind all the results from all the search pages together
      indeed_job_compiled<- dplyr::bind_rows(indeed_job_compiled,df_new)
      
    }
    indeed_job_compiled
  })
  
  #Output the results as a Data Table
  output$jobtable<-
    DT::renderDataTable({job_finder()},escape=FALSE)
  
  # Create the output for the download button
  output$downloadJobs <- downloadHandler(
    filename = function() {
      paste(input$query,"Job Postings", ".csv", sep = "")
    },
    content = function(file) {
      write.csv({job_finder()},file)
               
    }

)

}

shinyApp(ui=ui, server=server)  
