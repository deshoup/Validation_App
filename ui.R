#Packages to be installed and loaded############################
library(shiny)
library(shinyjs)
library(V8)#package needed to refresh page...only used if we upload csv files
library(formattable) #used for formattableOutput() function to produce error table more easily
library(DT)
library(rhandsontable)
# jsResetCode <- "shinyjs.reset = function() {history.go(0)}" #Javascript needed to refresh page...will only need on file upload screen
useShinyjs() #used to enable/dissable download button

fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "ODWClogo.gif"),
            tags$title("Data Validation App")),
  
  # Application title with ODWC logos####
  titlePanel(
    wellPanel(
      fluidRow(
        column(3,align="center", img(src="ODWClogo.gif", height="auto", width="110px")),
        column(6, align="center", tags$b(h2(HTML(paste0(
                    "ODWC Data Validation Application<sup style='font-size: 0.3em; vertical-align: super;'>© ",
                    paste0("2018-",format(Sys.Date(), "%Y")),"</sup>")))),
               h4("quality testing for data to upload into Oklahoma Fishery Analysis Application"),
               hr(), 
               h5("Created by Daniel E. Shoup and Dray D. Carl")),
              #below line vertically centers OSU logo...sets height to 1210 px
                tags$style(HTML('
                      .verticalcenter {
                      display: table-cell;                      
                      height: 120px;
                      vertical-align: middle;
                      }')),
        column(3, align="center", img(src="osulogo.png", height="auto", width="auto",class="verticalcenter"))
      )
    ),
    windowTitle = "OK Fishery Analysis App" #this text is what appears as browser title
  ),
  tabsetPanel(type = c("tabs"),
              
              
    #Validate  Data Tab####
    tabPanel("Validate Data",
      hr(),
      fluidRow(
        column(6,
          sidebarPanel(width = 12,
             tags$head(tags$style(".butn{background-color:#FFA500;} .btn-file{background-color:#FFA500;}.butnTmplt{
                background-color:#C0C0C0;} .butnTmplt{color: white;} .butnTmplt{float: right;}.butnTmplt{top=0%} 
                .butnTmplt {margin-right: 30px;")),
                # above background color and font color for button that downloads template (class=butnTmplt)
                # regular buttons (class=butn), and the file upload browse button (class=btn-file, but I do not specify...this is named via Shiny)
            h4(tags$b("1) Upload sample data file (should only include one combination of gear and lake at a time)")),
            
            helpText("Instructions and considerations for sample data entry are available in 
                     Data Entry Instructions Tab above"),
            
            fileInput("sampleData", "Select data file to be validated"),
            helpText("Reminder: Uploaded data can only include one gear at a time."),
            helpText("Upload file (.csv) must have correct format and column headings. To see proper columns click on Dowload Template 
                     buttons below"),
            fluidRow(
              downloadButton("sampleTemplate", label = "Download Sampling Data Template",class="butnTmplt"),
              downloadButton("ageTemplate", label = "Download Age Data Template",class="butnTmplt"),
            ),
            hr(),
            h4(tags$b("2) Observe error status and fix as needed")),
            helpText("Fix any errors identified by the Error Identification Table.
                      As you edit errors below, the Error Identification Table will update"),
            # helpText("Use 3rd panel to identify specific row numbers violating each error rule."),
            hr(),
            h4(tags$b("3) Download validated sample data")),
            # helpText("If all error rules are 'Okay', dataset is ready to download."),
            
            
            h4(textOutput("downloadMessage")),
            uiOutput("downloadButtonUI"), #works best of the options
            helpText("The downloaded file will be configured so it can be
                     upload to main app and analyzed."),
          ),  
        ),
        
      ##Error test result column with red errors or green Okays for each test###
        # column(3, align = "center",
        
        column(6,aligh = "center",
            wellPanel(
              uiOutput("errorIdentificationType"),
              textOutput("debug"),
              helpText("Status 'Error' indicates one or more records violates the corresponding error rules."),
              helpText("Definitions of errors found in Validation Rule Details Tab above."),
                fluidRow(
                 column(6, formattableOutput("sampleError"), uiOutput("cantTest")),
                 column(6, formattableOutput("sampleError2"))
                )
            )
        )   
        
        
      ),
      
      uiOutput("howToEditSampleData"),
      
      #Display uploaded data in table (will include Wr)
      # hr(),
      fluidRow(
        # textOutput("Data can be filtered using buttons above to show rows specific to errors"),
        # DT::dataTableOutput("sampleDataDisplayTable")
        # DT::DTOutput("sampleDataDisplayTable")
        rHandsontableOutput("sampleDataDisplayTable")
      )
    ),
    
    #Validation Rules Tab####
    tabPanel("Validation Rule Details",
      hr(),
      fluidPage(
        column(1),
        column(7,
          wellPanel(
            h3(tags$b("Validation Rule Details"), align = "center"),
            hr(),
            h5(tags$b("These data validation rules were put in place for two reasons. 1) To help ensure quality data are being used by the ODWC
                      to aid in making informed fisheries management decisions.  This also helps provide data integrity of the ODWC Fisheries Database to help
                      back findings or views.  2) To help ensure data run smoothly through the main Oklahoma Fishery Analysis Application.
                      For these reasons, please use this app to produce the best quality data you can."), align = "left"),
            hr(),
            h4("Correct column names and order"),
              helpText("Use the Download Template to see the proper headings and column order.  You are required to use
                       these columns (the app will rearrange them to put them in the right order).  An error in this row 
                       indicates your spreadsheet had missing required columns or had extra columns that are not in the template."),
            h4("Blank cells instead of periods"),
              helpText("If a cell does not have a value, a period should be put in the cell to indicate it is intentionally
                       being left blank.  Just leaving the cell blank is not acceptable."),
            h4("Invalid Lake Code"),
              helpText("Lake Code must be present in the established list of lake codes and cannot be blank.  
                        Check for spelling, capitalization, etc.
                       This list can be found in the SSP Manual or in the panel to the right side of this tab."),
              helpText("If a new Lake Code is needed, contact Dan Shoup to add one to the established list."),
            h4("Blank Station"),
              helpText("Station is a required field and cannot be blank.  Include a station (numerical, character, or alpha-numeric) for 
                       each site sampled. It is important that each gear deployment at a given lake on a given day has a unique station value."),
            h4("Missing station ID's based on time codes"),
              helpText("There are more time codes than station ID values, suggesting the same station ID was used for two 
                       different samples.  Station ID is important to the function of the app, so this will lead to errors.
                       Each gear replicate must have a unique station ID on a given day."),
            h4("Invalid Month, Day, or Year"),
              helpText("Month, Day, and Year fields are required and cannot be blank."),
              helpText("Month must be an integer between 1 and 12."),
              helpText("Day must be an integer between 1 and 31."),
              helpText("Year must be an integer between 1980 and the current year."),
            h4("Year, month, & day make valid date"),
              helpText("The combination of month, day, and year must be a possible date (i.e., no February 30th is possible)."),
            h4("One Gear Code"),
              helpText("Due to the nature of checking Gear Length and Effort based on Gear Code, only one Gear Code can be uploaded and validated
                       at a time.  Make sure only one Gear Code is present in the sample data and that there are no blanks."),
            h4("Invalid Gear Code"),
              helpText("Gear Code must be present in the established list of gear codes and cannot be blank.
                       This list can be found in the SSP Manual or in the panel to the right on this tab."),
            h4("Invalid Gear Length"),
              helpText("This validation rule is based on the type of gear, and is required (cannot be blank)."),
              helpText("If electrofishing, Gear Length (minutes of electrofishing) must be between 1 and 300 (total minutes fished for station sample)."),
              helpText("If trapnetting, Gear Length (length of lead) should be > 5 ft and < 150 ft."),
              helpText("If gillnetting, Gear Length (length of net) should be > 10 ft and < 2000."),
              helpText("If seining, Gear Length must be > 5 ft and < 100 ft."),
            h4("Invalid Effort"),
              helpText("This validation rule is based on the type of gear, and is required (cannot be blank)."),
              helpText("If electrofishing, Effort (1 unit = 10 min) should be between 0.1 (1-min) and 24 (240 min or 4 hrs)."),
              helpText("If trapnetting or gillnetting, Effort (# hrs fished, decimal) must be between 0.1 and 96."),
              helpText("If seining, Effort (area sampled, ft^2) must be more than 100."),
            h4("Invalid Species Code"),
              helpText("Species Codes must be present in the established list of species codes and cannot be blank (blank).  This list can
                       be found in the SSP Manual or in the panel to the right on this tab.  Contact Dan Shoup if a new species code needs to be added
                       to this list."),
              helpText("Make sure to use Species Code 98 if not fish were caught in the sample (don't leave it blank).  This is also essential
                       to CPUE calculations."),
            h4("Invalid Number of Individuals"),
              helpText("The number of individuals field cannot be 0 and cannot be blank.  Even for species code 98 (no fish in sample), enter a
                       one for number of individuals.  This is crucial for proper calculation of CPUE."),
            h4("Invalid Total Length or Weight"),
              helpText("TL and Wt values must be numeric and >0 (or a period is used to indicate
                       no data were taken for this record).  It is impossible for a fish to
                       be 0 mm or weigh 0 grams.  If a length or weight was not measured for a fish, use '.' in the cell."),
            h4("Invalid Total Length (Age-only Data)"),
              helpText("Total Lengths cannot be 0 or blank in age-only data. If a length is not available for a fish that was aged, 
                       the entire record should be deleted if it is an age data set (i.e., delete the row from the file). If you are
                       uploading age data with standard sampling data, do not omit the fish as it is needed to get the proper
                       CPUE.  Instead, just do not list an age for a fish that has a missing length"),
            h4("Abnormally large or small TL"),
              helpText("fish is unusually large or small for the species based on a table of values the ODWC SSP committee
                       developed.  This will not prevent you from using these data, but you should double check the value of 
                       these rows to be sure the fish really were exeptionally large or small and that this is not a mistake.
                       If a weight or TL is suspect, it is probably better to delete it (replace with a period). Do not delete
                       the whole row though as that would change CPUE.  Instead leave the missing TL or weight but have the
                       number of individuals = 1 so this row still counts in CPUE calculations."),
            h4("Invalid Relative Weight"),
              helpText("This validation rule aims to flag fish that may have mis-measured a length or weight was mistyped.  Relative weights
                       are calculated for all fish possible, and fish that have relative weights < 50 or > 150 are flagged with an error.  If the
                      user is confident the length and weight are correct, this error may be bypassed.  If not, we recommend just leaving the length 
                       and weight blank ('.') for that particular fish unless you know for sure which is incorrect (again, do not delete the whole 
                       row as that would impact CPUE calculations)."),
            h4("Invalid Age"),
              helpText("Depending on species, ages must be an integer between 0-20 (most species codes), 0-40 (species 
                       codes 114, 115 & 401), or 0-100 (species codes 302-307, 311, 321, 325 or 402-405).  You cannot 
                       enter anything other than an integer (e.g., no values like: YOY, 10+, 
                       etc.).  Age is required (no blanks are allowed) on age data files but is optional (blanks ['.'] are ok) 
                       on sampling data files.  If ages are given in sampling data, fish with ages will be assigned the 
                       age in the file and any missing ages will be estimated using an age-length key approach."),
            h4("Invalid Sex Code"),
              helpText("Only 'M', 'F', 'U' (unknown, includes immature), or a '.' (indicating intentionally blank cell) 
                       are allowed.  'U' should be reserved for a situation where sex determination was attempted but
                       was unsuccessful...if you did not look, a '.' should be used instead."),
            h4("Invalid Latitude and Longitude"),
              helpText("Only latitudes and longitudes within Oklahoma are allowed (Latitudes from 33.6 to 37.0 degrees; 
                       Longitudes of -103.002 to -94.431).  These columns are optional and can be ommited, but if they 
                       are included, blank cells need to be marked with '.'"), 
            h4("Comments"),
            helpText("This column is optional but if it is included, all blank cells should be marked with '.'")
          )
        ),
        column(3, align = "center",
          wellPanel(
            h3(tags$b("Download Established Codes")),
            hr(),
            downloadButton("lakeCodeList", label = "Download list of lake codes"),
            downloadButton("gearCodeList", label = "Download list of gear codes"),
            downloadButton("sppCodeList", label = "Download list of species codes")
          )
        ),
        column(1)
      )
    ),
    tabPanel("Data Entry Instructions",
      hr(),
      mainPanel(width=12,
                fluidRow(
                  column(2),
                  column(8, align="center",
                         tags$iframe(style="height:700px; width:100%",
                                     src="user.guide.validation.pdf#page=1&view=FitH")
                  )      
                )        
      )
    ),
    tabPanel("ODWC SSP Manual",
      hr(),
      mainPanel(width=12,
                fluidRow(
                  column(2),
                  column(8, align="center",
                         tags$iframe(style="height:1300px; width:100%",
                                     src="odwc.ssp.ofaa.pdf#page=1&view=FitH")
                  )      
                )        
      )
    )
  )
)