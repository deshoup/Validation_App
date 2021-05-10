#Packages to be installed and loaded############################
library(shiny)
library(shinyjs)
library(V8)#package needed to refresh page...only used if we upload csv files
library(formattable)
library(DT)
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" #Javascript needed to refresh page...will only need on file upload screen


fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "ODWClogo.gif"),
            tags$title("Data Validation App")),
  useShinyjs(),
  
  # Application title with ODWC logos
  titlePanel(
    wellPanel(
      fluidRow(
        column(3,align="center", img(src="ODWClogo.gif", height="auto", width="150px")),
        column(6, align="center", tags$b(h2("ODWC Data Validation Application")),
               h4("for input into Oklahoma Fishery Analysis Application"),
               hr(), 
               h5("Created by Dray D. Carl and Daniel E. Shoup")),
        column(3, align="center",img(src="osulogo.png", height="auto", width="180px"))
      )
    ),
    windowTitle = "OK Fishery Analysis App" #this text is what appears as browser title
  ),
  tabsetPanel(type = c("tabs"),
    tabPanel("Validate Sample Data",
      hr(),
      fluidRow(
        column(5,
          sidebarPanel(width = 12,
             tags$head(tags$style(".butn{background-color:#FFA500;} .btn-file{background-color:#FFA500;}.butnTmplt{
                background-color:#C0C0C0;} .butnTmplt{color: white;} .butnTmplt{float: right;}.butnTmplt{top=0%}")),
                # above background color and font color for button that downloads template (class=butnTmplt)
                # regular buttons (class=butn), and the file upload browse button (class=btn-file, but I do not specify...this is named via Shiny)
            h4(tags$b("1) Upload sample data file (should only include one combination of gear and lake at a time)")),
            
            helpText("Instructions and considerations for sample data entry are available in 
                     Data Entry Instructions Tab"),
            
            fileInput("sampleData", "Upload Fish Sample Data"),
            helpText("Reminder: Uploaded data can only include one gear at a time."),
            helpText("Upload file (.csv) must have correct format and column headings. To see proper columns click on Dowload Template 
                     button below"),
            downloadButton("sampleTemplate", label = "Download Template",class="butnTmplt"),
            
            HTML('<br/>', '<br/>'),
            #checkboxInput("period2NA", "Change Periods to NA's", value = FALSE), #decided to hard code this
            hr(),
            h4(tags$b("2) Initiate sample data validation check")),
            actionButton("validateSamp", "Initiate Validation",class="butn"),
            helpText("Error Identification Table will appear in middle panel."),
            hr(),
            h4(tags$b("3) Observe error status and fix as needed")),
            helpText("Fix any errors identified by the data validation function (Error Identification Table).
                      Then refresh page, reload data, and run validation function again until satisfied."),
            helpText("Use 3rd panel to identify specific row numbers violating each error rule."),
            hr(),
            h4(tags$b("4) Download validated sample data")),
            helpText("If all error rules are 'Okay', dataset is ready to download."),
            helpText("Validated data will have required SampleID field and will be ready to
                     upload to main app."),
            downloadButton("valSampleData", "Download validated sample data",class="butn")
          )  
        ),
        column(3, align = "center",
          wellPanel(
            h4(tags$b("Error Identification Table")),
            helpText("Status 'Error' indicates one or more records violates the corresponding error rules."),
            helpText("Definitions of errors found in Validation Rule Details Tab."),
            formattableOutput("sampleError")
          )
        ),
        column(4,
          wellPanel(
            h4(tags$b("Identify Row Numbers with Errors"), align = "center"),
            hr(),
            checkboxInput("Blanks", "Invalid Blank Cells (should be periods)"),
              textOutput("Blanks"),
            checkboxInput("sampLake", "Invalid Lake Code"),
              textOutput("sampLake"),
            checkboxInput("sampStation", "Blank Station"),
              textOutput("sampStation"),
            checkboxInput("sampMonth", "Invalid Month"),
              textOutput("sampMonth"),
            checkboxInput("sampDay", "Invalid Day"),
              textOutput("sampDay"),
            checkboxInput("sampYear", "Invalid Year"),
              textOutput("sampYear"),
            checkboxInput("sampGear", "Invalid Gear Code"),
              textOutput("sampGear"),
            checkboxInput("sampLength", "Invalid Gear Length"),
              textOutput("sampLength"),
            checkboxInput("sampEffort", "Invalid Effort"),
              textOutput("sampEffort"),
            checkboxInput("sampSpp", "Invalid Species Code"),
              textOutput("sampSpp"),
            checkboxInput("sampNOI", "Invalid Number of individuals"),
              textOutput("sampNOI"),
            checkboxInput("sampTL", "Invalid Total Length"),
              textOutput("sampTL"),
            checkboxInput("sampWt", "Invalid Weight"),
              textOutput("sampWt"),
            checkboxInput("sampWr", "Invalid Relative Weight"),
              textOutput("sampWr")
          )
        )
      ),
      #Display uploaded data in table (will include Wr)
      hr(),
      fluidRow(
        textOutput("Data can be filtered using buttons above to show rows specific to errors"),
        DT::dataTableOutput("sampleDataDisplayTable")
      )
    ),
    
    tabPanel("Validate Age Data",
      hr(),
      fluidRow(
        column(5,
          sidebarPanel(width = 12,
            h4(tags$b("1) Upload age data file")),
            helpText("Instructions and considerations for sample data entry are available in 
                     Data Entry Instructions Tab"),
            fileInput("ageData", "Upload Fish Age Data"),
            helpText("Upload file (.csv) must have correct format and column headings. To see proper columns click on Dowload 
                Template button below"),
            downloadButton("ageTemplate", label = "Download Template",class="butnTmplt"),
            HTML('<br/>', '<br/>'),
            
            #checkboxInput("ageperiod2NA", "Change Periods to NA's", value = FALSE), #decided to hard code this
            hr(),
            h4(tags$b("2) Initiate age data validation check")),
            actionButton("validateAge", "Initiate Validation",class="butn"),
            helpText("Error Identification Table will appear in middle panel."),
            hr(),
            h4(tags$b("3) Observe error status and fix as needed")),
            helpText("Fix any errors identified by the data validation function (Error Identification Table).
                     Then refresh page, reload data, and run validation function again until satisfied."),
            helpText("Use 3rd panel to identify specific row numbers violating each error rule."),
            hr(),
            h4(tags$b("4) Download validated age data")),
            helpText("If all error rules are 'Okay', age dataset is ready to download."),
            helpText("Validated age data will be ready to
                     upload to main app."),
            downloadButton("valAgeData", "Download validated age data",class="butn")
          )  
        ),
        column(3, align = "center",
           wellPanel(
             h4(tags$b("Error Identification Table")),
             helpText("Status 'Error' indicates one or more records violates the corresponding error rules."),
             helpText("Definitions of errors found in Validation Rule Details Tab."),
             formattableOutput("ageError")
            )
        ),
        column(4,
          wellPanel(
             h4(tags$b("Identify Row Numbers with Errors"), align = "center"),
             hr(),
             checkboxInput("ageLake", "Invalid Lake Code"),
              textOutput("ageLake"),
             checkboxInput("ageMonth", "Invalid Month"),
              textOutput("ageMonth"),
             checkboxInput("ageDay", "Invalid Day"),
              textOutput("ageDay"),
             checkboxInput("ageYear", "Invalid Year"),
              textOutput("ageYear"),
             checkboxInput("ageGear", "Invalid Gear Code"),
              textOutput("ageGear"),
             checkboxInput("ageSpp", "Invalid Species Code"),
              textOutput("ageSpp"),
             checkboxInput("ageNOI", "Invalid Number of individuals"),
              textOutput("ageNOI"),
             checkboxInput("ageTL", "Invalid Total Length"),
              textOutput("ageTL"),
             checkboxInput("age", "Invalid Age"),
             textOutput("age")
          )
        )
      )
    ),
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
                      For these reasons, please do not abstain from this step in the process"), align = "center"),
            hr(),
            h4("Invalid Lake Code"),
              helpText("Lake Code must be present in the established list of lake codes and cannot be blank.  
                        Check for spelling, capitalization, etc.
                       This list can be found in the SSP Manual or in the panel to the right."),
              helpText("If a new Lake Code is needed, contact Dan Shoup to add one to the established list."),
            h4("Blank Station"),
              helpText("Station is a required field and cannot be blank.  Include a station (numerical, character, or alpha-numeric) for 
                       each site sampled."),
            h4("Invalid Month, Day, or Year"),
              helpText("Month, Day, and Year fields are required and cannot be blank."),
              helpText("Month must be an integer between 1 and 12."),
              helpText("Day must be an integer between 1 and 31."),
              helpText("Year must be an integer between 1980 and the current year."),
            h4("One Gear Code"),
              helpText("Due to the nature of checking Gear Length and Effort based on Gear Code, only one Gear Code can be uploaded and validated
                       at a time.  Make sure only one Gear Code is present in the sample data and that there are no blanks."),
            h4("Invalid Gear Code"),
              helpText("Gear Code must be present in the established list of gear codes and cannot be blank.
                       This list can be found in the SSP Manual or in the panel to the right.  Pleas note that gear codes have recently been
                       updated, and make sure the correct code is being used."),
            h4("Invalid Gear Length"),
              helpText("This validation rule is based on the type of gear, and is required (cannot be blank)."),
              helpText("If electrofishing, Gear Length (minutes of efishing) must be between 1 and 300 (total minutes fished for station sample)."),
              helpText("If trapnetting, Gear Length (length of lead), should be > 5 ft and < 150 ft, but no validation check is made by the app."),
              helpText("If gillnetting, Gear Length (length of net), should be > 10 ft and < 2000 ft, but no validation check is made by the app."),
              helpText("If seining, Gear Length must be > 5 ft and < 100 ft."),
            h4("Invalid Effort"),
              helpText("This validation rule is based on the type of gear, and is required (cannot be blank)."),
              helpText("If electrofishing, Effort (1 unit = 10 min) should be between 0.1 (1-min) and 24 (240 min, 4 hrs), but no validation check is made by the app."),
              helpText("If trapnetting or gillnetting, Effort (# hrs fished, decimal) must be between 0.1 and 96."),
              helpText("If seining, Effort (area sampled, ft^2) must be more than 100"),
            h4("Invalid Species Code"),
              helpText("Species Codes must be present in the established list of species codes and cannot be blank (blank).  This list can
                       be found in the SSP Manual or in the panel to the right.  Contact Dan Shoup if a new species code needs to be added
                       to this list."),
              helpText("Make sure to use Species Code 98 if not fish were caught in the sample (don't leave it blank).  This is also essential
                       to CPUE calculations."),
            h4("Invalid Number of Individuals"),
              helpText("The number of individuals field cannot be 0 and cannot be blank.  Even for species code 98 (no fish in sample), enter a
                       one for number of individuals.  This is crucial for proper calculation of CPUE."),
            h4("Invalid Total Length or Weight (Sample Data)"),
              helpText("There are no limitations for lengths and weights, however these values cannot be zero.  It is impossible for a fish to
                       be 0 mm or weigh 0 grams.  If a length or weight was not measured for a fish, leave these fields blank (blank)."),
            h4("Invalid Total Length (Age Data)"),
              helpText("There are no limitations for Total Lengths, but these values cannot be 0 or blank.
                       A fish cannot be 0 mm, and an associated length is needed for age information."),
            h4("Invalid Relative Weight"),
              helpText("This validation rule is aimed to flag fish that may have mis-measured or mis-typed lengths or weights.  Relative weights
                       are calculated for all fish possible, and fish that have relative weights < 20 or > 200 are flagged with an error.  If the
                      user is confident the length and weight are correct, this error may be bypassed.  If not, we recommend just leaving the length 
                       and weight blank for that particular fish unless you know for sure which is incorrect."),
            h4("Invalid Age"),
              helpText("Ages must be an integer between 0 and 40.  Blanks are not allowed, and refrain from
                       entering anything other than an integer (e.g., YOY, 10+).")
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
                         tags$iframe(style="height:700px; width:100%",
                                     src="odwc.ssp.ofaa.pdf#page=1&view=FitH")
                  )      
                )        
      )
    )
  )
)