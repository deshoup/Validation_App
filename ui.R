#Packages to be installed and loaded############################
library(shiny)
library(shinyjs)
library(V8)#package needed to refresh page...only used if we upload csv files
library(formattable) #used for formattableOutput() function to produce error table more easily
library(DT)
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" #Javascript needed to refresh page...will only need on file upload screen


fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "ODWClogo.gif"),
            tags$title("Data Validation App")),
  useShinyjs(),
  
  # Application title with ODWC logos####
  titlePanel(
    wellPanel(
      fluidRow(
        column(3,align="center", img(src="ODWClogo.gif", height="auto", width="110px")),
        column(6, align="center", tags$b(h2("ODWC Data Validation Application")),
               h4("quality testing for data to upload into Oklahoma Fishery Analysis Application"),
               hr(), 
               h5("Created by Dray D. Carl and Daniel E. Shoup")),
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
              
              
    #Validate Sample Data Tab####
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
            h4(tags$b("2) Observe error status and fix as needed")),
            helpText("Fix any errors identified by the data validation function (Error Identification Table).
                      Then refresh page, reload data, and run validation function again until satisfied."),
            helpText("Use 3rd panel to identify specific row numbers violating each error rule."),
            hr(),
            h4(tags$b("3) Download validated sample data")),
            helpText("If all error rules are 'Okay', dataset is ready to download."),
            helpText("Validated data will have required SampleID field and will be ready to
                     upload to main app."),
            
            
            h4(textOutput("downloadMessage")),
            downloadButton("valSampleData", "Download validated sample data", class="butn", disabled = "disabled")
          )  
        ),
        
        #Middle column with red errors or green Okays for each test
        column(3, align = "center",
          wellPanel(
            h4(tags$b("Error Identification Table")),
            helpText("Status 'Error' indicates one or more records violates the corresponding error rules."),
            helpText("Definitions of errors found in Validation Rule Details Tab."),
            formattableOutput("sampleError")
          )
        ),
        
        #third column with check boxes to identify offending rows
        column(4,
          wellPanel(
            h4(tags$b("Identify Row Numbers with Errors"), align = "center"),
            hr(),
            checkboxInput("Blanks", "Invalid Blank Cells (all blank cells should be filled with periods)"),
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
            checkboxInput("sampDate", "Year, month, & day make valid date"),
              textOutput("sampDate"),
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
            checkboxInput("unusualTL", 'Abnormally large or small TL. (Correct TL if wrong, type "verified" in Verified.TL column if value is correct)'),
              textOutput("unusualTL"),
            checkboxInput("integerTL", "Decimals in TL (must be whole numbers)"),
            textOutput("integerTL"),
            checkboxInput("sampWt", "Invalid Weight"),
            textOutput("sampWt"),
            checkboxInput("sampWr", 'Unusual Relative Weight (<50 or >150). (Correct wt or TL if wrong, type "verified" in Verified.Wr column if values are correct)'),
              textOutput("sampWr")
          )
        )
      ),

      #Display uploaded data in table (will include Wr)
      hr(),
      fluidRow(
        # textOutput("Data can be filtered using buttons above to show rows specific to errors"),
        # DT::dataTableOutput("sampleDataDisplayTable")
        DT::DTOutput("sampleDataDisplayTable")
      )
    ),
    
    
    
    #Validate Age Data Tab####
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
            h4(tags$b("2) Observe error status and fix as needed")),
            helpText("Fix any errors identified by the data validation function (Error Identification Table).
                     Then refresh page, reload data, and run validation function again until satisfied."),
            helpText("Use 3rd panel to identify specific row numbers violating each error rule."),
            hr(),
            h4(tags$b("3) Download validated age data")),
            helpText("If all error rules are 'Okay', age dataset is ready to download."),
            helpText("Validated age data will be ready to
                     upload to main app."),
            
            
            h4(textOutput("downloadMessageAge")),
            downloadButton("valAgeData", "Download validated age data", class="butn", disabled = "disabled")
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
             checkboxInput("ageDate", "Year, month, & day make valid date"),
              textOutput("ageDate"),
             checkboxInput("ageGear", "Invalid Gear Code"),
              textOutput("ageGear"),
             checkboxInput("ageSpp", "Invalid Species Code"),
              textOutput("ageSpp"),
             checkboxInput("ageNOI", "Invalid Number of individuals"),
              textOutput("ageNOI"),
             checkboxInput("ageTL", "Invalid Total Length"),
              textOutput("ageTL"),
             checkboxInput("unusualAgeTL", "Abnormally large or small TL"),
              textOutput("unusualAgeTL"),
             checkboxInput("integerAgeTL", "Decimals in TL (must be whole numbers)"),
              textOutput("integerAgeTL"),
             checkboxInput("age", "Invalid Age (must be an whole number between 0-40)"),
             textOutput("age"),
          )
        )
     
      ), 
      
      # #Display uploaded age data in table with row numbers
      hr(),
      fluidRow(
        DT::dataTableOutput("ageDataDisplayTable")
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
                       these columns in these orders.  An error in this row indicates your spreadsheet had missing columns,
                       columns in the wrong order, or extra columns that are not in the template."),
            h4("Blank cells instead of periods"),
              helpText("if a cell does not have a value, a period should be put in the cell to indicate it is intentionally
                       being left blank.  Just leaving the cell blank is not acceptable."),
            h4("Invalid Lake Code"),
              helpText("Lake Code must be present in the established list of lake codes and cannot be blank.  
                        Check for spelling, capitalization, etc.
                       This list can be found in the SSP Manual or in the panel to the right."),
              helpText("If a new Lake Code is needed, contact Dan Shoup to add one to the established list."),
            h4("Blank Station"),
              helpText("Station is a required field and cannot be blank.  Include a station (numerical, character, or alpha-numeric) for 
                       each site sampled."),
            h4("Missing station ID's based on time codes"),
              helpText("There are more time codes than station ID values.  Station ID is important to the function of the app
                       and is no longer optional. Each gear replicate must have a unique station ID on a given day."),
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
              helpText("This test only looks to make sure TL and Wt values are numeric and >0 (or a period is used to indicate
                       no data were taken for this record).  It is impossible for a fish to
                       be 0 mm or weigh 0 grams.  If a length or weight was not measured for a fish, leave these fields blank (blank)."),
            h4("Invalid Total Length (Age Data)"),
              helpText("Total Lengthscannot be 0 or blank. A fish cannot be 0 mm, and an associated length is needed for age 
                       information. If a length is not available for a fish, the entire record should be deleted (i.e., delete
                       the row from the file)"),
            h4("Abnormally large or small TL"),
              helpText("fish is unusually large or small for the species based on a table of values the ODWC SSP committee
                       developed.  This will not prevent you from using these data, but you should double check the value of 
                       these rows to be sure the fish really were exeptionally large or smal and that this is not a mistake.
                       If a weight or TL is suspect, it is probably better to delete it (replace with a perioe). Do not delete
                       the whole row though as that would change CPUE.  Instead leave the missing TL or weight but have the
                       number of individuals = 1 so this row still counts in CPUE calculations"),
            h4("Invalid Relative Weight"),
              helpText("This validation rule is aimed to flag fish that may have mis-measured or mis-typed lengths or weights.  Relative weights
                       are calculated for all fish possible, and fish that have relative weights < 50 or > 150 are flagged with an error.  If the
                      user is confident the length and weight are correct, this error may be bypassed.  If not, we recommend just leaving the length 
                       and weight blank for that particular fish unless you know for sure which is incorrect (again, put a period in TL or Wt column
                      but do not leave them blank and do not delete the whole row as that woudl impact CPUE calculations)."),
            h4("Invalid Age (Age Data)"),
              helpText("Ages must be an integer between 0 and 40.  Blanks are not allowed, and refrain from
                       entering anything other than an integer (e.g., no values like: YOY, 10+, etc.).")
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