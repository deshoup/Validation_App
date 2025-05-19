Agency="ODWC" #ODWC or KDFWR

#Packages to be installed and loaded############################
library(dplyr)
library(plyr) #uses "join"...need to update so don't need this
library(tibble)
library(data.table)
library(FSA)
library(formattable) #used for formattableOutput() function to produce error table more easily
library(shinyjs)
library(DT)
library(rhandsontable) #make editable tables
library(rlang)#used to call function that evaluates each condition...have to use it to reference columns in dataToTest() within function context
library(jsonlite) #used for building invalid cell formatting (need to convert table to JSON)

# .csv's to upload
sampleTemplate <- read.csv("sampleTemplate.csv")
ageTemplate <- read.csv("ageTemplate.csv")
lakeCodes <- read.csv("lakeinfo.csv")
gearCodes <- read.csv("gearinfo.csv")
speciesCodes <- read.csv("speciesinfo.csv")
WSnames <- read.csv("WSnames.csv")
minMaxTL <- read.csv("minMaxTL.csv")%>% mutate(Species.Code = as.numeric(Species.Code))


function(input, output, session) {
  
# Adds ability to upload files greater than 5 MB (max now 100 MB)
options(shiny.maxRequestSize=100*1024^2)
  
# Downloadable csv of sample data template
  output$sampleTemplate <- downloadHandler(
    filename = function() {
      paste("sampleTemplate", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(sampleTemplate, file, row.names = FALSE)
    }
  )

# Downloadable csv of age data template
output$ageTemplate <- downloadHandler(
  filename = function() {
    paste("ageTemplate", "csv", sep = ".")
  },
  content = function(file) {
    write.csv(ageTemplate, file, row.names = FALSE)
  }
)
   
##Read in SampleData or ageData file provided by user####
dataType <- reactiveVal()
# sampleData <- reactiveVal()
dataToTest <- reactiveVal()
ageData <- reactiveVal()
fileUploadFLag <- reactiveVal() #used to reset rhansondTable if new data uploaded (needed to reset column heading formatting)
observeEvent(input$sampleData,{
  req(input$sampleData)
    #below imports csv as character & converts blanks and "NA" to NA's...flagging these as needing to be "." will 
    #be done below.  Code for exporting validated file will also convert "." to NA
  ##Sample Data import
    sampleData <- read.csv(input$sampleData$datapath, na.strings = c("","NA"), colClasses = "character")
      if(("Station" %in% colnames(sampleData) & ("Wt_g" %in% colnames(sampleData)) & 
                 ("Effort" %in% colnames(sampleData)))) { #is sampling data file
          dataType("samplingData")       
          #create optional Genetic.ID, Sex, Age, Latitude, Longitude, and Comments columns, if they do not exist
          if(!"Genetic.ID" %in% colnames(sampleData)){
            sampleData$Genetic.ID <- as.character(".")
          }
          if(!"Sex" %in% colnames(sampleData)){
            sampleData$Sex <- as.character(".")
          }
          if(!"Age" %in% colnames(sampleData)){
            sampleData$Age <- as.character(".")
          }
          if(!"Latitude" %in% colnames(sampleData)){
            sampleData$Latitude <- as.character(".")
          }
          if(!"Longitude" %in% colnames(sampleData)){
            sampleData$Longitude <- as.character(".")
          }
          if(!"Comments" %in% colnames(sampleData)){
            sampleData$Comments <- as.character(".")
          }

        #create Verified.TL/Wr columns if they do not exist. This will be used to mark abnormal TL/Wr values that have been verified
          if(!"Verified.TL" %in% colnames(sampleData)){
            sampleData$Verified.TL <- as.character(".")
          }
          if(!"Verified.Wr" %in% colnames(sampleData)){
            sampleData$Verified.Wr <- as.character(".")
          }
        #Add missing columns and arrange all in proper order. Rearranging is necessary as order will vary
          #depending on which ones were added by user versus added by the above code.  This could also fix user
          #error where columns are in wrong order (no need to repair this if I can do it here).

        #To add missing columns, the below code compares cols from template and df (names(.)), counts how many
          #differences exists and for missing column creates a list of NA values (list(NA)). It then names
          #(using setNames() function) each NA list with one of the missing column names.  The splicing
          #operator (!!!) is needed as it "unpacks" the named lists so setNames() can operate on each item
          #separately (it cannot recurse on a list so !!! allows it to function as if it could).
          #These are filled with NA, which will flag user that these columns are not properly filled out (they
          #at least need to change NA's to ".", but they may realize they have data to add)
        sampleData <- sampleData %>% add_column(!!!setNames(rep(list(NA), length(setdiff(colnames(sampleTemplate),
                        names(.)))), setdiff(colnames(sampleTemplate), names(.)))) %>%
                    relocate(any_of(colnames(sampleTemplate))) #now arrange in proper order, using any_of to ignore extra columns not in template (puts them at end of col names)
        dataToTest(sampleData) #write values to reactive dataToTest()
        fileUploadFLag("TRUE") #resets file update flag to reset rhansondTable
    ##Age Data import
      }else if (("Age" %in% colnames(sampleData)) &!("Station" %in% colnames(sampleData)) & 
                !("Effort" %in% colnames(sampleData))) { #is an age data file
        dataType("agingData")
        ageData <- read.csv(input$sampleData$datapath, na.strings = c("","NA"), colClasses = "character") 
        #Add optional Genetic.ID, Sex, and Verified.TL columns if they do not exist
        if(!"Genetic.ID" %in% colnames(ageData)){
          ageData$Genetic.ID <- as.character(".")
        }
        if(!"Sex" %in% colnames(ageData)){
          ageData$Sex <- as.character(".")}    
        if(!"Verified.TL" %in% colnames(ageData)){
          ageData$Verified.TL <- as.character(".")
        }
        ageData <- ageData %>% 
          rename_with(~ case_when(.x == "Gear" ~ "Gear.Code",
                                  .x == "TLmm" ~ "TL_mm",
                                  TRUE ~ .x),
                      .cols = any_of(c("Gear", "TLmm")) ) %>%   #renames Gear and TLmm to Gear.Code and TL_mm if needed but throws no error if they do not exist
          add_column(!!!setNames(rep(list(NA), length(setdiff(colnames(ageTemplate),
                      names(.)))), setdiff(colnames(ageTemplate), names(.)))) %>% #add any missing columns
          relocate(any_of(colnames(ageTemplate)))
        dataToTest(ageData) #write out to reactive dataframe
      fileUploadFLag("TRUE") #resets file update flag to properly format rhansondTable
      }else{
        showModal(modalDialog(
              title = "Warning: you have uploaded a file that does not have the proper headings to be a sampling data 
              file or an age data file. Please fix the file such that the required column headings are in your file 
              and try again.  Note you can download a template that indicates the correct column headings.",
              footer = actionButton("dismissSampData", "Dismiss")
            ))
            return(NULL) #prevents any more code from running if wrong file type loaded
      }
  })

# deal with dismiss of modal generated above by not being an age data or sample data file that was uploaded
observeEvent(input$dismissSampData, {
  shinyjs::reset("sampleData")  #reset file upload box to be blank
  removeModal()       # Close modal
})

#create reactive values we will use to streamline the code
lakeYrGear <- reactive({ #for naming sampling data files
  sampleData <- dataToTest()
  lake <- sampleData$Lake.Code[1]
  yr <- sampleData$Year[1]
  gear <- sampleData$Gear.Code[1]
  paste(lake, yr, gear, sep="_")
})

LkYrGrSp <- reactive({  #for naming age data files
  ageData <- dataToTest()
  lake <- ageData$Lake.Code[1]
  yr <- ageData$Year[1]
  gear <- ageData$Gear[1]
  spp <- ageData$Species.Code[1]
  paste(lake, yr, gear, spp, sep="_")
})
   

###Validation rule testing function#####
    #create function to call for testing variables
    testValidData <- function(data, test_name, column, condition) {
      invalid_rows <- which(condition(data[[column]])) #find rows that violated the specified condition
      list_container <- if (length(invalid_rows) > 0) { #invalid rows existed
            list(invalidRowNumbs = data.frame(
                    error = rep(test_name, length(invalid_rows)), #repeats (rep) test name for each row that was invalid
                    col = rep(which(names(data) == column), length(invalid_rows)), #same rep for column numbers (uses which() to find column number of current column being tested)
                    row = as.integer(invalid_rows)), #lists each row with invalid data separately, need to add 1 to rows because csv has column headings on first row
                errorTableValues = data.frame(
                   Error = test_name,
                   Status = "Error", stringsAsFactors = FALSE))
          } else { #if no invalid rows existed
            list(invalidRowNumbs = NULL, #return NULL if there are no invalid rows
                errorTableValues = data.frame(
                  Error = test_name,
                  Status = "Okay", stringsAsFactors = FALSE))
          }
    }

#Run Validation Tests####
validationResults <- reactive({
  req(dataType(), dataToTest(), sampleDataDisplay())
##Deal with tests that cannot easily use the above function and need additional coding
  #test column names and order ####
  if (dataType() == "samplingData"){ #is sampling data
    columnTemplate <- data.frame(colNames = colnames(read.csv("sampleTemplate.csv")))
  }else{ #is age data
    columnTemplate <- data.frame(colNames = colnames(read.csv("ageTemplate.csv")))
  }
    columnNames <- data.frame(colNames = colnames(dataToTest()))
    extraCols <- setdiff(columnNames$colNames, columnTemplate$colNames)
    colNameOrderResult <- if (identical(columnTemplate, columnNames)) {
      list(invalidRowNumbs = NULL,
           errorTableValues = data.frame(
              Error = "Correct column names and order",
              Status = "Okay", stringsAsFactors = FALSE))
    } else {
      list(invalidRowNumbs = data.frame(
              error = rep("Correct column names and order", length(extraCols)),
              col = match(extraCols, colnames(dataToTest())),
              row = rep(NA, length(extraCols))),
           errorTableValues = data.frame(
              Error = "Correct column names and order",
              Status = "Error", stringsAsFactors = FALSE)
      )}
  
  #test for blank cells (shows where "." should have been used instead)####
    # excludes Station as it has its own check later for both "." and NA as it cannot be left blank
  if (dataType() == "samplingData"){ #is sampling data
    columns_to_check <- setdiff(names(dataToTest()), c("Station",  "Comments", "Verified.TL", "Verified.Wr")) #omits several columns from list of columns but retains info about column number
  }else{columns_to_check <- names(dataToTest() %>% select(-Verified.TL)) }
    na_cells <- which(is.na(dataToTest()[, columns_to_check]), arr.ind = TRUE) #find and store location of all NA values (except those in Station column)
    na_result <- if (nrow(na_cells) > 0) {
      list(invalidRowNumbs = data.frame(
                 error = rep("No blank cells (used periods instead)", nrow(na_cells)),
                 col = match(columns_to_check[na_cells[, "col"]], names(dataToTest())), #gets column name from colums_to_check & matches it to column numbers from original df
                 row = na_cells[, "row"]),
           errorTableValues = data.frame(
                 Error = "No blank cells (used periods instead)", 
                 Status = "Error"))
    }else{
      list(invalidRowNumbs = NULL,
          errorTableValues = data.frame(
            Error = "No blank cells (used periods instead)", 
            Status = "Okay",stringsAsFactors = FALSE))
    }

  #missing Stations ID's based on time codes####
    #(appears a station number got used on the same day at 2 times...so should be 2 stations)
    if (dataType() == "samplingData"){ #is sampling data
        stationTimeCount <- dataToTest() %>% filter(!is.na(Station) & Station != ".") %>% 
          group_by(Lake.Code, Gear.Code, Year, Month, Day) %>% 
              summarize(Time_n_dist=n_distinct(Time), Station_n_dist=n_distinct(Station), .groups = 'drop')
        stationTimeCountResult <- list(
            invalidRowNumbs = NULL, #no row numbers can be calculated so don't need invalidRowNumbs
            errorTableValues = data.frame(
              Error = "No missing Stations ID's based on time codes",
              Status = if(any(stationTimeCount$Time_n_dist > stationTimeCount$Station_n_dist)){#any accounts for possible multiple lake gear, year, etc. which results in multiple values...this asks if any violates the rule
            "Error"}else{"Okay"},stringsAsFactors = FALSE))
    }
    
  #valid date from Month, Day, Year####
      renderedDates <- dataToTest() %>% mutate(renderedDate = as.Date(paste(Year, Month, Day, sep="-"), format="%Y-%m-%d"))
      invalid_rows <- which(is.na(renderedDates$renderedDate) | renderedDates$renderedDate < as.Date("1980-01-01") |
                            renderedDates$renderedDate > Sys.Date())
      Mo_D_Y_isDate <- if (length(invalid_rows) > 0) {
        date_columns <- which(colnames(dataToTest()) %in% c("Month", "Day", "Year"))
        list(invalidRowNumbs = data.frame(
                error = rep("Year, month, & day make valid date", length(invalid_rows) * length(date_columns)),
                col = rep(date_columns, each = length(invalid_rows)),
                row = rep(invalid_rows, times = length(date_columns))),
             errorTableValues = data.frame(
                Error = "Year, month, & day make valid date",
                Status = "Error", stringsAsFactors = FALSE))
      } else {
        list(invalidRowNumbs = NULL,
             errorTableValues = data.frame(
               Error = "Year, month, & day make valid date",
               Status = "Okay", stringsAsFactors = FALSE))
      }
  #gear.code-related tests (length and effort too)####  
  #Gear.Code functions...need to do here before rest of testValidData() ones so I can use it for testGear.Length_effort
    if (dataType() == "samplingData"){ #is sampling data  
    oneGearCode <- testValidData(dataToTest(), "Only one Gear Code in sample", "Gear.Code", function(x) 
      !x == lead(x, default = last(x)))
    }
    invalidGearCode <- testValidData(dataToTest(), "Valid Gear Code", "Gear.Code", function(x) 
      !as.numeric(x) %in% gearCodes$Gear.Code)
    
  #Gear.Code, Gear.Length, and Effort tests
      #create function to save space; this also uses testValidData() in it, but does some setup for building function
      if (dataType() == "samplingData"){ #is sampling data 
            testGear.Length_Effort <- function(data_df, min_len, max_len, min_Ef, max_Ef){
              list(Gear.Length = testValidData(data_df, "Valid Gear Length", "Gear.Length", function(x) {
                x_numeric <- suppressWarnings(as.numeric(x)) #create variable with suppressed as.numeric()
                x == "." | is.na(x) | x_numeric > max_len | x_numeric < min_len}),
                Effort = testValidData(data_df, "Valid Effort", "Effort", function(x) {
                  x_numeric <- suppressWarnings(as.numeric(x))
                  x_numeric > max_Ef | x_numeric < min_Ef | x == "." | is.na(x)}))
            }
        
        #now test Gear.Length and Effort by Gear.Code
          Gear.Length_Effort <- if(!"Only one Gear Code in sample" %in% oneGearCode$invalidRowNumbs$error &
                                   !"Valid Gear Code" %in% invalidGearCode$invalidRowNumbs$error) {
            if(unique(dataToTest()$Gear.Code) >= 40 && unique(dataToTest()$Gear.Code) != 68) {  
              # if electrofishing, gear length (minutes of e-fishing) must be between 1 and 300 (total minutes fished for station sample) and not null
              # if electrofishing, effort (1 unit = 10 min) must be between 0.1 (1-min) and 24 (240 min, 4 hrs) and not null
              testGear.Length_Effort(dataToTest(), min_len=1, max_len=300, min_Ef=0.1, max_Ef=24)
            } else if(unique(dataToTest()$Gear.Code) > 30 && unique(dataToTest()$Gear.Code < 40)) {
              # if trapnet, gear length (length of lead), must be > 5 ft and < 150 ft and not null
              # if trapnet, effort (# hrs) must be between 0.1 and 96 and not null
              testGear.Length_Effort(dataToTest(), min_len=5, max_len=150, min_Ef=0.1, max_Ef=96)
            } else if(unique(dataToTest()$Gear.Code) > 20 && unique(dataToTest()$Gear.Code) < 30) {
              # if gillnet, gear length (length of net), must be > 10 ft and < 2000 ft and not null
              # if gillnet, effort (# hrs) must be between 0.1 and 96 and not null
              testGear.Length_Effort(dataToTest(), min_len=10, max_len=2000, min_Ef=0.1, max_Ef=96)
            } else if(unique(dataToTest()$Gear.Code) < 20) {
              # if seine, gear length (length of seine), must be > 5 ft and < 100 ft and not null
              # if seine, effort (area sampled, ft^2) must be more than 100 and not null
              testGear.Length_Effort(dataToTest(), min_len=5, max_len=100, min_Ef=100, max_Ef=1000000)
            } else if(unique(dataToTest()$Gear.Code) == 68) {
              #bow fishing...no explicit rules for length or effort, so anything but NA is ok
              list(Gear.Length = testValidData(dataToTest(), "Valid Gear Length", "Gear.Length", function(x) is.na(x)),
                   Effort = testValidData(dataToTest(), "Valid Effort", "Effort", function(x) is.na(x)))
            }
          } else {#if there is not one and only 1 legitimate Gear.Code
            list(Gear.Length = list(invalidRowNumbs = NULL,
                                    errorTableValues = data.frame(Error = "Valid Gear Length",
                                      Status = "Can't test", stringsAsFactors = FALSE)),
                 Effort = list(invalidRowNumbs = NULL,
                               errorTableValues = data.frame(Error = "Valid Effort",
                                 Status = "Can't test", stringsAsFactors = FALSE))
            )}
      }
   
  #test TL for min and max values for abnormal fish sizes########################################
    sampleDataTL <- dataToTest() %>% 
        mutate(Species.Code = as.numeric(Species.Code),
               TL_mm = as.numeric(ifelse(TL_mm == ".", NA, TL_mm)))
      unusualTL <- left_join(sampleDataTL, minMaxTL, by= "Species.Code") 
      abnormalTL <- if(nrow(unusualTL) > 0){
        # create if for whether gear code was appropriate (depends on whether it was sampling or aging data)
        validGearCondition <- if(dataType() == "samplingData") {
                                !"Only one Gear Code in sample" %in% oneGearCode$invalidRowNumbs$error &
                                  !"Valid Gear Code" %in% invalidGearCode$invalidRowNumbs$error
                              } else {
                                !"Valid Gear Code" %in% invalidGearCode$invalidRowNumbs$error
                              }
        if(validGearCondition) {#apply whichever "if" above is appropriate based on whether using sampling or aging data
              if(unique(unusualTL$Gear.Code) == "10"){ #seine data
                testValidData(unusualTL, "Abnormally large or small TL", "TL_mm", function(x) x > unusualTL$maxTL & #no abnormally sm fish for seine data...any sm size is acceptable
                      !(tolower(unusualTL$Verified.TL) %in% c("verified", "\"verified\"")) ) #tolower converts to lower case on Verified
              }else{ #all gears other than gear 10 (seine)
                testValidData(unusualTL, "Abnormally large or small TL", "TL_mm", 
                      function(x) (x > unusualTL$maxTL | x < unusualTL$minTL) &
                        !(tolower(unusualTL$Verified.TL) %in% c("verified", "\"verified\""))) #tolower converts to lower case on Verified
              }
          }else{#if there is not one and only 1 legitimate Gear.Code
            list(invalidRowNumbs = NULL,
                 errorTableValues = data.frame(
                   Error = "Abnormally large or small TL",
                   Status = "Can't test", stringsAsFactors = FALSE))
            }
      }else{#if there were no unusualTL rows (all TL missing or Verified)
        list(invalidRowNumbs = NULL,
            errorTableValues = data.frame(
              Error = "Abnormally large or small TL",
              Status = "Okay", stringsAsFactors = FALSE))
      } 
    
    #test fish ages#######
    sampleDataAge <- filter(dataToTest(), !is.na(Age) & Age != ".") %>% 
      mutate(maxAge = case_when(Species.Code %in% 
                                  c("302","303","304","305","306","307","311","321","325","402","405") ~ 100,
                                Species.Code %in% c("114","115","401") ~ 40,
                                TRUE ~ 20))
    
    maxAge <- testValidData(sampleDataAge, "Invalid Age", "Age", 
                            function(x) suppressWarnings(as.numeric(x)) > sampleDataAge$maxAge)
      
  #test Wr#####
    if (dataType() == "samplingData"){ #is sampling data  
      #replaces Wr column with NA if row is fine or "invalid" if it is not
      sampleDataWr <- sampleDataDisplay() %>% mutate(Wr2 = as.numeric(Wr), Wr = case_when(
        (Wr>150 | Wr<50) & !Verified.Wr %in% c("verified", "Verified") ~ "invalid", TRUE ~ NA))
      invalidWr <- testValidData(sampleDataWr, "Invalid Relative Weight", "Wr",
                      function(x) x == "invalid")
    }
 
    
##Final list of validation test results (including above as well as those that use testValidationData() function)####
##Sampling data list
if (dataType() == "samplingData"){ #is sampling data  
    function_results <- list(
    colNameOrderResult, #"Correct column names and order" calculated above
    na_result, #"No blank cells (used periods instead)" calculated above
    testValidData(dataToTest(), "Valid Lake Code", "Lake.Code", function(x) !x %in% lakeCodes$Lake.Code),
    stationTimeCountResult, #"No missing Stations ID's based on time codes" calculated above
    testValidData(dataToTest(), "Valid Station", "Station", function(x) x == "." | is.na(x)),
    testValidData(dataToTest(), "Valid Month", "Month", function(x) !suppressWarnings(as.numeric(x)) %in% 
                                                  c(seq(1,12,1))),
    testValidData(dataToTest(), "Valid Day", "Day", function(x) !suppressWarnings(as.numeric(x)) %in% 
                                                  c(seq(1,31,1))),
    testValidData(dataToTest(), "Valid Year", "Year", function(x) !suppressWarnings(as.numeric(x)) %in% 
                                                  c(seq(1980,as.integer(format(Sys.Date(), "%Y")),1))),
    Mo_D_Y_isDate,
    oneGearCode,
    invalidGearCode,
    Gear.Length_Effort$Gear.Length,  
    Gear.Length_Effort$Effort,
    testValidData(dataToTest(), "Valid Species Code", "Species.Code", function(x) !suppressWarnings(as.numeric(x)) %in% 
                                                  speciesCodes$Species.Code),
    testValidData(dataToTest(), "Invalid Number of individuals", "Number.of.individuals", 
                                                  function(x) x == "0" | x == "." | is.na(x)),
    testValidData(dataToTest(), "Invalid Total Length", "TL_mm", function(x) x == "0"),
    abnormalTL,
    testValidData(dataToTest(), "TL must be a whole number", "TL_mm",function(x) {
                    numeric_TL <- suppressWarnings(as.numeric(x))
                     round(numeric_TL) != numeric_TL
                  }),
    testValidData(dataToTest(), "Invalid Weight", "Wt_g", function(x) suppressWarnings(as.numeric(x)) == 0),
    invalidWr,
    testValidData(dataToTest(), "Invalid Sex Code", "Sex", function(x) !is.na(x) & !x %in% c("F","M","U",".")),
    maxAge,
    testValidData(dataToTest(), "Invalid Latitude", "Latitude", function(x) !is.na(x) & x != "." & 
                (suppressWarnings(as.numeric(x)) > 37 | suppressWarnings(as.numeric(x)<33.6))),
    testValidData(dataToTest(), "Invalid Longitude", "Longitude", function(x) !is.na(x) & x != "." & 
                (suppressWarnings(as.numeric(x)) <=-103.00235 | suppressWarnings(as.numeric(x) >=-94.431))),
    testValidData(dataToTest(), "Invalid Verified.TL", "Verified.TL", function(x) {!is.na(x) & 
                !(x %in% c("verified", "Verified", "."))}),
    testValidData(dataToTest(), "Invalid Verified.Wr", "Verified.Wr", function(x) !is.na(x) & 
                  !(x %in% c("verified","Verified",".")))
  )
##List for age-only data
}else{  #age data
  function_results <- list(
    colNameOrderResult, #"Correct column names and order" calculated above
    na_result, #"No blank cells (used periods instead)" calculated above
    testValidData(dataToTest(), "Valid Lake Code", "Lake.Code", function(x) !x %in% lakeCodes$Lake.Code),
    testValidData(dataToTest(), "Valid Month", "Month", function(x) !suppressWarnings(as.numeric(x)) %in% 
                    c(seq(1,12,1))),
    testValidData(dataToTest(), "Valid Day", "Day", function(x) !suppressWarnings(as.numeric(x)) %in% 
                    c(seq(1,31,1))),
    testValidData(dataToTest(), "Valid Year", "Year", function(x) !suppressWarnings(as.numeric(x)) %in% 
                    c(seq(1980,as.integer(format(Sys.Date(), "%Y")),1))),
    Mo_D_Y_isDate,
    invalidGearCode,
    testValidData(dataToTest(), "Valid Species Code", "Species.Code", function(x) !suppressWarnings(as.numeric(x)) %in% 
                    speciesCodes$Species.Code),
    testValidData(dataToTest(), "Invalid Number of individuals", "Number.of.individuals", 
                  function(x) x == "0" | x == "." | is.na(x)),
    testValidData(dataToTest(), "Invalid Total Length", "TL_mm", function(x) x == "0" |
                    is.na(x) | x=="."), #cannot be blank if age data (but can be if age data uploaded with sampling data)
    abnormalTL,
    testValidData(dataToTest(), "TL must be a whole number", "TL_mm",function(x) {
      numeric_TL <- suppressWarnings(as.numeric(x))
      round(numeric_TL) != numeric_TL
    }),
    testValidData(dataToTest(), "Invalid Sex Code", "Sex", function(x) !is.na(x) & !(x %in% c("F","M","U","."))),
    maxAge,
    testValidData(dataToTest(), "Invalid Verified.TL", "Verified.TL", function(x) {!is.na(x) & 
        !(x %in% c("verified", "Verified", "."))})
    # testValidData(dataToTest(), "Invalid Verified.Wr", "Verified.Wr", function(x) !is.na(x) & 
    #                 !x %in% c("verified","Verified","."))
  )
  
}
    
    #combine Gear.Length_Effort results with previous results
    # function_results <- c(function_results1, 
    #                       list(Gear.Length_Effort$Gear.Length,  
    #                            Gear.Length_Effort$Effort))
    
  #below rearranges what starts out grouped by test (with row number and error table values nested under it)
  #to a list grouped by invalidRowNumbs and errorTableValues
    list(
      invalidRowNumbsList = do.call(rbind, c(lapply(function_results, function(x) x$invalidRowNumbs))),
      validationTableVals = do.call(rbind, c(lapply(function_results, function(x) x$errorTableValues)))
    )
})

#Produce error Table for center column#####
  validateSample <- reactive({
    req(validationResults())
    errorTable <- data.frame(validationResults()$validationTableVals)
    return(errorTable)
  })

# render error table#######################
  output$sampleError <- renderFormattable({
    req(validationResults())
    if (dataType() == "samplingData") {
        validated <- data.frame(validationResults()$validationTableVals[1:12,])
    }else{ #is aging data
        validated <- data.frame(validationResults()$validationTableVals[1:7,])
    }
            # function to conditionally color the text
            f1 <- formatter("span", style = ~ ifelse(Status == "Error", "color:red", 
                                                     ifelse(Status == "Okay", "color:green", "color:DarkOrange")))
            # Conditional formatter for the "Error" column
            # f2 <- formatter("span", style = ~ ifelse(Error == "No blank cells (used periods instead)" & Status == "Error", 
            #                                          "color:white; background-color:blue;", NA))
            f2 <- formatter("span", style = ~ ifelse(Error == "No blank cells (used periods instead)" & Status == "Error", 
                                                     "color:white; background-color:blue", 
                                              ifelse(Error == "Only one Gear Code in sample" & Status == "Error", 
                                                      "color:black; background-color:#FFA61F",
                                              ifelse(Error == "Year, month, & day make valid date" & Status == "Error", 
                                                             "color:black; background-color:#23FF71",       
                                              ifelse(Status == "Error", 
                                                            "color:black; background-color:#FF57C3;",       NA)))))
            validated <- formattable(validated, list(Status = f1, Error = f2))
  })
  output$sampleError2 <- renderFormattable({
    req(validationResults())
    if (dataType() == "samplingData") {
        validated <- data.frame(validationResults()$validationTableVals[13:24,])
    }else{ #is aging data
      validated <- data.frame(validationResults()$validationTableVals[8:15,])
    }
    rownames(validated) <- NULL #needed to prevent row numbers from being displayed in this second table
    # function to conditionally color the text in "status" column
    f1 <- formatter("span", style = ~ ifelse(Status == "Error", "color:red", 
                                             ifelse(Status == "Okay", "color:green", "color:DarkOrange")))
    f2 <- formatter("span", style = ~ ifelse(Status == "Error", "color:black; background-color:#FF57C3;", NA))
    validated <- formattable(validated, list(Status = f1, Error = f2))
  })
  
  
  #Create rhansondTable table for display of sample data at bottom of pg#####
  # that can be edited...includes calculated Wr for sampling data
  sampleDataDisplay<- reactive({
    req(dataToTest(), WSnames, dataType())
    # This is noticeably faster than using dplyr functions to perform the same operations...leave as is
    if (dataType() == "samplingData"){
      sampleDataDisplay <- join(dataToTest(), WSnames, by="Species.Code")#adds text that matches species code to Ogles required species for Ws equations#
      sampleDataDisplay$ID <- seq.int(nrow(sampleDataDisplay))#create an "ID" column that numbers rows
      wr_data <- sampleDataDisplay[!is.na(sampleDataDisplay$TL_mm) & sampleDataDisplay$TL_mm !="." & 
                                     !is.na(sampleDataDisplay$Wt_g) & sampleDataDisplay$Wt_g != "." &
                                     !is.na(sampleDataDisplay$wsname),] #filter to only rows that have needed values for Wr calculation
      wr_data["Wr"] <- (wrAdd(as.numeric(as.character(Wt_g)) ~ as.numeric(as.character(TL_mm)) + wsname,
                              units = "metric", data = wr_data))#calculates Wr value
      WrData <- subset(wr_data, select=c("ID", "Wr"))
      sampleDataDisplay <- join(sampleDataDisplay, WrData, by="ID")
      sampleDataDisplay["wsname"] <- NULL
      sampleDataDisplay$Wr <- round(sampleDataDisplay$Wr, 1)
    }else if(dataType() == "agingData"){
      sampleDataDisplay <- dataToTest()
      sampleDataDisplay$ID <- seq.int(nrow(sampleDataDisplay))#create an "ID" column that numbers rows
    }else{return(NULL)}
    sampleDataDisplay["row num"] <- sampleDataDisplay$ID +1
    sampleDataDisplay["Row num"] <- sampleDataDisplay$ID +1
    sampleDataDisplay["ID"] <- NULL
    sampleDataDisplay <- sampleDataDisplay %>% select("Row num",everything()) %>% as.data.frame()
    return(sampleDataDisplay)
  })
  
  #Render data table for sample data with rhandsontables
  output$sampleDataDisplayTable <- renderRHandsontable({
    req(dataType(), sampleDataDisplay(), validationResults(), fileUploadFLag())
     if(length(validationResults()$invalidRowNumbsList)==0){#table with no validation error formatting
      formattedTable <- rhandsontable(sampleDataDisplay(), stretchH = "all", rowHeaders = F, height=800)  %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>% #creates shading to show what row and column you have clicked in
        hot_cols(fixedColumnsLeft = 1) %>% #locks first column in place as you scroll to right
        hot_col("Row num", format = "0", width = 35, readOnly = TRUE, renderer = "
                      function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        td.style.background = '#F0F0F0';
                        td.style.fontWeight = 'bold';
                        return td; }") %>%
        hot_col("Day", width = 35) %>%
        hot_col("Year", width = 40) %>%
        hot_col("Gear.Code", title = "Gear. Code", format="0.00", width = 50) %>%
        hot_col("Species.Code", title = "Species .Code", width = 59) %>%
        hot_col("Number.of.individuals", title = "Number.of. individuals", width = 80) %>%
        hot_col("Sex", width = 35) %>%
        hot_col("Age", format="0", width = 35) %>%
        hot_col("row num", format="0", width = 31, readOnly = TRUE, renderer = "
                      function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        td.style.background = '#F0F0F0';
                        td.style.fontWeight = 'bold';
                        return td; }")
      
      if (dataType() == "samplingData"){ #formatting specific to sampling data
        formattedTable <- formattedTable %>%
          hot_col("Pool.Elevation", title = "Pool. Elevation", width = 65) %>%
          hot_col("Surface.Temp", title = "Surface .Temp", width = 56) %>%
          hot_col("Gear.Length", title = "Gear. Length", width = 55) %>%
          hot_col("Effort", width = 42) %>%
          hot_col("Latitude", format="0.000000") %>%
          hot_col("Longitude", format="0.000000") %>%
          hot_col("Wr", format="0.0", readOnly = TRUE) %>%
          hot_col("Time", width = 40)}
      return(formattedTable)
      
    }else{ #if errors were detected
      invalid_cells <- validationResults()$invalidRowNumbsList %>%
        # invalid_cells <- data.frame(error="test",col=1, row=1) %>%
        mutate(col = case_when(error == "Invalid Relative Weight" ~ col, TRUE ~ col+1)) #adjust for Row num column added to left side of dataframe (Wr does not need this as it evaluated the displayed data frame)
      # Convert invalid cell list to JSON;dataframe=rows makes 1 row per dataframe row in JSON;auto_unbox converts single-element arrays as scalar values...makes them easier to read and reference
      invalid_cells_json <- toJSON(invalid_cells , dataframe = "rows", auto_unbox = TRUE)
      # Make list of non-standard columns to highlight
      nonStandardColumns <- invalid_cells %>% #columns that should not be in data
        filter(error == "Correct column names and order") %>%
        pull(col)
      NonDayYrColumns <- invalid_cells %>% #Month, Day, and Year do not make valid date
        filter(error == "Year, month, & day make valid date") %>%
        pull(col)
      colOnlyHighlightErrors <- invalid_cells %>% #errors where whole column is wrong (mult values when only 1 allowed)
        filter(error == "Only one Gear Code in sample") %>%
        pull(col)
      invalidVerifiedText <- invalid_cells %>% #did not use "verified", "Verified", or "." in Verified.TL or Verified.Wr
        filter(error == "Invalid Verified.TL" | error == "Invalid Verified.Wr") %>%
        pull(col)
      specificErrors <- invalid_cells %>%
        filter(error %in% c("Invalid Month", "Invalid Day", "Invalid Year")) %>%
        pull(col)
      
      #create rhandsonTable with function for custom styling
      formattedTable <- rhandsontable(sampleDataDisplay(), stretchH = "all", rowHeaders = F, height = 800) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>% #creates shading to show what row and column you have clicked in
        hot_cols(fixedColumnsLeft = 1) %>% #locks first column in place as you scroll to right
        hot_table(customBorders = TRUE) %>% #tells rhansondTables to allow boarders
        #function to format invalid cells and columns
        hot_cols(renderer = sprintf("
              function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.TextRenderer.apply(this, arguments);
                  var invalidCells = %s; // Tells Java to parse the invalid cells each called invalidCels one at a time
                  var nonStandardColumns = %s; //identifies variable for nonStandardColumns
                  var NonDayYrColumns = %s; //identifies variable for highlighting when Month, Day, Yr are not valid date
                  var colOnlyHighlightErrors = %s; //identifies variable for whole-column errors
                  var style = {}; // initialize variable for style object

                  // Apply styling for non-standard columns
                  if (nonStandardColumns.includes(col + 1)) {
                    style.backgroundColor = '#FFFF00'; style.borderLeft = '2px solid blue'; //backgroundColor = yellow
                    style.borderRight = '2px solid blue';
                  }
                  
                  // Apply styling for month, day, and year that do not make date
                  var hasInvalidDateError = invalidCells.some(function(cell) {
                  return cell.row - 1 === row && NonDayYrColumns.includes(cell.col) && 
                         cell.error === 'Year, month, & day make valid date';
                  });
                  if (hasInvalidDateError && NonDayYrColumns.includes(col + 1)) {
                    style.backgroundColor = '#23FF71'; //Lime green
                  }
                
                  // Apply styling for other error types
                  for (var i = 0; i < invalidCells.length; i++) {
                    if (invalidCells[i].row - 1 === row && invalidCells[i].col - 1 === col) {
                    console.log(td.style.backgroundColor, td.style.color);
                      if (invalidCells[i].error === 'No blank cells (used periods instead)') {
                        style.backgroundColor = '#0000FF';//dark blue
                        style.color = '#white';
                        Object.assign(td.style, style);
                        } else if (invalidCells[i].error === 'Only one Gear Code in sample') {
                            style.fontWeight = '900';
                        } else if (invalidCells[i].error === 'Invalid Verified.TL' | invalidCells[i].error === 'Invalid Verified.Wr') {
                            style.backgroundColor = '#39BFFF'; //light blue
                        } else if (invalidCells[i].error !== 'Correct column names and order' & 
                                   invalidCells[i].error !== 'Year, month, & day make valid date') {
                            style.backgroundColor = '#FF57C3';style.fontWeight = '900';} //pink
                    }}
                  // Apply styling for whole-column errors
                  if (colOnlyHighlightErrors.includes(col + 1)) {
                      if (!style.backgroundColor || style.backgroundColor === 'yellow') {
                        style.backgroundColor = '#FFA61F';//color code is orange-brown
                      }
                  }
                  // Apply all of the above styles to the cell
                    Object.assign(td.style, style);
                    return td;
              }", invalid_cells_json, jsonlite::toJSON(nonStandardColumns), jsonlite::toJSON(NonDayYrColumns),
                                    jsonlite::toJSON(colOnlyHighlightErrors))) %>%
        #Additional styling for column header of non-standard columns
        hot_table(afterGetColHeader = htmlwidgets::JS(sprintf("
                function(col, TH) {
                  var nonStandardColumns = %s;
                  if (nonStandardColumns.includes(col + 1)) {
                    TH.style.backgroundColor = 'yellow';
                    TH.style.borderTop = '2px solid blue'; // Top border for header
                    TH.style.borderLeft = '2px solid blue'; // Top border for header
                    TH.style.borderRight = '2px solid blue'; // Top border for header
                }}", jsonlite::toJSON(nonStandardColumns)))) %>%
        #other cell formatting I want
        hot_col("Row num", format = "0", width = 35, readOnly = TRUE, renderer = "
                      function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        td.style.background = '#F0F0F0';
                        td.style.fontWeight = 'bold';
                        return td; }") %>%
        hot_col(col = "Lake.Code", type = "autocomplete", source = lakeCodes$Lake.Code, strict = FALSE) %>%
        hot_col("Day", width = 35) %>%
        hot_col("Year", width = 40) %>%
        hot_col("Gear.Code", title = "Gear. Code", type = "dropdown", source = gearCodes$Gear.Code,
                strict = FALSE, format="0.00", width = 50) %>%
        hot_col("Species.Code", title = "Species .Code", type = "dropdown", source = speciesCodes$Species.Code,
                strict = FALSE, width = 59) %>%
        hot_col("Number.of.individuals", title = "Number.of. individuals", width = 80) %>%
        hot_col("Sex", width = 35) %>%
        hot_col("Age", format="0", width = 35) %>%
        hot_col("row num", format="0", width = 31, readOnly = TRUE, renderer = "
                      function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        td.style.background = '#F0F0F0';
                        td.style.fontWeight = 'bold';
                        return td; }") %>% 
        #add statement to render font white while editing ()
        hot_table(afterInit = htmlwidgets::JS("function() {
          var css = '.handsontable td.htInvalid { color: black !important; }' +
                    '.handsontable td.isEditing { color: black !important; }' +
                    '.handsontable .handsontableInput { color: black !important; background-color: #D3D3D3 !important; }';
          var style = document.createElement('style');
          style.type = 'text/css';
          style.innerHTML = css;
          document.getElementsByTagName('head')[0].appendChild(style);
        }"))
      
      if (dataType() == "samplingData"){ #is sampling data
        formattedTable <- formattedTable %>%
          hot_col("Pool.Elevation", title = "Pool. Elevation", width = 65) %>%
          hot_col("Surface.Temp", title = "Surface .Temp", width = 56) %>%
          hot_col("Time", width = 40) %>%
          hot_col("Gear.Length", title = "Gear. Length", width = 55) %>%
          hot_col("Effort", width = 42) %>%
          hot_col("Latitude", format="0.000000") %>%
          hot_col("Longitude", format="0.000000") %>%
          hot_col("Wr", format="0.0", readOnly = TRUE) }
    }
      return(formattedTable)
  })
  
  
  #code to update values if user changes them in above rhansondTable
  observeEvent(input$sampleDataDisplayTable, {
    req(dataType())
    if (!is.null(input$sampleDataDisplayTable)) {
      edited_df <- hot_to_r(input$sampleDataDisplayTable)
      # Convert empty strings to NA
      edited_df[] <- lapply(edited_df, function(x) {
        if(is.character(x)) {x[x == ""] <- NA}
        return(x)
      })
      dataToTest(edited_df[, colnames(dataToTest())])
    }
  })
  
  ##Render text explaining color coding of errors in rhansondTable & "Can't Test" status#### 
  
  #comment in Error Identification Table about "can't test"
  observeEvent(validationResults(),{
    req(sampleDataDisplay(), validationResults())
    if ("Can't test" %in% validationResults()$validationTableVals$Status) {
      output$cantTest <- renderUI({
        h5("If status = ",span("Can't test", style = "color: DarkOrange"), ", it means other errors must be corrected before this test can be evaluated",
           style = "text-align: left;")})
    }
    
    #display type of data above Error Identification Table
    output$errorIdentificationType <- renderUI(
      if(dataType() == "samplingData") {
        h4(tags$b("Error Identification Table evaluating ", span("Sampling Data", style="color:blue; font-weight:bold")))
      }else{
        h4(tags$b("Error Identification Table evaluating ", span("Age Data", style="color:blue; font-weight:bold")))
      })
    
    #color coding on rhansondTable
    output$howToEditSampleData <- renderUI({
      if("Error" %in% validationResults()$validationTableVals$Status){
        tagList(
          h3(style = "color: blue;","If errors are detected above, you can edit the cells below to correct them. 
                    The app will automatically re-test cells that are edited."), 
          if (length(setdiff(validationResults()$invalidRowNumbsList$error, 
                             c("No blank cells (used periods instead)", "Only one Gear Code in sample",
                               "Correct column names and order"))) > 0) { #any error other than these 3
            h4(style = "color: black;", span("Pink", style = "color: black; background-color: #FF57C3;"), 
               "highlighting marks most validation errors (including blank or '.' values in cells that are not 
                     allowed to be blank). Use the Error Identification Table above to deterine the specific 
                     validation error these cells contain.")
          }, 
          if ("No blank cells (used periods instead)" %in% validationResults()$invalidRowNumbsList$error){
               h4(style = "color: black;", span("Blue", style = "color: white; background-color: #0000FF;"),
               "highlighting marks blank cells in a column that is alowed to contain '.' values but cannot be 
                     left blank (i.e., blanks are never allowed; '.' should be used to indicate a cell is intentionally 
                     .blank).")
          },
          if("Only one Gear Code in sample" %in% validationResults()$invalidRowNumbsList$error){
            h4(style = "color: black;", span("Orange-brown", style = "color: black; background-color: #FFA61F;"), 
               "highlighting indicates a whole-column error (e.g., only 1 Gear.Code can be tested 
                    at once, so if >1 Gear.Code occurs, the column will be orange).")
          }, 
          if("Year, month, & day make valid date" %in% validationResults()$invalidRowNumbsList$error){ 
            h4(style = "color: black;", span("Lime Green", style = "color: black; background-color: #23FF71;"), 
               "highlighting indicates the Month, Day, and Year combination do not make a legitemate date 
               (e.g., February 31st is not a date) or the date is before 1980 or is in the future.")
          }, 
          if ("Invalid Verified.TL" %in% validationResults()$invalidRowNumbsList$error |
              "Invalid Verified.Wr" %in% validationResults()$invalidRowNumbsList$error){
            h4(style = "color: black;", span("Light Blue", style = "color: black; background-color: #39BFFF;"),
               "highlighting marks cells in Verified.TL or Verified.Wr where there are values other than
               'verified' or '.' (suggesting 'verified' was mispelled and will not work).")
          },
          if("Correct column names and order" %in% validationResults()$invalidRowNumbsList$error){
            h4(style = "color: black;", span("Yellow", style = "color: black; background-color: #FFFF00;"), "indicates columns 
                     that are not allowed in the data file.")
          }
        )
      }else{
        h3(style = "color: green;","All validation rules have been met and a validated data file can be 
                    downloaded using the orange download button above.") 
      }
    })  
  })
  
  
  
  ##Test if sample data download should be allowed and create file download button#####
  #Test validated data to see if safe to download
  downloadSampleOK <- reactive({
    req(validationResults())
    errors <- as.character(validationResults()$validationTableVals$Status)
    if("Error" %in% errors){
      FALSE #don't download
    }else{
      TRUE #download ok
    }
  })
  #enable or disable download button based on above reactive
  output$downloadButtonUI <- renderUI({
    req(dataType())
    buttonText <- if (dataType() == "samplingData") {"Download validated sampling data"
    }else{"Dowload validated age data"}
    if(downloadSampleOK()) {
      downloadButton("valSampleData", buttonText, class="butn")
    } else {
      tags$button(id = "valSampleData", class = "btn btn-default shiny-download-link butn", 
                  type = "button", disabled = "disabled", 
                  buttonText)
    }
  })
  #add message about not being allowed to download files that do not pass validation
  observe({
    if(downloadSampleOK()==TRUE) {
      output$downloadMessage <- NULL #this removes the message if it was previously displayed
    }else{
      output$downloadMessage <- renderText("File did not pass validation, so it cannot be downloaded.
                      Please make edits in the data table below.  You will be able to download once it passes
                      the required validation tests.")
    }
  })
  
  # Download validated  data####
  observe({
    req(dataToTest(), downloadSampleOK())
    if(downloadSampleOK() == TRUE){ 
      output$valSampleData <- downloadHandler(
        filename = function() {
          if (dataType() == "samplingData") {
            paste(lakeYrGear(), "_SAMPLE_","validated_", date(), ".csv", sep = "")
          }else{
            paste(LkYrGrSp(), "_AGE_","validated_", date(), ".csv", sep = "")
          }
        },
        content = function(file) {
          sampleData <- dataToTest()
          sampleData[sampleData == "."] <- NA #Automatically convert period to NA's
          sampleData[sampleData == "NA"] <- NA
          
          sampleData$SampleID <- if (dataType() == "samplingData") {
            paste(sampleData$Lake.Code, sampleData$Station, sampleData$Month,
                  sampleData$Day, sampleData$Year, sampleData$Gear.Code,
                  sep = "") 
          }else{
            paste(sampleData$Lake.Code, sampleData$Year, sampleData$Month, 
                  sampleData$Gear, sep = "") 
          }
          sampleData <- sampleData %>%  relocate("SampleID", .after=Lake.Code)
          write.csv(sampleData, file, row.names = FALSE)
        }
      )
    }
  })

# Download pre-defined codes#################################
  
  # Downloadable csv of lake codes
  output$lakeCodeList <- downloadHandler(
    filename = function() {
      paste("lakeCodes", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(lakeCodes, file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of gear codes
  output$gearCodeList <- downloadHandler(
    filename = function() {
      paste("gearCodes", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(gearCodes, file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of species codes
  output$sppCodeList <- downloadHandler(
    filename = function() {
      paste("sppCodes", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(speciesCodes, file, row.names = FALSE)
    }
  )
  
}





