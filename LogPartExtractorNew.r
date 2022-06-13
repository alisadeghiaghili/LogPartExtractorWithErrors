source("loadLibs.R")
source("loadFuncs.R")

logFileName <- choose.files()

text <- readLines(logFileName) %>% 
  tolower()

reqDF <- data.frame("ReqStart" = as.numeric(NA), 
                    "ReqFinish" = as.numeric(NA)) %>% 
  na.omit()

reqStarts <- grep(x = text, pattern = "request starting")
reqFinishes <- grep(x = text, pattern = "request finished")

if (length(reqStarts) > length(reqFinishes)) {
  reqFinishes <- c(reqFinishes, NA)
} else if (length(reqStarts) < length(reqFinishes)) {
  reqStarts <- c(NA, reqStarts)
}

reqDF <- data.frame(ReqStart = reqStarts, 
                    ReqFinish = reqFinishes)

reqDF <- reqDF %>% 
  mutate(ReqStartMine = ReqStart,
         ReqFinishMine = ReqFinish)

if(is.na(reqDF[1,]$ReqStartMine)) {
  reqDF[1,]$ReqStartMine <- 1
}

if(is.na(reqDF[nrow(reqDF),]$ReqFinishMine)) {
  reqDF[nrow(reqDF),]$ReqFinishMine <- length(text)
}

reqDF <- reqDF %>% 
  fill(ReqStart, ReqStartMine) %>% 
  group_by(ReqStart, ReqStartMine) %>% 
  summarise(ReqFinish = max(ReqFinish),
            ReqFinishMine = max(ReqFinishMine)) %>% 
  mutate("Version" = as.character(NA),
         "ErrorLine" = as.character(NA),
         "ErrorMessage" = as.character(NA),
         "ResourceLine" = as.character(NA),
         "Resource" = as.character(NA),
         "RequestType" = as.character(NA), 
         "UserNameLine" = as.character(NA), 
         "UserName" = as.character(NA),
         "FileName" = logFileName)

for (dfRow in 1:nrow(reqDF)) {
  ResourceLines <- grep(x = text[reqDF[dfRow, ]$ReqStartMine:reqDF[dfRow, ]$ReqFinishMine], pattern = 'https://api.ime.co.ir/.*') + reqDF[dfRow, ]$ReqStartMine - 1
  if(length(ResourceLines) > 0) {
    reqDF[dfRow, ]$Version <- str_extract(string = text[c(ResourceLines)], pattern = "https://api.ime.co.ir/.*\\?") %>% str_remove("\\?") %>% str_extract(pattern = "v\\d+") %>% str_c(collapse = ", ")
    reqDF[dfRow, ]$ResourceLine <- ResourceLines %>% as.character() %>% str_c(collapse = ", ")
    reqDF[dfRow, ]$RequestType <- str_extract_all(string = text[c(ResourceLines)], pattern = "post|get|delete|put|patch|head") %>% 
      unlist() %>% 
      str_c(collapse = ", ")
    
    UserNameLine <- grep(x = text[reqDF[dfRow, ]$ReqStartMine:reqDF[dfRow, ]$ReqFinishMine], pattern = '\\[\\"name\\"\\,\\"[a-zA-Z.]*\\"\\]') + reqDF[dfRow, ]$ReqStartMine - 1
    reqDF[dfRow, ]$UserNameLine <- UserNameLine %>% as.character() %>% str_c(collapse = ", ")
    reqDF[dfRow, ]$UserName <- str_extract_all(string = text[reqDF[dfRow, ]$ReqStartMine:reqDF[dfRow, ]$ReqFinishMine], pattern = '\\[\\"name\\"\\,\\"[a-zA-Z.]*\\"\\]', simplify = TRUE) %>% 
      as.vector() %>% 
      str_c(collapse = ", ")
  }
  
  ErrorLines <- grep(x = text[reqDF[dfRow, ]$ReqStartMine:reqDF[dfRow, ]$ReqFinishMine], pattern = '\\[err\\]') + reqDF[dfRow, ]$ReqStartMine - 1
  if(length(ErrorLines) > 0) {
    reqDF[dfRow, ]$ErrorLine <- ErrorLines %>% as.character() %>% str_c(collapse = ", ")
    reqDF[dfRow, ]$ErrorMessage <- str_extract(string = text[c(ErrorLines)], pattern = "\\[err\\].*") %>% str_remove("\\[err\\]") %>% str_trim() %>% str_c(collapse = ", ")
  }
}

reqDF <- reqDF %>% 
  mutate(FileName = logFileName,
         Resource = ifelse(ResourceLine == "", NA, Resource),
         UserName = ifelse(UserNameLine == "", NA, UserName),
         ReqStartMine = NULL,
         ReqFinishMine = NULL) %>% 
  filter(!is.na(ResourceLine)) %>% 
  filter(ResourceLine != "")


# result <- reqDF %>% 
#   mutate(UserName = UserName %>% 
#            str_replace_all(pattern = ", ,", replacement = "") %>% 
#            str_replace_all(pattern = ", \\[", replacement = "") %>% 
#            str_trim() %>% 
#            str_replace(pattern = "\\]\\s+", replacement = " ") %>% 
#            str_replace_all(pattern = '\\"name\\",\\"', replacement = "") %>% 
#            str_replace_all(pattern = '\\]', replacement = " ") %>% 
#            str_replace_all(pattern = '\\"', replacement = "") %>% 
#            str_replace(pattern = " \\s+,$", replacement = "") %>% 
#            str_replace_all(pattern = " \\[", replacement = " ") %>% 
#            str_trim() %>% 
#            str_replace_all(pattern = " \\s+", replacement = ", ")) %>% 
#   separate_rows(Version, UserNameLine, UserName, ErrorLine, ErrorMessage, sep = "[, ]+")



#https://api.ime.co.ir/v\\d+/\\w+/\\w+?

ui <- fluidPage(
  titlePanel("LogPart Extractor"),
  DTOutput('tbl')
)

server <- function(input, output, session) {
  output$tbl = renderDT(
    datatable(reqDF,
              options = list(
                dom = "lBftrip",
                buttons = c('csv', 'excel'),
                server = FALSE,
                lengthMenu = c(10, 25, 50, 100), 
                pageLength = 100
              ),
              extensions = 'Buttons',
              selection = 'single',        ## enable selection of a single row
              filter = c("top"),              ## include column filters at the top
              rownames = TRUE,             ## show row numbers/names
    )              
  )
}

shinyApp(ui = ui, server = server)