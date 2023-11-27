# 加载必要的库
library(shiny)
library(DT)

# Shiny应用的UI部分
ui <- fluidPage(
  titlePanel("Athelete Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("name", "Name", choices = c("All", "")),
      selectInput("position", "Position", choices = c("All", "")),
      selectInput("dataType", "Data Type", choices = c("All", "")),
      selectInput("dateSession", "Date/Session", choices = c("All", "")),
      actionButton("toggle", "Show/hide additional info")
    ),
    
    mainPanel(
      DTOutput("table")
    )
  )
)

# Shiny应用的Server部分
server <- function(input, output, session) {
  
  # 读取数据
  data <- read.csv("data/shiny_force.csv", stringsAsFactors = FALSE)
  
  # 更新下拉菜单选项，避免重复并添加“All”选项
  observe({
    updateSelectInput(session, "name", choices = c("All", unique(data$Name)))
    updateSelectInput(session, "position", choices = c("All", unique(data$Position)))
    updateSelectInput(session, "dataType", choices = c("All", unique(data$Data_type)))
    updateSelectInput(session, "dateSession", choices = c("All", unique(data$Session)))
  })
  
  # 初始设定表格显示除Position, Data.Type, Date.Session之外的所有列
  shownColumns <- reactiveVal(setdiff(names(data), c("Position", "Data_type", "Session")))
  
  # 当点击按钮时，切换显示/隐藏额外信息
  observeEvent(input$toggle, {
    if (identical(shownColumns(), setdiff(names(data), c("Position", "Data_type", "Session")))) {
      shownColumns(names(data))
    } else {
      shownColumns(setdiff(names(data), c("Position", "Data_type", "Session")))
    }
  })
  
  # 根据选择的条件筛选数据并更新表格
  output$table <- renderDT({
    # 在这里定义filteredData
    filteredData <- data
    if (input$name != "All") {
      filteredData <- filteredData[filteredData$Name == input$name, ]
    }
    if (input$position != "All") {
      filteredData <- filteredData[filteredData$Position == input$position, ]
    }
    if (input$dataType != "All") {
      filteredData <- filteredData[filteredData$Data_type == input$dataType, ]
    }
    if (input$dateSession != "All") {
      filteredData <- filteredData[filteredData$Session == input$dateSession, ]
    }
    
    # 创建颜色渐变函数
    colorGradient <- colorRampPalette(c('red', 'green'))
    
    # 创建DataTable并应用条件格式化
    dt <- datatable(filteredData[, shownColumns(), drop = FALSE], 
                    options = list(
                      scrollX = TRUE, 
                      scrollY = "400px", 
                      paging = TRUE) # 开启分页，提高性能
    )
    
    # 创建颜色渐变函数
    colorGradient <- colorRampPalette(c('red', 'green'))
    
    # 对每个指定的列应用颜色渐变
    for (colName in c("Peak.Propulsive.Power",  "Peak.Relative.Braking.Power", "Relative.Peak.Landing.Force")) {
      if (colName %in% names(filteredData)) {
        validValues <- na.omit(as.numeric(filteredData[[colName]]))
        if (length(validValues) > 0) {
          cuts <- quantile(validValues, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
          
          dt <- dt %>% formatStyle(
            colName,
            backgroundColor = styleInterval(
              cuts[-length(cuts)],  # 移除最后一个元素
              colorGradient(10)
            )
          )
        }
      }
    }
    
    return(dt)
  })
}

# 运行Shiny应用
shinyApp(ui = ui, server = server)
