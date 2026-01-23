library(shiny)

# 1. UI - ส่วนการแสดงผล
ui <- fluidPage(
  titlePanel("ทดสอบการ Deploy: หวยปันสุข"),
  sidebarLayout(
    sidebarPanel(
      helpText("ถ้าคุณเห็นข้อความนี้ แสดงว่าการเชื่อมต่อจาก GitHub สำเร็จ!"),
      sliderInput("bins", "จำนวนแท่งกราฟ:", min = 1, max = 50, value = 30)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# 2. Server - ส่วนการคำนวณ
server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         main = "Histogram ทดสอบระบบ", xlab = "เวลาที่รอ (นาที)")
  })
}

# 3. รันแอป
shinyApp(ui = ui, server = server)
