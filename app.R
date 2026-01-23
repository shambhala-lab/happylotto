library(shiny)
library(shinyMobile)
library(dplyr)
library(tidyr)

# งวดวันที่
current_period <- "01 ก.พ. 69"

ui <- f7Page(
  title = "ลุ้นหวยกัน เพื่อนปันสุข",
  options = list(theme = "ios", dark = FALSE, color = "green"),
  f7TabLayout(
    navbar = f7Navbar(
      title = "ลุ้นหวยกัน เพื่อนปันสุข 🎫",
      hairline = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      
      # --- แท็บที่ 1: Intro ---
      f7Tab(
        title = "หน้าแรก",
        tabName = "Intro",
        icon = f7Icon("info_circle_fill"),
        f7BlockTitle("วัตถุประสงค์", size = "large"),
        f7Block(
          strong = TRUE,
          inset = TRUE,
          "ทำบุญให้โรงพยาบาลต่างๆ และ อื่นๆ"
        ),
        f7Card(
          title = paste("เลขท้าย 2 ตัว งวดวันที่", current_period),
          f7Badge("ตัวละ 50 บาท", color = "orange"),
          br(), br(),
          "บัญชีจ่าย โอนเข้าบัญชี:",
          f7List(
            mode = "media", # แก้จุดที่ 1
            f7ListItem(
              title = "นายปฐมพงศ์ สุขแสนโชติ", 
              subtitle = "219-1-49993-4 ธ.กรุงศรี",
              media = f7Icon("creditcard_fill")
            )
          ),
          footer = span(
            "ผู้ถูกรางวัลได้รับเงินสดสูงสุด 3,750 บาท (1:75) ",
            tags$small(style="color:red; font-weight:bold;", "หักทำบุญ 1,250 บาท")
          )
        )
      ),
      
      # --- แท็บที่ 2: ตารางจอง ---
      f7Tab(
        title = "จองเลข",
        tabName = "Booking",
        icon = f7Icon("square_grid_3x2_fill"),
        
        f7BlockTitle("เลือกหมายเลขที่ต้องการ", size = "medium"),
        f7Block(
          style = "overflow-y: auto; max-height: 400px; background: #ffffff; padding: 10px; border-radius: 15px; border: 1px solid #ddd;",
          uiOutput("lotto_grid")
        ),
        
        f7List(
          inset = TRUE,
          f7SmartSelect(
            inputId = "user_name",
            label = "เลือกชื่อผู้จอง",
            choices = c("ปู", "ปุ้น", "ป๊อบ", "ดอนนี่", "นาถ", "ตี๋", "อ้อ", "เอ", "เจ๊นก", "บอย", "เก๋", "โจ๊ก", "เบนซ์", "บอม", "ศรีกุล", "แอนเลอร์", "หน่อย", "โอเล่", "เก้อ", "หาญ", "เกมส์", "รวีวรรณ", "พจน์", "เอี่ยว"),
            openIn = "sheet"
          )
        ),
        f7Block(
          # แก้จุดที่ 2: ตรงนี้ใช้ fill = TRUE ได้เพราะเป็นปุ่มเดี่ยว
          f7Button(inputId = "confirm_booking", label = "ยืนยันการจอง", color = "green", fill = TRUE)
        )
      ),
      
      # --- แท็บที่ 3: การชำระเงิน ---
      f7Tab(
        title = "ยอดชำระ",
        tabName = "Payment",
        icon = f7Icon("money_dollar_circle_fill"),
        f7BlockTitle("สรุปรายการจองและสถานะเงิน"),
        uiOutput("payment_table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ข้อมูลทดสอบ (Static Data)
  raw_data <- list(
    "ปู" = c("31", "35", "38", "67", "83", "85"), "ปุ้น" = c("06", "18", "25", "52", "60", "68", "81", "86"),
    "ป๊อบ" = c("05", "11", "15", "29", "44", "48", "50", "51", "84", "92"), "ดอนนี่" = c("12", "20", "22", "57"),
    "นาถ" = c("26", "47", "64", "72"), "ตี๋" = c("08", "24", "42", "56", "65", "74", "80"),
    "อ้อ" = c("40", "61", "87"), "เอ" = c("17", "70", "71", "73"), "เจ๊นก" = c("09", "27"),
    "บอย" = c("01", "32", "59", "95"), "เก๋" = c("02", "69", "82", "94"), "โจ๊ก" = c("03", "13", "23", "88", "89", "98"),
    "เบนซ์" = c("37", "53", "75", "93"), "บอม" = c("91", "96", "97", "99"), "ศรีกุล" = c("39", "49"),
    "แอนเลอร์" = c("33", "76", "77", "78", "79"), "หน่อย" = c("10", "28"), "โอเล่" = c("46"),
    "เก้อ" = c("45", "54", "66"), "หาญ" = c("07", "55", "58"), "เกมส์" = c("00", "04", "14", "16"),
    "รวีวรรณ" = c("43", "62", "63", "90"), "พจน์" = c("19", "36"), "เอี่ยว" = c("21", "30", "34", "41")
  )
  paid_users <- c("เก้อ", "หาญ", "รวีวรรณ", "เอี่ยว")
  
  init_df <- stack(raw_data) %>%
    rename(number = values, name = ind) %>%
    mutate(number = sprintf("%02d", as.numeric(as.character(number))),
           status = if_else(name %in% paid_users, "จ่ายแล้ว", ""))
  
  booked_data <- reactiveVal(init_df)
  
  # 1. Render ตารางเลข 00-99 (แก้ไขเรื่องสีและ Outline)
  output$lotto_grid <- renderUI({
    data <- booked_data()
    lapply(0:99, function(i) {
      num_str <- sprintf("%02d", i)
      is_booked <- num_str %in% data$number
      booker <- if(is_booked) as.character(data$name[data$number == num_str]) else ""
      
      tags$div(
        style = "display: inline-block; width: 18%; margin: 1%; text-align: center; vertical-align: top;",
        f7Button(
          inputId = paste0("num_", num_str),
          label = num_str,
          # ถ้าจองแล้วใช้สีเทา (gray) ถ้าว่างใช้สีเขียว (green)
          color = if(is_booked) "gray" else "green",
          fill = TRUE # ใช้แบบทึบทั้งคู่เพื่อความสม่ำเสมอและเลี่ยง Error
        ),
        tags$small(style = "font-size: 0.6em; display: block; overflow: hidden; white-space: nowrap; text-overflow: ellipsis; color: #555;", booker)
      )
    })
  })
  
  # 2. Render ตารางชำระเงิน
  output$payment_table <- renderUI({
    data <- booked_data()
    summary_data <- data %>%
      group_by(name) %>%
      summarise(
        nums = paste(sort(number), collapse = "  "),
        count = n(),
        pay_status = first(status)
      ) %>%
      arrange(desc(pay_status), name)
    
    f7List(
      inset = TRUE,
      mode = "media", # แก้จุดที่ 4: ใส่เพื่อให้ subtitle ทำงานได้
      lapply(1:nrow(summary_data), function(i) {
        f7ListItem(
          title = paste0(summary_data$name[i], " (", summary_data$count[i], ")"),
          subtitle = summary_data$nums[i],
          right = if(summary_data$pay_status[i] == "จ่ายแล้ว") 
            f7Badge("จ่ายแล้ว", color = "blue") 
          else 
            ""
        )
      })
    )
  })
}

shinyApp(ui, server)
