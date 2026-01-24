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
      
      # --- แท็บที่ 2: ตารางจอง (ปรับปรุงใหม่) ---
      f7Tab(
        title = "จองเลข",
        tabName = "Booking",
        icon = f7Icon("square_grid_3x2_fill"),
        f7BlockTitle("เลือกหมายเลขที่ต้องการ", size = "medium"),
        
        f7Block(
          style = "overflow-y: auto; max-height: 450px; background: #ffffff; 
                      padding: 10px; border-radius: 15px; border: 1px solid #ddd; text-align: center;",
          uiOutput("lotto_grid")
        ),
        
        f7Block(
          f7Button(inputId = "pre_confirm", label = "ยืนยันการเลือก", color = "green", fill = TRUE)
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
    "ดอนนี่" = c("12", "20", "22", "57"),
    "นาถ" = c("26", "47", "64", "72"), 
    "ตี๋" = c("08", "24", "42", "56", "65", "74", "80"),
    "อ้อ" = c("40", "61", "87"),
    "เอ" = c("17", "70", "71", "73"),
    "เจ๊นก" = c("09", "27"),
    "บอย" = c("01", "32", "59", "95"), 
    "เก๋" = c("02", "69", "82", "94"),
    "โจ๊ก" = c("03", "13", "23", "88", "89", "98"),
    "บอม" = c("91", "96", "97", "99"), 
    "ศรีกุล" = c("39", "49"),
    "เก้อ" = c("45", "54", "66"),
    "พจน์" = c("19", "36"),
    "เอี่ยว" = c("21", "30", "34", "41")
  )
  paid_users <- c("เก้อ", "หาญ", "รวีวรรณ", "เอี่ยว")
  
  init_df <- stack(raw_data) %>%
    rename(number = values, name = ind) %>%
    mutate(number = sprintf("%02d", as.numeric(as.character(number))),
           status = if_else(name %in% paid_users, "จ่ายแล้ว", ""))
  
  # 1. ข้อมูลการจองหลัก (จากข้อความเดิมของคุณ)
  booked_data <- reactiveVal(init_df) # init_df คือตัวเดิมที่เรามี
  
  # 2. เก็บรายการที่ User กำลังจิ้มเลือก (ยังไม่ได้บันทึกลง DB)
  selected_nums <- reactiveVal(character(0))
  
  # 3. Render ตารางเลข 00-99 (เน้นสีที่ตัวเลข/Label)
  output$lotto_grid <- renderUI({
    data <- booked_data()
    selection <- selected_nums()
    
    lapply(0:99, function(i) {
      num_str <- sprintf("%02d", i)
      is_booked <- num_str %in% data$number
      is_selecting <- num_str %in% selection
      booker <- if(is_booked) as.character(data$name[data$number == num_str]) else ""
      
      # เปลี่ยนสี Font ของตัวเลขบนปุ่ม
      # ถ้าจองแล้ว = แดง, กำลังเลือก = ส้ม, ว่าง = ขาว
      text_color <- if(is_booked) "#A9A9A9" else if(is_selecting) "#FF9500" else "#FFFFFF"
      
      # ความหนาของตัวอักษร
      text_weight <- if(is_booked || is_selecting) "bold" else "normal"
      
      tags$div(
        style = "display: inline-block; width: 17%; margin: 1%; text-align: center; vertical-align: top;",
        f7Button(
          inputId = paste0("btn_", num_str),
          # ใช้ tags$span เพื่อคุมสีของตัวเลข label
          label = tags$span(num_str, style = paste0("color:", text_color, "; font-weight:", text_weight, "; font-size: 1.2em;")),
          color = "green", # ใช้พื้นหลังเขียวเหมือนกันหมด
          fill = TRUE
        ),
        tags$small(
          style = paste0("font-size: 0.6em; display: block; height: 1.2em; overflow: hidden; color:", 
                         if(is_booked) "#A9A9A9" else "#FF9500", ";"), 
          if(is_booked) booker else if(is_selecting) "เลือกอยู่" else ""
        )
      )
    })
  })
  
  # 4. Logic การกดปุ่มเลข (Toggle Selection)
  lapply(0:99, function(i) {
    num_str <- sprintf("%02d", i)
    observeEvent(input[[paste0("btn_", num_str)]], {
      data <- booked_data()
      # ถ้าเลขยังไม่ถูกจอง ให้ทำการ toggle selection
      if (!(num_str %in% data$number)) {
        current <- selected_nums()
        if (num_str %in% current) {
          selected_nums(setdiff(current, num_str))
        } else {
          selected_nums(c(current, num_str))
        }
      }
    })
  })
  
  # 5. เมื่อกดปุ่ม "ยืนยันการเลือก" -> เปิด Modal
  observeEvent(input$pre_confirm, {
    selection <- selected_nums()
    
    if (length(selection) == 0) {
      f7Noti(text = "กรุณาเลือกอย่างน้อย 1 หมายเลข", icon = f7Icon("exclamation_triangle"), color = "red")
    } else {
      # สร้าง Modal (Popup)
      f7Popup(
        id = "popup_booking",
        title = "ยืนยันการจอง",
        f7Block(
          strong(paste("คุณเลือกทั้งหมด", length(selection), "หมายเลข:")),
          p(paste(sort(selection), collapse = ", "), style = "font-size: 1.2em; color: #2196f3; font-weight: bold;")
        ),
        f7List(
          inset = TRUE,
          f7SmartSelect(
            inputId = "final_user_name",
            label = "ระบุชื่อผู้จอง",
            choices = c("ปู", "ปุ้น", "ป๊อบ", "ดอนนี่", "นาถ", "ตี๋", "อ้อ", "เอ", "เจ๊นก", "บอย", "เก๋", "โจ๊ก", "เบนซ์", "บอม", "ศรีกุล", "แอนเลอร์", "หน่อย", "โอเล่", "เก้อ", "หาญ", "เกมส์", "รวีวรรณ", "พจน์", "เอี่ยว"),
            openIn = "sheet"
          )
        ),
        f7Block(
          f7Row(
            f7Button(inputId = "cancel_booking", label = "ยกเลิก", color = "red"),
            f7Button(inputId = "final_confirm", label = "ตกลง", color = "green", fill = TRUE)
          )
        )
      )
    }
  })
  
  # 6. Logic เมื่อกดปุ่ม "ตกลง" ใน Modal เพื่อบันทึกการจองใหม่
  observeEvent(input$final_confirm, {
    new_nums <- selected_nums()
    user <- input$final_user_name
    
    if (length(new_nums) > 0) {
      # สร้างข้อมูลใหม่เพื่อเอาไปต่อท้าย data เดิม
      new_data <- data.frame(
        number = new_nums,
        name = user,
        status = "", # ยังไม่จ่าย
        stringsAsFactors = FALSE
      )
      
      # อัปเดตค่า booked_data
      updated_df <- bind_rows(booked_data(), new_data)
      booked_data(updated_df)
      
      # ล้างค่าที่เลือกไว้ และปิด Modal
      selected_nums(character(0))
      f7Popup(id = "popup_booking", action = "close")
      
      # แจ้งเตือนสวยๆ
      f7Toast(text = paste("จองเลขให้คุณ", user, "เรียบร้อย!"), position = "bottom", color = "green")
    }
  })  
  
  # 7. ปิด Modal เมื่อยกเลิก
  observeEvent(input$cancel_booking, {
    f7Popup(id = "popup_booking", action = "close")
  })

  
  
  # [2. Render ตารางชำระเงิน]
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
