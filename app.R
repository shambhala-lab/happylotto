library(shiny)
library(shinyMobile)
library(dplyr)
library(tidyr)

library(pool)
library(RPostgres) # ต้องมีเพื่อให้ dbPool รู้ว่าจะใช้ Engine ตัวไหน

source("secrets.R")

# สร้าง Pool (ใช้วิธีเรียกผ่าน pool แทน DBI)
pool <- dbPool(
  drv = Postgres(),
  host = db_config$host,
  dbname = db_config$dbname,
  user = db_config$user,
  password = db_config$pass,
  port = db_config$port,
  idleTimeout = 60000, # 1 นาทีปิดท่อ
  minSize = 0,         # เมื่อไม่มีคนใช้ ไม่ต้องคาเครื่องไว้เลย ให้เหลือ 0
  maxSize = 3          # แอปนี้ใช้คนเดียวหรือกลุ่มเล็ก 3 ท่อก็เหลือเฟือครับ  
)


# ทดสอบดึงข้อมูล "เลขว่าง" ที่เราคุยกันเมื่อกี้
# test_query <- function() {
#   con <- get_db_conn()
#   on.exit(dbDisconnect(con)) # ปิดการเชื่อมต่ออัตโนมัติเมื่อรันเสร็จ
#   
#   query <- "
#     SELECT all_nums.num
#     FROM (SELECT LPAD(generate_series(0, 99)::text, 2, '0') AS num) all_nums
#     LEFT JOIN lottery_bookings b ON all_nums.num = b.lotto_number AND b.period_id = 3
#     WHERE b.lotto_number IS NULL
#     ORDER BY all_nums.num;
#   "
#   dbGetQuery(con, query)
# }




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
        # ดึงเฉพาะ Card มาเสียบตรงนี้
        uiOutput("intro_card_db") 
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

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ฟังก์ชันสำหรับดึงวันที่งวดปัจจุบัน (Reactive)
  get_current_period_name <- reactive({
    # ไม่ต้องมี get_db_conn() และ dbDisconnect() แล้ว
    res <- dbGetQuery(pool, "SELECT display_name FROM lottery_periods WHERE status = 'กำลังเปิดจอง' LIMIT 1")
    
    if(nrow(res) > 0) res$display_name else "ไม่มีงวดที่เปิดจอง"
    
  })
  
  # Render ตัว Card โดยใช้ค่าที่ดึงมา
  output$intro_card_db <- renderUI({
    
    current_period <- get_current_period_name()
    
    f7Card(
      title = paste0("เลขท้าย 2 ตัว งวดวัน", current_period),
      f7Badge("ตัวละ 50 บาท", color = "orange"),
      
      br(), br(),
      
      "บัญชีจ่าย โอนเข้าบัญชี:",
      
      f7List(
        mode = "media",
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
  })  

  
  
  
  
  
  
  
  # ดึงข้อมูลการจองปัจจุบัน (เปลี่ยนชื่อจาก booked_data เดิม)
  booked_db <- reactive({
    # ใส่invalidateLater เพื่อให้แอปรีเฟรชข้อมูลอัตโนมัติทุก 30 วินาที (ถ้าต้องการ)
    # invalidateLater(30000) 
    
    query <- "
    SELECT b.lotto_number AS number, m.member_name AS name
    FROM lottery_bookings b
    JOIN lottery_members m ON b.member_id = m.id
    WHERE b.period_id = 3; -- งวด 1 ก.พ. 69
  "
    dbGetQuery(pool, query)
  })  
  
  # ในเซิร์ฟเวอร์ ดึงรายชื่อเพื่อนมาทำ choices  
  member_list <- reactive({
    res <- dbGetQuery(pool, "SELECT id, member_name FROM lottery_members ORDER BY member_name ASC")
    # ทำเป็น Named Vector: c("ชื่อ" = id) เพื่อให้ส่งค่า id กลับไปบันทึก
    setNames(res$id, res$member_name)
  })  
  
    
  # 3. Render ตารางเลข 00-99 (เน้นสีที่ตัวเลข/Label)
  output$lotto_grid <- renderUI({
    data <- booked_db()
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
      
      # เปลี่ยนมาดึงข้อมูลจาก Reactive ที่ต่อกับ DB
      data <- booked_db() 
      
      # ถ้าเลขยังไม่ถูกจอง (ไม่อยู่ใน DB) ให้ทำการ toggle selection
      if (!(num_str %in% data$number)) {
        current <- selected_nums()
        if (num_str %in% current) {
          selected_nums(setdiff(current, num_str))
        } else {
          selected_nums(c(current, num_str))
        }
      } else {
        # (Optional) ถ้าอยากให้กดเลขที่มีคนจองแล้วมีเสียงเตือนหรือ Toast ก็ใส่ตรงนี้ได้ครับ
        f7Toast(text = "เลขนี้มีเจ้าของแล้วจ้า", position = "bottom", color = "red")
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
          # ใน f7Popup (ตรง Smart Select) ให้แก้ choices เป็น:
          f7SmartSelect(
            inputId = "final_user_id", # เปลี่ยนเป็นส่ง id
            label = "ระบุชื่อผู้จอง",
            choices = member_list(),    # ใช้ค่าจาก reactive
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
    m_id <- as.integer(input$final_user_id) # รับเป็น ID มาเลย
    
    if (length(new_nums) > 0) {
      # สร้าง SQL ชุดเดียวเพื่อ INSERT หลายแถว (ป้องกัน SQL Injection)
      # หมายเหตุ: ในโปรเจกต์จริงควรใช้ sqlInterpolate แต่เบื้องต้นทำแบบนี้ให้เห็นภาพครับ
      
      con <- poolCheckout(pool) # หยิบการเชื่อมต่อออกมาทำงานพิเศษ
      on.exit(poolReturn(con))  # ทำเสร็จแล้วคืนท่อ
      
      # เริ่มต้น Transaction เพื่อความปลอดภัย (ถ้าพัง ให้พังทั้งหมด ไม่ลงค้างๆ คาๆ)
      dbBegin(con)
      tryCatch({
        for(num in new_nums) {
          dbExecute(con, 
                    "INSERT INTO lottery_bookings (period_id, member_id, lotto_number) VALUES (3, $1, $2)",
                    params = list(m_id, num)
          )
        }
        dbCommit(con)
        
        # ล้างค่าและปิด Popup
        selected_nums(character(0))
        f7Popup(id = "popup_booking", action = "close")
        f7Toast(text = "บันทึกข้อมูลลงฐานข้อมูลเรียบร้อย!", color = "green")
        
      }, error = function(e) {
        dbRollback(con)
        f7Noti(text = paste("เกิดข้อผิดพลาด:", e$message), color = "red")
      })
    }
  })
  
  # 7. ปิด Modal เมื่อยกเลิก
  observeEvent(input$cancel_booking, {
    f7Popup(id = "popup_booking", action = "close")
  })

  


  # [2. Render ตารางชำระเงิน]
  output$payment_table <- renderUI({
    # 1. ดึงข้อมูลจาก Reactive ที่เชื่อมกับ Database
    # แนะนำ: ถ้าอยากให้ละเอียดขึ้น ให้แก้ SQL ใน booked_db ให้ดึงคอลัมน์ payment_status มาด้วย
    # หรือจะเขียน Query ใหม่เฉพาะแท็บนี้เลยก็ได้ครับ
    
    query <- "
    SELECT m.member_name AS name, b.lotto_number AS number, b.payment_status
    FROM lottery_bookings b
    JOIN lottery_members m ON b.member_id = m.id
    WHERE b.period_id = 3
  "
    raw_data <- dbGetQuery(pool, query)
    
    if (nrow(raw_data) == 0) {
      return(f7Block(em("ยังไม่มีข้อมูลการจองในงวดนี้")))
    }
    
    # 2. ใช้ dplyr สรุปผลเหมือนเดิม
    summary_data <- raw_data %>%
      group_by(name) %>%
      summarise(
        nums = paste(sort(number), collapse = "  "),
        count = n(),
        # ใน DB เราเก็บเป็น boolean (T/F) เลยต้องเช็คค่าแบบนี้ครับ
        is_paid = any(payment_status == TRUE) 
      ) %>%
      arrange(is_paid, name) # เรียงคนยังไม่จ่ายขึ้นก่อน
    
    # 3. สร้าง UI List
    f7List(
      inset = TRUE,
      mode = "media",
      lapply(1:nrow(summary_data), function(i) {
        f7ListItem(
          title = paste0(summary_data$name[i], " (", summary_data$count[i], ")"),
          subtitle = summary_data$nums[i],
          # แสดง Badge ตามสถานะใน Database
          right = if(summary_data$is_paid[i]) 
            f7Badge("จ่ายแล้ว", color = "blue") 
          else 
            f7Badge("ค้างชำระ", color = "red")
        )
      })
    )
  })
  
  
  
  # เมื่อ User ปิด Browser ให้หยุดแอปทันที (เพื่อประหยัดชั่วโมง)
  session$onSessionEnded(function() {
    stopApp()
  })

    
}

shinyApp(ui, server)