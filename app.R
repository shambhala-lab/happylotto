library(shiny)
library(shinyMobile)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

library(pool)
library(RPostgres)

# Global Settings / App Info
APP_VERSION <- "2.1"
APP_BUILD   <- "20261003 (452d425)" # Short Commit Hash


# ดึงค่า Environment Variables (ถ้าไม่พบ ให้ใช้ค่า fallback ด้านหลัง)
pool <- dbPool(
  drv = Postgres(),
  host = Sys.getenv("DB_HOST"),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  #port = 5434, # test db
  port = 5432,
  idleTimeout = 60000, # 10 นาทีปิดท่อ
  minSize = 3,         
  maxSize = 5          
)

# --- ฟังก์ชันสำหรับทำความสะอาดชื่อผู้จอง ---
clean_member_name <- function(name) {
  if (is.null(name) || name == "") return("")
  
  name %>% 
    trimws() %>%
    # ตัดเฉพาะ สระ หรือ วรรณยุกต์ ที่เผลอพิมพ์นำหน้าพยัญชนะ (ยกเว้นพวก เ แ โ ใ ไ ที่อยู่หน้าได้)
    # เราจะตัดกลุ่ม ะ า ิ ี ึ ื ุ ู ั ํ ่ ้ ๊ ๋ ็ ์ ที่อยู่ตัวแรกสุดทิ้ง
    gsub("^[ะาิีึืุูัํ่้๊๋็์]+", "", .) %>%
    trimws()
}

ui <- f7Page(
  title = "ลุ้นหวยกัน เพื่อนปันสุข",
  options = list(theme = "ios", dark = FALSE, color = "green"),
  
  # --- เพิ่ม JS และ CSS สำหรับจัดการหน้า Disconnect ---
  tags$head(
    tags$style(HTML("
      /* ซ่อนกล่อง Oops... disconnected สีเทา */
      #shiny-disconnected-msg {
        display: none !important;
      }
    ")),
    
    tags$script(HTML("
      $(document).on('shiny:disconnected', function(event) {
        document.body.innerHTML = 
          '<div style=\"background: #1c1c1e; height: 100vh; display: flex; flex-direction: column; align-items: center; justify-content: center; color: white; font-family: -apple-system, BlinkMacSystemFont, sans-serif; text-align: center; padding: 20px;\">' +
          '  <div style=\"font-size: 64px; margin-bottom: 20px;\">🔒</div>' +
          '  <h2 style=\"margin: 0; font-weight: 600; font-size: 22px;\">ปิดการทำงานเรียบร้อยแล้ว</h2>' +
          '  <p style=\"color: #8e8e93; margin-top: 10px; font-size: 14px; line-height: 1.5;\">Session ปัจจุบันถูกยุติแล้ว<br>ขอบคุณที่ใช้งาน HappyLotto App</p>' +
          '  <button onclick=\"window.location.reload()\" style=\"margin-top: 30px; padding: 12px 32px; border-radius: 25px; border: none; background: #34c759; color: white; font-size: 16px; font-weight: bold; cursor: pointer; box-shadow: 0 4px 12px rgba(52,199,89,0.3);\">เข้าสู่ระบบอีกครั้ง</button>' +
          '</div>';
      });
    "))
  ),
  
  # --- เพิ่มหน้า Login เข้าไป ---
  f7Login(id = "login", title = "Welcome", cancellable = TRUE),
  
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
          f7Grid(cols = 2,
                 # ปุ่มเช็คเลขว่าง
                 f7Button(inputId = "check_available", label = "เช็คเลขว่าง", color = "blue", fill = TRUE),
                 # ปุ่มยืนยันเดิม
                 f7Button(inputId = "pre_confirm", label = "ยืนยันการเลือก", color = "green", fill = TRUE)
          )          
        )
      ),
      
      # --- แท็บที่ 3: การชำระเงิน ---
      f7Tab(
        title = "ยอดชำระ",
        tabName = "Payment",
        icon = f7Icon("money_dollar_circle_fill"),
        f7BlockTitle("สรุปรายการจองและสถานะเงิน"),
        uiOutput("payment_table"),
        
        # เพิ่มปุ่มปิดงวดไว้ท้ายตาราง
        f7Block(
          f7Button(inputId = "close_period_btn", label = "สรุปปิดงวด", color = "red", fill = TRUE)
        )
      ),
      
      # --- แท็บที่ 4: ตั้งค่าระบบ (Settings) ---
      f7Tab(
        title = "ตั้งค่า",
        tabName = "Settings",
        icon = f7Icon("gear_alt_fill"),
        
        # 1. จัดการงวดปัจจุบัน (โชว์งวดปัจจุบัน + ช่องแก้ไข)
        f7BlockTitle("จัดการงวดปัจจุบัน"),
        f7List(
          inset = TRUE,
          mode = "media",
          uiOutput("current_period_settings_ui") # Server จะส่ง ชื่องวดปัจจุบัน + f7DatePicker / f7Text มาโชว์
        ),
        f7Block(
          f7Button(inputId = "update_period_date_btn", label = "บันทึกการเปลี่ยนวันที่", color = "green", fill = TRUE)
        ),
        
        # 2. ข้อมูลเวอร์ชันแอป
        f7BlockTitle("เกี่ยวกับระบบ"),
        f7List(
          inset = TRUE,
          mode = "media",
          f7ListItem(
            title = "HappyLotto App",
            subtitle = HTML(paste0("เวอร์ชัน ", APP_VERSION, "<br/>Build ", APP_BUILD)),
            media = f7Icon("info_circle")
          )
        ),
        
        # 3. การทำงานของระบบ & สถานะ Database
        f7BlockTitle("การทำงานของระบบ"),
        f7List(
          inset = TRUE,
          mode = "media",
          # แสดงสถานะการเชื่อมต่อ DB
          uiOutput("db_status_ui")
        ),
        f7Block(
          f7Button(inputId = "shutdown_app_btn", 
                   label = "ปิดการทำงานแอปพลิเคชัน",
                   color = "red", fill = TRUE)
        )

      )
      
      
    )
  )
)

server <- function(input, output, session) {
  
  # Temporary list of numbers before PRE-CONFIRM
  selected_nums <- reactiveVal(character(0))
  
  # Chunk of selected numbers after PRE-CONFIRM
  confirmed_list <- reactiveVal(character(0))  
  
  # A selected name who buy the tickets
  confirmed_name <- reactiveVal("") 
  
  db_trigger <- reactiveVal(0)  
  
  # ==========================================
  # ส่วนของ SERVER (แท็บที่ 1: อินโทร)
  # ==========================================  
  
  # --- ฟังก์ชันดึง ID งวดปัจจุบันที่ 'กำลังเปิดจอง' ---
  current_period_id <- reactive({
    
    db_trigger() # <--- เพิ่มบรรทัดนี้
    
    # ดึงงวดที่ status = 'กำลังเปิดจอง' มา 1 อัน
    res <- dbGetQuery(pool, "SELECT id FROM lottery_periods WHERE status = 'กำลังเปิดจอง' LIMIT 1")
    
    if(nrow(res) > 0) {
      return(as.integer(res$id))
    } else {
      return(NULL) # กรณีไม่มีงวดไหนเปิดจองเลย
    }
  })
  
  
  # ฟังก์ชันสำหรับดึงวันที่งวดปัจจุบัน (Reactive)
  get_current_period_name <- reactive({
    
    db_trigger() # <--- เพิ่มบรรทัดนี้
    
    # ไม่ต้องมี get_db_conn() และ dbDisconnect() แล้ว
    res <- dbGetQuery(pool, "SELECT display_name FROM lottery_periods WHERE status = 'กำลังเปิดจอง' LIMIT 1")
    
    if(nrow(res) > 0) res$display_name else "ไม่มีงวดที่เปิดจอง"
    
  })
  
  # Render ตัว Card โดยใช้ค่าที่ดึงมา
  output$intro_card_db <- renderUI({
    
    current_period <- get_current_period_name()
    
    f7Card(
      title = paste0("เลขท้าย 2 ตัว งวดวันที่ ", current_period),
      f7Badge("ตัวละ 50 บาท", color = "orange"),
      
      br(), br(),
      
      "บัญชีจ่าย โอนเข้าบัญชี:",
      
      br(), br(),
      
      f7List(
        mode = "media",
        f7ListItem(
          title = "นายปฐมพงศ์ สุขแสนโชติ", 
          subtitle = "219-1-49993-4 ธ.กรุงศรี",
          media = f7Icon("creditcard_fill")
        )
      ),
      
      br(), br(),
      
      "ผู้ถูกรางวัลได้รับเงินสดสูงสุด 3,750 บาท (1:75) ",
      
      br(), br(),
      
      footer = span(
        tags$small(style="color:red; font-weight:bold;", "**หมายเหตุ** หักทำบุญ 1,250 บาท")
      )
    )
  })
  
  
  # ==========================================
  # ส่วนของ SERVER (แท็บที่ 2: การจอง)
  # ==========================================
  
  # ดึงข้อมูลการจองปัจจุบัน (เปลี่ยนชื่อจาก booked_data เดิม)
  booked_db <- reactive({
    db_trigger()
    p_id <- current_period_id() # ดึงค่าจาก reactive
    req(p_id) # ต้องมีงวดที่เปิดอยู่ถึงจะทำต่อ
    
    query <- "SELECT b.lotto_number AS number, m.member_name AS name
                FROM lottery_bookings b
                JOIN lottery_members m ON b.member_id = m.id
                WHERE b.period_id = $1"
    
    dbGetQuery(pool, query, params = list(p_id))
  })
  
  
  # ในเซิร์ฟเวอร์ ดึงรายชื่อเพื่อนมาทำ choices  
  member_list <- reactive({
    db_trigger() # <--- เติมไว้เพื่อให้รายชื่อเพื่อนอัปเดตแบบ Real-time     
    res <- dbGetQuery(pool, "SELECT id, member_name FROM lottery_members ORDER BY member_name ASC")
    # ทำเป็น Named Vector: c("ชื่อ" = id) เพื่อให้ส่งค่า id กลับไปบันทึก
    setNames(res$id, res$member_name)
  })  
  
  
  # 3. Render ตารางเลข 00-99 (เน้นสีที่ตัวเลข/Label)
  output$lotto_grid <- renderUI({
    
    data <- booked_db()
    selection <- selected_nums()
    
    
    #--- 3.1 Render ตาราง ---
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
  
  #--- 3.2 การจัดการปุ่มตัวเลข (จอง/ยกเลิก) ---
  lapply(0:99, function(i) {
    num_str <- sprintf("%02d", i)
    
    # --- 3.2.1 เลือกเลขจอง  ---
    observeEvent(input[[paste0("btn_", num_str)]], {
      # ถ้ากดปุ่มปุ๊บ ให้เช็คข้อมูลล่าสุดจาก DB ทันที
      data <- booked_db() 
      current <- selected_nums()
      
      if (!(num_str %in% data$number)) {
        if (num_str %in% current) {
          selected_nums(setdiff(current, num_str))
        } else {
          selected_nums(c(current, num_str))
        }
      } else {
        # --- [Logic ใหม่: ถ้าเลขมีคนจองไปแล้ว] ---
        f7Dialog(
          id = paste0("confirm_delete_", num_str),
          title = "⚠️ เลขนี้มีเจ้าของแล้ว",
          text = paste0("เลข ", num_str, " ถูกจองไปแล้ว คุณต้องการ 'ยกเลิกการจอง' ของเลขนี้ใช่หรือไม่?"),
          type = "confirm" # แบบ confirm จะมีปุ่ม ตกลง (OK) และ ยกเลิก (Cancel)
        )
      }
    })
    
    # --- 3.2.2 ยกเลิกเลขจอง  ---
    observeEvent(input[[paste0("confirm_delete_", num_str)]], {
      req(isTRUE(input[[paste0("confirm_delete_", num_str)]]))
      
      period_id_val <- current_period_id()
      req(!is.null(period_id_val))
      
      query <- sprintf(
        "DELETE FROM public.lottery_bookings WHERE lotto_number = '%s' AND period_id = %d", 
        num_str, 
        period_id_val
      )
      
      tryCatch({
        dbExecute(pool, query)
        f7Toast(text = paste0("ยกเลิกการจองเลข ", num_str, " ของงวดนี้เรียบร้อยแล้ว"), position = "bottom")
        db_trigger(db_trigger() + 1)
        # (อย่าลืมสั่งอัปเดต booked_db() หรือ trigger หน้าจอตรงนี้ด้วยนะครับพี่)
      }, error = function(e) {
        f7Toast(text = "เกิดข้อผิดพลาดในการลบข้อมูล", position = "bottom")
      })
    })
    
  })
  
  # 5. เมื่อกดปุ่ม "ยืนยันการเลือก" -> เปิด Modal
  observeEvent(input$pre_confirm, {
    selection <- selected_nums()
    
    if (length(selection) == 0) {
      f7Notif(text = "กรุณาเลือกอย่างน้อย 1 หมายเลข")
    } else {
      
      confirmed_list(selection) # <--- "แช่แข็ง" เลขที่เลือกไว้ที่นี่
      
      f7Popup(
        id = "popup_booking",
        title = "ยืนยันการจอง",
        swipeToClose = TRUE,
        page = FALSE,
        
        # --- จุดที่เปลี่ยน: ใช้ uiOutput แทนการเขียนข้อความตรงๆ ---
        uiOutput("booking_summary_ui"),
        
        f7List(
          inset = TRUE,
          # ส่วนที่แก้ไข: เปลี่ยนเป็นช่องพิมพ์ปลายเปิด + ตัวช่วยเลือก (Datalist)
          tags$li(
            class = "item-content item-input",
            tags$div(
              class = "item-inner",
              tags$div(class = "item-title item-label", "ระบุชื่อผู้จอง"),
              uiOutput("name_input_field")
            )
          ),
          # รายชื่อสมาชิกทั้งหมดในระบบที่จะไปโผล่เป็นเงาให้เลือก
          tags$datalist(
            id = "member_datalist",
            lapply(names(member_list()), function(name) {
              tags$option(value = name)
            })
          )
        ),
        
        f7Block(
          f7Button(inputId = "final_confirm", label = "ตกลง", color = "green", fill = TRUE)
        )
      )
    }
  })
  
  output$name_input_field <- renderUI({
    tags$input(
      id = "final_user_name",
      type = "text",
      value = "", # <--- บังคับให้เป็นค่าว่างทุกครั้งที่วาดใหม่
      placeholder = "พิมพ์ชื่อ หรือเลือกรายชื่อ...",
      list = "member_datalist",
      style = "width: 100%; height: 40px; border: none; font-size: 16px;"
    )
  })  
  
  output$booking_summary_ui <- renderUI({
    
    selection <- confirmed_list() # <--- ใช้ตัวแปรที่โดนแช่แข็งไว้
    name <- confirmed_name()      # <--- ดึงชื่อที่แช่แข็งไว้มาใช้
    
    booking_sum <- paste0("(", length(selection), ")")
    booking_list <- paste(sort(selection), collapse = ", ")
    booking_success <- paste("จองสำเร็จ", name, booking_sum,  ":", booking_list)
    
    
    # ป้องกันกรณีค่าว่าง
    if (length(selection) == 0) 
      return(p("ยังไม่ได้เลือกหมายเลข"))
    # จองสำเร็จ
    else if (length(selected_nums()) == 0) 
      return(p(booking_success))
    
    f7Block(
      strong(paste("คุณเลือกทั้งหมด", length(selection), "หมายเลข:")),
      p(paste(sort(selection), collapse = ", "), 
        style = "font-size: 1.2em; color: #2196f3; font-weight: bold;")
    )
  })
  
  # 6. Logic เมื่อกดปุ่ม "ตกลง" ใน Modal (เวอร์ชันทดสอบชื่อเดิม)
  observeEvent(input$final_confirm, {
    # 1. รับค่าชื่อจากช่องพิมพ์
    u_name <- clean_member_name(input$final_user_name)
    req(u_name != "") # ถ้าชื่อว่าง ไม่ต้องทำต่อ
    
    confirmed_name(u_name)
    
    # 2. ค้นหา ID จากชื่อ (Lookup)
    sql_check <- "SELECT id FROM lottery_members WHERE member_name = $1 LIMIT 1"
    member_res <- dbGetQuery(pool, sql_check, params = list(u_name))
    
    if (nrow(member_res) > 0) {
      # --- กรณีเจอชื่อในระบบ (แอนเลอร์ / แอนอรทัย) ---
      m_id <- as.integer(member_res$id[1])
      
      # 3. เริ่มกระบวนการบันทึก (ยกมาจากโค้ด Last Working ของพี่)
      new_nums <- selected_nums()
      if (length(new_nums) > 0) {
        con <- poolCheckout(pool)
        success <- FALSE
        tryCatch({
          dbBegin(con)
          for(num in new_nums) {
            # ตัด params ออกตามที่พี่ต้องการ เพื่อความ Clean
            sql_insert <- sprintf(
              "INSERT INTO lottery_bookings (period_id, member_id, lotto_number) VALUES (%d, %d, '%s')",
              current_period_id(), m_id, num
            )
            dbExecute(con, sql_insert)
          }
          dbCommit(con)
          success <- TRUE
        }, error = function(e) {
          dbRollback(con)
          f7Toast(text = paste("เกิดข้อผิดพลาด:", e$message))
        })
        poolReturn(con)
        
        if (success) {
          # ล้างค่าและปิด Popup
          selected_nums(character(0))
          db_trigger(db_trigger() + 1)
          
          # สั่งปิด Popup (เนื่องจาก shinyMobile เวอร์ชันพี่ไม่มี toggleF7Popup)
          session$sendCustomMessage(type = "f7-action", message = list(target = "#popup_booking", action = "close"))
          
          f7Toast(text = "บันทึกสำเร็จ!")
        }
      }
    } else {
      # ถ้าไม่เจอชื่อ ให้เปิด Dialog ถาม
      f7Dialog(
        id = "confirm_add_new_member", # ID ต้องตรงกับตัว observeEvent ด้านบน
        title = "สมาชิกใหม่",
        text = sprintf("ไม่พบชื่อ '%s' ต้องการเพิ่มสมาชิกและจองเลยไหม?", u_name),
        type = "confirm"
      )
    }
  }) 
  
  
  # --- บล็อกสุดท้าย: เมื่อกดยืนยันจาก Dialog เพื่อเพิ่มสมาชิกใหม่ ---
  observeEvent(input$confirm_add_new_member, {
    # ตรวจสอบว่าพี่กด "ตกลง" (TRUE) หรือไม่
    req(isTRUE(input$confirm_add_new_member))
    
    # ดึงชื่อที่เราพักไว้ในช่องพิมพ์ (หรือจะใช้ถังพักชื่อถ้าพี่สร้างไว้)
    u_name <- clean_member_name(input$final_user_name)
    req(u_name != "")
    
    tryCatch({
      # 1. เพิ่มชื่อใหม่ (ใช้ params แทน sprintf)
      dbExecute(pool, "INSERT INTO lottery_members (member_name) VALUES ($1)", params = list(u_name))
      
      # 2. ดึง ID ที่เพิ่งสร้างใหม่ (ใช้ params แทน sprintf)
      new_res <- dbGetQuery(pool, "SELECT id FROM lottery_members WHERE member_name = $1 LIMIT 1", params = list(u_name))
      
      if (nrow(new_res) > 0) {
        new_m_id <- as.integer(new_res$id[1])
        
        # 3. บันทึกการจองทันที (ใช้ Logic ชุดเดียวกับ "เอี่ยว")
        new_nums <- selected_nums()
        if (length(new_nums) > 0) {
          con <- poolCheckout(pool)
          success <- FALSE
          tryCatch({
            dbBegin(con)
            for(num in new_nums) {
              sql_insert <- sprintf(
                "INSERT INTO lottery_bookings (period_id, member_id, lotto_number) VALUES (%d, %d, '%s')",
                current_period_id(), new_m_id, num
              )
              dbExecute(con, sql_insert)
            }
            dbCommit(con)
            success <- TRUE
          }, error = function(e) {
            dbRollback(con)
            f7Toast(text = paste("บันทึกเลขจองไม่สำเร็จ:", e$message))
          })
          poolReturn(con)
          
          if (success) {
            # ล้างค่าและปิด Popup
            selected_nums(character(0))
            db_trigger(db_trigger() + 1)
            session$sendCustomMessage(type = "f7-action", message = list(target = "#popup_booking", action = "close"))
            f7Toast(text = sprintf("เพิ่มคุณ '%s' และบันทึกจองสำเร็จ!", u_name))
          }
        }
      }
    }, error = function(e) {
      f7Toast(text = paste("เพิ่มสมาชิกใหม่ไม่สำเร็จ:", e$message))
    })
  })  
  
  # ฟังก์ชันช่วยบันทึกการจอง (ยกมาจากโค้ดเดิมของพี่)
  execute_booking <- function(m_id) {
    new_nums <- selected_nums()
    if (length(new_nums) > 0) {
      con <- poolCheckout(pool)
      success <- FALSE
      tryCatch({
        dbBegin(con)
        for(num in new_nums) {
          dbExecute(con,
                    "INSERT INTO lottery_bookings (period_id, member_id, lotto_number) VALUES ($1, $2, $3)",
                    params = list(current_period_id(), m_id, num))
        }
        dbCommit(con)
        success <- TRUE
      }, error = function(e) {
        dbRollback(con)
        f7Toast(text = paste("เกิดข้อผิดพลาด:", e$message))
      })
      poolReturn(con)
      
      if (success) {
        selected_nums(character(0))
        db_trigger(db_trigger() + 1)
        # ปิด Popup และเคลียร์ค่า
        session$sendCustomMessage(type = "f7-action", message = list(target = "#popup_booking", action = "close"))
        f7Toast(text = "บันทึกสำเร็จ!")
      }
    }
  }  
  
  observeEvent(input$check_available, {
    data <- booked_db()
    all_nums <- sprintf("%02d", 0:99)
    booked_nums <- data$number
    available_nums <- setdiff(all_nums, booked_nums)
    avail_sum = paste0("เลขว่าง (", length(available_nums), ")")
    avail_list <- paste(available_nums, collapse = " - ")
    
    f7Dialog(
      title = avail_sum,
      text = avail_list
    )
    
  })
  
  
  
  # ==========================================
  # ส่วนของ SERVER (แท็บที่ 3: ยอดชำระ)
  # ==========================================
  
  # 1. แสดงผลรายการคนจองและปุ่มสถานะเงิน
  output$payment_table <- renderUI({
    req(input$tabs == "Payment")
    db_trigger() 
    
    p_id <- current_period_id()
    req(p_id)
    
    query <- "
    SELECT m.id AS member_id, m.member_name AS name, b.lotto_number AS number, b.payment_status
    FROM lottery_bookings b
    JOIN lottery_members m ON b.member_id = m.id
    WHERE b.period_id = $1"
    
    raw_data <- dbGetQuery(pool, query, params = list(p_id))
    
    if (nrow(raw_data) == 0) return(f7Block(em("ยังไม่มีข้อมูลการจอง")))
    
    summary_data <- raw_data %>%
      group_by(member_id, name) %>%
      summarise(
        nums = paste(sort(number), collapse = "  "),
        count = n(),
        is_paid = all(payment_status == TRUE),
        .groups = 'drop'
      )
    
    #arrange(is_paid, name) -> เรียงลำดับไม่ถูก (เอาสระไปไว้หลัง ฮ)
    
    # วิธ๊แก้การเรียงลำดับชื่อ
    correct_order <- str_order(summary_data$name, locale = "th")
    summary_data <- summary_data[correct_order, ]
    
    # หลังจากเรียงชื่อแบบไทยเสร็จแล้ว ค่อยจัดกลุ่ม is_paid (เอา FALSE ขึ้นก่อน)
    # arrange ของ dplyr จะรักษาลำดับเดิมไว้ถ้าค่าเท่ากัน (Stable Sort)
    summary_data <- summary_data %>% arrange(is_paid)      
    
    f7List(
      inset = TRUE,
      mode = "media",
      lapply(1:nrow(summary_data), function(i) {
        f7ListItem(
          title = paste0(summary_data$name[i], " (", summary_data$count[i], ")"),
          subtitle = summary_data$nums[i],
          right = f7Button(
            inputId = paste0("pay_btn_", summary_data$member_id[i]),
            label = tags$span(
              if(summary_data$is_paid[i]) "จ่ายแล้ว" else "ค้างชำระ", 
              style = paste0("color:", if(summary_data$is_paid[i]) "#4cd964" else "#ff3b30", "; font-weight: bold;")
            ),
            color = if(summary_data$is_paid[i]) "white" else "orange",
            fill = !summary_data$is_paid[i]
          )
        )
      })     
    )
  })
  
  # 1. ถังจดบันทึก ID ที่เราสร้าง Observer ไปแล้ว (วางไว้บนสุดของ server)
  pay_observers <- reactiveVal(numeric(0))
  
  # 2. ตัวจัดการปุ่มกด (จ่ายเงิน) แบบ Dynamic ที่ฉลาดขึ้น
  observe({
    db_trigger() # รับสัญญาณเมื่อมีการเปลี่ยนแปลง (เช่น มีสมาชิกใหม่จองเลขเข้ามา)
    
    p_id <- current_period_id()
    req(p_id)
    
    # ดึงรายชื่อสมาชิกที่มี "การจอง" ในงวดปัจจุบัน
    current_bookings <- dbGetQuery(pool, 
                                   "SELECT DISTINCT m.id 
                                    FROM lottery_bookings b
                                    JOIN lottery_members m ON b.member_id = m.id
                                    WHERE b.period_id = $1", 
                                   params = list(p_id))
    
    if (nrow(current_bookings) > 0) {
      # หาว่าใครบ้างที่ "ยังไม่มี" หูฟัง (Observer)
      new_member_ids <- setdiff(current_bookings$id, pay_observers())
      
      for (m_id in new_member_ids) {
        # ใช้ local เพื่อ "ล็อก" ค่า m_id ไว้เฉพาะของคนนั้นๆ
        local({
          this_id <- m_id
          
          # ดักจับการกดปุ่ม "ค้างชำระ" (pay_btn_...)
          observeEvent(input[[paste0("pay_btn_", this_id)]], {
            # ดึงข้อมูลล่าสุด ณ วินาทีที่กด (กันคนกดซ้อน)
            m_info <- dbGetQuery(pool, "SELECT member_name FROM lottery_members WHERE id = $1", params = list(this_id))
            m_name <- m_info$member_name[1]
            
            pending_res <- dbGetQuery(pool, 
                                      "SELECT count(*) as count FROM lottery_bookings 
                                       WHERE member_id = $1 AND period_id = $2 AND payment_status = FALSE", 
                                      params = list(this_id, current_period_id()))
            pending <- pending_res$count
            
            if (pending > 0) {
              f7Dialog(
                id = paste0("dialog_pay_", this_id),
                title = "ยืนยันการชำระเงิน",
                text = paste0("คุณ ", m_name, " มียอดจอง ", pending, " ใบ (", pending * 50, " บาท)"),
                type = "confirm"
              )
            }
          }, ignoreInit = TRUE)
          
          # ดักจับการกดยืนยัน 'ตกลง' ใน Dialog (dialog_pay_...)
          observeEvent(input[[paste0("dialog_pay_", this_id)]], {
            req(isTRUE(input[[paste0("dialog_pay_", this_id)]]))
            
            dbExecute(pool, 
                      "UPDATE lottery_bookings SET payment_status = TRUE 
                       WHERE member_id = $1 AND period_id = $2",
                      params = list(this_id, current_period_id()))
            
            db_trigger(db_trigger() + 1)
            f7Toast(text = "บันทึกการชำระเงินเรียบร้อย", color = "green")
          }, ignoreInit = TRUE)
        })
      }
      
      # อัปเดตรายชื่อคนที่มี Observer แล้ว จะได้ไม่สร้างซ้ำในรอบหน้า
      pay_observers(unique(c(pay_observers(), new_member_ids)))
    }
  })
  
  observeEvent(input$close_period_btn, {
    p_id <- current_period_id()
    req(p_id)
    
    # ดึงข้อมูลมาเช็ค: จำนวนที่จอง และ จำนวนที่จ่ายแล้ว
    check_status <- dbGetQuery(pool, 
                               "SELECT 
        COUNT(*) as total_booked,
        SUM(CASE WHEN payment_status = TRUE THEN 1 ELSE 0 END) as total_paid
       FROM lottery_bookings 
       WHERE period_id = $1", params = list(p_id))
    
    total_booked <- as.integer(check_status$total_booked)
    total_paid <- as.integer(check_status$total_paid)
    
    # --- เงื่อนไขการปิดงวด ---
    if (total_booked < 100) {
      f7Notif(text = paste("ยังปิดงวดไม่ได้: จองไปแล้ว", total_booked, "ใบ (ต้องครบ 100)"), color = "orange")
    } else if (total_paid < 100) {
      f7Notif(text = paste("ยังปิดงวดไม่ได้: ค้างชำระอยู่", 100 - total_paid, "ใบ"), color = "red")
    } else {
      # ถ้าผ่านทุกเงื่อนไข ให้ขึ้น Pop-up ยืนยันครั้งสุดท้าย
      f7Dialog(
        id = "confirm_close_period",
        title = "ยืนยันการปิดงวด",
        text = "เมื่อปิดงวดแล้ว จะไม่สามารถจองหรือแก้ไขข้อมูลในงวดนี้ได้อีก ยืนยันหรือไม่?",
        type = "confirm"
      )
    }
  })

  # Trigger ส่งสัญญาณเมื่อมีการ Auto-create งวดใหม่
  new_period_created_trigger <- reactiveVal(NULL)  
  
  observeEvent(input$confirm_close_period, {
    
    req(isTRUE(input$confirm_close_period))    
    p_id <- current_period_id()
    req(p_id)
    
    # ใช้ TryCatch เพื่อความปลอดภัย ถ้าอัปเดตตัวนึงพลาด อีกตัวต้องไม่พัง
    tryCatch({
      con <- poolCheckout(pool)
      dbBegin(con)
      
      # 1. อัปเดตงวดปัจจุบันให้ 'จบงวดแล้ว'
      dbExecute(con, 
                "UPDATE lottery_periods SET status = 'จบงวดแล้ว' WHERE id = $1", 
                params = list(p_id))
      
      # 2. ค้นหา ID ของงวดถัดไป (เรียงตาม draw_date ที่ต่อจากงวดปัจจุบัน)
      next_period <- dbGetQuery(con, 
                                "SELECT id FROM lottery_periods 
           WHERE draw_date > (SELECT draw_date FROM lottery_periods WHERE id = $1)
           ORDER BY draw_date ASC LIMIT 1", 
                                params = list(p_id))
      
      # 3. ถ้าเจองวดถัดไป ให้เปลี่ยน status เป็น 'กำลังเปิดจอง'
      if (nrow(next_period) > 0) {
        next_id <- as.integer(next_period$id)
        dbExecute(con, 
                  "UPDATE lottery_periods SET status = 'กำลังเปิดจอง' WHERE id = $1", 
                  params = list(next_id))
        
        msg <- "ปิดงวดเก่า และเปิดงวดถัดไปให้แล้วครับ!"
      } else {
        new_created_period <- auto_create_next_period(con, p_id)
        msg <- paste0("ปิดงวดเรียบร้อย และสร้างงวดใหม่ (", new_created_period$display_name, ") ให้แล้วครับ!")
        #msg <- "ปิดงวดเรียบร้อย (ไม่มีงวดถัดไปในระบบ)"
      }
      
      dbCommit(con)
      poolReturn(con)
      
      # ดีดนิ้ว Trigger ให้ทุกอย่างในแอปอัปเดตตามสถานะใหม่ใน DB
      db_trigger(db_trigger() + 1)
      f7Toast(text = msg)

      # ส่งสัญญาณให้ observeEvent ข้างนอกทำงาน
      if (!is.null(new_created_period)) {
        new_period_created_trigger(new_created_period)
      }
            
    }, error = function(e) {
      if(exists("con")) {
        dbRollback(con)
        poolReturn(con)
      }
      f7Toast(text = paste("เกิดข้อผิดพลาด:", e$message))
    })
    
  })  
  
  
  # 1. คำนวณวันที่งวดถัดไป (1 -> 16 เดือนเดิม, 16 -> 1 เดือนถัดไป)
  get_next_lottery_date <- function(current_date) {
    current_date <- as.Date(current_date)
    day_val <- day(current_date)
    
    if (day_val == 1) {
      # ถ้าเป็นวันที่ 1 ให้ไปวันที่ 16 เดือนเดียวกัน
      next_date <- make_date(year(current_date), month(current_date), 16)
    } else {
      # ถ้าเป็นวันที่ 16 (หรือวันอื่น) ให้ข้ามไปวันที่ 1 ของเดือนถัดไป
      first_of_this_month <- make_date(year(current_date), month(current_date), 1)
      next_date <- first_of_this_month %m+% months(1)
    }
    return(next_date)
  }
  
  # 2. แปลง Date เป็น display_name ภาษาไทยแบบ Minimal (เช่น "16 ต.ค. 69")
  format_thai_display_name <- function(date_input) {
    date_obj <- as.Date(date_input)
    
    months_th <- c("ม.ค.", "ก.พ.", "มี.ค.", "เม.ย.", "พ.ค.", "มิ.ย.", 
                   "ก.ค.", "ส.ค.", "ก.ย.", "ต.ค.", "พ.ย.", "ธ.ค.")
    
    day_num <- day(date_obj)
    month_name <- months_th[month(date_obj)]
    year_th <- (year(date_obj) + 543) %% 100 # เอา พ.ศ. 2 หลักสุดท้าย
    
    sprintf("%d %s %02d", day_num, month_name, year_th)
  }
  
  # 3. ฟังก์ชัน Auto-Create งวดใหม่ใน DB
  auto_create_next_period <- function(con, current_p_id) {
    # ดึง draw_date ของงวดปัจจุบัน
    curr_info <- dbGetQuery(con, "SELECT draw_date FROM lottery_periods WHERE id = $1", params = list(current_p_id))
    req(nrow(curr_info) > 0)
    
    curr_date <- curr_info$draw_date[1]
    next_date <- get_next_lottery_date(curr_date)
    next_display_name <- format_thai_display_name(next_date)
    
    # INSERT งวดใหม่ พร้อมตั้ง status เป็น 'กำลังเปิดจอง'
    res <- dbGetQuery(con, 
                      "INSERT INTO lottery_periods (draw_date, display_name, status) 
                     VALUES ($1, $2, 'กำลังเปิดจอง') RETURNING id", 
                      params = list(next_date, next_display_name))
    
    list(
      id = as.integer(res$id[1]),
      draw_date = next_date,
      display_name = next_display_name
    )
  }  
  
  
  # ==========================================
  # ส่วนของ SERVER (แท็บที่ 4: การตั้งค่า)
  # ==========================================  
  
  # --- 1.1 แสดงงวดปัจจุบันโดย Reuse ฟังก์ชันเดิมจากแท็บ 1 ---
  output$current_period_settings_ui <- renderUI({
    current_period <- get_current_period_name()
    
    f7ListItem(
      header = "งวดปัจจุบัน",
      title = current_period,
      media = f7Icon("calendar")
    )
  })
  
  # --- 1.2 Pop-up Dialog สำหรับแก้ไข display_name ---
  observeEvent(input$update_period_date_btn, {
    current_period <- get_current_period_name()
    
    f7Dialog(
      id = "change_period_dialog",
      title = "แก้ไขข้อมูลงวดปัจจุบัน",
      type = "prompt",
      text = paste0("ปัจจุบัน: ", current_period)
    )
  })
  
  # --- 1.3 เมื่อกดตกลงใน Dialog ให้ UPDATE ลง DB ---
  observeEvent(input$change_period_dialog, {
    # รับค่าข้อความที่พิมพ์จาก Modal Prompt
    req(input$change_period_dialog)
    new_display <- trimws(input$change_period_dialog)
    
    if (nchar(new_display) > 0) {
      # UPDATE เฉพาะงวดที่ 'กำลังเปิดจอง'
      dbExecute(
        pool,
        "UPDATE lottery_periods SET display_name = $1 WHERE status = 'กำลังเปิดจอง'",
        params = list(new_display)
      )
      
      # Trigger ให้หน้าจอทุกแท็บอัปเดตข้อมูลใหม่ทันที
      if (exists("db_trigger")) db_trigger(db_trigger() + 1)
      
      f7Toast(
        text = "อัปเดตข้อมูลงวดเรียบร้อยแล้ว!",
        position = "bottom",
        color = "green"
      )
    }
  })
  
  
  # --- 3.1 แสดงสถานะการเชื่อมต่อ Database (ปรับใช้ header เพื่อป้องกัน Error) ---
  output$db_status_ui <- renderUI({
    # ทดสอบ Query เช็คการเชื่อมต่อ
    db_ok <- tryCatch({
      res <- dbGetQuery(pool, "SELECT 1")
      is.data.frame(res) && nrow(res) > 0
    }, error = function(e) {
      FALSE
    })
    
    if (db_ok) {
      f7ListItem(
        header = "Database (PostgreSQL)",
        title = "สถานะ: เชื่อมต่อปกติ",
        media = f7Icon("checkmark_alt_circle_fill", style = "color: green;")
      )
    } else {
      f7ListItem(
        header = "Database (PostgreSQL)",
        title = "สถานะ: ไม่สามารถเชื่อมต่อได้",
        media = f7Icon("xmark_circle_fill", style = "color: red;")
      )
    }
  })  

  # --- 3.2 ปุ่มปิดการทำงานแอปพลิเคชัน ---
  observeEvent(input$shutdown_app_btn, {
    f7Dialog(
      id = "confirm_shutdown_dialog",
      title = "ยืนยันการปิดแอปพลิเคชัน",
      type = "confirm",
      text = "คุณต้องการปิดแอปใช่ไหม?"
    )
  })
  
  # เมื่อผู้ใช้กด OK ใน Dialog ปิดแอป
  observeEvent(input$confirm_shutdown_dialog, {
    req(input$confirm_shutdown_dialog)
    if (isTRUE(input$confirm_shutdown_dialog)) {
      session$close()
    }
  })

  # เมื่อ User ปิด Browser ให้หยุดแอปทันที (เพื่อประหยัดชั่วโมง)
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # f7Login  
  loginData <- f7LoginServer(id = "login")

}

# --- นอก UI/Server ---
onStop(function() {
  poolClose(pool)
})

shinyApp(ui, server)
