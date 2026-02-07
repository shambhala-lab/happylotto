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
  idleTimeout = 60000, # 10 นาทีปิดท่อ
  minSize = 3,         # เมื่อไม่มีคนใช้ ไม่ต้องคาเครื่องไว้เลย ให้เหลือ 0
  maxSize = 5          # แอปนี้ใช้คนเดียวหรือกลุ่มเล็ก 3 ท่อก็เหลือเฟือครับ  
)



ui <- f7Page(
  title = "ลุ้นหวยกัน เพื่อนปันสุข",
  options = list(theme = "ios", dark = FALSE, color = "green"),
  
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
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  # Debug only -- ข้อมูลทดสอบ (Static Data) 
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
  
  booked_data <- reactiveVal(init_df) # init_df คือตัวเดิมที่เรามี
  
  # Temporary list of numbers before PRE-CONFIRM
  selected_nums <- reactiveVal(character(0))
  
  # Chunk of selected numbers after PRE-CONFIRM
  confirmed_list <- reactiveVal(character(0))  
  
  db_trigger <- reactiveVal(0)  
  
  
  
  
  
  
  
  # --- ฟังก์ชันดึง ID งวดปัจจุบันที่ 'กำลังเปิดจอง' ---
  current_period_id <- reactive({
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
      
      br(), br(),
      
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
  
  lapply(0:99, function(i) {
    num_str <- sprintf("%02d", i)
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
        f7Toast(text = "เลขนี้มีเจ้าของแล้วจ้า", position = "bottom", color = "red")
      }
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
          f7SmartSelect(
            inputId = "final_user_id",
            label = "ระบุชื่อผู้จอง",
            choices = member_list(),
            openIn = "sheet"
          )
        ),
        
        f7Block(
          f7Button(inputId = "final_confirm", label = "ตกลง", color = "green", fill = TRUE)
        )
      )
    }
  })
  
  output$booking_summary_ui <- renderUI({

    selection <- confirmed_list() # <--- ใช้ตัวแปรที่โดนแช่แข็งไว้
    
    booking_sum <- paste0("(", length(selection), ")")
    booking_list <- paste(sort(selection), collapse = ", ")
    booking_success <- paste("จองสำเร็จ", booking_sum,  ":", booking_list)
    
    
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

  # 6. Logic เมื่อกดปุ่ม "ตกลง" ใน Modal เพื่อบันทึกการจองใหม่
  observeEvent(input$final_confirm, {
    new_nums <- selected_nums()
    m_id <- as.integer(input$final_user_id)
    
    if (length(new_nums) > 0) {
      con <- poolCheckout(pool)
      
      success <- FALSE # สร้างตัวแปรเช็คสถานะ
      
      tryCatch({
        dbBegin(con)
        # แก้ตรงบรรทัด INSERT
        for(num in new_nums) {
          dbExecute(con, 
                    "INSERT INTO lottery_bookings (period_id, member_id, lotto_number) VALUES ($1, $2, $3)",
                    params = list(current_period_id(), m_id, num))
        }
        dbCommit(con)
        success <- TRUE # บันทึกสำเร็จ
      }, error = function(e) {
        dbRollback(con)
        f7Toast(text = paste("เกิดข้อผิดพลาด:", e$message), color = "red")
      })
      
      # คืนท่อก่อน
      poolReturn(con)
      
      
      # ถ้าสำเร็จค่อยทำงานต่อ
      if (success) {
        # 1. ล้างเลขที่เลือกค้างไว้ก่อนเลย
        selected_nums(character(0))
        
        # 2. หน่วงเวลานิดนึง (ประมาณ 0.2 วินาที) ให้ DB เขียนเสร็จชัวร์ๆ
        Sys.sleep(0.2)
        
        # 3. ดีดนิ้วเรียกข้อมูลใหม่ (วางไว้ล่างสุด)
        db_trigger(db_trigger() + 1)

        f7Toast(text = "บันทึกสำเร็จ!", color = "green")
      }
    }
  })
  
  
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
      ) %>%
      arrange(is_paid, name)
    
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
  
  # 2. จัดการปุ่มกด (จ่ายเงิน) และ Pop-up ยืนยัน
  observe({
    # ดึงรายชื่อสมาชิกมาสร้างตัวดักจับ (Observer)
    members <- dbGetQuery(pool, "SELECT id, member_name FROM lottery_members")
    
    for (i in 1:nrow(members)) {
      local({
        m_id <- members$id[i]
        m_name <- members$member_name[i]
        
        # เมื่อกดปุ่ม 'ค้างชำระ' ของแต่ละคน
        observeEvent(input[[paste0("pay_btn_", m_id)]], {
          # เช็คยอดที่ค้างอยู่จริง
          pending <- dbGetQuery(pool, 
                                "SELECT count(*) as count FROM lottery_bookings 
                                 WHERE member_id = $1 AND period_id = $2 AND payment_status = FALSE", 
                                 params = list(m_id, current_period_id()))$count
          
          if (pending > 0) {
            f7Dialog(
              id = paste0("dialog_pay_", m_id),
              title = "ยืนยันการชำระเงิน",
              text = paste0("คุณ ", m_name, " มียอดจอง ", pending, " ใบ\n",
                            "รวมยอดเงิน: ", pending * 50, " บาท"),
              type = "confirm"
            )
          }
        })
        
        # เมื่อกดยืนยัน 'ตกลง' ใน Dialog
        observeEvent(input[[paste0("dialog_pay_", m_id)]], {
          # เช็คว่ากดปุ่มตกลง (TRUE) หรือไม่
          if (isTRUE(input[[paste0("dialog_pay_", m_id)]])) {
            # อัปเดต DB
            dbExecute(pool, 
                      "UPDATE lottery_bookings SET payment_status = TRUE 
                       WHERE member_id = $1 AND period_id = $2",
                       params = list(m_id, current_period_id()))
            
            # ดีดนิ้ว Trigger ให้หน้าจอ Refresh ทันที
            db_trigger(db_trigger() + 1)
            
            f7Toast(text = paste("บันทึกการชำระเงินเรียบร้อย"), color = "green")
          }
        })
      })
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
  
  # เมื่อแอดมินกดยืนยันใน Dialog
  observeEvent(input$confirm_close_period_old, {
      p_id <- current_period_id()
      
      # อัปเดต DB: เปลี่ยนสถานะงวดปัจจุบัน
      dbExecute(pool, 
                "UPDATE lottery_periods SET status = 'จบงวดแล้ว' WHERE id = $1", 
                params = list(p_id))
      
      # ดีด Trigger ให้ทุกหน้าจอรู้ว่า 'กำลังเปิดจอง' หายไปแล้ว
      db_trigger(db_trigger() + 1)
      
      f7Toast(text = "ปิดงวดเรียบร้อยแล้ว!")
  })
  
  observeEvent(input$confirm_close_period, {
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
          msg <- "ปิดงวดเรียบร้อย (ไม่มีงวดถัดไปในระบบ)"
        }
        
        dbCommit(con)
        poolReturn(con)
        
        # ดีดนิ้ว Trigger ให้ทุกอย่างในแอปอัปเดตตามสถานะใหม่ใน DB
        db_trigger(db_trigger() + 1)
        f7Toast(text = msg, color = "blue")
        
      }, error = function(e) {
        if(exists("con")) {
          dbRollback(con)
          poolReturn(con)
        }
        f7Toast(text = paste("เกิดข้อผิดพลาด:", e$message), color = "red")
      })

  })  
  
  
  
  # เมื่อ User ปิด Browser ให้หยุดแอปทันที (เพื่อประหยัดชั่วโมง)
  session$onSessionEnded(function() {
    stopApp()
  })

  
  # f7Login  
  loginData <- f7LoginServer(id = "login")
  
  # exportTestValues(
  #   status = loginData$status(),
  #   user = loginData$user(),
  #   admin = loginData$password(),
  #   authenticated = loginData$authenticated(),
  #   cancelled = loginData$cancelled()
  # )  
  
  
    
}


shinyApp(ui, server)
