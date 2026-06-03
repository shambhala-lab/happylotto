library(shiny)
library(shinyMobile)
library(dplyr)
library(tidyr)
library(stringr)

library(pool)
library(RPostgres) # ต้องมีเพื่อให้ dbPool รู้ว่าจะใช้ Engine ตัวไหน

source("secrets.R") # Test VS Production DB configurations


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
          msg <- "ปิดงวดเรียบร้อย (ไม่มีงวดถัดไปในระบบ)"
        }
        
        dbCommit(con)
        poolReturn(con)
        
        # ดีดนิ้ว Trigger ให้ทุกอย่างในแอปอัปเดตตามสถานะใหม่ใน DB
        db_trigger(db_trigger() + 1)
        f7Toast(text = msg)
        
      }, error = function(e) {
        if(exists("con")) {
          dbRollback(con)
          poolReturn(con)
        }
        f7Toast(text = paste("เกิดข้อผิดพลาด:", e$message))
      })

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