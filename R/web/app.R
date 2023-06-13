library(shiny)
#devtools::install_github('nicholasrazali/package')
library(AljabarLinear)
library(shinyMatrix)
library(plotly)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)

hide_hitung <- function(session){
  updateNumericInput(session, "hit_rows_1", value = 2)
  updateNumericInput(session, "hit_cols_1", value = 2)
  hide("hit_matriks_1")
  hide("hit_matriks_2")
  hide("hit_input_matrix_1")
  hide("hit_input_matrix_2")
  hide("hit_reset_btn")
  hide("hit_submit_1")
  hide("hit_submit_2")
  hide("hit_radio")
  hide("hitung")
  hide("hit_output_matrix_1")
  hide("hit_output_matrix_2")
  hide("hit_matrix_input_1")
  hide("hit_matrix_input_2")
  hide("output_matrix_hit_2")
  show("submit_1")
}
hide_persamaan <- function(session){
  updateNumericInput(session, "pers_ukuran", value = 2)
  hide("pers_matriks")
  hide("pers_input_matrix")
  hide("pers_reset_btn")
  hide("pers_submit_btn")
  hide("pers_radio")
  hide("persamaan")
  hide("pers_output_matrix")
  hide("pers_matrix_input")
  hide("pers_vec")
  hide("pers_output_vector")
  hide("output_pers_vec")
  hide("gauss_manual")
  show("pers_submit")
}
hide_determinan <- function(session){
  updateNumericInput(session, "det_ukuran", value = 2)
  hide("det_matriks")
  hide("det_input_matrix")
  hide("det_reset_btn")
  hide("det_submit_btn")
  hide("det_radio")
  hide("determinan")
  hide("det_output_matrix")
  hide("det_matrix_input")
  show("det_submit")
}
hide_invers <- function(session){
  updateNumericInput(session, "inv_ukuran", value = 2)
  hide("inv_matriks")
  hide("inv_input_matrix")
  hide("inv_reset_btn")
  hide("inv_submit_btn")
  hide("inv_radio")
  hide("invers")
  hide("inv_output_matrix")
  hide("inv_matrix_input")
  show("inv_submit")
}
hide_dotcross <- function(session){
  updateTextInput(session, "dot_vec", value = "")
  updateTextInput(session, "dot_vec_2", value = "")
  hide("plot_cross_ui")
  hide("dotcross")
  hide("plot_cross")
  hide("dotcross_radio")
  hide("dot_output_vector_2")
  hide("output_dot_2")
  hide("dot_output_vector_1")
  hide("dotcross_reset")
  show("dot_submit_1")
}
hide_interpolasi <- function(session){
  updateTextInput(session, "x_vec", value = "")
  updateTextInput(session, "y_vec", value = "")
  hide("poli")
  hide("hitung_poli")
  hide("x_output_vector")
  hide("y_output_vector")
  hide("poli_reset")
  hide("output_koor_y")
  show("x_submit")
}
hide_hitvec <- function(session){
  updateTextInput(session, "hit_vec", value = "")
  updateTextInput(session, "hit_vec_2", value = "")
  hide("plot_hitung_ui")
  hide("hitvec")
  hide("plot_hitung")
  hide("hitvec_radio")
  hide("hitvec_output_vector_2")
  hide("output_hitvec_2")
  hide("hitvec_output_vector_1")
  hide("hitvec_reset")
  show("hitvec_submit_1")
}
hide_proyeksi <- function(session){
  updateTextInput(session, "proyeksi_vec", value = "")
  updateTextInput(session, "proyeksi_vec_2", value = "")
  hide("proyeksi")
  hide("plot_proyeksi_ui")
  hide("proyeksi_2")
  hide("plot_proyeksi")
  hide("proyeksi_radio")
  hide("proyeksi_output_vector_2")
  hide("output_proyeksi_2")
  hide("proyeksi_output_vector_1")
  hide("proyeksi_reset")
  show("proyeksi_submit_1")
}
is_valid_fraction <- function(value) {
  fraction_parts <- strsplit(value, "/")[[1]]
  if (length(fraction_parts) != 2)
    return(FALSE)
  numerator <- as.integer(fraction_parts[1])
  denominator <- as.integer(fraction_parts[2])
  !any(is.na(numerator), is.na(denominator), denominator == 0)
}

#### ui ####
ui <- navbarPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML("
        .reset-button {
          text-align: right;
        }

        #footer {
       background-color: #f8f8f8;
        padding: 10px;
        text-align: center;
        font-size: 14px;
        color: #555;
        position: fixed;
        bottom: 0;
        width: 100%;
        }
      ")
    )
  ),
  title = "Aljabar Linear",
  tabPanel(
    "Home",
    h1("Home page")
  ),
  tabPanel(
    "Topik",
    sidebarLayout(
      sidebarPanel(
        selectInput("functions", "Pilih Jenis Materi", choices = c("Select", "Matrix", "Vektor")),
        width = 3
      ),
      mainPanel(
        uiOutput("dynamicTabs"),
        width = 9
      )
    )
  ),
  tags$div(
    class = "footer",
    "©2023 Nicholas"
  )
)

#### server ####
server <- function(input, output, session) {

  output$dynamicTabs <- renderUI(
  if(input$functions == "Matrix"){
      #### matrix ####
      navbarPage(
        id = "nav_matrix",
        title = NULL,
        header = NULL,
        footer = NULL,
        ##### determinan ######
        tabPanel("Determinan",
                 uiOutput("det_reset"),
                 h4("Ukuran matriks yang dimasukkan akan menghasilkan matriks n x n "),
                 numericInput("det_ukuran", "Masukkan Ukuran Baris dan Kolom:", value = 2, min = 1, max = 5, width = "30%"),
                 actionButton("det_submit", "Submit"),

                 uiOutput("det_matrix_input"),
                 uiOutput("det_submit_matrix"),
                 verbatimTextOutput("det_output_matrix"),

                 uiOutput("radio_det"),
                 verbatimTextOutput("determinan")),

        ##### invers ######
        tabPanel("Invers",
                 uiOutput("inv_reset"),
                 h4("Ukuran matriks yang dimasukkan akan menghasilkan matriks n x n "),
                 numericInput("inv_ukuran", "Masukkan Ukuran Baris dan Kolom:", value = 2, min = 1, max = 5, width = "30%"),
                 actionButton("inv_submit", "Submit"),

                 uiOutput("inv_matrix_input"),
                 uiOutput("inv_submit_matrix"),
                 verbatimTextOutput("inv_output_matrix"),

                 uiOutput("radio_inv"),
                 verbatimTextOutput("invers")),

        ##### hitung ######
        tabPanel("Perhitungan",
                 uiOutput("hit_reset"),
                 h4("Masukkan matrix pertama"),
                 numericInput("hit_rows_1", "Masukkan Ukuran Baris:", value = 2, min = 1, max=5),
                 numericInput("hit_cols_1", "Masukkan Ukuran Kolom:", value = 2, min = 1, max=5),
                 actionButton("submit_1", "Submit"),

                 uiOutput("hit_matrix_input_1"),
                 uiOutput("hit_submit_matrix_1"),
                 verbatimTextOutput("hit_output_matrix_1"),

                 uiOutput("output_matrix_hit_2"),
                 verbatimTextOutput("hit_output_matrix_2"),

                 uiOutput("radio_hit"),

                 verbatimTextOutput("hitung")),

        ##### persamaan linear ######
        tabPanel("Persamaan Linear",
                 uiOutput("pers_reset"),
                 h4("Ukuran matriks yang dimasukkan akan menghasilkan matriks n x n "),
                 numericInput("pers_ukuran", "Masukkan Ukuran Baris dan Kolom:", value = 2, min = 1, max = 5, width = "30%"),
                 actionButton("pers_submit", "Submit"),

                 uiOutput("pers_matrix_input"),
                 uiOutput("pers_submit_matrix"),
                 verbatimTextOutput("pers_output_matrix"),

                 uiOutput("output_pers_vec"),
                 verbatimTextOutput("pers_output_vector"),

                 uiOutput("radio_pers"),

                 uiOutput("gauss_manual"),
                 verbatimTextOutput("persamaan")),

        ##### polinomial ######
        tabPanel("Polinomial",
                 uiOutput("poli_reset"),
                 h5(tags$b("Masukkan koordinat x (x1, x2, x3, ...., xn)")),
                 textInput("x_vec", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 actionButton("x_submit", "Submit"),
                 verbatimTextOutput("x_output_vector"),

                 uiOutput("output_koor_y"),
                 verbatimTextOutput("y_output_vector"),

                 uiOutput("hitung_poli"),
                 verbatimTextOutput("poli"))
      )

    } else if(input$functions == "Vektor"){
      #### vector ####
      navbarPage(
        id = "nav_vector",
        title = NULL,
        header = NULL,
        footer = NULL,
        ##### hitung vektor #####
        tabPanel("Perhitungan",
                 uiOutput("hitvec_reset"),
                 h5(tags$b("Masukkan vektor pertama ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
                 textInput("hit_vec",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 actionButton("hitvec_submit_1", "Submit"),
                 verbatimTextOutput("hitvec_output_vector_1"),

                 uiOutput("output_hitvec_2"),
                 verbatimTextOutput("hitvec_output_vector_2"),

                 uiOutput("radio_hitvec"),

                 verbatimTextOutput("hitvec"),
                 uiOutput("plot_hitung_ui")),

        ##### proyeksi #####
        tabPanel("Proyeksi",
                 uiOutput("proyeksi_reset"),
                 h5(tags$b("Masukkan vektor pertama ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
                 textInput("proyeksi_vec",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 actionButton("proyeksi_submit_1", "Submit"),
                 verbatimTextOutput("proyeksi_output_vector_1"),

                 uiOutput("output_proyeksi_2"),
                 verbatimTextOutput("proyeksi_output_vector_2"),

                 uiOutput("radio_proyeksi"),

                 verbatimTextOutput("proyeksi"),
                 verbatimTextOutput("proyeksi_2"),
                 uiOutput("plot_proyeksi_ui")),

        ##### dot cross ######
        tabPanel("Dot and Cross",
                 uiOutput("dotcross_reset"),
                 h5(tags$b("Masukkan vektor pertama ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
                 textInput("dot_vec",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 actionButton("dot_submit_1", "Submit"),
                 verbatimTextOutput("dot_output_vector_1"),

                 uiOutput("output_dot_2"),
                 verbatimTextOutput("dot_output_vector_2"),

                 uiOutput("radio_dotcross"),

                 verbatimTextOutput("dotcross"),
                 uiOutput("plot_cross_ui")),

        ##### distance ######
        tabPanel("Distance",
                 uiOutput("dist_reset"),
                 radioButtons("dist_radio",
                              label = "Pilih perhitungan jarak terhadap",
                              choices = c("Jarak 2 titik", "Jarak titik ke garis", "Jarak titik ke bidang"),
                              selected = "none",
                              inline = TRUE),

                 uiOutput("input_vector_dist"),
                 verbatimTextOutput("titik_output_vector_1"),
                 verbatimTextOutput("bidang_output_vector_1"),
                 verbatimTextOutput("titik_output_vector"),

                 uiOutput("input_vector_dist_2"),
                 verbatimTextOutput("titik_output_vector_2"),
                 verbatimTextOutput("bidang_output_vector_2"),
                 verbatimTextOutput("bidang_output_vector"),

                 uiOutput("hitung_distance"),

                 verbatimTextOutput("distance")),

        ##### transformasi ######
        tabPanel("Transformasi Linear",
                 uiOutput("trans_reset"),
                 h5(tags$b("Masukkan vektor ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
                 textInput("trans_vec", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 actionButton("trans_submit", "Submit"),
                 verbatimTextOutput("trans_output_vector"),

                 uiOutput("output_trans"),

                 uiOutput("radio_trans"),
                 uiOutput("input_info_trans"),
                 verbatimTextOutput("refleksi_output_vector"),
                 verbatimTextOutput("proyeksi_output_vector"),
                 verbatimTextOutput("kontraksi_output_vector"),
                 verbatimTextOutput("rotasi_output_vector_alfa"),

                 uiOutput("rotasi_axis_input"),
                 verbatimTextOutput("rotasi_output_vector"),


                 verbatimTextOutput("transformasi"),
                 uiOutput("plot_ui"))
      )


    }
  )

  #### reset mengganti tab ####
  # observeEvent(input$functions,{
  #   if(input$functions == "Matrix"){
  #     matrix_det_a(NULL)
  #     hide_determinan(session)
  #   } else if(input$functions == "Vektor"){
  #     matrix_det_a(NULL)
  #     hide_determinan(session)
  #
  #     matrix_hit_1_a(NULL)
  #     matrix_hit_2_a(NULL)
  #     hide_hitung(session)
  #
  #     matrix_pers_a(NULL)
  #     pers_vector_a(NULL)
  #     hide_persamaan(session)
  #
  #     matrix_a(NULL)
  #     hide_invers(session)
  #
  #   }
  # })
  # observeEvent(input$nav_matrix,{
  #   if(input$nav_matrix == "Determinan"){
  #     matrix_hit_1_a(NULL)
  #     matrix_hit_2_a(NULL)
  #     hide_hitung(session)
  #
  #     matrix_pers_a(NULL)
  #     pers_vector_a(NULL)
  #     hide_persamaan(session)
  #
  #     matrix_a(NULL)
  #     hide_invers(session)
  #   } else if(input$nav_matrix == "Invers"){
  #     matrix_hit_1_a(NULL)
  #     matrix_hit_2_a(NULL)
  #     hide_hitung(session)
  #
  #     matrix_det_a(NULL)
  #     hide_determinan(session)
  #
  #     matrix_pers_a(NULL)
  #     pers_vector_a(NULL)
  #     hide_persamaan(session)
  #   } else if(input$nav_matrix=="Perhitungan"){
  #     matrix_det_a(NULL)
  #     hide_determinan(session)
  #
  #     matrix_pers_a(NULL)
  #     pers_vector_a(NULL)
  #     hide_persamaan(session)
  #
  #     matrix_a(NULL)
  #     hide_invers(session)
  #
  #   } else if(input$nav_matrix=="Persamaan Linear"){
  #     matrix_hit_1_a(NULL)
  #     matrix_hit_2_a(NULL)
  #     hide_hitung(session)
  #
  #     matrix_det_a(NULL)
  #     hide_determinan(session)
  #
  #     matrix_a(NULL)
  #     hide_invers(session)
  #   }
  # })



  #### input matrix invers ####
  matrix_data <- reactiveVal(NULL)
  observeEvent(input$inv_submit, {

    if(is.na(input$inv_ukuran))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$inv_ukuran > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$inv_ukuran < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    width <- paste0((input$inv_ukuran *40),"px")
    output$inv_matrix_input <- renderUI({
      list(
        h5("Masukkan matriks",id = "id_matriks"),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "inv_input_matrix",
            value = matrix_data(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        ))
    })

    hide("inv_submit")

    matrix_data(matrix("", nrow = input$inv_ukuran, ncol = input$inv_ukuran, dimnames = list(NULL, NULL)))
    output$inv_submit_matrix <-renderUI({
      actionButton("inv_submit_btn", "input matrix")
    })
    show("inv_matrix_input")
    output$inv_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("inv_reset_btn", "Reset")
      )
    })
  })

  observeEvent(input$btn_close, {
    removeModal()
  })

  matrix_a <- reactiveVal(NULL)
  observeEvent(input$inv_submit_btn, {
    mat <- c()
    for (i in 1:(input$inv_ukuran * input$inv_ukuran)) {
      if(is.na(input$inv_input_matrix[[i]])) return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$inv_input_matrix[[i]])
    }
    matrix_a(matrix(mat, nrow = input$inv_ukuran, ncol = input$inv_ukuran))
    hide("inv_submit_btn")
    output$inv_output_matrix <- renderPrint({
      matrix_a()
    })
    output$radio_inv <- renderUI(
      radioButtons("inv_radio",
                   label = "Pilih metode",
                   choices = c("Row Reduction", "Adjoint Matrix"),
                   selected = "none",
                   inline = TRUE))

    show("inv_output_matrix")
    show("invers")

    output$invers <- renderPrint(
      "Silahkan pilih metode"
    )
  })

  observeEvent(input$inv_radio, {
    output$invers <- renderPrint(
      if(input$inv_radio == "Row Reduction"){
        invers_row_reduction(matrix_a())
      }
      else if(input$inv_radio == "Adjoint Matrix"){
        invers_adjoint_matriks(matrix_a())
      }
    )
  })
  observeEvent(input$inv_reset_btn, {
    matrix_a(NULL)
    hide_invers(session)
  })


  #### input matrix determinan ####
  matrix_det <- reactiveVal(NULL)
  observeEvent(input$det_submit, {
    if(is.na(input$det_ukuran))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$det_ukuran > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$det_ukuran < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    width <- paste0((input$det_ukuran *40),"px")
    output$det_matrix_input <- renderUI({
      list(
        h5("Masukkan matriks", id = "det_matriks"),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "det_input_matrix",
            value = matrix_det(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        )
      )
    })
    matrix_det(matrix("", nrow = input$det_ukuran, ncol = input$det_ukuran, dimnames = list(NULL, NULL)))
    output$det_submit_matrix <-renderUI({
      actionButton("det_submit_btn", "input matrix")
    })

    hide("det_submit")
    show("det_matrix_input")
    output$det_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("det_reset_btn", "Reset")
      )
    })
  })

  matrix_det_a <- reactiveVal(NULL)
  observeEvent(input$det_submit_btn, {
    mat <- c()
    for (i in 1:(input$det_ukuran * input$det_ukuran)) {
      if(is.na(input$det_input_matrix[[i]])) return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$det_input_matrix[[i]])
    }
    matrix_det_a(matrix(mat, nrow = input$det_ukuran, ncol = input$det_ukuran))
    hide("det_submit_btn")

    output$radio_det <- renderUI(
      radioButtons("det_radio",
                   label = "Pilih metode",
                   choices = c("Row Reduction", "Cofactor"),
                   selected = "none",
                   inline = TRUE))

    output$det_output_matrix <- renderPrint({
      matrix_det_a()
    })
    show("det_output_matrix")
    show("determinan")

    output$determinan <- renderPrint(
      "Silahkan pilih metode"
    )
  })

  observeEvent(input$det_radio, {
    output$determinan <- renderPrint(
      if(input$det_radio == "Row Reduction"){
        determinan_row_reduction(matrix_det_a())
      }
      else if(input$det_radio == "Cofactor"){
        determinan_cofactor(matrix_det_a())
      }
    )
  })
  observeEvent(input$det_reset_btn, {
    matrix_det_a(NULL)
    hide_determinan(session)
  })


  #### input matrix persamaan ####
  matrix_pers <- reactiveVal(NULL)
  observeEvent(input$pers_submit, {
    if(is.na(input$pers_ukuran))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$pers_ukuran > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$pers_ukuran < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    width <- paste0((input$pers_ukuran *40),"px")

    output$pers_matrix_input <- renderUI({
      list(
        h5("Masukkan matriks", id = "pers_matriks_1"),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "pers_input_matrix",
            value = matrix_pers(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        ))
    })
    matrix_pers(matrix("", nrow = input$pers_ukuran, ncol = input$pers_ukuran, dimnames = list(NULL, NULL)))

    output$pers_submit_matrix <-renderUI({
      actionButton("pers_submit_btn", "input matrix")
    })

    hide("pers_submit")
    show("pers_matrix_input")

    output$pers_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("pers_reset_btn", "Reset")
      )
    })
  })

  matrix_pers_a <- reactiveVal(NULL)
  observeEvent(input$pers_submit_btn, {
    mat <- c()
    for (i in 1:(input$pers_ukuran * input$pers_ukuran)) {
      if(is.na(input$pers_input_matrix[[i]])) return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, as.numeric(input$pers_input_matrix[[i]]))
    }
    matrix_pers_a(matrix(mat, nrow = input$pers_ukuran, ncol = input$pers_ukuran))

    hide("pers_submit_btn")

    output$pers_output_matrix <- renderPrint({
      matrix_pers_a()
    })

    show("pers_output_matrix")
    show("output_pers_vec")

    output$output_pers_vec <- renderUI(list(
      h5(tags$b("Masukkan vektor hasil (pisahkan dengan koma)")),
      textInput("pers_vec", label = NULL, placeholder = "contoh : 1,2,3"),
      actionButton("pers_submit_vec", "Submit")
    ))
  })

  pers_vector_a <- reactiveVal(NULL)
  observeEvent(input$pers_submit_vec, {

    pers_vector_input <- as.numeric(strsplit(input$pers_vec, ",")[[1]])
    if(length(pers_vector_input) == 0)  return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))

    for(i in 1:length(pers_vector_input)){
      if(is.na(pers_vector_input[[i]]))  return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(pers_vector_input)!=input$pers_ukuran)  return(shinyalert("Kesalahan Input", "Banyaknya vektor harus sama dengan ukuran matriks", type = "error"))
    pers_vector_a(c(pers_vector_input))
    hide("pers_submit_vec")

    output$pers_output_vector <- renderPrint({
      pers_vector_a()
    })

    show("pers_output_vector")

    output$radio_pers <- renderUI(
      radioButtons("pers_radio",
                   label = "Pilih metode",
                   choices = c("Gauss Jordan", "Cramer's Rule","Input Manual"),
                   selected = "none",
                   inline = TRUE))

    show("persamaan")

    output$persamaan <- renderPrint(
      "Silahkan pilih metode"
    )
  })

  hasil_verbatim <- reactiveVal("")
  ket <- reactiveVal("")
  observeEvent(input$pers_radio, {
    hide("gauss_manual")
    output$persamaan <- renderPrint(
      if(input$pers_radio == "Gauss Jordan"){
        gauss_jordan(matrix_pers_a(), pers_vector_a())
      }
      else if(input$pers_radio == "Cramer's Rule"){
        cramer_rule(matrix_pers_a(),pers_vector_a())
      }
    )

    if(input$pers_radio == "Input Manual"){
      show("gauss_manual")
      output$gauss_manual <- renderUI(
        list(
          sidebarLayout(
            sidebarPanel(
              radioButtons("pilihan", "Pilihan:",
                           choices = c("Tukar Baris", "Kalikan Baris", "Lipat Baris")),
              uiOutput("gauss_pilih")
            ),
            mainPanel(
              verbatimTextOutput("keterangan"),
              verbatimTextOutput("output")
            )
          )
        )
      )

      tes <<- gauss_jordan_manual(matrix_pers_a(),pers_vector_a())
      ket("Augmented matriks")
      hasil_verbatim(tes[1])
    }
  })

  output$gauss_pilih <- renderUI({
    if(input$pilihan == "Tukar Baris"){
      list(
        numericInput("baris_1", "Masukkan baris ke:", value = ""),
        numericInput("baris_2", "Tukar baris ke:", value = ""),
        actionButton("selesai", "Selesai")
      )
    }
    else if(input$pilihan == "Kalikan Baris"){
      list(
        numericInput("baris_kali", "Masukkan baris ke:", value = ""),
        textInput("konstanta", "Dikalikan sebesar:", value = ""),
        actionButton("selesai", "Selesai")
      )
    }
    else{
      list(
        numericInput("baris_lipat", "Masukkan baris tujuan:", value = ""),
        numericInput("baris_lipat_2", "Lipat dengan baris ke:", value = ""),
        textInput("konst_lipat", "Dengan kelipatan:", value = ""),
        actionButton("selesai", "Selesai")
      )
    }
  })


  observeEvent(input$selesai, {
    updateNumericInput(session, "baris_1", value = "")
    updateNumericInput(session, "baris_2", value = "")
    updateNumericInput(session, "baris_kali", value = "")
    updateTextInput(session, "konstanta", value = "")
    updateNumericInput(session, "baris_lipat", value = "")
    updateNumericInput(session, "baris_lipat_2", value = "")
    updateTextInput(session, "konst_lipat", value = "")



    if(input$pilihan == "Tukar Baris"){
      if(is.na(input$baris_1))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_1 > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      if(is.na(input$baris_2))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_2 > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      ket(paste0("Menukar baris ",input$baris_1, " dengan baris ", input$baris_2))
      hasil_verbatim(tes$tukar(input$baris_1, input$baris_2))

    }
    else if(input$pilihan == "Kalikan Baris"){

      if(is.na(input$baris_kali))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_kali > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      konstanta = 0
      if(input$konstanta == "") return(shinyalert("Kesalahan Input", "Masukkan angka terlebih dahulu", type = "error"))
      else if(is_valid_fraction(input$konstanta)) konstanta = eval(parse(text = input$konstanta))
      else if(is.na(as.numeric(input$konstanta))) return(shinyalert("Kesalahan Input", "Terjadi kesalahan input bukan angka", type = "error"))
      else konstanta = (as.numeric(input$konstanta))

      ket(paste0("Mengalikan baris ",input$baris_kali, " sebesar ", konstanta))
      hasil_verbatim(tes$kali(input$baris_kali,konstanta))

    }
    else{
      if(is.na(input$baris_lipat))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_lipat > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      if(is.na(input$baris_lipat_2))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_lipat_2 > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      konstanta_kali = 0
      if(input$konst_lipat == "") return(shinyalert("Kesalahan Input", "Masukkan angka terlebih dahulu", type = "error"))
      else if(is_valid_fraction(input$konst_lipat)) konstanta_kali = eval(parse(text = input$konst_lipat))
      else if(is.na(as.numeric(input$konst_lipat))) return(shinyalert("Kesalahan Input", "Terjadi kesalahan input bukan angka", type = "error"))
      else konstanta_kali = (as.numeric(input$konst_lipat))


      ket(paste0("Melipat baris ",input$baris_lipat, " dengan baris ",input$baris_lipat_2, " dikalikan ", konstanta_kali))
      hasil_verbatim(tes$lipat(input$baris_lipat_2,input$baris_lipat,konstanta_kali))
    }
  })

  output$output <- renderPrint({
    print(hasil_verbatim())
  })
  output$keterangan <- renderPrint({
    print(ket())
  })




  observeEvent(input$pers_reset_btn, {
    matrix_pers_a(NULL)
    pers_vector_a(NULL)
    hide_persamaan(session)
  })

  #### input matrix Hitung ####
  ### hitung 1
  matrix_hit_1 <- reactiveVal(NULL)
  observeEvent(input$submit_1, {
    if(is.na(input$hit_rows_1))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$hit_rows_1 > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$hit_rows_1 < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    if(is.na(input$hit_cols_1))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$hit_cols_1 > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$hit_cols_1 < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    width <- paste0((input$hit_cols_1 *40),"px")
    output$hit_matrix_input_1 <- renderUI({
      list(
        h5("Masukkan matriks pertama", id = "hit_matriks_1"),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "hit_input_matrix_1",
            value = matrix_hit_1(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        ))
    })
    matrix_hit_1(matrix("", nrow = input$hit_rows_1, ncol = input$hit_cols_1, dimnames = list(NULL, NULL)))

    output$hit_submit_matrix_1 <-renderUI({
      actionButton("hit_submit_1", "input matrix")
    })


    hide("submit_1")
    show("hit_matrix_input_1")

    output$hit_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("hit_reset_btn", "Reset")
      )
    })

  })
  matrix_hit_1_a <- reactiveVal(NULL)
  observeEvent(input$hit_submit_1, {
    mat <- c()
    for (i in 1:(input$hit_rows_1 * input$hit_cols_1)) {
      if(is.na(input$hit_input_matrix_1[[i]]))  return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$hit_input_matrix_1[[i]])
    }
    matrix_hit_1_a(matrix(mat, nrow = input$hit_rows_1, ncol = input$hit_cols_1))
    hide("hit_submit_1")

    output$hit_output_matrix_1 <- renderPrint({
      matrix_hit_1_a()
    })

    show("hit_output_matrix_1")
    show("output_matrix_hit_2")

    output$output_matrix_hit_2 <- renderUI({
      list(h4("Masukkan matrix kedua"),
           numericInput("hit_rows_2", "Masukkan Jumlah Baris:", value = 2, min = 1, max=5),
           numericInput("hit_cols_2", "Masukkan Jumlah Kolom:", value = 2, min = 1, max=5),
           actionButton("submit_2", "Submit"),

           uiOutput("hit_matrix_input_2"),
           uiOutput("hit_submit_matrix_2"))
    })
  })

  ### hitung 2
  matrix_hit_2 <- reactiveVal(NULL)
  observeEvent(input$submit_2, {

    if(is.na(input$hit_rows_2))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$hit_rows_2 > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$hit_rows_2 < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))


    if(is.na(input$hit_cols_2))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$hit_cols_2 > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$hit_cols_2 < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    width <- paste0((input$hit_cols_2 *40),"px")
    output$hit_matrix_input_2 <- renderUI({
      list(
        h5("Masukkan matriks kedua", id = "hit_matriks_2"),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "hit_input_matrix_2",
            value = matrix_hit_2(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        ))
    })

    matrix_hit_2(matrix("", nrow = input$hit_rows_2, ncol = input$hit_cols_2, dimnames = list(NULL, NULL)))

    hide("submit_2")
    show("hit_matrix_input_2")

    output$hit_submit_matrix_2 <-renderUI({
      actionButton("hit_submit_2", "input matrix")
    })
  })
  matrix_hit_2_a <- reactiveVal(NULL)
  observeEvent(input$hit_submit_2, {
    mat <- c()
    for (i in 1:(input$hit_rows_2 * input$hit_cols_2)) {
      if(is.na(input$hit_input_matrix_2[[i]]))  return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$hit_input_matrix_2[[i]])
    }
    matrix_hit_2_a(matrix(mat, nrow = input$hit_rows_2, ncol = input$hit_cols_2))

    hide("hit_submit_2")

    output$radio_hit <- renderUI(
      radioButtons("hit_radio",
                   label = "Pilih",
                   choices = c("Hitung", "Step Perkalian"),
                   selected = "none",
                   inline = TRUE))

    output$hit_output_matrix_2 <- renderPrint({
      matrix_hit_2_a()
    })

    show("hit_output_matrix_2")
    show("hitung")

    output$hitung <- renderPrint(
      "Silahkan pilih metode"
    )
  })

  observeEvent(input$hit_radio, {
    output$hitung <- renderPrint(
      if(input$hit_radio == "Hitung"){
        hitung_matriks(matrix_hit_1_a(),matrix_hit_2_a())
      }
      else if(input$hit_radio == "Step Perkalian"){
        perkalian_matriks_dengan_langkah(matrix_hit_1_a(),matrix_hit_2_a())
      }
    )
  })
  observeEvent(input$hit_reset_btn, {
    matrix_hit_1_a(NULL)
    matrix_hit_2_a(NULL)
    hide_hitung(session)
  })


  #### input vector hitung ####
  hit_vector <- reactiveVal(NULL)
  observeEvent(input$hitvec_submit_1, {
    vector_hit <- as.numeric(strsplit(input$hit_vec, ",")[[1]])

    if(length(vector_hit) == 0) return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_hit)){
      if(is.na(vector_hit[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_hit)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_hit)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))
    hit_vector(vector_hit)


    hide("hitvec_submit_1")
    output$hitvec_output_vector_1 <- renderPrint({
      hit_vector()
    })
    show("hitvec_output_vector_1")
    show("output_hitvec_2")
    show("dotcross_reset")

    show("hitvec_reset")

    output$hitvec_reset <- renderUI({
        div(
        class = "reset-button",
        actionButton("hitvec_reset_btn", "Reset")
      )
    })

    output$output_hitvec_2 <- renderUI({
      list(
        h5(tags$b("Masukkan vektor kedua ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
        textInput("hit_vec_2",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        actionButton("hitvec_submit_2", "Submit")
      )
    })
  })

  hit_vector_2 <- reactiveVal(NULL)
  observeEvent(input$hitvec_submit_2, {
    vector_hit_2 <- as.numeric(strsplit(input$hit_vec_2, ",")[[1]])
    if(length(vector_hit_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_hit_2)){
      if(is.na(vector_hit_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(hit_vector())!=length(vector_hit_2)) return(shinyalert("Kesalahan Input", "Banyaknya vektor kedua harus sama dengan vektor pertama", type = "error"))

    hide("hitvec_submit_2")
    show("hitvec_output_vector_2")
    hit_vector_2(vector_hit_2)
    output$hitvec_output_vector_2 <- renderPrint({
      hit_vector_2()
    })


    output$radio_hitvec <- renderUI(
      radioButtons("hitvec_radio",
                   label = "Pilih metode",
                   choices = c("Tambah", "Kurang"),
                   selected = "none",
                   inline = TRUE)
    )
    show("hitvec")
    output$hitvec <- renderPrint({
      "Silahkan pilih metode"
    })

  })

  observeEvent(input$hitvec_radio, {
    show("plot_hitung_ui")

    output$plot_hitung_ui <- renderUI(
        plotlyOutput("plot_hitung") %>% withSpinner()
    )
    show("plot_hitung")

      output$plot_hitung <- renderPlotly({
      if(input$hitvec_radio == "Tambah"){
        tambah_vektor(hit_vector(),hit_vector_2())
      }
      else if(input$hitvec_radio == "Kurang"){
        kurang_vektor(hit_vector(),hit_vector_2())
      }
    })


    output$hitvec <- renderPrint(
      if(input$hitvec_radio == "Tambah"){
        tambah_vektor(hit_vector(),hit_vector_2())
      }
      else if(input$hitvec_radio == "Kurang"){
        kurang_vektor(hit_vector(),hit_vector_2())
      }
    )
  })

  observeEvent(input$hitvec_reset_btn, {
    hit_vector(NULL)
    hit_vector_2(NULL)
    hide_hitvec(session)
  })

  #### input proyeksi #####
  proyeksi_vector <- reactiveVal(NULL)
  observeEvent(input$proyeksi_submit_1, {

    vector_proyeksi <- as.numeric(strsplit(input$proyeksi_vec, ",")[[1]])

    if(length(vector_proyeksi) == 0) return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_proyeksi)){
      if(is.na(vector_proyeksi[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_proyeksi)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_proyeksi)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))
    proyeksi_vector(vector_proyeksi)


    hide("proyeksi_submit_1")
    output$proyeksi_output_vector_1 <- renderPrint({
      proyeksi_vector()
    })
    show("proyeksi_output_vector_1")
    show("output_proyeksi_2")
    show("proyeksi_reset")

    output$proyeksi_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("proyeksi_reset_btn", "Reset")
      )
    })

    output$output_proyeksi_2 <- renderUI({
      list(
        h5(tags$b("Masukkan vektor kedua ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
        textInput("proyeksi_vec_2",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        actionButton("proyeksi_submit_2", "Submit")
      )
    })
  })

  proyeksi_vector_2 <- reactiveVal(NULL)
  observeEvent(input$proyeksi_submit_2, {
    vector_proyeksi_2 <- as.numeric(strsplit(input$proyeksi_vec_2, ",")[[1]])
    if(length(vector_proyeksi_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_proyeksi_2)){
      if(is.na(vector_proyeksi_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(proyeksi_vector())!=length(vector_proyeksi_2)) return(shinyalert("Kesalahan Input", "Banyaknya vektor kedua harus sama dengan vektor pertama", type = "error"))

    hide("proyeksi_submit_2")
    show("proyeksi_output_vector_2")
    proyeksi_vector_2(vector_proyeksi_2)
    output$proyeksi_output_vector_2 <- renderPrint({
      proyeksi_vector_2()
    })


    output$radio_proyeksi <- renderUI(
      radioButtons("proyeksi_radio",
                   label = "Pilih metode",
                   choices = c("Norm", "Proyeksi Vektor"),
                   selected = "none",
                   inline = TRUE)
    )

    show("proyeksi")
    output$proyeksi <- renderPrint({
      "Silahkan pilih metode"
    })

  })

  observeEvent(input$proyeksi_radio, {
    show("plot_proyeksi_ui")

    output$plot_proyeksi_ui <- renderUI(
      plotlyOutput("plot_proyeksi") %>% withSpinner()
    )

    show("proyeksi_2")
    show("plot_proyeksi")
    output$plot_proyeksi <- renderPlotly({
      if(input$proyeksi_radio == "Proyeksi Vektor"){
        proyeksi_vektor(proyeksi_vector(),proyeksi_vector_2())
      }
    })

    output$proyeksi <- renderPrint(
      if(input$proyeksi_radio == "Norm"){
        norm_vektor(proyeksi_vector())
      }
      else if(input$proyeksi_radio == "Proyeksi Vektor"){
        proyeksi_vektor(proyeksi_vector(),proyeksi_vector_2())
      }
    )
    output$proyeksi_2<-renderPrint(
      if(input$proyeksi_radio == "Norm"){
        norm_vektor(proyeksi_vector_2())
      }
    )
  })

  observeEvent(input$proyeksi_reset_btn, {
    proyeksi_vector(NULL)
    proyeksi_vector_2(NULL)
    hide_proyeksi(session)
  })

  #### input dot product dan cross product ####
  dot_vector <- reactiveVal(NULL)
  observeEvent(input$dot_submit_1, {

    vector_dot <- as.numeric(strsplit(input$dot_vec, ",")[[1]])

    if(length(vector_dot) == 0) return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_dot)){
      if(is.na(vector_dot[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_dot)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_dot)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))
    dot_vector(vector_dot)


    hide("dot_submit_1")
    output$dot_output_vector_1 <- renderPrint({
      dot_vector()
    })
    show("dot_output_vector_1")
    show("output_dot_2")
    show("dotcross_reset")

    output$dotcross_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("dotcross_reset_btn", "Reset")
      )
    })

    output$output_dot_2 <- renderUI({
      list(
        h5(tags$b("Masukkan vektor kedua ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
        textInput("dot_vec_2",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        actionButton("dot_submit_2", "Submit")
      )
    })
  })

  dot_vector_2 <- reactiveVal(NULL)
  observeEvent(input$dot_submit_2, {
    vector_dot_2 <- as.numeric(strsplit(input$dot_vec_2, ",")[[1]])
    if(length(vector_dot_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_dot_2)){
      if(is.na(vector_dot_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(dot_vector())!=length(vector_dot_2)) return(shinyalert("Kesalahan Input", "Banyaknya vektor kedua harus sama dengan vektor pertama", type = "error"))

    hide("dot_submit_2")
    show("dot_output_vector_2")
    dot_vector_2(vector_dot_2)
    output$dot_output_vector_2 <- renderPrint({
      dot_vector_2()
    })


    output$radio_dotcross <- renderUI(
      radioButtons("dotcross_radio",
                   label = "Pilih metode",
                   choices = c("Dot Product", "Cross Product"),
                   selected = "none",
                   inline = TRUE)
    )

    show("dotcross")
    output$dotcross <- renderPrint({
      "Silahkan pilih metode"
    })

  })

  observeEvent(input$dotcross_radio, {
    show("plot_cross_ui")

    output$plot_cross_ui <- renderUI(
      plotlyOutput("plot_cross") %>% withSpinner()
    )

    show("plot_cross")
    output$plot_cross <- renderPlotly({
      if(input$dotcross_radio == "Dot Product"){
        dot_product(dot_vector(), dot_vector_2())
      }
      else if(input$dotcross_radio == "Cross Product"){
        if(length(dot_vector()) == 3) cross_product(dot_vector(), dot_vector_2())
      }
    })
    output$dotcross <- renderPrint(
      if(input$dotcross_radio == "Dot Product"){
        dot_product(dot_vector(), dot_vector_2())
      }
      else if(input$dotcross_radio == "Cross Product"){
        if(length(dot_vector()) == 2) "Cross product tidak dapat dilakukan pada vektor 2 dimensi"
        else cross_product(dot_vector(), dot_vector_2())
      }
    )
  })

  observeEvent(input$dotcross_reset_btn, {
    dot_vector(NULL)
    dot_vector_2(NULL)
    hide_dotcross(session)
  })


  #### intput Polinomial ####
  x_vector <- reactiveVal(NULL)
  observeEvent(input$x_submit, {
    vector_x <- as.numeric(strsplit(input$x_vec, ",")[[1]])

    if(length(vector_x) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_x)){
      if(is.na(vector_x[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(vector_x)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))


    x_vector(vector_x)
    hide("x_submit")

    output$x_output_vector <- renderPrint({
      x_vector()
    })

    show("x_output_vector")
    show("output_koor_y")
    show("poli_reset")

    output$poli_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("poli_reset_btn", "Reset")
      )
    })

    output$output_koor_y <- renderUI({
      list(
        h5(tags$b("Masukkan koordinat y (y1, y2, y3, ..., yn)")),
        textInput("y_vec", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        actionButton("y_submit", "Submit")
      )
    })
  })

  y_vector <- reactiveVal(NULL)
  observeEvent(input$y_submit, {
    vector_y <- as.numeric(strsplit(input$y_vec, ",")[[1]])

    if(length(vector_y) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_y)){
      if(is.na(vector_y[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(vector_y)!=length(x_vector())) return(shinyalert("Kesalahan Input", "Banyaknya vektor y harus sama dengan vektor x", type = "error"))

    hide("y_submit")
    show("y_output_vector")

    y_vector(vector_y)
    output$y_output_vector <- renderPrint({
      y_vector()
    })

    show("hitung_poli")

    output$hitung_poli <- renderUI(actionButton("poli_submit", "Hitung Interpolasi"))
  })

  observeEvent(input$poli_submit, {
    hide("poli_submit")
    output$poli <- renderPrint(
      interpolasi_polinomial(x_vector(),y_vector())
    )
    show("poli")
  })

  observeEvent(input$poli_reset_btn, {
    x_vector(NULL)
    y_vector(NULL)
    hide_interpolasi(session)

  })



  #### input distance ####

  observeEvent(input$dist_radio,{

    hide("distance")
    hide("hitung_distance")
    hide("bidang_output_vector")
    hide("bidang_output_vector_2")
    hide("titik_output_vector_2")
    hide("input_vector_dist_2")
    hide("titik_output_vector_1")
    hide("bidang_output_vector_1")
    hide("titik_output_vector")

    output$input_vector_dist <- renderUI(
      if (input$dist_radio == "Jarak 2 titik") {
        list(
          h5(tags$b("Masukkan titik pertama ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
          textInput("vec_titik_1", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
          actionButton("titik_submit_1", "Submit")
        )
      } else if (input$dist_radio == "Jarak titik ke garis") {
        list(
          h5(tags$b("Masukkan titik ((x,y) karena untuk vektor 2 dimensi")),
          textInput("vec_bidang_1", label = NULL, width = "30%", placeholder = "contoh : 1,2"),
          actionButton("bidang_submit_1", "Submit")
        )
      } else {
        list(
          h5(tags$b("Masukkan titik ((x,y,z) karena untuk vektor 3 dimensi)")),
          textInput("vec_titik", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
          actionButton("titik_submit", "Submit")
        )
      }
    )
  })


  ### 2 titik
  titik_vector_1 <- reactiveVal(NULL)
  observeEvent(input$titik_submit_1, {
    vector_titik_1 <- as.numeric(strsplit(input$vec_titik_1, ",")[[1]])

    if(length(vector_titik_1) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_titik_1)){
      if(is.na(vector_titik_1[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_titik_1)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_titik_1)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))

    hide("titik_submit_1")

    titik_vector_1(vector_titik_1)
    output$titik_output_vector_1 <- renderPrint({
      titik_vector_1()
    })

    show("titik_output_vector_1")
    show("input_vector_dist_2")

    output$input_vector_dist_2 <- renderUI({
      list(
        h5(tags$b("Masukkan titik kedua ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
        textInput("vec_titik_2", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        actionButton("titik_submit_2", "Submit")
      )
    })

    output$dist_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("dist_reset_btn", "Reset")
      )
    })
  })

  bidang_vector_1 <- reactiveVal(NULL)
  observeEvent(input$bidang_submit_1, {
    vector_bidang_1 <- as.numeric(strsplit(input$vec_bidang_1, ",")[[1]])

    if(length(vector_bidang_1) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_bidang_1)){
      if(is.na(vector_bidang_1[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_bidang_1) != 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor harus 2 elemen", type = "error"))

    hide("bidang_submit_1")


    bidang_vector_1(vector_bidang_1)
    output$bidang_output_vector_1 <- renderPrint({
      bidang_vector_1()
    })


    show("bidang_output_vector_1")
    show("input_vector_dist_2")


    output$input_vector_dist_2 <- renderUI({
      list(
        h5(tags$b("Masukkan koefisien garis ((a,b,c) dari persamaan ax +by + c = 0 ))")),
        textInput("vec_bidang_2", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        actionButton("bidang_submit_2", "Submit")
      )
    })

    output$dist_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("dist_reset_btn", "Reset")
      )
    })
  })

  titik_vector <- reactiveVal(NULL)
  observeEvent(input$titik_submit, {
    vector_titik <- as.numeric(strsplit(input$vec_titik, ",")[[1]])

    if(length(vector_titik) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_titik)){
      if(is.na(vector_titik[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_titik) != 3) return(shinyalert("Kesalahan Input", "Banyaknya vektor harus 3 elemen", type = "error"))

    hide("titik_submit")


    titik_vector(vector_titik)
    output$titik_output_vector <- renderPrint({
      titik_vector()
    })


    show("titik_output_vector")
    show("input_vector_dist_2")


    output$input_vector_dist_2 <- renderUI({
      list(

        h5(tags$b("Masukkan koefisien bidang ((a,b,c,d) dari persamaan ax +by + cz + d = 0 )")),
        textInput("vec_bidang", label = NULL, width = "30%", placeholder = "contoh : 1,2,3,4"),
        actionButton("bidang_submit", "Submit")
      )
    })

    output$dist_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("dist_reset_btn", "Reset")
      )
    })
  })

  titik_vector_2 <- reactiveVal(NULL)
  observeEvent(input$titik_submit_2, {
    vector_titik_2 <- as.numeric(strsplit(input$vec_titik_2, ",")[[1]])

    if(length(vector_titik_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_titik_2)){
      if(is.na(vector_titik_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_titik_2) != length(titik_vector_1())) return(shinyalert("Kesalahan Input", "Banyaknya vektor titik kedua harus sama dengan vektor titik pertama", type = "error"))

    hide("titik_submit_2")

    titik_vector_2(vector_titik_2)
    output$titik_output_vector_2 <- renderPrint({
      titik_vector_2()
    })

    show("titik_output_vector_2")
    show("hitung_distance")
    output$hitung_distance <- renderUI(if(input$titik_submit_2>0) actionButton("distance_submit", "Hitung Jarak"))
  })

  bidang_vector_2 <- reactiveVal(NULL)
  observeEvent(input$bidang_submit_2, {
    vector_bidang_2 <- as.numeric(strsplit(input$vec_bidang_2, ",")[[1]])

    if(length(vector_bidang_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_bidang_2)){
      if(is.na(vector_bidang_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_bidang_2) != 3) return(shinyalert("Kesalahan Input", "Banyaknya vektor garis harus 3 elemen", type = "error"))

    hide("bidang_submit_2")

    bidang_vector_2(vector_bidang_2)
    output$bidang_output_vector_2 <- renderPrint({
      bidang_vector_2()
    })
    show("bidang_output_vector_2")
    show("hitung_distance")
    output$hitung_distance <- renderUI(if(input$bidang_submit_2>0) actionButton("distance_submit", "Hitung Jarak"))
  })

  bidang_vector <- reactiveVal(NULL)
  observeEvent(input$bidang_submit, {
    vector_bidang <- as.numeric(strsplit(input$vec_bidang, ",")[[1]])

    if(length(vector_bidang) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_bidang)){
      if(is.na(vector_bidang[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_bidang) != 4) return(shinyalert("Kesalahan Input", "Banyaknya vektor bidang harus 4 elemen", type = "error"))

    hide("bidang_submit")

    bidang_vector(vector_bidang)
    output$bidang_output_vector <- renderPrint({
      bidang_vector()
    })

    show("bidang_output_vector")
    show("hitung_distance")
    output$hitung_distance <- renderUI(if(input$bidang_submit>0) actionButton("distance_submit", "Hitung Jarak"))
  })


  observeEvent(input$distance_submit, {
    hide("distance_submit")
    show("distance")
    if (input$dist_radio == "Jarak 2 titik") {
      if(input$distance_submit>0){
        output$distance <- renderPrint(
          distance_dua_titik(titik_vector_1(),titik_vector_2())
        )
      }
    } else if (input$dist_radio == "Jarak titik ke garis") {
      output$distance <- renderPrint(
        distance_point_line(bidang_vector_1(),bidang_vector_2())
      )
    } else {
      output$distance <- renderPrint(
        distance_point_plane(titik_vector(),bidang_vector())
      )
    }
  })


  observeEvent(input$dist_reset_btn, {
    updateNumericInput(session, "vec_titik_1", value = "")
    updateNumericInput(session, "vec_bidang_1", value = "")
    updateNumericInput(session, "vec_titik", value = "")
    hide("distance")
    hide("hitung_distance")
    hide("bidang_output_vector")
    hide("bidang_output_vector_2")
    hide("titik_output_vector_2")
    hide("input_vector_dist_2")
    hide("titik_output_vector_1")
    hide("bidang_output_vector_1")
    hide("titik_output_vector")
    hide("dist_reset_btn")
    show("titik_submit_1")
    show("bidang_submit_1")
    show("titik_submit")
  })

  #### input transformasi linear ####
  trans_vector <- reactiveVal(NULL)
  observeEvent(input$trans_submit, {
    vector_trans <- as.numeric(strsplit(input$trans_vec, ",")[[1]])

    if(length(vector_trans) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_trans)){
      if(is.na(vector_trans[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_trans)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_trans)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))

    hide("trans_submit")

    trans_vector(vector_trans)
    output$trans_output_vector <- renderPrint({
      trans_vector()
    })

    show("trans_output_vector")
    show("radio_trans")

    output$trans_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("trans_reset_btn", "Reset")
      )
    })

    output$radio_trans <- renderUI(radioButtons("trans_radio",
                                                label = "Pilih metode",
                                                choices = c("Refleksi", "Rotasi", "Proyeksi", "Kontraksi/Dilatasi"),
                                                selected = "none",
                                                inline = TRUE))
  })

  observeEvent(input$trans_radio,{
    show("input_info_trans")
    hide("refleksi_output_vector")
    hide("proyeksi_output_vector")
    hide("kontraksi_output_vector")
    hide("rotasi_output_vector")
    hide("rotasi_output_vector_alfa")
    hide("rotasi_axis_input")
    hide("plot")
    hide("transformasi")

    output$input_info_trans <- renderUI({
      if (input$trans_radio == "Refleksi") {
        list(
          if(length(trans_vector()) == 2){
            h5(tags$b("Masukkan terhadap axis (x / y / line)"))
          }

          else {
            h5(tags$b("Masukkan terhadap bidang (xy / xz / yz)"))
          },
          textInput("vec_refleksi", label = NULL, width = "30%"),
          actionButton("refleksi_submit", "Submit")
        )
      } else if (input$trans_radio == "Rotasi") {
        list(
          h5(tags$b("Besar Sudut (diisi dengan angka)")),
          textInput("vec_rotasi_alfa", label = NULL, width = "30%"),
          actionButton("rotasi_submit_alfa", "Submit")
        )
      } else if(input$trans_radio == "Proyeksi"){
        list(
          if(length(trans_vector()) == 2){
            h5(tags$b("Masukkan terhadap axis (x / y)"))
          }
          else {
            h5(tags$b("Masukkan terhadap bidang (xy / xz / yz)"))
          },
          textInput("vec_proyeksi", label = NULL, width = "30%"),

          actionButton("proyeksi_submit", "Submit")
        )
      } else {
        list(
          h5(tags$b("Masukkan besaran skalar (diisi dengan angka)")),
          textInput("vec_kontraksi", label = NULL, width = "30%"),
          actionButton("kontraksi_submit", "Submit")
        )
      }
    })
  })

  refleksi_vector <- reactiveVal(NULL)
  observeEvent(input$refleksi_submit, {
    vector_ref <- input$vec_refleksi

    if(vector_ref == "") return(shinyalert("Kesalahan Input", ifelse(length(trans_vector()) == 2, "Masukkan axis terlebih dahulu", "Masukkan bidang terlebih dahulu"), type = "error"))
    else if (!(vector_ref %in%  c("x","y","line")) && length(trans_vector()) == 2) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'x', 'y', atau 'line'", type = "error"))
    }
    else if (!(vector_ref %in%  c("xy","xz","yz")) && length(trans_vector()) == 3) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'xy', 'xz', atau 'yz'", type = "error"))
    }

    hide("refleksi_submit")
    refleksi_vector(vector_ref)
    output$refleksi_output_vector <- renderPrint({
      refleksi_vector()
    })

    show("refleksi_output_vector")

    output$transformasi <- renderPrint(
      reflection_vector(trans_vector(),refleksi_vector())
    )

    show("plot_ui")

    output$plot_ui <- renderUI(
      plotlyOutput("plot") %>% withSpinner()
    )

    output$plot <- renderPlotly({
      reflection_vector(trans_vector(),refleksi_vector())
    })

    show("plot")
    show("transformasi")
  })

  proyeks_vector <- reactiveVal(NULL)
  observeEvent(input$proyeksi_submit, {
    vector_pro <- input$vec_proyeksi

    if(vector_pro == "") return(shinyalert("Kesalahan Input", ifelse(length(trans_vector()) == 2, "Masukkan axis terlebih dahulu", "Masukkan bidang terlebih dahulu"), type = "error"))
    else if (!(vector_pro %in%  c("x","y")) && length(trans_vector()) == 2) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'x', atau 'y", type = "error"))
    }
    else if (!(vector_pro %in%  c("xy","xz","yz")) && length(trans_vector()) == 3) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'xy', 'xz', atau 'yz'", type = "error"))
    }

    hide("proyeksi_submit")

    proyeks_vector(vector_pro)
    output$proyeksi_output_vector <- renderPrint({
      proyeks_vector()
    })

    show("proyeksi_output_vector")

    show("plot_ui")

    output$plot_ui <- renderUI(
      plotlyOutput("plot") %>% withSpinner()
    )

    output$plot <- renderPlotly({
      projection_vector(trans_vector(),proyeks_vector())
    })

    output$transformasi <- renderPrint(
      projection_vector(trans_vector(),proyeks_vector())
    )
    show("plot")
    show("transformasi")



  })

  kontraksi_vector <- reactiveVal(NULL)
  observeEvent(input$kontraksi_submit, {
    vector_kont <- input$vec_kontraksi

    if(vector_kont == "") return(shinyalert("Kesalahan Input", "Masukkan angka terlebih dahulu",type = "error"))
    else if(is_valid_fraction(vector_kont)) kontraksi_vector(eval(parse(text = vector_kont)))
    else if(is.na(as.numeric(vector_kont))) return(shinyalert("Kesalahan Input", "Terjadi kesalahan input bukan angka",type = "error"))
    else kontraksi_vector(as.numeric(vector_kont))

    hide("kontraksi_submit")
    output$kontraksi_output_vector <- renderPrint({
      kontraksi_vector()
    })

    show("kontraksi_output_vector")

    show("plot_ui")

    output$plot_ui <- renderUI(
      plotlyOutput("plot") %>% withSpinner()
    )

    output$plot <- renderPlotly({
      contraction_dilatation(trans_vector(),kontraksi_vector())
    })

    output$transformasi <- renderPrint(
       contraction_dilatation(trans_vector(),kontraksi_vector())
    )
    show("plot")
    show("transformasi")
  })

  rotasi_vector_alfa <- reactiveVal(NULL)
  observeEvent(input$rotasi_submit_alfa, {
    vector_rot_alfa <- as.numeric(input$vec_rotasi_alfa)

    if(is.na(vector_rot_alfa)) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input", type = "error"))

    hide("rotasi_submit_alfa")
    rotasi_vector_alfa(vector_rot_alfa)
    output$rotasi_output_vector_alfa <- renderPrint({
      rotasi_vector_alfa()
    })

    show("rotasi_output_vector_alfa")

    if(length(trans_vector()) == 2){
      show("plot_ui")

      output$plot_ui <- renderUI(
        plotlyOutput("plot") %>% withSpinner()
      )

      output$plot <- renderPlotly({
        rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
      })

      output$transformasi <- renderPrint(
        rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
      )
      show("plot")
      show("transformasi")
    }
    else{
      show("rotasi_axis_input")
      output$rotasi_axis_input <- renderUI(
        list(
          h5(tags$b("Masukkan terhadap axis (x / y / z)")),
          textInput("vec_rotasi", label = NULL, width = "30%"),
          actionButton("rotasi_submit", "Submit")
        )
      )
    }


  })

  rotasi_vector <- reactiveVal(NULL)
  observeEvent(input$rotasi_submit, {
    vector_rot <- input$vec_rotasi

    if(vector_rot == "") return(shinyalert("Kesalahan Input", "Masukkan axis terlebih dahulu", type = "error"))
    else if (!(vector_rot %in%  c("x","y","z")) ) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'x', 'y', atau 'z'", type = "error"))
    }

    hide("rotasi_submit")

    rotasi_vector(vector_rot)
    output$rotasi_output_vector <- renderPrint({
      rotasi_vector()
    })

    show("rotasi_output_vector")

    show("plot_ui")

    output$plot_ui <- renderUI(
      plotlyOutput("plot") %>% withSpinner()
    )

    output$plot <- renderPlotly({
        rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
    })

    output$transformasi <- renderPrint(
        rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
    )
    show("plot")
    show("transformasi")


  })

  observeEvent(input$trans_reset_btn, {
    refleksi_vector(NULL)
    trans_vector(NULL)
    updateTextInput(session, "trans_vec", value = "")
    hide("trans_reset_btn")
    hide("transformasi")
    hide("plot_ui")
    hide("plot")
    hide("refleksi_output_vector")
    hide("proyeksi_output_vector")
    hide("kontraksi_output_vector")
    hide("rotasi_output_vector")
    hide("rotasi_output_vector_alfa")
    hide("rotasi_axis_input")
    hide("trans_output_vector")
    hide("trans_radio")
    hide("input_info_trans")
    show("trans_submit")

  })

}

shinyApp(ui, server)
