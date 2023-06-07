library(shiny)
# devtools::install_github('nicholasrazali/package')
library(AljabarLinear)
library(shinyMatrix)
library(plotly)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML("
      .size{
        width: 120px;
      }
    ")
    )
  ),

#### script ####
  tags$script(HTML('
    $(document).ready(function() {
      Shiny.addCustomMessageHandler("change_matrix_size", function(message) {
        var width = message.width * 40 + "px";
        $(".size").css("width", width);
      });

      function setSavedWidth() {
        $(".size").css("width", "50px");
        console.log($(".size").css("width"));
      }

      Shiny.addCustomMessageHandler("setSavedWidth", function(message) {
        setSavedWidth();
      });
    });
  ')),

#### /script ####

  titlePanel("Aljabar Linear"),
  sidebarLayout(
    sidebarPanel(
      selectInput("functions", "Pilih Jenis Materi", choices = c("Select", "Matrix", "Vektor")),

    ),
    mainPanel(
      uiOutput("dynamicTabs"),
    )
  ),
)

server <- function(input, output, session) {
  ##### tab #####
  output$dynamicTabs <- renderUI(
    ##### matrix ######
    if(input$functions == "Matrix"){
      navbarPage(
        id = "nav_matrix",
        title = NULL,
        header = NULL,
        footer = NULL,
        ##### determinan ######
        tabPanel("Determinan",
                 uiOutput("det_reset"),
                 numericInput("det_ukuran", "Masukkan Ukuran Baris dan Kolom:", value = 2, min = 1, max = 5, width = "30%"),
                 actionButton("det_submit", "Submit"),

                 uiOutput("det_matrix_input"),
                 uiOutput("det_submit_matrix"),
                 verbatimTextOutput("det_output_matrix"),

                 uiOutput("radio_det"),

                 verbatimTextOutput("determinan")),

        ##### invers ######
        tabPanel("Invers",
                 numericInput("inv_rows", "Masukkan Jumlah Baris:", value = 2, min = 1, max = 5),
                 numericInput("inv_cols", "Masukkan Jumlah Kolom:", value = 2, min = 1, max = 5),
                 actionButton("inv_submit", "Submit"),

                 uiOutput("inv_matrix_input"),
                 uiOutput("inv_submit_matrix"),
                 verbatimTextOutput("inv_output_matrix"),

                 uiOutput("radio_inv"),

                 verbatimTextOutput("invers")),

        ##### hitung ######
        tabPanel("Perhitungan",
                 h3("Masukkan matrix pertama"),
                 numericInput("hit_rows_1", "Masukkan Jumlah Baris:", value = 2, min = 1),
                 numericInput("hit_cols_1", "Masukkan Jumlah Kolom:", value = 2, min = 1),
                 actionButton("submit_1", "Submit"),

                 uiOutput("hit_matrix_input_1"),
                 uiOutput("hit_submit_matrix_1"),
                 verbatimTextOutput("hit_output_matrix_1"),

                 uiOutput("output_matrix_hit_2"),

                 uiOutput("radio_hit"),

                 verbatimTextOutput("hitung")),

        ##### persamaan linear ######
        tabPanel("Persamaan Linear",
                 numericInput("pers_rows", "Masukkan Jumlah Baris:", value = 2, min = 1),
                 numericInput("pers_cols", "Masukkan Jumlah Kolom:", value = 2, min = 1),
                 actionButton("pers_submit", "Submit"),

                 uiOutput("pers_matrix_input"),
                 uiOutput("pers_submit_matrix"),
                 verbatimTextOutput("pers_output_matrix"),

                 uiOutput("output_pers_vec"),

                 uiOutput("radio_pers"),

                 verbatimTextOutput("persamaan"))
      )

    } else if(input$functions == "Vektor"){
      ##### vector ######
      navbarPage(
        id = "nav_vector",
        title = NULL,
        header = NULL,
        footer = NULL,
        ##### dot cross ######
        tabPanel("Dot and Cross",
                 textInput("dot_vec", "Masukkan vektor pertama (pisahkan dengan koma)", "0"),
                 actionButton("dot_submit_1", "Submit"),
                 verbatimTextOutput("dot_output_vector_1"),

                 uiOutput("output_dot_2"),

                 uiOutput("radio_dotcross"),

                 verbatimTextOutput("dotcross")),
        ##### polinomial ######
        tabPanel("Polinomial",
                 textInput("x_vec", "Masukkan Koordinat x (pisahkan dengan koma)", "0"),
                 actionButton("x_submit", "Submit"),
                 verbatimTextOutput("x_output_vector"),

                 uiOutput("output_koor_y"),

                 uiOutput("hitung_poli"),

                 verbatimTextOutput("poli")),

        ##### distance ######
        tabPanel("Distance",
                 radioButtons("dist_radio",
                              label = "Pilih perhitungan jarak terhadap",
                              choices = c("Jarak 2 titik", "Jarak 2 bidang", "Jarak titik ke bidang"),
                              selected = "Jarak 2 titik",
                              inline = TRUE),

                 uiOutput("input_vector_dist"),
                 uiOutput("input_vector_dist_2"),
                 uiOutput("hitung_distance"),

                 verbatimTextOutput("distance")),

        ##### transformasi ######
        tabPanel("Transformasi Linear",
                 textInput("trans_vec", "Masukkan vektor (pisahkan dengan koma)", "0"),
                 actionButton("trans_submit", "Submit"),
                 verbatimTextOutput("trans_output_vector"),

                 uiOutput("output_trans"),

                 uiOutput("radio_trans"),
                 uiOutput("input_info_trans"),

                 verbatimTextOutput("transformasi"),
                 plotlyOutput("plot"))

      )

    }else {
      h1("Perhitungan Aljabar Linear dengan Menggunakan R")
    }
  )

  #### reset mengganti tab ####
  observeEvent(input$nav_matrix,{
    if(input$nav_matrix == "Invers"){
      updateNumericInput(session, "det_ukuran", value = 2)
      matrix_det(NULL)
      matrix_det_a(NULL)
      hide("det_input_matrix")
      show("det_submit")
      hide("det_reset_btn")
      hide("det_submit_btn")
      hide("det_radio")
      hide("determinan")
      hide("det_output_matrix")
    }

  })



  ##### input matrix invers #####
  matrix_data <- reactiveVal(NULL)

  observeEvent(input$inv_submit, {
    output$inv_matrix_input <- renderUI({
      matrixInput(
        inputId = "inv_input_matrix",
        label = "Masukkan matriks:",
        value = matrix_data(),
        rows = list(extend = FALSE, names = FALSE),
        cols = list(extend = FALSE, names = FALSE),
        inputClass = "size",
        class = "numeric",
      )
    })
      session$sendCustomMessage(type = "change_matrix_size", message = list(
        width = input$inv_cols,
        height = input$inv_rows
      ))



    matrix_data(matrix(0, nrow = input$inv_rows, ncol = input$inv_cols, dimnames = list(NULL, NULL)))
    output$inv_submit_matrix <-renderUI({
      actionButton("inv_submit_btn", "input matrix")
    })

    session$sendCustomMessage(type = "setSavedWidth",NULL)
  })

  matrix_a <- reactiveVal(NULL)
  observeEvent(input$inv_submit_btn, {
    mat <- c()
    for (i in 1:(input$inv_rows * input$inv_cols)) {
      mat <- c(mat, input$inv_input_matrix[[i]])
    }
    matrix_a(matrix(mat, nrow = input$inv_rows, ncol = input$inv_cols))

    output$inv_output_matrix <- renderPrint({
      matrix_a()
    })

    output$radio_inv <- renderUI(radioButtons("inv_radio",
                                              label = "Pilih metode",
                                              choices = c("Row Reduction", "Adjoint Matrix"),
                                              selected = "Row Reduction",
                                              inline = TRUE))

    #### output akhir untuk Invers
    output$invers <- renderPrint(
      if(input$inv_radio == "Row Reduction"){
        invers_row_reduction(matrix_a())
      }
      else {
        adjoint_matriks(matrix_a())
      }
    )
  })

  ##### input matrix determinan #####
  matrix_det <- reactiveVal(NULL)
  observeEvent(input$det_submit, {
    output$det_matrix_input <- renderUI({
      matrixInput(
        inputId = "det_input_matrix",
        label = "Masukkan matriks:",
        value = matrix_data(),
        rows = list(extend = FALSE, names = FALSE),
        cols = list(extend = FALSE, names = FALSE),
        inputClass = "size",
        class = "numeric",
      )
    })
    matrix_data(matrix("", nrow = input$det_ukuran, ncol = input$det_ukuran, dimnames = list(NULL, NULL)))
    output$det_submit_matrix <-renderUI({
      actionButton("det_submit_btn", "input matrix")
    })
    hide("det_submit")

    output$det_reset <- renderUI({
      actionButton("det_reset_btn", "Reset")
    })
  })

  matrix_det_a <- reactiveVal(NULL)
  observeEvent(input$det_submit_btn, {
    mat <- c()
    for (i in 1:(input$det_ukuran * input$det_ukuran)) {
      if(is.na(input$det_input_matrix[[i]])) return(showNotification("Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$det_input_matrix[[i]])
    }
    matrix_det_a(matrix(mat, nrow = input$det_ukuran, ncol = input$det_ukuran))
    hide("det_submit_btn")


    output$radio_det <- renderUI(radioButtons("det_radio",
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
    updateNumericInput(session, "det_ukuran", value = 2)
    matrix_det(NULL)
    matrix_det_a(NULL)
    hide("det_input_matrix")
    show("det_submit")
    hide("det_reset_btn")
    hide("det_submit_btn")
    hide("det_radio")
    hide("determinan")
    hide("det_output_matrix")
  })






  #=== input matrix persamaan linear
  matrix_pers <- reactiveVal(NULL)
  observeEvent(input$pers_submit, {
    output$pers_matrix_input <- renderUI({
      matrixInput(
        inputId = "pers_input_matrix",
        label = "Masukkan matriks:",
        value = matrix_pers(),
        rows = list(extend = FALSE, names = FALSE),
        cols = list(extend = FALSE, names = FALSE),
        class = "numeric",
      )
    })
    matrix_pers(matrix(0, nrow = input$pers_rows, ncol = input$pers_cols, dimnames = list(NULL, NULL)))

    output$pers_submit_matrix <-renderUI({
      actionButton("pers_submit_btn", "input matrix")
    })
  })

  matrix_pers_a <- reactiveVal(NULL)
  observeEvent(input$pers_submit_btn, {
    mat <- c()
    for (i in 1:(input$pers_rows * input$pers_cols)) {
      mat <- c(mat, as.numeric(input$pers_input_matrix[[i]]))
    }
    matrix_pers_a(matrix(mat, nrow = input$pers_rows, ncol = input$pers_cols))

    output$pers_output_matrix <- renderPrint({
      matrix_pers_a()
    })

    output$output_pers_vec <- renderUI(list(
      textInput("pers_vec", "Masukkan vektor (pisahkan dengan koma)", "0"),
      actionButton("pers_submit_vec", "Submit"),
      verbatimTextOutput("pers_output_vector")
    ))
  })

  pers_vector_a <- reactiveVal(NULL)
  observeEvent(input$pers_submit_vec, {
    pers_vector_input <- as.numeric(strsplit(input$pers_vec, ",")[[1]])
    pers_vector_a <- c(pers_vector_input)
    output$pers_output_vector <- renderPrint({
      pers_vector_a
    })

    output$radio_pers <- renderUI(radioButtons("pers_radio",
                                               label = "Pilih metode",
                                               choices = c("Gauss Jordan", "Cramer's Rule"),
                                               selected = "Gauss Jordan",
                                               inline = TRUE))

    #### output untuk persamaan linear
    output$persamaan <- renderPrint(
      if(input$pers_radio == "Gauss Jordan"){
        gauss_jordan(matrix_pers_a(), pers_vector_a)
      }
      else {
        cramer_rule(matrix_pers_a(),pers_vector_a)
      }
    )
  })

  #==== matrix hitung
  ### hitung 1
  matrix_hit_1 <- reactiveVal(NULL)
  observeEvent(input$submit_1, {
    output$hit_matrix_input_1 <- renderUI({
      matrixInput(
        inputId = "hit_input_matrix_1",
        label = "Masukkan matriks Pertama:",
        value = matrix_hit_1(),
        rows = list(extend = FALSE, names = FALSE),
        cols = list(extend = FALSE, names = FALSE),
        class = "numeric",
      )
    })
    matrix_hit_1(matrix(0, nrow = input$hit_rows_1, ncol = input$hit_cols_1, dimnames = list(NULL, NULL)))

    output$hit_submit_matrix_1 <-renderUI({
      actionButton("hit_submit_1", "input matrix")
    })
  })

  matrix_hit_1_a <- reactiveVal(NULL)
  observeEvent(input$hit_submit_1, {
    mat <- c()
    for (i in 1:(input$hit_rows_1 * input$hit_cols_1)) {
      mat <- c(mat, input$hit_input_matrix_1[[i]])
    }
    matrix_hit_1_a(matrix(mat, nrow = input$hit_rows_1, ncol = input$hit_cols_1))

    output$hit_output_matrix_1 <- renderPrint({
      matrix_hit_1_a()
    })

    output$output_matrix_hit_2 <- renderUI({
      list(h3("Masukkan matrix kedua"),
           numericInput("hit_rows_2", "Masukkan Jumlah Baris:", value = 2, min = 1),
           numericInput("hit_cols_2", "Masukkan Jumlah Kolom:", value = 2, min = 1),
           actionButton("submit_2", "Submit"),

           uiOutput("hit_matrix_input_2"),
           uiOutput("hit_submit_matrix_2"),
           verbatimTextOutput("hit_output_matrix_2"))
    })
  })

  ### hitung 2
  matrix_hit_2 <- reactiveVal(NULL)
  observeEvent(input$submit_2, {
    output$hit_matrix_input_2 <- renderUI({
      matrixInput(
        inputId = "hit_input_matrix_2",
        label = "Masukkan matriks Kedua:",
        value = matrix_hit_2(),
        rows = list(extend = FALSE, names = FALSE),
        cols = list(extend = FALSE, names = FALSE),
        class = "numeric",
      )
    })

    matrix_hit_2(matrix(0, nrow = input$hit_rows_2, ncol = input$hit_cols_2, dimnames = list(NULL, NULL)))

    output$hit_submit_matrix_2 <-renderUI({
      actionButton("hit_submit_2", "input matrix")
    })
  })

  matrix_hit_2_a <- reactiveVal(NULL)
  observeEvent(input$hit_submit_2, {
    mat <- c()
    for (i in 1:(input$hit_rows_2 * input$hit_cols_2)) {
      mat <- c(mat, input$hit_input_matrix_2[[i]])
    }
    matrix_hit_2_a(matrix(mat, nrow = input$hit_rows_2, ncol = input$hit_cols_2))

    output$radio_hit <- renderUI(radioButtons("hit_radio",
                                              label = "Pilih",
                                              choices = c("Hitung", "Step Perkalian"),
                                              selected = "Hitung",
                                              inline = TRUE))

    output$hit_output_matrix_2 <- renderPrint({
      matrix_hit_2_a()
    })

    #### output untuk Perhitungan
    output$hitung <- renderPrint(
      if(input$hit_radio == "Hitung"){
        hitung_matriks(matrix_hit_1_a(),matrix_hit_2_a())
      }
      else {
        perkalian_matriks_dengan_langkah(matrix_hit_1_a(),matrix_hit_2_a())
      }
    )


  })



  ##### Vector
  #### output untuk dot product dan cross product
  dot_vector <- reactiveVal(NULL)
  observeEvent(input$dot_submit_1, {
    vector_dot <- as.numeric(strsplit(input$dot_vec, ",")[[1]])
    dot_vector(vector_dot)
    output$dot_output_vector_1 <- renderPrint({
      dot_vector()
    })

    output$output_dot_2 <- renderUI({
      list(
        textInput("dot_vec_2", "Masukkan vektor kedua (pisahkan dengan koma)", "0"),
        actionButton("dot_submit_2", "Submit"),
        verbatimTextOutput("dot_output_vector_2")
      )
    })
  })

  dot_vector_2 <- reactiveVal(NULL)
  observeEvent(input$dot_submit_2, {
    vector_dot_2 <- as.numeric(strsplit(input$dot_vec_2, ",")[[1]])
    dot_vector_2(vector_dot_2)
    output$dot_output_vector_2 <- renderPrint({
      dot_vector_2()
    })

    output$radio_dotcross <- renderUI(
      radioButtons("dotcross_radio",
                   label = "Pilih metode",
                   choices = c("Dot Product", "Cross Product"),
                   selected = "Dot Product",
                   inline = TRUE)
    )

    output$dotcross <- renderPrint({
      if (input$dotcross_radio == "Dot Product") {
        dot_product(dot_vector(), dot_vector_2())
      } else {
        cross_product(dot_vector(), dot_vector_2())
      }
    })
  })




  #### output untuk Polinomial
  x_vector <- reactiveVal(NULL)
  observeEvent(input$x_submit, {
    vector_x <- as.numeric(strsplit(input$x_vec, ",")[[1]])
    x_vector(vector_x)
    output$x_output_vector <- renderPrint({
      x_vector()
    })

    output$output_koor_y <- renderUI({
      list(
        textInput("y_vec", "Masukkan Koordinat y (pisahkan dengan koma)", "0"),
        actionButton("y_submit", "Submit"),
        verbatimTextOutput("y_output_vector")
      )
    })
  })

  y_vector <- reactiveVal(NULL)
  observeEvent(input$y_submit, {
    vector_y <- as.numeric(strsplit(input$y_vec, ",")[[1]])
    y_vector(vector_y)
    output$y_output_vector <- renderPrint({
      y_vector()
    })

    output$hitung_poli <- renderUI(actionButton("poli_submit", "Hitung Interpolasi"))
  })

  observeEvent(input$poli_submit, {
    output$poli <- renderPrint(
      interpolasi_polinomial(x_vector(),y_vector())
    )

    output$hitung_poli <- renderUI(actionButton("submit", "Hitung Interpolasi"))
  })


  #### output untuk dot product dan cross product

  output$input_vector_dist <- renderUI(
    if (input$dist_radio == "Jarak 2 titik") {
      list(
        textInput("vec_titik_1", "Masukkan titik pertama (pisahkan dengan koma)", "0"),
        actionButton("titik_submit_1", "Submit"),
        verbatimTextOutput("titik_output_vector_1")
      )
    } else if (input$dist_radio == "Jarak 2 bidang") {
      list(
        textInput("vec_bidang_1", "Masukkan bidang pertama (pisahkan dengan koma)", "0"),
        actionButton("bidang_submit_1", "Submit"),
        verbatimTextOutput("bidang_output_vector_1")
      )
    } else {
      list(
        textInput("vec_titik", "Masukkan titik (pisahkan dengan koma)", "0"),
        actionButton("titik_submit", "Submit"),
        verbatimTextOutput("titik_output_vector")
      )
    }
  )
  ### 2 titik
  titik_vector_1 <- reactiveVal(NULL)
  observeEvent(input$titik_submit_1, {
    vector_titik_1 <- as.numeric(strsplit(input$vec_titik_1, ",")[[1]])
    titik_vector_1(vector_titik_1)
    output$titik_output_vector_1 <- renderPrint({
      titik_vector_1()
    })

    output$input_vector_dist_2 <- renderUI({
      list(
        if (input$dist_radio == "Jarak 2 titik") {
          if(input$titik_submit_1 > 0){
            list(
              textInput("vec_titik_2", "Masukkan titik kedua (pisahkan dengan koma)", "0"),
              actionButton("titik_submit_2", "Submit"),
              verbatimTextOutput("titik_output_vector_2")
            )
          }
        } else if (input$dist_radio == "Jarak 2 bidang") {
          if(input$bidang_submit_1 > 0){
            list(
              textInput("vec_bidang_2", "Masukkan bidang kedua (pisahkan dengan koma)", "0"),
              actionButton("bidang_submit_2", "Submit"),
              verbatimTextOutput("bidang_output_vector_2")
            )
          }
        } else {
          if(input$titik_submit > 0){
            list(
              textInput("vec_bidang", "Masukkan bidang (pisahkan dengan koma)", "0"),
              actionButton("bidang_submit", "Submit"),
              verbatimTextOutput("bidang_output_vector")
            )
          }
        }
      )
    })
  })

  bidang_vector_1 <- reactiveVal(NULL)
  observeEvent(input$bidang_submit_1, {
    vector_bidang_1 <- as.numeric(strsplit(input$vec_bidang_1, ",")[[1]])
    bidang_vector_1(vector_bidang_1)
    output$bidang_output_vector_1 <- renderPrint({
      bidang_vector_1()
    })

    output$input_vector_dist_2 <- renderUI({
      list(
        if (input$dist_radio == "Jarak 2 titik") {
          if(input$titik_submit_1 > 0){
            list(
              textInput("vec_titik_2", "Masukkan titik kedua (pisahkan dengan koma)", "0"),
              actionButton("titik_submit_2", "Submit"),
              verbatimTextOutput("titik_output_vector_2")
            )
          }
        } else if (input$dist_radio == "Jarak 2 bidang") {
          if(input$bidang_submit_1 > 0){
            list(
              textInput("vec_bidang_2", "Masukkan bidang kedua (pisahkan dengan koma)", "0"),
              actionButton("bidang_submit_2", "Submit"),
              verbatimTextOutput("bidang_output_vector_2")
            )
          }
        } else {
          if(input$titik_submit > 0){
            list(
              textInput("vec_bidang", "Masukkan bidang (pisahkan dengan koma)", "0"),
              actionButton("bidang_submit", "Submit"),
              verbatimTextOutput("bidang_output_vector")
            )
          }
        }
      )
    })
  })

  titik_vector <- reactiveVal(NULL)
  observeEvent(input$titik_submit, {
    vector_titik <- as.numeric(strsplit(input$vec_titik, ",")[[1]])
    titik_vector(vector_titik)
    output$titik_output_vector <- renderPrint({
      titik_vector()
    })

    output$input_vector_dist_2 <- renderUI({
      list(
        if (input$dist_radio == "Jarak 2 titik") {
          if(input$titik_submit_1 > 0){
            list(
              textInput("vec_titik_2", "Masukkan titik kedua (pisahkan dengan koma)", "0"),
              actionButton("titik_submit_2", "Submit"),
              verbatimTextOutput("titik_output_vector_2")
            )
          }
        } else if (input$dist_radio == "Jarak 2 bidang") {
          if(input$bidang_submit_1 > 0){
            list(
              textInput("vec_bidang_2", "Masukkan bidang kedua (pisahkan dengan koma)", "0"),
              actionButton("bidang_submit_2", "Submit"),
              verbatimTextOutput("bidang_output_vector_2")
            )
          }
        } else {
          if(input$titik_submit > 0){
            list(
              textInput("vec_bidang", "Masukkan bidang (pisahkan dengan koma)", "0"),
              actionButton("bidang_submit", "Submit"),
              verbatimTextOutput("bidang_output_vector")
            )
          }
        }
      )
    })
  })

  titik_vector_2 <- reactiveVal(NULL)
  observeEvent(input$titik_submit_2, {
    vector_titik_2 <- as.numeric(strsplit(input$vec_titik_2, ",")[[1]])
    titik_vector_2(vector_titik_2)
    output$titik_output_vector_2 <- renderPrint({
      titik_vector_2()
    })

    output$hitung_distance <- renderUI(if(input$titik_submit_2>0) actionButton("distance_submit", "Hitung Jarak"))
  })

  bidang_vector_2 <- reactiveVal(NULL)
  observeEvent(input$bidang_submit_2, {
    vector_bidang_2 <- as.numeric(strsplit(input$vec_bidang_2, ",")[[1]])
    bidang_vector_2(vector_bidang_2)
    output$bidang_output_vector_2 <- renderPrint({
      bidang_vector_2()
    })

    output$hitung_distance <- renderUI(if(input$bidang_submit_2>0) actionButton("distance_submit", "Hitung Jarak"))
  })

  bidang_vector <- reactiveVal(NULL)
  observeEvent(input$bidang_submit, {
    vector_bidang <- as.numeric(strsplit(input$vec_bidang, ",")[[1]])
    bidang_vector(vector_bidang)
    output$bidang_output_vector <- renderPrint({
      bidang_vector()
    })

    output$hitung_distance <- renderUI(if(input$bidang_submit>0) actionButton("distance_submit", "Hitung Jarak"))
  })
  observeEvent(input$distance_submit, {
    if (input$dist_radio == "Jarak 2 titik") {
      if(input$distance_submit>0){
        output$distance <- renderPrint(
          distance_dua_titik(titik_vector_1(),titik_vector_2())
        )
      }
    } else if (input$dist_radio == "Jarak 2 bidang") {
      output$distance <- renderPrint(
        distance_parallel_plane(bidang_vector_1(),bidang_vector_2())
      )
    } else {
      output$distance <- renderPrint(
        distance_point_plane(titik_vector(),bidang_vector())
      )
    }
  })

  #### output untuk transformasi linear
  trans_vector <- reactiveVal(NULL)
  observeEvent(input$trans_submit, {
    vector_trans <- as.numeric(strsplit(input$trans_vec, ",")[[1]])
    trans_vector(vector_trans)
    output$trans_output_vector <- renderPrint({
      trans_vector()
    })

    output$radio_trans <- renderUI(radioButtons("trans_radio",
                                                label = "Pilih metode",
                                                choices = c("Refleksi", "Rotasi", "Proyeksi", "Kontraksi/Dilatasi"),
                                                selected = "Refleksi",
                                                inline = TRUE))

    output$input_info_trans <- renderUI({
      if (input$trans_radio == "Refleksi") {
        list(
          textInput("vec_refleksi", "Terhadap axis/bidang (x,y,z/xy,xz,yz)", ""),
          actionButton("refleksi_submit", "Submit"),
          verbatimTextOutput("refleksi_output_vector")
        )
      } else if (input$trans_radio == "Rotasi") {
        list(
          textInput("vec_rotasi_alfa", "Besar Sudut (diisi dengan angka)", ""),
          actionButton("rotasi_submit_alfa", "Submit"),
          verbatimTextOutput("rotasi_output_vector_alfa"),

          br(),

          textInput("vec_rotasi", "Terhadap axis/bidang (x,y,z/xy,xz,yz)", ""),
          actionButton("rotasi_submit", "Submit"),
          verbatimTextOutput("rotasi_output_vector")
        )
      } else if(input$trans_radio == "Proyeksi"){
        list(
          textInput("vec_proyeksi", "Terhadap axis/bidang (x,y,z/xy,xz,yz)", ""),
          actionButton("proyeksi_submit", "Submit"),
          verbatimTextOutput("proyeksi_output_vector")
        )
      } else {
        list(
          textInput("vec_kontraksi", "Sebesar (diisi dengan ukuran)", ""),
          actionButton("kontraksi_submit", "Submit"),
          verbatimTextOutput("kontraksi_output_vector")
        )
      }
    })
  })

  refleksi_vector <- reactiveVal(NULL)
  observeEvent(input$refleksi_submit, {
    vector_ref <- input$vec_refleksi
    refleksi_vector(vector_ref)
    output$refleksi_output_vector <- renderPrint({
      refleksi_vector()
    })
  })

  proyeks_vector <- reactiveVal(NULL)
  observeEvent(input$proyeksi_submit, {
    vector_pro <- input$vec_proyeksi
    proyeks_vector(vector_pro)
    output$proyeksi_output_vector <- renderPrint({
      proyeks_vector()
    })
  })

  kontraksi_vector <- reactiveVal(NULL)
  observeEvent(input$kontraksi_submit, {
    vector_kont <- as.numeric(input$vec_kontraksi)
    kontraksi_vector(vector_kont)
    output$kontraksi_output_vector <- renderPrint({
      kontraksi_vector()
    })
  })

  rotasi_vector_alfa <- reactiveVal(NULL)
  observeEvent(input$rotasi_submit_alfa, {
    vector_rot_alfa <- as.numeric(input$vec_rotasi_alfa)
    rotasi_vector_alfa(vector_rot_alfa)
    output$rotasi_output_vector_alfa <- renderPrint({
      rotasi_vector_alfa()
    })
  })

  rotasi_vector <- reactiveVal(NULL)
  observeEvent(input$rotasi_submit, {
    vector_rot <- input$vec_rotasi
    rotasi_vector(vector_rot)
    output$rotasi_output_vector <- renderPrint({
      rotasi_vector()
    })
  })


  output$plot <- renderPlotly({
    if (input$trans_radio == "Refleksi") {
      reflection_vector(trans_vector(),refleksi_vector())
    } else if (input$trans_radio == "Rotasi") {
      rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
    } else if(input$trans_radio == "Proyeksi"){
      projection_vector(trans_vector(),proyeks_vector())
    } else {
      contraction_dilatation(trans_vector(),kontraksi_vector())
    }
  })

  output$transformasi <- renderPrint(
    if (input$trans_radio == "Refleksi") {
      reflection_vector(trans_vector(),refleksi_vector())
    } else if (input$trans_radio == "Rotasi") {
      rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
    } else if(input$trans_radio == "Proyeksi"){
      projection_vector(trans_vector(),proyeks_vector())
    } else {
      contraction_dilatation(trans_vector(),kontraksi_vector())
    }
  )

}

shinyApp(ui, server)
