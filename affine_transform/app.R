default_image_url <-
  # "https://cdn.pixabay.com/photo/2017/07/23/15/01/border-collie-2531633_960_720.jpg"
  "https://cdn.pixabay.com/photo/2018/01/26/16/16/winter-3109041_960_720.jpg"

# Minimal example of Shiny widget using 'magick' images
ui <- fluidPage(
  titlePanel("Magick Shiny Demo"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("base", "Upload base image", accept = c('image/png', 'image/jpeg')),
      fileInput("trans", "Upload transformed image", accept = c('image/png', 'image/jpeg')),
      
      sliderInput("rotation", "Rotation", 0, min = -45, max = 45),
      sliderInput("dx", "x move", 0, min = -500, max = 500),
      sliderInput("dy", "y move", 0, min = -500, max = 500)
      
    ),
    mainPanel(
      paste0("Photo by pixabay (", default_image_url, ")"),
      imageOutput("img")
    )
  )
)

server <- function(input, output, session) {
  
  library(magick)
  library(EBImage)
  
  
  # Start with placeholder img
  image_base <- EBImage::readImage(default_image_url)@.Data[,,1]
  image_trans <- image_base
  
  # When uploading new image
  observeEvent(input$base, {
    if (length(input$base$datapath))
      image_base <<- EBImage::readImage(input$base$datapath)
  })

  observeEvent(input$trans, {
    if (length(input$trans$datapath))
      image_trans <<-  EBImage::readImage(input$base$datapath)
  })
  
  # A plot of fixed size
  output$img <- renderImage({
    
    rot <- as.numeric(input$rotation) * pi / 180
    
    affine_matrix <- 
      matrix(c(cos(rot), sin(rot), input$dx, 
               -sin(rot), cos(rot), -input$dy),
             nrow = 3, ncol = 2)
    
    image_afn <- EBImage::affine(image_trans, affine_matrix)
    
    image_match <- EBImage::rgbImage(red = image_afn@.Data, blue = image_base@.Data, green = image_base@.Data*0)

    # Numeric operators
    tmpfile <-
      EBImage::writeImage(image_match, files = paste0(tempfile(fileext='jpg'), ".jpg"))
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })
}

shinyApp(ui, server)

### local test

# image_base <- EBImage::readImage("530_2018-04-12-142200-0042.jpg")
# image_trans <-  EBImage::readImage("570_2018-04-12-142201-0041.jpg")
# 
# rot <- 5 * pi / 180
# dx = 10
# dy = 20
# 
#   affine_matrix <-
#     matrix(c(cos(rot), sin(rot), dx,
#              -sin(rot), cos(rot), -dy),
#            nrow = 3, ncol = 2)
# 
#   image_afn <- EBImage::affine(image_trans, affine_matrix)
# 
#   image_match <- EBImage::rgbImage(red = image_afn@.Data, blue = image_base@.Data, green = image_base@.Data*0)
# 
#   tmpfile <-
#     EBImage::writeImage(image_match, files = paste0(tempfile(fileext='jpg'), ".jpg"))
#   
#   # Return a list
#   list(src = tmpfile, contentType = "image/jpeg")
#   