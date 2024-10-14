# Load required libraries
library(shiny)
library(qrcode)
library(png)
library(base64enc)

# Define UI for the application
ui <- fluidPage(
  # Add CSS to change the background color of the page
  tags$style(HTML("
    body {
      background-color: #faf6fd;  
    } 
    .well {
      background-color: #e1dde3;  
    }
  ")),
  titlePanel("Free QR Code Generator"),
  h4("(c) Dev Paudel, dpaudel@outlook.com"),
  sidebarLayout(
    sidebarPanel(
      textInput("url", "Enter Website URL:", value = "https://www.roteping.com"),
      actionButton("generate", "Generate QR Code"),
      br(),
      br(),
      downloadButton("downloadQR", "Download QR Code")
    ),
    mainPanel(
      plotOutput("qrImage", width = "300px", height = "300px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to generate QR code when button is pressed
  qr_code_data <- eventReactive(input$generate, {
    req(input$url)  # Ensure input URL is available 
    # Generate QR code
    qr_code(input$url)
  })
  
  # Render the QR code image in the UI
  output$qrImage <- renderPlot({
    req(qr_code_data())
    plot(qr_code_data())
  })
  
  # Function to extract domain name from URL
  extract_domain <- function(url) {
    # Remove protocol if present
    domain <- sub("^https?://", "", url)
    # Remove www. if present
    domain <- sub("^www\\.", "", domain)
    # Extract the domain (everything before the first slash or end of string)
    domain <- sub("/.*$", "", domain)
    # Extract the part before the last dot (if present)
    domain <- sub("\\.[^.]*$", "", domain)
    return(domain)
  }
  
  # Download handler for the QR code
  output$downloadQR <- downloadHandler(
    filename = function() {
      req(input$url)
      domain <- extract_domain(input$url)
      paste0("qrcode-", format(Sys.Date(), "%Y%m%d"), "-", domain, ".png")
    },
    content = function(file) {
      req(qr_code_data())
      png(file, width = 300, height = 300)
      plot(qr_code_data())
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
