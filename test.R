library(htmltools)

# Define the source URL for the iframe
iframe_url <- "https://www.example.com"

# Create the iframe tag
iframe_tag <- tags$iframe(src = iframe_url, frameborder = "0", width = "500", height = "300")

# Create an HTML document and include the iframe tag
html_document <- tags$html(
  tags$head(tags$title("IFrame Example")),
  tags$body(
    tags$h1("IFrame Example"),
    iframe_tag
  )
)

# Save the HTML document to a file
html_file <- "iframe_example.html"
writeLines(as.character(html_document), html_file)
