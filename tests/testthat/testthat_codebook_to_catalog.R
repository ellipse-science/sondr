library(testthat)

test_that("codebook_to_catalog generates correct Markdown", {
   # Set up test data
   data <- data.frame(question = c("What is your favorite color?", "What is your name?"),
                      answers = c("Blue;Green;Red", "Alice;Bob"))
   filename <- tempfile()  # Create a temporary file
   title <- "My Codebook" 

   # Call your function
   codebook_to_catalog(data, filename, title)

   # Expectations (what you expect the file to contain)
   expected_content <- "# My Codebook\n\n1. What is your favorite color?\n     - Blue\n     - Green\n     - Red\n\n2. What is your name?\n     - Alice\n     - Bob\n"

   # Read the actual file content 
   file_content <- readLines(filename)

   # Assertion: Check if the content matches your expectation
   expect_equal(file_content, expected_content)
})
