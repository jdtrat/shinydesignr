
#' Get image text size in pixels for preview image
#'
#' @param input
#'
#' @return
#' @export
#'
#' @examples
get_image_text_size <- function(input) {

  switch(input$text_class,
         h1 = "32",
         h2 = "24",
         h3 = "18.72",
         h4 = "16",
         h5 = "13.28",
         h6 = "10.72",
         p = "16")
}

#' Update Shiny app preview image
#'
#' @param existing_preview
#' @param input
#'
#' @return
#' @export
#'
#' @examples
update_preview <- function(existing_preview, input) {

  new_image <- magick::image_blank(width = (input$image_brush$xmax - input$image_brush$xmin),
                                   height = (input$image_brush$ymax - input$image_brush$ymin),
                                   color = "#d4ebf2") %>%
    magick::image_border(color = "blue",
                         geometry = "2x2")

  if (input$ui_type == "Image") {
    new_image <- new_image %>%
      magick::image_annotate(text = input$text_value,
                             gravity = input$class_location,
                             size = get_image_text_size(input = input))
  }

  updated_image <- magick::image_composite(existing_preview,
                                           new_image,
                                           offset =  paste0("+", input$image_brush$xmin, "+", input$image_brush$ymin),
                                           operator = "SrcOver")

  return(updated_image)

}
