library(htmltools)

make_carousel <- function(id, items, alt, class_opts = '', ...) {
  
  if (missing(alt) || length(alt) < length(items)) {
    alt <- paste0('Slide ', seq_along(items))
  }

  indicators <- make_carousel_indicators(id, items)
  
  items <- make_carousel_items(id, items, alt)
  
  prev_button <- tags$button(
    class = 'carousel-control-prev',
    type = 'button',
    `data-bs-target` = paste0('#', id),
    `data-bs-slide` = 'prev',
    tags$span(class = 'carousel-control-prev-icon', `aria-hidden` = TRUE),
    tags$span(class = 'visually-hidden', 'Previous')
  )
  
  next_button <- tags$button(
    class = 'carousel-control-next',
    type = 'button',
    `data-bs-target` = paste0('#', id),
    `data-bs-slide` = 'next',
    tags$span(class = 'carousel-control-next-icon', `aria-hidden` = TRUE),
    tags$span(class = 'visually-hidden', 'Next')
  )
  
  div(
    id = id, 
    class = paste('carousel slide', class_opts), 
    ...,
    `data-bs-ride` = 'carousel',
    indicators,
    items,
    prev_button,
    next_button
  )
}


make_carousel_indicators <- function(id, items) {
  btns <- lapply(seq_along(items), function(i) {
    if (i == 1) {
      tags$button(
        type = 'button',
        `data-bs-target` = paste0('#', id),
        `data-bs-slide-to` = 0,
        class = 'active',
        `aria-current` = TRUE,
        `aria-label` = 'Slide 1'
      )
    } else {
      tags$button(
        type = 'button',
        `data-bs-target` = paste0('#', id),
        `data-bs-slide-to` = i - 1L,
        `aria-label` = paste0('Slide ', i)
      )
    }
  })
  
  div(
    class = 'carousel-indicators',
    tagList(btns)
  )
}

make_carousel_items <- function(id, items, alt) {
  items <- lapply(seq_along(items), function(i) {
    if (i == 1) {
      tags$div(
        class = 'carousel-item active',
        tags$img(
          src = items[i],
          class = 'd-block w-100',
          alt = alt[i]
        )
      )
    } else {
      tags$div(
        class = 'carousel-item',
        tags$img(
          src = items[i],
          class = 'd-block w-100',
          alt = alt[i]
        )
      )
    }
  })
  
  div(
    class = 'carousel',
    tagList(items)
  )
}