library(htmltools)

make_carousel <- function(id, items, alt, titles, captions, class_opts = '', ...) {
  
  if (missing(alt) || length(alt) < length(items)) {
    alt <- paste0('Slide ', seq_along(items))
  }
  if (missing(titles) || length(titles) < length(items)) {
    titles <- rep(NA_character_, length(items))
  }
  if (missing(captions) || length(captions) < length(items)) {
    captions <- rep(NA_character_, length(items))
  }

  indicators <- make_carousel_indicators(id, items)
  
  items <- make_carousel_items(id, items, alt, titles, captions)
  
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

make_carousel_items <- function(id, items, alt, captions, titles) {
  items <- lapply(seq_along(items), function(i) {

    if (i == 1) {
      out_div <- tags$div(
        class = 'carousel-item active',
        tags$img(
          src = items[i],
          class = 'd-block w-100',
          alt = alt[i],
          style = 'object-fit:contain; width: 250px; height: 250px;'
        )
      )
    } else {
      out_div <- tags$div(
        class = 'carousel-item',
        tags$img(
          src = items[i],
          class = 'd-block w-100',
          alt = alt[i],
          style = 'object-fit:contain; width: 250px; height: 250px;'
        )
      )
    }
    if (!is.na(captions[i]) || !is.na(titles[i])) {
      out_div <- tagAppendChild(out_div, tags$div(
        class = 'carousel-caption d-none d-md-block',
        h5(if (is.na(titles[i])) NULL else titles[i]),
        p(if (is.na(captions[i])) NULL else captions[i])
      ))
    }
    
    out_div
  })
  
  div(
    class = 'carousel-inner',
    tagList(items)
  )
}