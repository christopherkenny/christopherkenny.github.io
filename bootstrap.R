# built to follow https://getbootstrap.com/docs/5.1/components/carousel/
make_carousel <- function(id, items, alt) {
  
  if (missing(alt) || length(alt) < length(items)) {
    alt <- paste0('Slide ', seq_along(items))
  }
  
  out <- paste0('<div id="', id, '" class="carousel slide" data-bs-ride="carousel">')
  
  out <- c(out, make_carousel_indicators(id, items))

  out <- c(out, make_carousel_items(id, items, alt))

  out <- c(out, 
    paste0('  <button class="carousel-control-prev" type="button" data-bs-target="#', id, '" data-bs-slide="prev">'),
    '    <span class="carousel-control-prev-icon" aria-hidden="true"></span>',
    '    <span class="visually-hidden">Previous</span>',
    '  </button>',
    paste0('  <button class="carousel-control-next" type="button" data-bs-target="#', id, '" data-bs-slide="next">'),
    '    <span class="carousel-control-next-icon" aria-hidden="true"></span>',
    '    <span class="visually-hidden">Next</span>',
    '  </button>',
    '</div>'
  )
  
  cat(out, sep = '\n')
}


make_carousel_items <- function(id, items, alt) {
  out <- '  <div class="carousel-inner">'
  
  for (i in seq_along(items)) {
    if (i == 1) {
      out <- c(out, paste0('    <div class="carousel-item active">'))
    } else {
      out <- c(out, '    <div class="carousel-item">')
    }
    out <- c(out, paste0('      <img src="', items[i], '" class="d-block w-100" alt="', alt[i], '">'))
    out <- c(out, '    </div>')
  }
  
  out <- c(out, '  </div>')
  out
}

make_carousel_indicators <- function(id, items) {
  out <- '  <div class="carousel-indicators">'
  
  for (i in seq_along(items)) {
    if (i == 1) {
      out <- c(out, paste0('    <button type="button" data-bs-target="#', id, '" data-bs-slide-to="0" class="active" aria-current="true" aria-label="Slide 1"></button>'))
    } else {
      out <- c(out, paste0('    <button type="button" data-bs-target="#', id, '" data-bs-slide-to="', i - 1L, '" aria-label="Slide ', i,'"></button>'))
    }
  }
  
  out <- c(out, '  </div>')
  
  out
}
