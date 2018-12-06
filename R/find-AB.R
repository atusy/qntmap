#' Find AB
#' @importFrom dplyr mutate right_join select
#' @importFrom purrr map 
#' @importFrom stats setNames
#' @importFrom tidyr nest spread unnest
#' @importFrom tibble tibble
#' @param A A
#' @param B B
#' @param stg stg
#' @noRd
find_AB <- function(A, B, stg) {pipeline({
  A  #AB
    select(phase3, elm, a, a_se) 
    nest(-phase3, .key = '.A') 
    mutate(
      .A = 
        map(
          .A,
          function(.A)
            mutate(
              B, 
              .B = map(
                .B,
                mutate,
                val = b * .A$a,
                se = L2(b * .A$a_se, .A$a * b_se),
                b = NULL,
                b_se = NULL
              )
            )
        ) 
    )  
    unnest 
    unnest  
    nest(-elm) 
    mutate(
      val = pipeline({
          data 
            map(select, -se) 
            map(spread, phase3, val)
        }),
      se =  pipeline({
          data 
            map(select, -val) 
            map(spread, phase3, se)
        }),
      data = NULL
    )  
    nest(-elm) 
    mutate(data = setNames(data, elm)) 
    `[[`('data') 
    map(unlist, recursive = FALSE)  
    map(map, right_join, tibble(stg), by = 'stg') 
    map(map, select, -stg) 
})}

#' fix AB value when composition of certain phases are constant
#' @inheritParams find_AB
#' @param AB A list of parameters alpha and beta
#' @param fix 
#' Csv file indicating composition of the phases. `NULL`` returns input AB.
#' @param X Membership degrees.
#' @param fine_th A threshold of X
#' @importFrom purrr map map2_dbl
#' @importFrom matrixStats weightedMedian
#' @noRd
find_AB_fix <- function(AB, fix = NULL, X, fine_th = .90, xmap) {
  
  if(is.null(fix)) return(AB)

  AB_fix <- pipeline({
    fix
      fread
      select(which(names(.) %in% c('phase', names(AB))))
      filter(phase %in% names(AB[[1]]$val))
      mutate(w = unclass(X * (X > fine_th))[phase])
      gather(elm, wt, -phase, -w)
      mutate(
        i = pipeline({
          xmap[str_replace(elm, '[0-9]*O[0-9]*', '')]
            map(unlist, use.names = FALSE)
            map2_dbl(w, weightedMedian, na.rm = TRUE)
        }),
        val = wt / i,
        w = NULL, wt = NULL, i = NULL
      )
      nest(-elm, -phase)
      mutate(data = setNames(data, phase), phase = NULL)
      nest(-elm)
      mutate(data = setNames(data, elm), elm = NULL)
      `[[`('data')
      map(`[[`, 'data')
      map(map, unlist, use.names = FALSE)
      map(unlist)
  }) 
  
  for(e in names(AB_fix)) {
    for(p in names(AB_fix[[e]])){
      if(is.finite(AB_fix[[e]][[p]])) {
        AB[[e]][['val']][[p]][] <- AB_fix[[e]][[p]]
        AB[[e]][['se']][[p]][] <- 0
      }
    }
  }
  
  AB
}
