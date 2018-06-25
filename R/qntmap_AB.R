#' Find AB
#' @importFrom dplyr mutate
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom purrr map 
#' @importFrom stats setNames
#' @importFrom tidyr nest 
#' @importFrom tidyr sperad
#' @importFrom tidyr unnest
#' @param A A
#' @param B B
#' @param stg stg
#'  
qntmap_AB <- function(A, B, stg) {pipeline({
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
                se = sqrt((b * .A$a_se) ^ 2 + (.A$a * b_se) ^ 2),
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
#' @inheritParams qntmap_AB
#' @param AB AB
#' @param fix csv file indicating composition of the phases. NULL returns input AB
#' @param X membership
#' @fine_th threshold of X
#' @importFrom data.table fread
#' @importFrom purrr map
#' @importFrom purrr map2_dbl
#' @importFrom matrixStats weightedMedian

qntmap_AB_fix <- function(AB, fix = NULL, X, fine_th = .90) {
  
  if(is.null(fix)) return(AB)

  AB_fix <- pipeline({
    fix
      fread
      select(which(names(.) %in% c('phase', names(AB))))
      filter(phase %in% names(AB[[1]]$val))
      mutate(w = unclass(X * (X > fine_th))[phase])
      gather(elm, wt, -phase, -w, -stg)
      mutate(
        i = pipeline({
          elm
            map(function(e) qltmap[[e]])
            map(unlist, use.names = FALSE)
            map2_dbl(w, weightedMedian, na.rm = TRUE)
        }),
        w = NULL,
        val = wt / i,
        wt = NULL,
        i = NULL
      )
      nest(-elm, -phase)
      mutate(data = setNames(data, phase), phase = NULL)
      nest(-elm)
      mutate(data = setNames(data, elm), elm = NULL)
      `[[`('data')
      map(`[[`, 'data')
      map(map, unname)
      map(unlist)
  })
  
  for(e in names(AB_fix)) {
    for(p in names(AB_fix[[e]])){
      AB[[e]][['val']][[p]][] <- AB_fix[[e]][[p]]
      AB[[e]][['se']][[p]][] <- 0
    }
  }
  
  AB
}

