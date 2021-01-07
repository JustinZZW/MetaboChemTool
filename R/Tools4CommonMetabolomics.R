################################################################################
# Adduct mz calculation --------------------------------------------------------

#' @title calculateExactMass
#' @author Zhiwei Zhou
#' @description calculate exact mass
#' @param formula chemical formula
#' @export
#' @examples
#' calculateExactMass(formula = "C2H5OH")


# calculateExactMass(formula = "C2H5OH")
setGeneric(name = 'calculateExactMass',
           def = function(
             formula
           ){
             molecule <- Rdisop::getMolecule(formula)
             # getFormula(molecule)
             Rdisop::getMass(molecule)
           })


#' @title calculateMz
#' @author Zhiwei Zhou
#' @description calculate adduct mz with exact mass
#' @param exact_mass
#' @param adduct
#' @param delta_mz
#' @export
#' @examples

# exact_mass <- 180.0634
# adduct <- '[M-H]-'
# delta_mz <- -1.0073
# calculateMz(exact_mass = 180.0634,
#             adduct = '[M-H]-',
#             delta_mz = -1.0073)
#
# calculateMz(exact_mass = 180.0634,
#             adduct = '[2M-H]-',
#             delta_mz = -1.0073)

setGeneric(name = 'calculateMz',
           def = function(
             exact_mass,
             adduct,
             delta_mz
           ){

             if (stringr::str_detect(adduct, pattern = '2M')) {
               mz <- exact_mass*2 + delta_mz
             } else if (stringr::str_detect(adduct, pattern = '3M')) {
               mz <- exact_mass*3 + delta_mz
             } else {
               mz <- exact_mass + delta_mz
             }

             mz
           })


# exact_mass <- 180.0634
# adduct <- '[M-H]-'
# delta_mz <- -1.0073
# transformMz(exact_mass = 180.0634, type = 'adduct', polarity = 'positive')
# transformMz(exact_mass = 180.0634, type = 'nl', polarity = 'positive')
# transformMz(exact_mass = 180.0634, adduct_list = c('[M-H]-', '[M+H]+'))

setGeneric(name = 'transformMz',
           function(
             exact_mass,
             formula = NULL,
             adduct_list = NULL,
             type = c('adduct', 'nl'),
             polarity = c('positive', 'negative'),
             # lib_adduct_nl = lib_adduct_nl,
             ...
           ){

             if (all(is.null(exact_mass), is.null(formula))) {
               stop('Please input exact_mass or formula.')
             }

             if (!is.null(formula)) {
               exact_mass <- calculateExactMass(formula)
             }

             if (is.null(adduct_list)) {
               lib <- switch(polarity,
                             'positive' = {
                               lib_adduct_nl$positive
                             },
                             'negative' = {
                               lib_adduct_nl$negative
                             }
               )

               lib <- switch(type,
                             'adduct' = {
                               lib %>%
                                 dplyr::filter(type == 'Adduct') %>%
                                 dplyr::filter(credential == 'Yes')
                             },
                             'nl' = {
                               lib %>%
                                 dplyr::filter(type == 'NeutralLoss') %>%
                                 dplyr::filter(credential == 'Yes')
                             })
             } else {
               lib <- lib_adduct_nl$positive %>%
                 dplyr::bind_rows(lib_adduct_nl$negative)

               if (!all(adduct_list %in% lib$adduct)) {
                 stop('Sorry, not all adduct included in the adduct list\n')
               }

               lib <- lib %>%
                 dplyr::filter(adduct %in% adduct_list) %>%
                 dplyr::arrange(match(adduct, adduct_list))
             }


             result_mz <- sapply(seq_along(lib$adduct), function(i){
               calculateMz(exact_mass = exact_mass,
                           adduct = lib$adduct[i],
                           delta_mz = lib$delta_mz[i])
             })

             result <- tibble::tibble(exact_mass = exact_mass,
                                      adduct = lib$adduct,
                                      mz = result_mz)


             return(result)

           })




# convertMz2Adduct(base_mz = 181.0707,
#                  base_adduct = '[M+H]+',
#                  adduct_list = NULL,
#                  type = 'adduct',
#                  polarity = 'positive')

# convertMz2Adduct(base_mz = 181.0707,
#                  base_adduct = '[M+H]+',
#                  adduct_list = NULL,
#                  type = 'nl',
#                  polarity = 'negative')

# convertMz2Adduct(base_mz = 181.0707,
#                  base_adduct = '[M+H]+',
#                  adduct_list = c('[M-H2O+H]+', '[M+Na]+'),
#                  type = 'nl',
#                  polarity = 'negative')

# convertMz2Adduct(base_mz = 143.0347,
#                  base_adduct = '[2M-H]-',
#                  adduct_list = c('[M-H2O+H]+', '[M+Na]+'))

# convertMz2Adduct(base_mz = 143.0347,
#                  base_adduct = '[3M-H]-',
#                  adduct_list = c('[M-H2O+H]+', '[M+Na]+'))

setGeneric(name = 'convertMz2Adduct',
           def = function(
             base_mz,
             base_adduct,
             # lib_adduct_nl = lib_adduct_nl,
             ...
             # adduct_list = NULL,
             # type = c('adduct', 'nl'),
             # polarity = c('positive', 'negative')
           ){

             lib <- lib_adduct_nl$positive %>%
               dplyr::bind_rows(lib_adduct_nl$negative)

             if (!(base_adduct %in% lib$adduct)) {
               stop('Sorry, base_adduct is not included\n')
             }

             temp_delta_mz <- lib %>%
               dplyr::filter(adduct == base_adduct) %>%
               dplyr::pull(delta_mz)


             if (stringr::str_detect(base_adduct, pattern = '2M')) {
               temp_exact_mass <- (base_mz - temp_delta_mz)/2
             } else if (stringr::str_detect(base_adduct, pattern = '3M')) {
               temp_exact_mass <- (base_mz - temp_delta_mz)/3
             } else {
               temp_exact_mass <- base_mz - temp_delta_mz
             }

             # temp_exact_mass <- base_mz - temp_delta_mz

             result <- transformMz(exact_mass = temp_exact_mass,
                                   ...)

             return(result)

           })
