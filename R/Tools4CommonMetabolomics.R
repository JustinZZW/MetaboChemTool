################################################################################
# Adduct mz calculation --------------------------------------------------------
#   calculateExactMass ---------------------------------------------------------
#' @title calculateExactMass
#' @author Zhiwei Zhou
#' @description calculate exact mass from formula
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


#   calculateMz ----------------------------------------------------------------
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


#   transformMz ----------------------------------------------------------------
#' @title transformMz
#' @author Zhiwei Zhou
#' @param exact_mass numeric
#' @param formula Default: NULL
#' @param adduct_list Default: NULL
#' @param type 'adduct' or 'nl'.
#' @param polarity 'positive' or 'negative'
#' @export
#' @examples
#' transformMz(exact_mass = 180.0634, type = 'adduct', polarity = 'positive')
#' transformMz(exact_mass = 180.0634, type = 'nl', polarity = 'positive')
#' transformMz(exact_mass = 180.0634, adduct_list = c('[M-H]-', '[M+H]+'))

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




#' @title convertMz2Adduct
#' @author Zhiwei ZHou
#' @description mz conversion between different adduct forms
#' @param base_mz numeric
#' @param base_adduct character
#' @param ... Other arguments passed on to [transformMz()]
#' @export
#' @examples
#' convertMz2Adduct(base_mz = 181.0707,
#'                  base_adduct = '[M+H]+',
#'                  adduct_list = NULL,
#'                  type = 'adduct',
#'                  polarity = 'positive')
#'
#' convertMz2Adduct(base_mz = 181.0707,
#'                  base_adduct = '[M+H]+',
#'                  adduct_list = c('[M-H2O+H]+', '[M+Na]+'),
#'                  type = 'nl',
#'                  polarity = 'negative')
#'
#' convertMz2Adduct(base_mz = 143.0347,
#'                  base_adduct = '[3M-H]-',
#'                  adduct_list = c('[M-H2O+H]+', '[M+Na]+'))


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



#   generateFormulaMassFromSmiles ----------------------------------------------
#' @title generateFormulaMassFromSmiles
#' @author Zhiwei Zhou
#' @description generate formula and mass from smiles
#' @param smiles
#' @return a data.frame with exact_mass and formula
#' @export
#' @examples
#' test <- generateFormulaMassFromSmiles('CN1C=NC(C[C@H](N)C(O)=O)=C1')


# generateFormulaMassFromSmiles('CN1C=NC(C[C@H](N)C(O)=O)=C1')

setGeneric('generateFormulaMassFromSmiles',
           def = function(smiles) {
             if (is.na(smiles) | is.null(smiles)) {
               formula_result <- data.frame(exact_mass=NA,
                                            formula=NA,
                                            stringsAsFactors = F)

               return(formula_result)
             }

             molecule <- try(rcdk::parse.smiles(smiles)[[1]], silent = TRUE)

             if (class(molecule) == 'try-error') {
               formula_result <- data.frame(exact_mass=NA,
                                            formula=NA,
                                            stringsAsFactors = F)

               return(formula_result)
             }

             rcdk::convert.implicit.to.explicit(molecule)
             formula <- rcdk::get.mol2formula(molecule, charge=0)
             formula_result <- data.frame(exact_mass=formula@mass,
                                          formula=formula@string,
                                          stringsAsFactors = F)

             return(formula_result)
           }
)




################################################################################
# structure format conversion --------------------------------------------------
#   convertStructureFormat -----------------------------------------------------

#' @title convertStructureFormat
#' @author Zhiwei Zhou
#' @description R wrapper for OpebBabel
#' @param file_input
#' @param file_output
#' @param format_input See \link{https://open-babel.readthedocs.io/en/latest/FileFormats/Overview.html#file-formats}
#' @param format_output See \link{https://open-babel.readthedocs.io/en/latest/FileFormats/Overview.html#file-formats}
#' @param options
#' @export
#' @example
#' file_input <- 'I:/00_projects/03_MetDNA2/00_data/20200706_KEGG_settle_and_remove_redunancy/test.smiles'
#' file_output <- 'I:/00_projects/03_MetDNA2/00_data/20200706_KEGG_settle_and_remove_redunancy/test_output.inchikey'
#' format_input <- 'smiles'
#' format_output <- 'inchikey'


# file_input <- 'I:/00_projects/03_MetDNA2/00_data/20200706_KEGG_settle_and_remove_redunancy/test.smiles'
# file_output <- 'I:/00_projects/03_MetDNA2/00_data/20200706_KEGG_settle_and_remove_redunancy/test_output.inchikey'
# format_input <- 'smiles'
# format_output <- 'inchikey'
# convertStructureFormat(file_input = file_input,
#                        file_output = file_output)

setGeneric(name = 'convertStructureFormat',
           def = function(
             file_input,
             file_output,
             format_input,
             format_output,
             options = ''
           ){
             command <- paste0('obabel ',
                               '-i',
                               format_input,
                               ' ',
                               file_input,
                               ' -o',
                               format_output,
                               ' -O',
                               file_output)

             if (nchar(options) != 0) {
               paste0(command, ' ', options)
             }

             system(command = command)

           }
)


################################################################################
# formula functions ------------------------------------------------------------

#' @title limitElements
#' @author Zhiwei Zhou
#' @param cpd_formula
#' @param element_necessary Necessary elements. Default: 'C'
#' @param element_included Limit elements. Default: c("C", 'H', 'N', 'O', 'P', 'S', 'F', 'Cl', 'Br', 'I')
#' @return Logistical value. TRUE or FALSE
#' @export
#' @example
#' cpd_formula <- 'H2O'
#' limitElements(cpd_formula)
#' cpd_formula <- 'C6H12O6'
#' limitElements(cpd_formula)
#' cpd_formula <- 'C55H68MgN4O6'
#' limitElements(cpd_formula)

# cpd_formula <- 'H2O'
# limitElements(cpd_formula)
# cpd_formula <- 'C6H12O6'
# limitElements(cpd_formula)
# cpd_formula <- 'C55H68MgN4O6'
# limitElements(cpd_formula)
# cpd_formula <- 'C53H60ClN3O16'
# limitElements(cpd_formula)


setGeneric(name = 'limitElements',
           def = function(cpd_formula,
                          element_necessary = 'C',
                          element_included = c("C", 'H', 'N', 'O', 'P', 'S', 'F', 'Cl', 'Br', 'I')){
             if (missing(cpd_formula)) {
               stop('Please input cpd_formula')
             }

             element_composition <- try(CHNOSZ::makeup(formula = cpd_formula),
                                        silent = TRUE)



             element_all <- names(element_composition)

             if (!(element_necessary %in% element_all)) {
               return(FALSE)
             }

             if (all(element_all %in% element_included)) {
               return(TRUE)
             } else {
               return(FALSE)
             }

           })


################################################################################
# startup message --------------------------------------------------------------
.onAttach <- function(libname, pkgname){
  packageStartupMessage("
If you have any questions, please send email to zhiwei92@126.com.
Authors: Zhiwei Zhou.
Maintainer: Zhiwei Zhou
Version 0.0.1.03 (20210519)
--------------
* Add module: Tools4PublicDB
* Add functions: extractPropertyFromHMDB(), extractPropertyFromPubChem(), convertStructureFormat()
* Add manual for: calculateMz, transformMz, generateFormulaMassFromSmiles
* Add function name: generateFormula ---> generateFormulaMassFromSmiles
* Add functions: limitElements()
* Add MetDNA adducts
* Add functions: copyMetDNA2
")
}
