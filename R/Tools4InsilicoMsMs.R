################################################################################
# General ----------------------------------------------------------------------


#' @title convertAdduct4Tools
#' @description convert adduct types between different in-silico annotation tools
#' @author Zhiwei Zhou
#' @param adduct adduct type
#' @param tool_from annotation tool name, including: 'metdna2', 'msfinder', 'sirius', 'metdna', 'metdna1_hilic', 'metdna1_rp'. Default: 'metdna2'
#' @param tool_to annotation tool name, including:  'metdna2', 'msfinder', 'sirius', 'metdna', 'metdna1_hilic', 'metdna1_rp'. Default: 'metdna2'
#' @export
#' @examples
#' # single adduct
#' convertAdduct4Tools(adduct = '[M+H]+', tool_from = 'metdna2', tool_to = 'sirius')
#' # multiple adducts
#' convertAdduct4Tools(adduct = c('[M+H]+', '[M-H+2Na]+'), tool_from = 'metdna2', tool_to = 'msfinder')

# convertAdduct4Tools(adduct = '[M+H]+', tool_from = 'metdna2', tool_to = 'sirius')
# convertAdduct4Tools(adduct = c('[M+H]+', '[M-H+2Na]+'), tool_from = 'metdna2', tool_to = 'msfinder')

setGeneric(name = 'convertAdduct4Tools',
           def = function(
             adduct,
             tool_from = c('metdna2', 'msfinder', 'sirius', 'metdna', 'metdna1_hilic', 'metdna1_rp'),
             tool_to = c('metdna2', 'msfinder', 'sirius', 'metdna', 'metdna1_hilic', 'metdna1_rp')
           ){
             tool_from <- match.arg(tool_from)
             tool_to <- match.arg(tool_to)

             # data('lib_adduct_conv', envir = environment())

             switch (tool_from,
                     'metdna2' = {list_adduct_from <- lib_adduct_conv$MetDNA2},
                     'msfinder' = {list_adduct_from <- lib_adduct_conv$MSFINDER},
                     'sirius' = {list_adduct_from <- lib_adduct_conv$SIRIUS},
                     'metdna' = {list_adduct_from <- lib_adduct_conv$MetDNA},
                     'metdna1_hilic' = {list_adduct_from <- lib_adduct_conv$MetDNA1_hilic},
                     'metdna1_rp' = {list_adduct_from <- lib_adduct_conv$MetDNA1_rp}
             )

             switch (tool_to,
                     'metdna2' = {list_adduct_to <- lib_adduct_conv$MetDNA2},
                     'msfinder' = {list_adduct_to <- lib_adduct_conv$MSFINDER},
                     'sirius' = {list_adduct_to <- lib_adduct_conv$SIRIUS},
                     'metdna' = {list_adduct_to <- lib_adduct_conv$MetDNA},
                     'metdna1_hilic' = {list_adduct_to <- lib_adduct_conv$MetDNA1_hilic},
                     'metdna1_rp' = {list_adduct_to <- lib_adduct_conv$MetDNA1_rp}
             )

             idx <- match(adduct, list_adduct_from)

             if (any(is.na(idx))) {
               warning('Note: some adduct type not included in adduct_conv\n')
             }

             adduct_from <- list_adduct_from[idx]
             adduct_to <- list_adduct_to[idx]

             result <- tibble::tibble(adduct_input = adduct,
                                      adduct_from = adduct_from,
                                      adduct_to = adduct_to)

             return(result)
           })





################################################################################
# MSFINDER ---------------------------------------------------------------------
#    convertAdduct4MsFinder ----------------------------------------------------

#' @title convertAdduct4MsFinder
#' @author Zhiwei Zhou
#' @description convert adduct type to fit MS-FINDER (Default: MSFINDER V3.24)
#' @param adduct adduct type need to convert. e.g., '[M+H]+', '[M-H+2Na]+'
#' @param source 'MetDNA2'
#' @export
#' @examples
#' convertAdduct4MsFinder(adduct = '[M-H+2Na]+')


setGeneric(name = 'convertAdduct4MsFinder',
           def = function(adduct,
                          source = 'MetDNA2'){

             if (source == 'MetDNA2') {
               result <- switch (adduct,
                                 'M+' = '[M]+',
                                 '[M+H]+' = '[M+H]+',
                                 '[M+NH4]+' = '[M+NH4]+',
                                 '[M+Na]+' = '[M+Na]+',
                                 '[M-H+2Na]+' = '[M+2Na-H]+',
                                 '[M+K]+' = '[M+K]+',
                                 '[M-H+2K]+' = '[M+2K-H]+',
                                 '[2M+H]+' = '[2M+H]+',
                                 '[2M+NH4]+' = '[2M+NH4]+',
                                 '[2M+Na]+' = '[2M+Na]+',
                                 '[2M+K]+' = '[2M+K]+',
                                 '[M-H2O+H]+' = '[M+H-H2O]+',
                                 '[M-2H+3Na]+' = '[M+3Na-2H]+',
                                 '[M-2H+3K]+' = '[M+3K-2H]+',
                                 '[M+CH3CN+H]+' = '[M+ACN+H]+',
                                 '[M+CH3CN+Na]+' = '[M+ACN+Na]+',
                                 '[M+CH3COO+2H]+' = '[M+CH3COO+2H]+',
                                 '[M+HCOO+2H]+' = '[M+H+HCOOH]+',
                                 '[M+HCOO+H+K]+' = '[M+K+HCOOH]+',
                                 '[M+HCOO+H+Na]+' = '[M+Na+HCOOH]+',
                                 'M-' = '[M]-',
                                 '[M-H]-' = '[M-H]-',
                                 '[M+Na-2H]-' = '[M+Na-2H]-',
                                 '[M+K-2H]-' = '[M+K-2H]-',
                                 '[M+NH4-2H]-' = '[M+NH4-2H]-',
                                 '[2M-H]-' = '[2M-H]-',
                                 '[M+CH3COO]-' = '[M+CH3COO]-',
                                 '[2M+Na-2H]-' = '[2M+Na-2H]-',
                                 '[M-H2O-H]-' = '[M-H2O-H]-'
               )
             }

             return(result)

           })



################################################################################
# MSFINDER ---------------------------------------------------------------------
#    convertAdduct4Sirius ----------------------------------------------------

#' @title convertAdduct4Sirius
#' @author Zhiwei Zhou
#' @description convert adduct type to fit MS-FINDER (Default: MSFINDER V3.24)
#' @param adduct adduct type need to convert. e.g., '[M+H]+', '[M-H+2Na]+'
#' @param source 'MetDNA2'
#' @export
#' @examples
#' convertAdduct4MsFinder(adduct = '[M-H+2Na]+')

# convertAdduct4Sirius(adduct = '[M-H+2Na]+')

setGeneric(name = 'convertAdduct4Sirius',
           def = function(adduct,
                          source = 'MetDNA2'){

             if (source == 'MetDNA2') {
               result <- switch (adduct,
                                 'M+' = '[M]+',
                                 '[M+H]+' = '[M + H]+',
                                 '[M+NH4]+' = '[M + H3N + H]+',
                                 '[M+Na]+' = '[M + Na]+',
                                 '[M-H+2Na]+' = '[M - H + Na + Na]+',
                                 '[M+K]+' = '[M + K]+',
                                 '[M-H+2K]+' = '[M - H + K + K]+',
                                 '[2M+H]+' = '[M + M + H]+',
                                 '[2M+NH4]+' = '[M + M + NH4]+',
                                 '[2M+Na]+' = '[M + M + Na]+',
                                 '[2M+K]+' = '[M + M + K]+',
                                 '[M-H2O+H]+' = '[M - H2O + H]+',
                                 '[M-2H+3Na]+' = '[M - H - H + Na + Na + Na]+',
                                 '[M-2H+3K]+' = '[M - H - H + K + K + K]+',
                                 '[M+CH3CN+H]+' = '[M + C2H3N + H]+',
                                 '[M+CH3CN+Na]+' = '[M + C2H3N + Na]+',
                                 '[M+CH3COO+2H]+' = '[M + C2H3O2 + H + H]+',
                                 '[M+HCOO+2H]+' = '[M + C2HO2 + H + H]+',
                                 '[M+HCOO+H+K]+' = '[M + C2HO2 + H + K]+',
                                 '[M+HCOO+H+Na]+' = '[M + C2HO2 + H + Na]+'
                                 # 'M-' = '[M]-',
                                 # '[M-H]-' = '[M-H]-',
                                 # '[M+Na-2H]-' = '[M+Na-2H]-',
                                 # '[M+K-2H]-' = '[M+K-2H]-',
                                 # '[M+NH4-2H]-' = '[M+NH4-2H]-',
                                 # '[2M-H]-' = '[2M-H]-',
                                 # '[M+CH3COO]-' = '[M+CH3COO]-',
                                 # '[2M+Na-2H]-' = '[2M+Na-2H]-',
                                 # '[M-H2O-H]-' = '[M-H2O-H]-'
               )
             }

             return(result)

           })

################################################################################
# MetDNA2 ----------------------------------------------------------------------

#' @title copyMetDNA2
#' @author Zhiwei Zhou
#' @param path_from '.'
#' @param path_to '.'
#' @param dir_ignore file names for ignore
#' @export
#' @examples
#' copyMetDNA2(path_from = 'Z:/memberdata/ZHOUzhiwei/01_data/20210517_nist_urine_extended_steps/dp_pos_step0',
#'             path_to = 'H:/00_projects/03_MetDNA2/00_data/20210519_mutiple_ext_step_nist_urine',
#'             dir_ignore = '02_formula_prediction')

# copyMetDNA2(path_from = 'Z:/memberdata/ZHOUzhiwei/01_data/20210517_nist_urine_extended_steps/dp_pos_step0',
#             path_to = 'H:/00_projects/03_MetDNA2/00_data/20210519_mutiple_ext_step_nist_urine',
#             dir_ignore = '02_formula_prediction')

setGeneric(name = 'copyMetDNA2',
           function(
             path_from = '.',
             path_to = '.',
             dir_ignore = NULL
           ){
             # 00 annotation table
             file_list <- list.files(file.path(path_from, '00_annotation_table'))
             if (length(dir_ignore) > 0) {
               file_list <- file_list[!(file_list %in% dir_ignore)]
             }

             if ('00_intermediate_data' %in% file_list) {
               dir.create(file.path(path_to, '00_annotation_table', '00_intermediate_data'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '00_annotation_table', '00_intermediate_data'),
                         to = file.path(path_to, '00_annotation_table'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }

             file_list <- file_list[!(file_list %in% c('00_intermediate_data'))]

             walk(file_list, function(x){
               file.copy(from = file.path(path_from, '00_annotation_table', x),
                         to = file.path(path_to, '00_annotation_table'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             })




             # 01 01 result initial seed annotation
             file_list <- list.files(file.path(path_from, '01_result_initial_seed_annotation'))
             if (length(dir_ignore) > 0) {
               file_list <- file_list[!(file_list %in% dir_ignore)]
             }

             if ('00_intermediate_data' %in% file_list) {
               dir.create(file.path(path_to, '01_result_initial_seed_annotation', '00_intermediate_data'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '01_result_initial_seed_annotation', '00_intermediate_data'),
                         to = file.path(path_to, '01_result_initial_seed_annotation'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }

             if ('01_rt_calibration_plot' %in% file_list) {
               dir.create(file.path(path_to, '01_result_initial_seed_annotation', '01_rt_calibration_plot'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '01_result_initial_seed_annotation', '01_rt_calibration_plot'),
                         to = file.path(path_to, '01_result_initial_seed_annotation'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }

             if ('02_experimental_ms2_spec_plot' %in% file_list) {
               dir.create(file.path(path_to, '01_result_initial_seed_annotation', '02_experimental_ms2_spec_plot'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '01_result_initial_seed_annotation', '02_experimental_ms2_spec_plot'),
                         to = file.path(path_to, '01_result_initial_seed_annotation'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }


             file_list <- file_list[!(file_list %in% c('00_intermediate_data',
                                                       '01_rt_calibration_plot',
                                                       '02_experimental_ms2_spec_plot'))]

             walk(file_list, function(x){
               file.copy(from = file.path(path_from, '01_result_initial_seed_annotation', x),
                         to = file.path(path_to, '01_result_initial_seed_annotation'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             })


             # 02 result MRN annotation --------------------------------
             file_list <- list.files(file.path(path_from, '02_result_MRN_annotation'))
             if (length(dir_ignore) > 0) {
               file_list <- file_list[!(file_list %in% dir_ignore)]
             }

             if ('00_intermediate_data' %in% file_list) {
               dir.create(file.path(path_to, '02_result_MRN_annotation', '00_intermediate_data'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '02_result_MRN_annotation', '00_intermediate_data'),
                         to = file.path(path_to, '02_result_MRN_annotation'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }

             if ('01_surrogate_ms2_spec_plot' %in% file_list) {
               dir.create(file.path(path_to, '02_result_MRN_annotation', '01_surrogate_ms2_spec_plot'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '02_result_MRN_annotation', '01_surrogate_ms2_spec_plot'),
                         to = file.path(path_to, '02_result_MRN_annotation'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }

             file_list <- file_list[!(file_list %in% c('00_intermediate_data', '01_surrogate_ms2_spec_plot'))]


             walk(file_list, function(x){
               file.copy(from = file.path(path_from, '02_result_MRN_annotation', x),
                         to = file.path(path_to, '02_result_MRN_annotation'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             })



             # 03 annotation credential -----------------------------
             file_list <- list.files(file.path(path_from, '03_annotation_credential'))
             if (length(dir_ignore) > 0) {
               file_list <- file_list[!(file_list %in% dir_ignore)]
             }

             if ('00_intermediate_data' %in% file_list) {
               dir.create(file.path(path_to, '03_annotation_credential', '00_intermediate_data'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '03_annotation_credential', '00_intermediate_data'),
                         to = file.path(path_to, '03_annotation_credential'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }

             if ('01_pseudo_MS1_spec' %in% file_list) {
               dir.create(file.path(path_to, '03_annotation_credential', '01_pseudo_MS1_spec'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '03_annotation_credential', '01_pseudo_MS1_spec'),
                         to = file.path(path_to, '03_annotation_credential'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }

             if ('02_formula_prediction' %in% file_list) {
               dir.create(file.path(path_to, '03_annotation_credential', '02_formula_prediction'),
                          showWarnings = FALSE, recursive = TRUE)
               file.copy(from = file.path(path_from, '03_annotation_credential', '02_formula_prediction'),
                         to = file.path(path_to, '03_annotation_credential'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             }

             file_list <- file_list[!(file_list %in% c('00_intermediate_data', '01_pseudo_MS1_spec', '02_formula_prediction'))]

             walk(file_list, function(x){
               file.copy(from = file.path(path_from, '03_annotation_credential', x),
                         to = file.path(path_to, '03_annotation_credential'),
                         recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
             })


           })

