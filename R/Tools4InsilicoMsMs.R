

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
                                 '[M+CH3CN+Na]+' = '[M+2ACN+H]+',
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
