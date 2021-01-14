################################################################################
# Extract informations from public metabolomics database -----------------------

#   extractPropertyFromHMDB ----------------------------------------------------
#' @title extractPropertyFromHMDB
#' @author Zhiwei Zhou
#' @description Extract properties from HMDB
#' @param id_hmdb Identifier of human metabolome database
#' @return a data.frame of properties
#' @export
#' @examples
#' test <- extractPropertyFromHMDB(id_hmdb = 'HMDB0000001')

# test <- extractPropertyFromHMDB(id_hmdb = 'HMDB0000001')

setGeneric(name = 'ExtractPropertyFromHMDB',
           def = function(id_hmdb){

             if (is.na(id_hmdb)) {
               result <- rep(NA, 28) %>% matrix(nrow = 1) %>% as.data.frame()
               colnames(result) <- c("id", "acceptor_count_ChemAxon", "average_mass_ChemAxon", "bioavailability_ChemAxon",
                                     "donor_count_ChemAxon", "formal_charge_ChemAxon", "formula_ChemAxon",
                                     "ghose_filter_ChemAxon", "inchi_ChemAxon", "inchikey_ChemAxon", "iupac_ChemAxon",
                                     "logp_ALOGPS", "logp_ChemAxon", "logs_ALOGPS", "mddr_like_rule_ChemAxon",
                                     "mono_mass_ChemAxon", "number_of_rings_ChemAxon", "physiological_charge_ChemAxon",
                                     "pka_strongest_acidic_ChemAxon", "pka_strongest_basic_ChemAxon",
                                     "polar_surface_area_ChemAxon", "polarizability_ChemAxon", "refractivity_ChemAxon",
                                     "rotatable_bond_count_ChemAxon", "rule_of_five_ChemAxon", "smiles_ChemAxon",
                                     "solubility_ALOGPS", "veber_rule_ChemAxon")

               return(result)
             }

             raw_data <- paste0('http://www.hmdb.ca/metabolites/', id_hmdb, '.xml') %>%
               xml2::read_xml(options=NULL)

             temp <- xml2::xml_find_first(raw_data, "//predicted_properties")

             md_name <- temp %>% xml2::xml_find_all('//kind') %>% xml2::xml_text()
             md_value <- temp %>% xml2::xml_find_all('//value') %>% xml2::xml_text()
             md_source <- temp %>% xml2::xml_find_all('//source') %>% xml2::xml_text()


             result <- data.frame(md_name, md_value, md_source, stringsAsFactors = F) %>%
               dplyr::mutate(id = id_hmdb,
                             name = paste(md_name, md_source, sep = '_')) %>%
               dplyr::distinct(name, .keep_all = TRUE) %>%
               dplyr::select(id, name, md_value) %>%
               tidyr::spread(name, md_value)
             # dplyr::select(id,
             #               exact_mass = mono_mass_ChemAxon,
             #               formal.charge = formal_charge_ChemAxon,
             #               physiological.charge = physiological_charge_ChemAxon,
             #               predicted.logp.ALOGPS = logp_ALOGPS,
             #               predicted.logp.ChemAxon = logp_ChemAxon,
             #               logs = logs_ALOGPS,
             #               pka.strongest.acidic = pka_strongest_acidic_ChemAxon,
             #               pka.strongest.basic = pka_strongest_basic_ChemAxon,
             #               acceptor.count = acceptor_count_ChemAxon,
             #               donor.count = donor_count_ChemAxon,
             #               polar.surface.area = polar_surface_area_ChemAxon,
             #               rotatable.bond.count = rotatable_bond_count_ChemAxon,
             #               refractivity = refractivity_ChemAxon,
             #               polarizability = polarizability_ChemAxon)

             return(result)

           })



#   extractPropertyFromPubChem ----------------------------------------------------
#' @title extractPropertyFromPubChem
#' @author Zhiwei Zhou
#' @description Extract properties from Pubchem
#' @param id_pubchem Identifier of pubchem
#' @return a data.frame of properties
#' @export
#' @examples
#' test <- extractPropertyFromPubChem(id_pubchem = '14130')

# test <- extractPropertyFromPubChem(id_pubchem = '14130')

setGeneric(name = 'extractPropertyFromPubChem',
           def = function(id_pubchem){

             if (is.na(id_pubchem)) {
               result <- rep(NA, 19) %>% matrix(nrow = 1) %>% as.data.frame()
               colnames(result) <- c("id",  "Complexity", "Compound Is Canonicalized",
                                     "Covalently-Bonded Unit Count", "Defined Atom Stereocenter Count",
                                     "Defined Bond Stereocenter Count", "Exact Mass", "Formal Charge",
                                     "Heavy Atom Count", "Hydrogen Bond Acceptor Count", "Hydrogen Bond Donor Count",
                                     "Isotope Atom Count", "Molecular Weight", "Monoisotopic Mass", "Rotatable Bond Count",
                                     "Topological Polar Surface Area", "Undefined Atom Stereocenter Count",
                                     "Undefined Bond Stereocenter Count", "XLogP3")

               return(result)
             }


             raw_data <- xml2::read_xml(paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/',
                                               id_pubchem,
                                               '/XML/?response_type=display'))

             idx <- (raw_data %>%
                       xml2::as_list() %>%
                       .[[1]] %>%
                       names(.) == 'Section') %>%
               which()

             idx2 <- sapply(idx, function(i){
               temp <- raw_data %>%
                 xml2::as_list() %>%
                 .[[1]] %>%
                 .[[i]] %>%
                 .[[1]] %>%
                 .[[1]]
             })

             idx <- idx[which(idx2 == "Chemical and Physical Properties")]

             temp_data <- raw_data %>%
               xml2::as_list() %>%
               .[[1]] %>%
               .[[idx]] %>%
               .[[3]]

             idx <- (names(temp_data) == 'Section') %>% which()

             result <- lapply(idx, function(i){
               # cat(i, ' ')
               md_name <- temp_data[[i]]$TOCHeading[[1]]
               value <- temp_data[[i]]$Information$Value$Number[[1]]
               value <- ifelse(is.null(value), NA, value)
               result <- data.frame(name = md_name, md_value = value, stringsAsFactors = F)
               return(result)
             }) %>%
               dplyr::bind_rows() %>%
               dplyr::mutate(id = id_pubchem) %>%
               dplyr::select(id, dplyr::everything()) %>%
               tidyr::spread(name, md_value)

             # c('id', 'Monoisotopic Mass', 'Formal Charge', 'XLogP3', "Hydrogen Bond Acceptor Count",
             #   "Hydrogen Bond Donor Count", "Topological Polar Surface Area", "Rotatable Bond Count")

             return(result)

           }
)
