createIMPACT159Regions <- function() {
  #' regions.IMPACT159.plus is all the regions larger than a single political unit (as defined by an ISO3 code)
  #' and what political units are included
  regions.IMPACT159.plus <- data.frame(
    region_code.IMPACT159 = character(0),
    region_members = character(0),
    region_name.IMPACT159 = character(0),
    stringsAsFactors = FALSE
  )
  #' @param region_code.IMPACT159 - temporary variable to hold countries that make up a region
  region_code.IMPACT159 <- "BLT"
  ISO3_lst <- c("EST", "LTU", "LVA")
  region_name.IMPACT159 <- "Baltic States"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('BLT Baltic States is Estonia EST, Lithuania LTU, Latvia
  # LVA')

  region_code.IMPACT159 <- "BLX"
  ISO3_lst <- c("BEL", "LUX")
  region_name.IMPACT159 <- "Belgium-Luxembourg"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('BLX Belgium-Luxembourg is Belgium BEL, Luxembourg LUX')

  region_code.IMPACT159 <- "CHM"
  ISO3_lst <- c("CHN", "HKG", "MAC", "TWN")
  region_name.IMPACT159 <- "China plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('CHM China plus is China CHN, Hong Kong HKG, Macao MAC,
  # Taiwan TWN')

  region_code.IMPACT159 <- "CHP"
  ISO3_lst <- c("CHE", "LIE")
  region_name.IMPACT159 <- "Switzerland plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('CHP Switzerland plus is Switzerland CHE Liechtenstein LIE')

  region_code.IMPACT159 <- "CRB"
  ISO3_lst <- c("ABW", "AIA", "ATG", "BES", "BHS", "BLM", "BRB", "CUW", "CYM",
                "DMA", "GLP", "GRD", "KNA", "LCA", "MAF", "MSR", "MTQ", "PRI", "SXM",
                "TCA", "TTO", "VCT", "VGB", "VIR")
  region_name.IMPACT159 <- "Other Caribbean"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('CRB Other Caribbean is Aruba ABW, Anguilla AIA, Netherlands
  # Antilles (obsolete) ANT, Antigua ATG Bonaire, Sint Eustatius, and
  # Saba BES, Bahamas BHS, St,Barthélemy BLM, Barbados BRB, Curacao CUW,
  # Cayman Islands CYM Dominica DMA, Guadeloupe GLP, Grenada GRD,
  # St,Kitts and Nevis KNA, St,Lucia LCA, Saint Martin MAF Montserrat
  # MSR, Martinique MTQ, Puerto Rico PRI, Sint Maarten SXM, Turks and
  # Caicos Islands TCA Trinidad and Tobago TTO, St,Vincent and Grenadines
  # VCT, British Virgin Islands VGB, U.S,Virgin Islands VIR') ANT dropped
  # from this list

  #DNP is commented out because the latest version of the IMPACT regions has Denmark and Greenland separately
  # region_code.IMPACT159 <- "DNP"
  # ISO3_lst <- c("DNK", "GRL")
  # region_name.IMPACT159 <- "Denmark plus"
  # regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, plusCnst(region_code.IMPACT159,
  #                                                                  ISO3_list, region_name.IMPACT159))
  # txt <- c('DNP Denmark plus is DNK Denmark GRL Greenland')

  region_code.IMPACT159 <- "FNP"
  ISO3_lst <- c("ALA", "FIN")
  region_name.IMPACT159 <- "Finland plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('FNP Finland plus is Aland Islands ALA Finland FIN')

  region_code.IMPACT159 <- "FRP"
  ISO3_lst <- c("FRA", "MCO")
  region_name.IMPACT159 <- "France plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('FRP France plus is France FRA Monaco MCO')

  region_code.IMPACT159 <- "GSA"
  ISO3_lst <- c("GUF", "GUY", "SUR")
  region_name.IMPACT159 <- "Guyanas"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('GSA Guyanas is South America French Guiana GUF Guyana GUY
  # Suriname SUR')

  region_code.IMPACT159 <- "ITP"
  ISO3_lst <- c("ITA", "MLT", "SMR", "VAT")
  region_name.IMPACT159 <- "Italy plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('ITP Italy plus is Italy ITA Malta MLT San Marino SMR
  # Vatican City VAT')

  region_code.IMPACT159 <- "MOR"
  ISO3_lst <- c("MAR", "ESH")
  region_name.IMPACT159 <- "Morocco plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('MOR Morocco plus is Morocco MAR Western Sahara ESH')

  region_code.IMPACT159 <- "OAO"
  # Antartic (ATA) added to this list
  ISO3_lst <- c("ATA", "BMU", "BVT", "CPV", "FLK", "FRO", "SGS", "SHN", "SJM",
                "SPM", "STP")
  region_name.IMPACT159 <- "Other Atlantic Ocean"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('OAO Other Atlantic Ocean is Bermuda BMU Bouvet Island BVT
  # Cape Verde CPV Falkland Islands FLK Faroe Islands FRO South Georgia
  # and South Sandwich Islands SGS Saint Helena, Ascension, and Tristan
  # de Cunha SHN Svalbard and Jan Mayen SJM Saint Pierre and Miquelon SPM
  # Sao Tome and Principe STP')

  region_code.IMPACT159 <- "OBN"
  ISO3_lst <- c("BIH", "MKD", "MNE", "SRB")
  region_name.IMPACT159 <- "Other Balkans"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('OBN Other Balkans is Bosnia-Herzegovina BIH Macedonia (FYR)
  # MKD Montenegro MNE Serbia SRB')

  region_code.IMPACT159 <- "OIO"
  ISO3_lst <- c("ATF", "CCK", "COM", "CXR", "HMD", "IOT", "MDV", "MUS", "MYT",
                "REU", "SYC")
  region_name.IMPACT159 <- "Other Indian Ocean"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('OIO Other Indian Ocean is Southern Territories ATF Keeling
  # Islands CCK Comoros COM Christmas Island CXR Heard and McDonald
  # Islands HMD British Indian Ocean Territory IOT Maldives MDV Mauritius
  # MUS Mayotte MYT Réunion REU Seychelles SYC') CXR deleted from this
  # list

  region_code.IMPACT159 <- "OPO"
  ISO3_lst <- c("ASM", "COK", "FSM", "GUM", "KIR", "MHL", "MNP", "NCL", "NFK",
                "NIU", "NRU", "PCN", "PLW", "PYF", "TKL", "TON", "TUV", "UMI", "WLF",
                "WSM")
  region_name.IMPACT159 <- "Other Pacific Ocean"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('OPO Other Pacific Ocean is American Samoa ASM Cook Islands
  # COK Micronesia FSM Guam GUM Kiribati KIR Marshall Islands MHL
  # Northern Mariana Islands MNP New Caledonia NCL Norfolk Island NFK
  # Niue NIU Nauru NRU Pitcairn PCN Palau PLW French Polynesia PYF
  # Tokelau TKL Tonga TON Tuvalu TUV Minor Outlying Islands UMI Wallis
  # and Futuna WLF Samoa WSM')

  region_code.IMPACT159 <- "OSA"
  ISO3_lst <- c("BRN", "SGP")
  region_name.IMPACT159 <- "Other Southeast Asia"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('OSA OtherSoutheast Asia is Brunei BRN Singapore SGP')

  region_code.IMPACT159 <- "RAP"
  ISO3_lst <- c("ARE", "BHR", "KWT", "OMN", "QAT")
  region_name.IMPACT159 <- "Rest of Arab Peninsula"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('RAP Rest of Arab Peninsula is United Arab Emirates ARE
  # Bahrain BHR Kuwait KWT Oman OMN Qatar QAT')

  region_code.IMPACT159 <- "SDP"
  ISO3_lst <- c("SSD", "SDN")
  region_name.IMPACT159 <- "Sudan plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('SDP Sudan plus is SSD Sudan SDN South Sudan')

  region_code.IMPACT159 <- "SPP"
  ISO3_lst <- c("AND", "ESP", "GIB")
  region_name.IMPACT159 <- "Spain plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('SPP Spain plus is Andorra AND Spain ESP Gibraltar GIB')

  region_code.IMPACT159 <- "UKP"
  ISO3_lst <- c("GBR", "GGY", "IMN","JEY")
  region_name.IMPACT159 <- "Great Britain plus"
  temp <- plusCnst(region_code.IMPACT159,ISO3_lst,region_name.IMPACT159)
  regions.IMPACT159.plus <- rbind(regions.IMPACT159.plus, temp)
  # txt <- c('UKP Great Britain plus is Great Britain GBR Guernsey GGY
  # Isle of Man IMN Jersey JEY')

  colnames(regions.IMPACT159.plus) <-
    c("region_code.IMPACT159", "ISO_code", "region_name.IMPACT159")

  # Create regions.IMPACT159 ----
  # The next lines of code get a list of IMPACT 3 regions that are not in IMPACT159.plus
  IMPACT159regions <- fileNameList("IMPACT159regions")
  regions.IMPACT159 <- openxlsx::read.xlsx(IMPACT159regions)
  colnames(regions.IMPACT159) <-
    c("region_code.IMPACT159", "region_name.IMPACT159")
  #' @param regions.IMPACT159.region_name.IMPACT159 regions in IMPACT159 that are only one country
  regions.IMPACT159.cty <-
    regions.IMPACT159[!regions.IMPACT159$region_code.IMPACT159 %in% regions.IMPACT159.plus$region_code.IMPACT159,]
  regions.IMPACT159.cty$ISO_code <-
    regions.IMPACT159.cty$region_code.IMPACT159
  regions.IMPACT159 <- rbind(regions.IMPACT159.cty, regions.IMPACT159.plus)
  regions.IMPACT159 <-
    regions.IMPACT159[order(regions.IMPACT159$ISO_code), ]
  return(regions.IMPACT159)
}
