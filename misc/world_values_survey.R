library(tidyverse)
library(data.table)
library(readxl)

path_to_dir <- "/Users/samuellindquist/Library/CloudStorage/Dropbox/expert_bias/code_data" # sams mac
#path_to_dir <- "C:/Users/slindquist/Dropbox/expert_bias/code_data" # sams windows

setwd(path_to_dir)

# only bringing in some variables
id_vars <- c("COUNTRY_ALPHA")

# Create a named list based on provided data
confidence_vars <- list(
  `Confidence: Churches` = "E069_01",
  `Confidence: Armed Forces` = "E069_02",
  `Confidence: Education System` = "E069_03",
  `Confidence: The Press` = "E069_04",
  `Confidence: Labour Unions` = "E069_05",
  `Confidence: The Police` = "E069_06",
  `Confidence: Parliament` = "E069_07",
  `Confidence: The Civil Services` = "E069_08",
  `Confidence: Social Security System` = "E069_09",
  `Confidence: Television` = "E069_10",
  `Confidence: The Government` = "E069_11",
  `Confidence: The Political Parties` = "E069_12",
  `Confidence: Major Companies` = "E069_13",
  `Confidence: The Environmental Protection Movement` = "E069_14",
  `Confidence: The Women´s Movement` = "E069_15",
  `Confidence: Justice System/Courts` = "E069_16",
  `Confidence: The European Union` = "E069_17",
  `Confidence: Major regional organization (combined from country-specific)` = "E069_18",
  `Confidence: NATO` = "E069_19",
  `Confidence: The United Nations` = "E069_20",
  `Confidence: The Arab League` = "E069_21",
  `Confidence: The Association of South East Asian Nations -ASEAN` = "E069_22",
  `Confidence: The Organization for African Unity-OAU` = "E069_23",
  `Confidence: The NAFTA` = "E069_24",
  `Confidence: The Andean pact` = "E069_25",
  `Confidence: The Mercosur` = "E069_26",
  `Confidence: The SAARC` = "E069_27",
  `Confidence: The ECO` = "E069_28",
  `Confidence: The APEC` = "E069_29",
  `Confidence: The Free Commerce Treaty (Tratado de libre comercio)` = "E069_30",
  `Confidence: The United American States Organization (Organización de Estados Unidos Americanos - OEA)` = "E069_31",
  `Confidence: The “Movimiento en pro de Vieques”(Puerto Rico)` = "E069_32",
  `Confidence: Local/Regional Government` = "E069_33",
  `Confidence: SADC/SADEC` = "E069_34",
  `Confidence: East African Cooperation (EAC)` = "E069_35",
  `Confidence: The Presidency` = "E069_38",
  `Confidence: The Civil Society Groups` = "E069_39",
  `Confidence: Charitable or humanitarian organizations` = "E069_40",
  `Confidence: Banks` = "E069_41",
  `Confidence: CARICOM` = "E069_42",
  `Confidence: CIS` = "E069_43",
  `Confidence: Confidence in CER with Australia` = "E069_44",
  `Confidence: International Monetary Found (IMF)` = "E069_45",
  `Confidence: Non governmental Organizations (NGOs)` = "E069_46",
  `Confidence: The American Forces` = "E069_47",
  `Confidence: The non-Iraqi television` = "E069_48",
  `Confidence: TV News` = "E069_49",
  `Confidence: Religious leaders` = "E069_51",
  `Confidence: Evangelic Church` = "E069_52",
  `Confidence: Universities` = "E069_54",
  `Confidence: The Organization of the Islamic World` = "E069_55",
  `Confidence: The Organization of American States (OAE)` = "E069_56",
  `Confidence: UNASUR` = "E069_57",
  `Confidence: The Arab Maghreb Union` = "E069_58",
  `Confidence: Cooperation Council for the Arab states of Gulf (GCC)` = "E069_59",
  `Confidence: Mainland government` = "E069_60",
  `Confidence: The World Bank` = "E069_63",
  `Confidence: Elections` = "E069_64",
  `Confidence: International Criminal Court (ICC)` = "E069_65",
  `Confidence: UNDP United Nations Development Programme` = "E069_66",
  `Confidence: The African Union (AU)` = "E069_67",
  `Political system: Having experts make decisions` = "E115"
)

vars_to_keep <- c(id_vars, confidence_vars)

df <- fread("input/world_values_database/world_values_database_raw.csv",
               nrows = 100,
               select = vars_to_keep)


df <- read_excel("~/Desktop/Political_system_Having_experts_not_government_make_decisions_according_to_what_they_think_is_bes copy.xls")

