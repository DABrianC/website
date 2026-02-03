library(tidyverse)
library(viridis)
library(patchwork)
library(circlize)
library(here)
library(networkD3)
library(htmltools)
library(ellmer)
library(ollamar)


df <- read_csv(here::here("posts/career_shifts/connections.csv"), skip = 2)

colnames(df) <- df[1, ]

df2 <- df |>
  filter(MSI == "x" | EnCompass == "x") |>
  mutate(
    MSI_ENC = case_when(
      MSI == "x" | EnCompass == "x" ~ "International Development"
    )
  )

#writexl::write_xlsx(df2, path = "data.xlsx")

#define sectors for classification
sectors <- c(
  "Technology",
  "International Development",
  "Non-Profit & International Organizations",
  "Research & Evaluation",
  "Finance & Banking",
  "Government & Public Sector",
  "Healthcare & Pharmaceutical",
  "Education",
  "Legal Services",
  "Media & Communications",
  "Retail & Consumer Goods",
  "Independent Consulting & Other Services"
)

#function to classify companies and return output
classify_company <- function(company) {
  if (is.na(company) || company == "") {
    return("Unknown")
  }

 # the prompt 
  prompt <- paste0(
    "Classify into ONE sector. If the Company name includes "Management Systems International", "Tetra Tech", or "Encompass LLC", then it should be classified as "International Development". Respond with ONLY the sector name from this list:\n",
    paste(sectors, collapse = ", "),
    "\n\n",
    "Company: ",
    company,
    "\n",
    "\nSector:"
  )

  # Use generate() from ellmer to 
  response <- generate(
    model = "llama3.2",
    prompt = prompt,
    output = "text",
    seed = 24 # for reproducability
  )

  sector <- str_trim(response)
  sector <- gsub(
    "^(Sector:|Answer:|Classification:)\\s*",
    "",
    sector,
    ignore.case = TRUE
  )
  sector <- str_trim(sector)

  if (!(sector %in% sectors)) {
    sector <- "Independent Consulting & Other Services"
  }

  return(sector)
}

df2$`Current Sector` <- map_chr(
  df2$Company,
  classify_company,
  .progress = "Classifying companies"  # Custom message
)

# List of known International Development companies
intl_dev_companies <- c(
  "management systems international", "msi",
  "tetra tech",
  "encompass llc", "encompass",
  "chemonics", "chemonics international",
  "abt associates", "abt global",
  "palladium",
  "dai global", "dai",
  "fhi 360",
  "social impact"
)

classify_company <- function(company) {
  if (is.na(company) || company == "") {
    return("Unknown")
  }
  
  company_lower <- tolower(company)
  
  # First, check for exact matches
  if (any(sapply(intl_dev_companies, function(x) grepl(x, company_lower, fixed = TRUE)))) {
    return("International Development")
  }
  
  # If no match, use LLM with examples
  prompt <- paste0(
    "Classify into ONE sector based on these examples:\n\n",
    
    "International Development: Management Systems International, Tetra Tech, EnCompass LLC, Chemonics, Abt Associates\n",
    "Technology: Microsoft, Google, Amazon\n",
    "Finance & Banking: Goldman Sachs, JPMorgan\n",
    "Non-Profit & International Organizations: Save the Children, World Bank\n\n",
    
    "Available sectors: ", paste(sectors, collapse = ", "), "\n\n",
    "Company: ", company, "\n",
    "Sector:"
  )

  response <- generate(
    model = "llama3.2",
    prompt = prompt,
    output = "text",
    seed = 24,
    temperature = 0.0
  )

  sector <- str_trim(response)
  sector <- gsub("^(Sector:|Answer:|Classification:)\\s*", "", 
                 sector, ignore.case = TRUE)
  sector <- str_trim(sector)

  if (!(sector %in% sectors)) {
    sector <- "Independent Consulting & Other Services"
  }

  return(sector)
}

df2$`Current Sector` <- map_chr(
  df2$Company,
  classify_company,
  .progress = "Classifying companies"  # Custom message
)

  #New attempt
library(tidyverse)
library(ellmer)
library(ollamar)

# Define sectors
sectors <- c(
  "Technology",
  "International Development",
  "Non-Profit & International Organizations",
  "Research & Evaluation",
  "Finance & Banking",
  "Government & Public Sector",
  "Healthcare & Pharmaceutical",
  "Education",
  "Legal Services",
  "Media & Communications",
  "Retail & Consumer Goods",
  "Independent Consulting & Other Services"
)

# List of known International Development companies
intl_dev_companies <- c(
  "management systems international", "msi",
  "tetra tech",
  "encompass llc", "encompass",
  "chemonics", "chemonics international",
  "abt associates", "abt global",
  "palladium",
  "dai global", "dai",
  "fhi 360",
  "social impact",
  "socha",
  "counterpart international",
  "ibtci", "international business & technical consultants",
  "corus international",
  "idinsight",
  "itad"
)

classify_company <- function(company, position = "") {
  if (is.na(company) || company == "") {
    return("Unknown")
  }
  
  company_lower <- tolower(company)
  
  # First, check for exact matches with known IntDev companies
  if (any(sapply(intl_dev_companies, function(x) grepl(x, company_lower, fixed = TRUE)))) {
    return("International Development")
  }
  
  # Build comprehensive prompt with clear examples and reasoning
  prompt <- paste0(
    "You are an expert at classifying organizations by business sector.\n\n",
    
    "SECTOR DEFINITIONS WITH EXAMPLES:\n\n",
    
    "1. International Development: USAID contractors, development consulting firms\n",
    "   Examples: Chemonics, Tetra Tech, DAI, Palladium, Management Systems International\n\n",
    
    "2. Non-Profit & International Organizations: Charities, NGOs, foundations, UN agencies\n",
    "   Examples: Save the Children, World Bank, Catholic Relief Services, Gates Foundation, UNICEF\n\n",
    
    "3. Research & Evaluation: M&E firms, research organizations, think tanks\n",
    "   Examples: IDinsight, Ipsos, Brookings Institution, Itad\n\n",
    
    "4. Government & Public Sector: Government agencies, public administration\n",
    "   Examples: USAID, U.S. State Department, Maryland Department of Environment, County Government\n\n",
    
    "5. Technology: Software, IT services, cloud, data analytics, tech consulting\n",
    "   Examples: Microsoft, Amazon, Google, SAIC, tech consultancies\n\n",
    
    "6. Finance & Banking: Banks, investment firms, financial services, insurance\n",
    "   Examples: Goldman Sachs, Capital One, JPMorgan, Northwestern Mutual\n\n",
    
    "7. Healthcare & Pharmaceutical: Hospitals, clinics, pharma, medical services\n",
    "   Examples: Kaiser Permanente, Pfizer, medical practices\n\n",
    
    "8. Education: Schools, universities, educational organizations\n",
    "   Examples: Harvard University, educational non-profits\n\n",
    
    "9. Legal Services: Law firms, legal consultancies\n",
    "   Examples: Major law firms, legal advisors\n\n",
    
    "10. Independent Consulting & Other Services: Small consulting firms, self-employed, freelancers, coaching, niche services\n",
    "    Examples: Self-employed, independent consultants, small LLCs, boutique firms\n\n",
    
    "CLASSIFICATION RULES:\n",
    "- If company does USAID/development work or M&E for development projects → International Development or Research & Evaluation\n",
    "- If company is a charity/NGO → Non-Profit & International Organizations\n",
    "- If self-employed, freelance, small LLC → Independent Consulting & Other Services\n",
    "- Government agencies → Government & Public Sector\n",
    "- Banks, financial firms → Finance & Banking\n",
    "- Tech companies, IT consulting → Technology\n\n",
    
    "NOW CLASSIFY THIS COMPANY:\n",
    "Company: ", company, "\n",
    if (!is.na(position) && position != "") paste0("Position: ", position, "\n"),
    "\nThink about what this company does, then respond with ONLY ONE sector name from the list.\n",
    "Sector:"
  )

  response <- generate(
    model = "llama3.2",
    prompt = prompt,
    output = "text",
    seed = 24,
    temperature = 0.1  # Slight randomness for better reasoning
  )

  # Clean response
  sector <- str_trim(response)
  sector <- gsub("^(Sector:|Answer:|Classification:|\\d+\\.\\s*)\\s*", "", 
                 sector, ignore.case = TRUE)
  sector <- str_trim(sector)
  
  # Remove any trailing punctuation or explanation
  sector <- gsub("\\s*[,\\.;:].*$", "", sector)
  
  # Validate it's in our list (fuzzy match)
  if (!(sector %in% sectors)) {
    matches <- agrep(sector, sectors, max.distance = 0.3, value = TRUE)
    if (length(matches) > 0) {
      sector <- matches[1]
    } else {
      sector <- "Independent Consulting & Other Services"
    }
  }

  return(sector)
}

# Apply classification with position for better context
df2$`Current Sector` <- map2_chr(
  df2$Company,
  df2$Position,
  classify_company,
  .progress = "Classifying companies"
)

# Check results
  table(df2$`Current Sector`)

library(tidyverse)
library(ollamar)

# Enable Ark
options(ollamar.use_ark = TRUE)
  
# Pull the faster model
pull('llama3.2:1b')

# Test/warm up (correct syntax)
generate(
  model = "llama3.2:1b", 
  prompt = "test", 
  num_predict = 1,
  output = "text"
)

# Your classification function
classify_company_fast <- function(company) {
  if (is.na(company) || company == "") return("Unknown")
  
  company_lower <- tolower(company)
  
  if (any(sapply(intl_dev_companies, function(x) grepl(x, company_lower, fixed = TRUE)))) {
    return("International Development")
  }
  
  prompt <- paste0(
    "Classify into ONE sector. Respond with ONLY the sector name.\n",
    "Sectors: Technology, International Development, Non-Profit & International Organizations, ",
    "Research & Evaluation, Finance & Banking, Government & Public Sector, ",
    "Healthcare & Pharmaceutical, Education, Legal Services, Media & Communications, ",
    "Retail & Consumer Goods, Independent Consulting & Other Services\n\n",
    "Company: ", company, "\n",
    "Sector:"
  )

  response <- generate(
    model = "llama3.2:1b",
    prompt = prompt,
    seed = 24,
    num_predict = 15,
    num_ctx = 512,
    temperature = 0.0,
    output = "text"
  )

  sector <- str_trim(response)
  sector <- gsub("^(Sector:|Answer:|Classification:)\\s*", "", sector, ignore.case = TRUE)
  sector <- str_trim(sector)

  if (!(sector %in% sectors)) {
    sector <- "Independent Consulting & Other Services"
  }

  return(sector)
}

# Apply
df2$`Current Sector` <- map_chr(
  df2$Company,
  classify_company_fast,
  .progress = "Classifying companies"
  )

df3 <- df2 |> 
  count(MSI_ENC, `Current Sector`) |> 
  rename(source = MSI_ENC,
         target = `Current Sector`,
         value = n) |> 
  mutate(source = "Previous: Int'l Development")

# create nodes dataframe (unique list of all )
nodes <- data.frame(
  name = c(unique(df3$source), unique(df3$target))
) |> 
  distinct()

links <- df3 |> 
  mutate(source_id = match(source, nodes$name)-1,
         target_id = match(target, nodes$name)-1
        )
  
sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "name",
  units = "connections",
  fontSize = 14,
  nodeWidth = 30,
  fontFamily = "Arial",
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
  sinksRight = TRUE
  )

sankey
