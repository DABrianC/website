library(tidyverse)
library(viridis)
library(patchwork)
library(circlize)
library(here)
library(networkD3)
library(htmltools)
library(ellmer)
library(ollamar)


 Technology, International Development, Non-Profit & International Organizations, ",
    "Research & Evaluation, Finance & Banking, Government & Public Sector, ",
    "Healthcare & Pharmaceutical, Education, Legal Services, Media & Communications, ",
    "Retail & Consumer Goods, Independent Consulting & Other Services

df <- read_csv(here::here("posts/career_shifts/connections.csv"), skip = 2)

colnames(df) <- df[1, ]

df2 <- df |>
  filter(MSI == "x" | EnCompass == "x") |>
  mutate(
    MSI_ENC = case_when(
      MSI == "x" | EnCompass == "x" ~ "International Development"
    )
  )


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
  "making cents international",
  "management systems international",
  "management sciences for health",
  "msi",
  "tetra tech",
  "encompass llc",
  "encompass",
  "gates foundation",
  "chemonics",
  "chemonics international",
  "creative associates international",
  "counterpart international",
  "abt associates",
  "abt global",
  "palladium",
  "dai global",
  "dai",
  "fhi 360",
  "social impact",
  "socha",
  "counterpart international",
  "ibi - international business initiatives",
  "ibtci",
  "international business & technical consultants",
  "international business & technical consultants, inc. (IBTCI)",
  "corus international",
  "idinsight",
  "global communities",
  "WICE"
)

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
  if (is.na(company) || company == "") {
    return("Unknown")
  }

  company_lower <- tolower(company)

  #if (
  # any(sapply(intl_dev_companies, function(x) {
  #   grepl(x, company_lower, fixed = TRUE)
  # }))
  # ) {
  #   return("International Development")
  # }

  if (any(str_detect(company_lower, fixed(intl_dev_companies)))) {
    return("International Development")
  }

  prompt <- paste0(
    "Classify into ONE sector. Respond with ONLY the sector name.\n",
    "Sectors: Technology, International Development, Non-Profit & International Organizations, ",
    "Research & Evaluation, Finance & Banking, Government & Public Sector, ",
    "Healthcare & Pharmaceutical, Education, Legal Services, Media & Communications, ",
    "Retail & Consumer Goods, Independent Consulting & Other Services\n\n",
    "Company: ",
    company,
    "\n",
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

# Apply
df2$`Current Sector` <- map_chr(
  df2$Company,
  classify_company_fast,
  .progress = "Classifying companies"
)

df3 <- df2 |>
  count(MSI_ENC, `Current Sector`) |>
  rename(source = MSI_ENC, target = `Current Sector`, value = n) |>
  mutate(
    source = "Former MSI and EnCompass colleagues",
    percent = paste0(round(value / sum(value) * 100, 1), "%")
  ) |>
  arrange(desc(value))

# create nodes dataframe (unique list of all )
nodes <- data.frame(
  name = c(unique(df3$source), unique(df3$target))
) |>
  distinct()

links <- df3 |>
  mutate(
    source_id = match(source, nodes$name) - 1,
    target_id = match(target, nodes$name) - 1,
    percent = paste0(round(value / sum(value) * 100, 1), "%")
  )


# Make sure percent column exists
#links <- links %>%
# mutate(percent = (value / sum(value)) * 100)

# Convert to plain data frame
links <- as.data.frame(links)
nodes <- as.data.frame(nodes)

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
  colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20);"),
  sinksRight = TRUE
) |>
  htmlwidgets::onRender(
    "
    function(el, x) {
      setTimeout(function() {
        // Create our own tooltip element with modern design
        var tooltip = d3.select('body').append('div')
          .attr('class', 'custom-sankey-tooltip')
          .style('position', 'absolute')
          .style('display', 'none')
          .style('background', 'rgba(255, 255, 255, 0.98)')
          .style('backdrop-filter', 'blur(10px)')
          .style('border', 'none')
          .style('border-radius', '16px')
          .style('padding', '20px 24px')
          .style('font-size', '14px')
          .style('font-weight', '400')
          .style('box-shadow', '0 20px 60px rgba(0, 0, 0, 0.15), 0 0 0 1px rgba(0, 0, 0, 0.05)')
          .style('pointer-events', 'none')
          .style('z-index', '9999')
          .style('font-family', '-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif')
          .style('min-width', '200px')
          .style('transform', 'translateY(-8px)')
          .style('transition', 'all 0.2s ease');
        
        var nodeColors = {};
        d3.select(el).selectAll('.node').each(function(d) {
          nodeColors[d.name] = d3.select(this).select('rect').style('fill');
        });
        
        var linkNodes = d3.select(el).selectAll('.link');
        
        linkNodes
          .style('stroke', function(d) {
            return nodeColors[d.target.name];
          })
          .style('stroke-opacity', 0.2)
          .style('cursor', 'pointer');
        
        linkNodes
          .on('click', function(d) {
            d3.event.stopPropagation();
            
            d3.select(this).style('stroke-opacity', 0.9);
            
            // Get the target color
            var targetColor = nodeColors[d.target.name];
            
            // Calculate percent from the value
            var totalValue = d3.sum(linkNodes.data(), function(link) { return link.value; });
            var percent = (d.value / totalValue) * 100;
            
            // tooltip design
            tooltip
              .html(
                '<div style=\"border-left: 4px solid ' + targetColor + '; padding-left: 12px;\">' +
                '<div style=\"font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; color: #999; font-weight: 600; margin-bottom: 8px;\">Careers Shifting</div>' +
                '<div style=\"font-size: 15px; font-weight: 600; color: #1a1a1a; margin-bottom: 12px; line-height: 1.4;\">' + 
                d.source.name + '<br><span style=\"color: #999;\">→</span> ' + d.target.name + '</div>' +
                '<div style=\"display: flex; align-items: baseline; gap: 8px; margin-bottom: 4px;\">' +
                '<span style=\"font-size: 32px; font-weight: 700; color: ' + targetColor + ';\">' + percent.toFixed(1) + '%</span>' +
                '</div>' +
                '<div style=\"font-size: 12px; color: #666;\">' + d.value + ' professionals</div>' +
                '</div>'
              )
              .style('display', 'block')
              .style('left', (d3.event.pageX + 15) + 'px')
              .style('top', (d3.event.pageY - 50) + 'px');
            
            setTimeout(function() {
              linkNodes.style('stroke-opacity', 0.2);
            }, 300);
          })
          .on('mouseover', function() {
            d3.select(this).style('stroke-opacity', 0.5);
          })
          .on('mouseout', function() {
            d3.select(this).style('stroke-opacity', 0.2);
          });
        
        d3.select(el).selectAll('.node')
          .style('cursor', 'pointer')
          .on('click', function(d) {
            d3.event.stopPropagation();
            
            // Get the node color
            var nodeColor = nodeColors[d.name];
            
            tooltip
              .html(
                '<div style=\"border-left: 4px solid ' + nodeColor + '; padding-left: 12px;\">' +
                '<div style=\"font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; color: #999; font-weight: 600; margin-bottom: 8px;\">Sector</div>' +
                '<div style=\"font-size: 16px; font-weight: 600; color: #1a1a1a; margin-bottom: 12px;\">' + d.name + '</div>' +
                '<div style=\"font-size: 12px; color: #666;\"><span style=\"font-weight: 600; color: #333;\">' + d.value + '</span> professionals</div>' +
                '</div>'
              )
              .style('display', 'block')
              .style('left', (d3.event.pageX + 15) + 'px')
              .style('top', (d3.event.pageY - 50) + 'px');
          });
        
        d3.select('body').on('click', function() {
          tooltip.style('display', 'none');
        });
        
      }, 200);
    }
  "
  )

sankey


#save it to the folder
saveWidget(sankey, "sankey_chart.html", selfcontained = TRUE)
