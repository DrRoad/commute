extracss <- "
html, body {
  width:100%;
  height:100%
}
#map {
  height: 100% !important;
  position: absolute !important;
  top: 0;
  left: 0;
}
#loading {
  cursor: progress !important;
}
#loading p {
  border-radius: 5px;
  background-color: rgba(255, 255, 255, 0.8);
  padding: 6px 8px;
  box-shadow: 0 0 15px rgba(0,0,0,0.2);
  font-size: 1.5em;
  font-weight: bold;
}
#mapcontrol, #infobuttoncontainer, #infopanel {
  background-color: rgba(255, 255, 255, 0.8);
  border-radius: 5px;
  box-shadow: 0 0 15px rgba(0,0,0,0.2);
  padding: 6px 8px;
  font: 14px/16px Arial, Helvetica, sans-serif;
}
#mapcontrol, #control2 {
  z-index: 1000;
}
#mapcontrol {
  width: 22em;
  max-height: 90%;
  overflow: auto;
  -ms-overflow-style: none;  /* IE and Edge */
  scrollbar-width: none;  /* Firefox */
}
#mapcontrol::-webkit-scrollbar {
  display: none;
}
#infopanel {
  display: none;
  background-color: rgba(255, 255, 255, 0.9);
  padding: 10px;
  z-index: 1001;
}
#lochtml ul {
  padding-left: 15px;
}
.radio label span p {
  margin-top: 3px;
  margin-bottom: 0px;
}
.leaflet-container {
  background-color: #84e1e1;
}
#infobuttoncontainer {
  margin-bottom: 16px;
  z-index: 1002;
}
#infobuttoncontainer .shiny-input-container,
#infobuttoncontainer div div {
  margin-right: 0px !important;
  margin-bottom: 0px !important;
}
h3 {
  margin-top: 5px;
  margin-bottom: 5px;
}
.overflowable {
  max-height: 100%;
  overflow: auto;
  -ms-overflow-style: none;  /* IE and Edge */
  scrollbar-width: none;  /* Firefox */
}
.overflowable::-webkit-scrollbar {
  display: none;
}
blockquote {
  font-size: 1em;
  font-style: italic;
  border: none;
  margin: 0;
}
.scrollbuffer {
  height: 50px;
}
.overflowable:before {
  content:'';
  width:100%;
  height:50px;    
  position:absolute;
  left:0;
  bottom:0;
  right:0;
  background:linear-gradient(transparent 0px, 
    rgba(255, 255, 255, 0.9));
}
#infobuttoncontainer label {
  font-weight: bold;
}
.locinfo {
  max-width: 100%;
}
.loading p {
  margin: 0;
}
hr {
  margin-top: 10px;
  margin-bottom: 10px;
  border-top: 1px solid #000;
}
.leaflet-control-search {
  box-shadow: none;
}
"

attribhtml <- '
<a href="http://leafletjs.com" 
title="A JS library for interactive maps">Leaflet</a> | <a 
href="http://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/"
title="Source data">
StatsNZ</a> | <a href="http://petras.space/page/cv/" title="Hire me!">
Petra Lamborn</a> | Numbers subject to <a
href="http://archive.stats.govt.nz/about_us/legisln-policies-protocols/
confidentiality-of-info-supplied-to-snz/safeguarding-confidentiality.aspx"
title="A method of preserving confidentiality and anonymity">
random rounding</a>
'

infotext <- div(class="overflowable",
  h3("How did Kiwis commute in 2018?"),
  p("This tool maps the 2018 census",
  a(href=
    "https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/", 
    "commuter data"), 
  "to help visualise transport ",
    "connections. It is ", a(href="http://petras.space", "Petra Lamborn's"),
    " entry for the ", em("There and Back Again"),
    a(href=
"https://www.stats.govt.nz/2018-census/there-and-back-
again-data-visualisation-competition", "data visualisation competition",
    .noWS = "after"), ". The employment data counts employed persons 15 years or ",
    "older who gave an employment location in the 2018 census, while the ",
    "education data counts people in education 15 years or older who gave an ",
    "education location in the 2018 census—including older highschoolers and ",
    "university students, but not e.g. primary school students."),
  h4("Options"),
  p("The top right panel (toggleable via the blue switch) allows ",
    "you to choose between visualising the people who commute from ",
    "and to each area to both employment and education, and between ",
    "showing numbers of people or ",
    "their primary mode of transportation."),
  p("Select an area by clicking on it; deselect by clicking it again ",
    "or clicking in the water. When an area is selected the map is ",
    "coloured according to the people who commute to or from that area ",
    "(including the people who commute within the area). When no area is ",
    "selected, colouring is according to commutes to or from all ",
    "localities. Hover over areas for a summary (on a tablet, hold press)."),
  h4("FAQ"),
  shiny::tags$blockquote("Why are so many areas marked as 'works at ",
                         "home'?"),
  p("Working from home includes working from your kitchen table, ",
    "but also farming. Anyone who is employed but does not commute ",
    "is included in this category."),
  shiny::tags$blockquote("Why is every number divisible by three?"),
  p("To ensure anonymity and the confidentiality of your census data, ",
    "Statistics New Zealand employs a technique called ",
    em(a(href=paste0(
      "http://archive.stats.govt.nz/about_us/legisln-policies-protocols/",
      "confidentiality-of-info-supplied-to-snz/safeguarding-confidentiality.aspx"),
         "random rounding", .noWS = "after"), .noWS = "after"), ". ",
    "This method rounds all values to a multiple of three, but one-third of ",
    "the time rounds to the second nearest multiple, rather than the closest. ",
    "Additionally, rounded values less than six are censored."),
    p("Note that this also means totals do not always add up."),
  shiny::tags$blockquote("In some views there are areas that are greyed ",
                         "out when looking at transport type, but not ",
                         "when looking at numbers of people. How does ",
                         "that work?"),
  p("As mentioned above some low values are censored to avoid providing ",
    "potentially identifiable information. However it is possible for the ",
    "total number of people to be large enough to escape censoring while ",
    "all transport", em("type"), "numbers are too small and so no most ",
    "common type can be determined."),
  div(class="scrollbuffer")
)

keyboardjs <- tags$head(tags$script(HTML("
$(function(){ 
  $(document).keyup(function(e) {
    switch(e.key) {
      case 'm':
        document.getElementsByName('radioeduemp')[0].checked = true;
        Shiny.onInputChange('radioeduemp', 'Employment')
        break;
      case 'd':
        document.getElementsByName('radioeduemp')[1].checked = true;
        Shiny.onInputChange('radioeduemp', 'Education')
        break;
      case 'f':
        document.getElementsByName('radioinout')[0].checked = true;
        Shiny.onInputChange('radioinout', 'res')
        break;
      case 't':
        document.getElementsByName('radioinout')[1].checked = true;
        Shiny.onInputChange('radioinout', 'work')
        break;
      case 'o':
        document.getElementsByName('radiocolour')[0].checked = true;
        Shiny.onInputChange('radiocolour', 'type')
        break;
      case 'u':
        document.getElementsByName('radiocolour')[1].checked = true;
        Shiny.onInputChange('radiocolour', 'number')
        break;
      default:
        break;
    }
  });
})
")))