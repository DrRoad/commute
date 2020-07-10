extracss <- "
#map {
  height: 100% !important;
  position: absolute !important;
  top: 0;
  left: 0;
}
#loading {
  cursor: progress !important;
  z-index: 1002;
}
#loading p {
  border-radius: 5px;
  background-color: rgba(255, 255, 255, 0.8);
  padding: 6px 8px;
  box-shadow: 0 0 15px rgba(0,0,0,0.2);
  font-size: 1.5em;
  font-weight: bold;
  margin: 0;
}
#mapcontrol, #infopanel {
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
  width: 20em;
  max-height: 95%;
  overflow: auto;
  -ms-overflow-style: none;  /* IE and Edge */
  scrollbar-width: none;  /* Firefox */
}
#mapcontrol::-webkit-scrollbar {
  display: none;
}
#infopanel {
  background-color: rgba(255, 255, 255, 0.9);
  padding: 10px;
  z-index: 1001;
}
#lochtml ul {
  padding-left: 15px;
}
.leaflet-container {
  background-color: #84e1e1;
}
#infobuttoncontainer {
  padding: 6px 8px;
  z-index: 1002;
  font: 14px/16px Arial, Helvetica, sans-serif;
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
  bottom:5px;
  right:0;
  background:linear-gradient(rgba(255, 255, 255, 0) 0px, rgba(255, 255, 255, 0.9));
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
.shortcut {
  text-decoration: underline 1px dotted;
}
#shortcutlist {
  columns: 20em 2;
}
kbd {
  color: black;
  background-color: #eee;
  border-radius: 4px;
  padding: 1px 6px;
  border-color: #444;
  border-width: 1px;
  -webkit-box-shadow: 1px 1px #444;
  box-shadow: 1px 1px #444;
  font-size: 120%;
  font-family: arial, Helvetica, sans-serif;
  cursor: pointer;
}
kbd:hover {
    background-color: #ddd;
    box-shadow: 0.5px 0.5px;
    -webkit-box-shadow: 0.5px 0.5px;
}
#infoint li {
  padding-bottom: 10px;
}
.radio-btn-icon-no {
    color: rgba(255, 255, 255, 0);
}
.btn.radiobtn {
    text-align: left;
}
#control2 .state label,
#infobuttoncontainer .state label
{
    width: 0px;
}
"

attribhtml <- '
<a href="http://leafletjs.com" 
title="A JS library for interactive maps">Leaflet</a> | <a 
href="http://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/"
title="Source data">
StatsNZ</a> | <a href="http://petras.space/" title="Hire me!">
Petra Lamborn</a> (<a href="https://git.petras.space/petra/commute"
title="Source code">Source</a>)
'

infotext <- div(class="overflowable", id="infoint", tabindex="2",
  h3("How did Kiwis commute in 2018?"),
  p("This tool maps the 2018 census",
  a(href=
    "https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/", 
    "commuter data"), 
  "to help visualise transport ",
    "connections. It is ", a(href="http://petras.space", title="Hire me!",
    "Petra Lamborn's"),
    " entry for the ", em("There and Back Again"),
    a(href=
"https://www.stats.govt.nz/2018-census/there-and-back-
again-data-visualisation-competition", "data visualisation competition",
    .noWS = "after"), 
    HTML(". The employment portion of the dataset captures employed persons ",
    "15 years or older who gave an employment address or location ",
    "in the 2018 census.",
    "The education dataset counts people in education 15 years or older who ",
    "gave an education location in the 2018 census, ",
    "including older highschoolers and ",
    "university students but not e.g. primary school students.")),
  h4("The data"),
  p("The 2018 New Zealand Census of Population and Dwellings",
    a(href=paste0("https://cdm20045.contentdm.oclc.org/digital/",
      "collection/p20045coll2/id/713/rec/3"), "questionnaire"), 
    "asked individuals for primary locations of employment and education ",
    "and their usual method of transportation. Stats NZ has",
    a(href=
"https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/",
      "released data"),
    "aggregated at the level of", 
    a(href=paste0(
"http://archive.stats.govt.nz/methods/classifications-and-standards/",
"classification-related-stats-standards/geographic-areas/pg4.aspx#gsc.tab=0"),
   "Statistical Area 2", .noWS = "after"), ".",
    shiny::tags$abbr(title = "Statistical Area 2", "SA2"),
    "boundaries typically enclose areas with a population of a few ",
    "thousand, corresponding approximately to urban suburbs and rural towns. ",
    "The shapes of these areas have been heavily simplified in this map ",
    "to reduce bandwidth and memory usage. ",
    "The original boundaries can be viewed and downloaded",
    a(href=
"https://datafinder.stats.govt.nz/layer/92212-statistical-area-2-2018-generalised/",
    "from the Stats NZ datafinder website", .noWS = "after"), "."),
  h4("Using this tool"),
  p("The options avaliable allow ",
    "you to select the employment or education datasets. ",
    "You may also choose to visualise the people who commute from ",
    "or to each area, and select between showing numbers of people or ",
    "their primary mode of transportation. \"Primary\" is calculated by ",
    "FPP rules: it means the most common, not the majority."),
  p("Select an area by clicking on it; deselect by clicking it again ",
    "or clicking the sea. When an area is selected the map is ",
    "coloured according to the people who commute to or from that area ",
    "(including the people who commute within the area). When no area is ",
    "selected, colouring is according to commutes to or from all ",
    "localities. Hover over areas for a summary (on a tablet, hold press)."),
  p("At rest the map is coloured by the most common type of transportation ",
    "used by people commuting to employment from each area."),
  h4("Keyboard shortcuts"),
  shiny::tags$ul(id="shortcutlist",
    shiny::tags$li(shiny::tags$kbd("I"), ": show/hide this info page"),
    shiny::tags$li(shiny::tags$kbd("M"), ": focus map"),
    shiny::tags$ul(
      shiny::tags$li(shiny::tags$kbd("+"), shiny::tags$kbd("-"),
                     ": zoom map"),
      shiny::tags$li(shiny::tags$kbd(HTML("&larr;")),
                     shiny::tags$kbd(HTML("&uarr;")),
                     shiny::tags$kbd(HTML("&rarr;")),
                     shiny::tags$kbd(HTML("&darr;")),
                     ": move map")
    ),
    shiny::tags$li(shiny::tags$kbd("S"), ": search map"),
    shiny::tags$li(shiny::tags$kbd("L"), ": show/hide options panel"),
    shiny::tags$li(shiny::tags$kbd("P"), ": show employment data"),
    shiny::tags$li(shiny::tags$kbd("D"), ": show education data"),
    shiny::tags$li(shiny::tags$kbd("F"), ": show people commuting from localities"),
    shiny::tags$li(shiny::tags$kbd("T"), ": show people commuting to localities"),
    shiny::tags$li(shiny::tags$kbd("O"), ": colour by most common commute type"),
    shiny::tags$li(shiny::tags$kbd("U"), ": colour by number of commuters")
  ),
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
  shiny::tags$blockquote("Does anyone commute to (or from?) the Chatham Islands?"),
  p("No, but some people commute within it. Of 249 people commuting to ",
    "employment, 105 travel by private car, 24 by company car, 18 walk, 12 ",
    "are passengers in cars driven by other people, and 93 work at home. ",
    "Of 84 people commuting to education, 39 take a schoolbus, 9 walk, 9 are ",
    "passengers in a car, and 15 are educated at home."),
  div(class="scrollbuffer")
)

keyboardjs <- tags$head(tags$script(HTML("
$(function(){ 
  $(document).keyup(function(e) {
    var active = document.activeElement
    if (active.id == 'searchtext9') return;
    switch(e.key) {
      case 'p':
      case 'P':
        document.getElementsByName('radioeduemp')[0].click();
        Shiny.onInputChange('radioeduemp', 'Employment')
        break;
      case 'd':
      case 'D':
        document.getElementsByName('radioeduemp')[1].click();
        Shiny.onInputChange('radioeduemp', 'Education')
        break;
      case 'f':
      case 'F':
        document.getElementsByName('radioinout')[0].click();
        Shiny.onInputChange('radioinout', 'res')
        break;
      case 't':
      case 'T':
        document.getElementsByName('radioinout')[1].click();
        Shiny.onInputChange('radioinout', 'work')
        break;
      case 'o':
      case 'O':
        document.getElementsByName('radiocolour')[0].click();
        Shiny.onInputChange('radiocolour', 'type')
        break;
      case 'u':
      case 'U':
        document.getElementsByName('radiocolour')[1].click();
        Shiny.onInputChange('radiocolour', 'number')
        break;
      case 'i':
      case 'I':
        shinyjs.click('mapinfobutton')
        break;
      case 'm':
      case 'M':
        document.getElementById('map').focus()
        break;
      case 's':
      case 'S':
        var evObj = document.createEvent('Events');
        evObj.initEvent('click', true, false);
        document.getElementsByClassName('search-button')[0].dispatchEvent(evObj);
        break;
      case 'l':
      case 'L':
        document.getElementById('controlswitch').click();
        break;
      default:
        break;
    }
  });
})
")))