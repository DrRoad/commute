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
#infopanel {
  display: none;
  padding: 30px;
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
"