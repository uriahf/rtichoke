// !preview r2d3 data=data.frame(cat = rep(c('(0,0.1]', '(0.1,0.2]', '(0.2,0.3]', '(0.3,0.4]', '(0.4,0.5]', '(0.5,0.6]', '(0.6,0.7]', '(0.7,0.8]', '(0.8,0.9]', '(0.9,1]'), 2),values =  c(runif(20)), name = rep(c('real_positives', 'real_negatives'), each = 10)) |> tidyr::pivot_wider(names_from = "name", values_from = "values"), container = 'div', options = list(highlightMetrics = list("TP" = TRUE, "TN" = TRUE, "FP" = TRUE, "FN" = TRUE))
//
// r2d3: https://rstudio.github.io/r2d3
//

// console.log(options.listenTO)
console.log(options.outerDiv)

// console.log(options.highlightMetrics.TP)
const slidervaluetolisten = document.getElementById(options.listenTO);
//const sliderelse = document.getElementById("filter_long-confusion-matrix_probability_threshold");

slidervaluetolisten.addEventListener("input", function() {
    updatePlot(Number(slidervaluetolisten.value))
 });


const marginTop = 25;
const marginRight = 20;
const marginBottom = 35;
const marginLeft = 60;

const x = d3
  .scaleBand()
  .domain(Array.from(new Set(data.map((d) => d.cat))))
  .range([0, width]);
  
const x2 = d3.scaleLinear()
  .domain([0, 1])
  .range([0, width])  



const tooltip = d3.select("#" + options.outerDiv)
//const tooltip = d3.select("body")
  .append("div")
  .attr("class", "tooltip")
  .style("opacity", 1)
  .style("position", "absolute")
  .style("background-color", "blue")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", "10px");



const keys = ['real_positives', 'real_negatives']

stackLayout = d3.stack().keys(keys)(data)

//console.log(stackLayout)

const y = d3
  .scaleLinear()
  .domain([0, d3.max(stackLayout, (d) => d3.max(d, (d) => d[1]))])
  .nice()
  .range([height, 0]);
    
const color = d3
  .scaleOrdinal()
  .domain(keys)
  .range(d3.schemeCategory10);
  
var mouseleave = function() {
    tooltip
      .style("opacity", 0)
      
  }  

var mouseover = function(ev, d, tooltipcolor) {
  //console.log(x(d.data.cat) > x2(Number(sliderelse.value)))
  
  
  tooltip
      .style("opacity", 1)
      .style("background-color", tooltipcolor)
     
  let loc = d3.pointer(ev)
  
  //console.log(d3.mouse(this)[1])
  

  tooltip.html(
    d[1] - d[0] + " Observations<br>" + "Probability Percentile: " + d.data.cat)
    //    .style("top",  (d3.mouse(this)[1]) + "px")
    //    .style("left", (d3.mouse(this)[0]) + "px")
      .style("left", loc[0] + 90 +  "px") // It is important to put the +90: other wise the tooltip is exactly where the point is an it creates a weird effect
      .style("top", loc[1] + 30 + "px")


  }  
  
var barHeight = Math.ceil(height / data.length);

const svg = div.append("svg")
  .attr("width", width + marginLeft + marginRight)
  .attr("height", height + marginTop + marginBottom)
      .style("border", "1px dotted #000");
      
const g = svg
  .append("g")
  .attr("transform", `translate(${marginLeft}, ${marginTop})`);
  
  
   
g.append("g")
   .selectAll(".bucket-real-negatives")
  .data(stackLayout)
   .join("g")
   .attr("class", "bucket-real-negatives")
   .filter(function(d, i) {
     return i == 0
   })
   .selectAll("rect")
   .data((d) => d)
   .join("rect")
    .attr("x", function(d, i){
     return x(d.data.cat)})
   .attr("y", (d) => y(d[1]))
   .attr("height", (d) => y(d[0]) - y(d[1]))
   .attr("width", x.bandwidth())
   .attr('fill', function(d,i) {
    if (x(d.data.cat) < x2(1) ) {
       return "#FAC8CD"
     } else {
     return "#009e73"
     }})
     .on("mouseover", (ev, d) => mouseover(ev, d))
 .on("mousemove", function(ev, d) { 
   d3.select(this)
      .style("stroke", "black")
      .style("stroke-width", "2")
  .on("mouseout", function() {
    
    d3.select(this)
      .style("stroke-width", "0")
    
    mouseleave()

  })
   
   if (x(d.data.cat) <= x2(Number(slidervaluetolisten.value)) ) {
       return mouseover(ev, d, "#FAC8CD")
     } else {
     return mouseover(ev, d, "#009e73")
     }
 })

g.append("g")
   .selectAll(".bucket-real-positives")
  .data(stackLayout)
   .join("g")
   .attr("class", "bucket-real-positives")
   .filter(function(d, i) {
     return i == 1
   })
   .selectAll("rect")
   .data((d) => d)
   .join("rect")
    .attr("x", function(d, i){
     return x(d.data.cat)})
   .attr("y", (d) => y(d[1]))
   .attr("height", (d) => y(d[0]) - y(d[1]))
   .attr("width", x.bandwidth())
   .attr('fill', function(d,i) {
    if (x(d.data.cat) < x2(1) ) {
       return "#009e73"
     } else {
     return "#FAC8CD"
     }
    })
    
g.append("line")
  .attr("class", "verticalline")
  .attr("x1", x2(1))  //<<== change your code here
  .attr("y1", 0)
  .attr("x2", x2(1))  //<<== and here
  .attr("y2", height )
  .style("stroke-dasharray", ("3, 3"))  // <== This line here!!
  .style("stroke-width", 2)
  .style("stroke", "black")
  .style("fill", "none");   
  
 
function updatePlot(sliderelse, checkedValues) {

g.selectAll(".verticalline")
  .attr("x1", x2(sliderelse))  //<<== change your code here
  .attr("x2", x2(sliderelse))  //<<== and here
  
  svg.selectAll(".bucket-real-negatives rect")
    .attr('fill', function(d,i) {
    if (x(d.data.cat) < x2(sliderelse) ) {
       return "#FAC8CD"
     } else {
     return "#009e73"
     }})
     
  svg.selectAll(".bucket-real-positives rect")
    .attr('fill', function(d,i) {
    if (x(d.data.cat) < x2(sliderelse) ) {
       return "#009e73"
     } else {
     return "#FAC8CD"
     }
    })
  
  
}


   
g.append("g")
    .attr("transform", `translate(0,${height})`)
    .call(d3.axisBottom(x));
    
 g.append("g").call(d3.axisLeft(y));
 

 

 
return svg.node();