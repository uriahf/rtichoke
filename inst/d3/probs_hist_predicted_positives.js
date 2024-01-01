// !preview r2d3 data=data.frame(cat = rep(c('(0,0.1]', '(0.1,0.2]', '(0.2,0.3]', '(0.3,0.4]', '(0.4,0.5]', '(0.5,0.6]', '(0.6,0.7]', '(0.7,0.8]', '(0.8,0.9]', '(0.9,1]'), 2),values =  c(runif(20)), name = rep(c('real_positives', 'real_negatives'), each = 10)) |> tidyr::pivot_wider(names_from = "name", values_from = "values"), container = 'div'
//
// r2d3: https://rstudio.github.io/r2d3
//

var checkRealPositives = document.getElementById('real-positives')
var checkRealNegatives = document.getElementById('real-negatives')

var checkboxes = [checkRealPositives, checkRealNegatives]

//console.log("checkboxes ", checkboxes)

//const sliderelse = document.getElementById//("sliderelse");


const sliderelseValues = document.getElementById("sliderelseValues");

const sliderelse = document.getElementById("filter_long-confusion-matrix_probability_threshold");

console.log(sliderelse)


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
     //console.log(x(d.data.cat) < x2(0.3))
     //console.log(i)
     return x(d.data.cat)})
   .attr("y", (d) => y(d[1]))
   .attr("height", (d) => y(d[0]) - y(d[1]))
   .attr("width", x.bandwidth())
   .attr("fill", function(d, i) {
     if (x(d.data.cat) < x2(1) ) {
       return "#F4FFF0"
     } else {
     return "#009e73"
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
     //console.log(x(d.data.cat) < x2(0.3))
     //console.log(i)
     return x(d.data.cat)})
   .attr("y", (d) => y(d[1]))
   .attr("height", (d) => y(d[0]) - y(d[1]))
   .attr("width", x.bandwidth())
   .attr("fill", function(d, i) {
     if (x(d.data.cat) < x2(1) ) {
       return "#FFF7F8"
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

//console.log("checkedValues ", checkedValues)


 
g.selectAll(".verticalline")
  .attr("x1", x2(sliderelse))  
  .attr("x2", x2(sliderelse))  
  
  svg.selectAll(".bucket-real-negatives rect")
    .attr('fill', function(d,i) {
    if (x(d.data.cat) <= x2(sliderelse) ) {
       return "#F4FFF0"
     } else {
     return "#009e73"
     }})
     
  svg.selectAll(".bucket-real-positives rect")
    .attr('fill', function(d,i) {
    if (x(d.data.cat) <= x2(sliderelse) ) {
       return "#FFF7F8"
     } else {
     return "#FAC8CD"
     }
    })
  
  
}

  

sliderelse.addEventListener("input", function() {
    updatePlot(Number(sliderelse.value))
  });
   
g.append("g")
    .attr("transform", `translate(0,${height})`)
    .call(d3.axisBottom(x));
    
 g.append("g").call(d3.axisLeft(y));
 

 

 
return svg.node();