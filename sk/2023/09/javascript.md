# Javascript


<html>
  <head>
    <link rel="stylesheet" type="text/css" href="/static/main.css" media="screen" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  </head>
  <script>
  function foo() {
      document.getElementById("output").innerText = xmlHttp.responseText      
  }
</script>
    
  <body onload="foo()">
    <div id="output">
    </div>  
  </body>
  
</html>


alert

console.log(text)




var xmlHttp = new XMLHttpRequest();
xmlHttp.open( "GET", url = 'http://192.168.43.49:5000/static/recom/test2.csv', false ); 
xmlHttp.send( null );
document.getElementById("output").innerText = xmlHttp.responseText








function get_data() {

  url = "http://192.168.43.49:5000/static/recom/movie_title_int.json";
  return fetch(url)
         .then((response) => { if(response.ok)  return response.json(); })
         .then((json) => {
            const out = json['Jumanji (1995)']
            return out;
         });
}
       
function foo() {

  this.get_data().then((out) => {
    document.getElementById("output").innerText = out
   });

}
















Kaynaklar

[1] https://www.w3schools.com/howto/howto_js_autocomplete.asp  