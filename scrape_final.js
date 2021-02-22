var url ='http://metsahyvinvointi.fi/blog/metsahyvinvoinnin-seminaarit-metsapaivilla-2019/';
var i ='814';
var page = new WebPage();
var fs = require('fs'); 

page.settings.userAgent = 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0';

page.open(url, function (status) { 
         just_wait(); 
}); 


function just_wait() { 
    setTimeout(function() { 
              fs.write(i + '.html', page.content, 'w'); 
           phantom.exit(); 
    }, 2500); 
}  
