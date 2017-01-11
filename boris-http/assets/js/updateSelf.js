var fetchNew;
fetchNew = function(){
  var xhr;
  xhr = new XMLHttpRequest();
  xhr.open('GET', window.location.href);
  xhr.responseType = "document";
  xhr.setRequestHeader('Accept', 'text/html');
  xhr.onload = function(){
    if (xhr.status !== 200) return;
    var elem = document.getElementsByTagName('body')[0];
    elem.parentNode.replaceChild(this.responseXML.body, elem);
  };
  return xhr.send();
};
window.setInterval(fetchNew, 1000 * 240);